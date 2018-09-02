(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit uCreateProcessAsync;
{
  Usage:

    Call CreateProcessAsync just like regular CreateProcess, except that the function
    accepts a callback which gets called when the process finishes. You can also store
    a Thread instance that is waiting for the process to finish.

    Possible endings:

    1) Process fails to start. In that case, CreateProcessAsync returns FALSE and no
       thread gets created.

    2) Process terminates before the caller application does: OnProcessFinished gets
       called, then the Thread is destroyed automatically.

    3) Application wants to terminate while the process is still running: Run
       Thread.Terminate followed optionally by Thread.WaitFor (it's not required,
       because the Terminate instructs the Thread not to fire the event in any
       case, so even if the event's object is freed, nothing bad will happen).
       The thread will finish almost immediately anyway, it won't wait for the
       called process to finish.
}

{$INCLUDE 'pepak.inc'}
{$DEFINE REGISTERWAITFORSINGLEOBJECT}
  // Use RegisterWaitForSingleObject rather than a dedicated thread. It is more
  // efficient and seems to be more stable, too, but is only available with
  // Windows 2000 and newer.

interface

uses
  Classes, Windows;

type
  TProcessFinishedEvent = procedure (Sender: TThread; hProcess, hThread: THandle; ResultCode: DWORD) of object;

function CreateProcessAsync(lpApplicationName: PChar; lpCommandLine: PChar;
  lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: Pointer;
  lpCurrentDirectory: PChar; const lpStartupInfo: TStartupInfo;
  var lpProcessInformation: TProcessInformation;
  out Thread: TThread;
  out WaitHandle: THandle;
  OnProcessFinished: TProcessFinishedEvent
  ): BOOL;

{$IFNDEF DELPHI8_UP}
function UnregisterWait(WaitHandle: THandle): BOOL; stdcall;
{$ENDIF}

implementation

{$IFNDEF DELPHI8_UP}
const
  WT_EXECUTEONLYONCE = $8;

type
  TWaitOrTimerCallback
    = procedure(lpParameter: Pointer; TimerOrWaitFired: BOOL); stdcall;

  TRegisterWaitForSingleObjectFn
    = function(var phNewWaitObject: THandle;
               hObject: THandle;
               Callback: TWaitOrTimerCallback;
               Context: Pointer;
               dwMilliseconds: ULONG;
               dwFlags: ULONG
               ): BOOL; stdcall;

  TUnregisterWaitFn
    = function(WaitHandle: THandle): BOOL; stdcall;

var
  KernelDll: THandle;
  RegisterWaitForSingleObject: TRegisterWaitForSingleObjectFn;
  UnregisterWaitFn: TUnregisterWaitFn;

function UnregisterWait(WaitHandle: THandle): BOOL;
begin
  if Assigned(UnregisterWaitFn) then
    Result := UnregisterWaitFn(WaitHandle)
  else
    Result := True;
end;

{$ENDIF}

{$IFDEF REGISTERWAITFORSINGLEOBJECT}
type
  PRegisteredProcessInfo = ^TRegisteredProcessInfo;
  TRegisteredProcessInfo = record
    hProcess, hThread: THandle;
    dwProcessId, dwThreadId: DWORD;
    WaitHandle: THandle;
    RealCallback: TProcessFinishedEvent;
    end;

procedure RegisterWaitForSingleObjectCallback(lpParameter: Pointer; TimerOrWaitFired: BOOL); stdcall;
var ProcInfo: PRegisteredProcessInfo;
    ResultCode: DWORD;
begin
  ProcInfo := lpParameter;
  try
    UnregisterWait(ProcInfo^.WaitHandle);
    if not GetExitCodeProcess(ProcInfo^.hProcess, ResultCode) then
      ResultCode := $ffffffff;
    if Assigned(ProcInfo^.RealCallback) then
      ProcInfo^.RealCallback(nil, ProcInfo^.hProcess, ProcInfo^.hThread, ResultCode);
    CloseHandle(ProcInfo^.hProcess);
    CloseHandle(ProcInfo^.hThread);
  finally
    FreeMem(ProcInfo);
    end;
end;
{$ENDIF}

type
  TWaitForProcessEndThread = class(TThread)
    protected
      ResultCode: DWORD;
      hProcess: THandle;
      hThread: THandle;
      OnProcessFinished: TProcessFinishedEvent;
      procedure Execute; override;
      procedure SyncFinished; virtual;
    public
      constructor Create(AProcess, AThread: THandle; AOnProcessFinished: TProcessFinishedEvent); virtual;
      end;

{ TWaitForProcessEndThread }

constructor TWaitForProcessEndThread.Create(AProcess, AThread: THandle; AOnProcessFinished: TProcessFinishedEvent);
begin
  inherited Create(True);
  hProcess := AProcess;
  hThread := AThread;
  OnProcessFinished := AOnProcessFinished;
  FreeOnTerminate := True;
  Resume;
end;

procedure TWaitForProcessEndThread.Execute;
var b: boolean;
begin
  while not Terminated do
    begin
    b := GetExitCodeProcess(hProcess, ResultCode);
    if b and (ResultCode = STILL_ACTIVE) then
      Sleep(100)
    else
      begin
      WaitForSingleObject(hProcess, INFINITE);  // asi zbytecne
      CloseHandle(hProcess);
      CloseHandle(hThread);
      if not b then
        ResultCode := $ffffffff;
      Synchronize(SyncFinished);
      Break;
      end;
    end;
end;

procedure TWaitForProcessEndThread.SyncFinished;
begin
  if not Terminated then
    if Assigned(OnProcessFinished) then
      OnProcessFinished(Self, hProcess, hThread, ResultCode);
end;

{ CreateProcessAsync }

function CreateProcessAsync;
{$IFDEF REGISTERWAITFORSINGLEOBJECT}
var ProcInfo: PRegisteredProcessInfo;
{$ENDIF}
begin
  Thread := nil;
  WaitHandle := 0;
  Result := CreateProcess(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
  if Result then
    begin
    {$IFDEF REGISTERWAITFORSINGLEOBJECT}
    {$IFNDEF DELPHI8_UP}
    if Assigned(RegisterWaitForSingleObject) then
    {$ENDIF}
      begin
      GetMem(ProcInfo, Sizeof(TRegisteredProcessInfo));
      ProcInfo^.hProcess := lpProcessInformation.hProcess;
      ProcInfo^.hThread := lpProcessInformation.hThread;
      ProcInfo^.dwProcessId := lpProcessInformation.dwProcessId;
      ProcInfo^.dwThreadId := lpProcessInformation.dwThreadId;
      ProcInfo^.WaitHandle := 0;
      ProcInfo^.RealCallback := OnProcessFinished;
      if RegisterWaitForSingleObject(WaitHandle, lpProcessInformation.hProcess, @RegisterWaitForSingleObjectCallback, ProcInfo, INFINITE, WT_EXECUTEONLYONCE) then
        begin
        ProcInfo^.WaitHandle := WaitHandle;
        Exit;
        end
      else
        FreeMem(ProcInfo);
      end;
    {$ENDIF}
    Thread := TWaitForProcessEndThread.Create(lpProcessInformation.hProcess, lpProcessInformation.hThread, OnProcessFinished);
    end
  else
    Thread := nil;
end;

initialization
  {$IFNDEF DELPHI8_UP}
  KernelDll := LoadLibrary('kernel32.dll');
  if KernelDll <> 0 then
    begin
    RegisterWaitForSingleObject := GetProcAddress(KernelDll, 'RegisterWaitForSingleObject');
    UnregisterWaitFn := GetProcAddress(KernelDll, 'UnregisterWait');
    end
  else
    begin
    RegisterWaitForSingleObject := nil;
    UnregisterWaitFn := nil;
    end;
  {$ENDIF}

finalization
  {$IFNDEF DELPHI8_UP}
  if KernelDll <> 0 then
    begin
    FreeLibrary(KernelDll);
    KernelDll := 0;
    RegisterWaitForSingleObject := nil;
    UnregisterWaitFn := nil;
    end;
  {$ENDIF}

end.
