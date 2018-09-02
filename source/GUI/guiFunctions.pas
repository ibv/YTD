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

unit guiFunctions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Windows, ShellApi,
  {$IFDEF SETUP}
    uSetup,
    {$IFNDEF GUI_WINAPI}
      Forms, Dialogs,
      {$IFDEF DELPHIXE4_UP}
      UITypes,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  SynaCode, SynaUtil,
  uFunctions, uDownloadList, uMessages, uStrings, uOptions;

function GetProgressStr(DoneSize, TotalSize: int64): string;
procedure ReportBug(DownloadList: TDownloadList; Index: integer);
function IsHttpProtocol(const Url: string): boolean;
procedure NewVersionFound(Options: TYTDOptions; const Url: string; OwnerHandle: THandle);

{$IFDEF SINGLEINSTANCE}
procedure RegisterMainInstance(const MainFormHandle: THandle);
procedure UnregisterMainInstance(const MainFormHandle: THandle);
function FindMainInstance(out MainFormHandle: THandle): boolean;
{$ENDIF}

implementation

function GetProgressStr(DoneSize, TotalSize: int64): string;
var n: int64;
begin
  Result := PrettySize(DoneSize);
  if TotalSize > 0 then
    begin
    n := 1000*DoneSize div TotalSize;
    Result := Format('%s (%d.%d%%)', [Result, n div 10, n mod 10]);
    end
end;

procedure ReportBug(DownloadList: TDownloadList; Index: integer);
var BugReportUrl: string;
begin
  BugReportUrl := Format(BUGREPORT_URL,
                       [ APPLICATION_VERSION,
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList.Urls[Index]))),
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList[Index].Downloader.LastErrorMsg)))
                       ]);
  Run(BugReportUrl);
end;

function IsHttpProtocol(const Url: string): boolean;
var Protocol, User, Password, Host, Port, Path, Params: string;
begin
  ParseUrl(Url, Protocol, User, Password, Host, Port, Path, Params);
  if AnsiCompareText(Protocol, 'http') = 0 then
    Result := True
  else if AnsiCompareText(Protocol, 'https') = 0 then
    Result := True
  else
    Result := False;
end;

procedure NewVersionFound(Options: TYTDOptions; const Url: string; OwnerHandle: THandle);
{$IFDEF SETUP}
var FileName: string;
{$ENDIF}
begin
  if Url <> '' then
    {$IFDEF SETUP}
      {$IFDEF GUI_WINAPI}
      if MessageBox(OwnerHandle, PChar(MSG_DOWNLOAD_OR_UPGRADE), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = idYes then
      {$ELSE}
      if MessageDlg(MSG_DOWNLOAD_OR_UPGRADE, mtInformation, [mbYes, mbNo], 0) = idYes then
      {$ENDIF}
        begin
        if Options.DownloadNewestVersion(FileName) then
          if (AnsiCompareText(ExtractFileExt(FileName), '.exe') = 0) and Run(FileName, Format('%s "%s"', [SETUP_PARAM_UPGRADE_GUI, ExtractFilePath(ParamStr(0))])) then
            {$IFDEF GUI_WINAPI}
            ExitProcess(0)
            {$ELSE}
            Application.Terminate
            {$ENDIF}
          else
            {$IFDEF GUI_WINAPI}
            MessageBox(OwnerHandle, PChar(Format(MSG_FAILED_TO_UPGRADE, [FileName])), PChar(APPLICATION_TITLE), MB_OK or MB_ICONSTOP or MB_TASKMODAL)
            {$ELSE}
            MessageDlg(Format(MSG_FAILED_TO_UPGRADE, [FileName]), mtError, [mbOK], 0)
            {$ENDIF}
        else
          {$IFDEF GUI_WINAPI}
          MessageBox(OwnerHandle, PChar(MSG_FAILED_TO_DOWNLOAD_UPGRADE), PChar(APPLICATION_TITLE), MB_OK or MB_ICONSTOP or MB_TASKMODAL);
          {$ELSE}
          MessageDlg(Format(MSG_FAILED_TO_DOWNLOAD_UPGRADE, [FileName]), mtError, [mbOK], 0)
          {$ENDIF}
        end
      else
    {$ENDIF}
    Run(Url, OwnerHandle);
end;

{$IFDEF SINGLEINSTANCE}

const
  MAXIMUM_INSTANCE_COUNT = 256;

type
  TInstanceInfo = packed record
    Count: integer;
    MainFormHandles: array[0..MAXIMUM_INSTANCE_COUNT-1] of THandle;
    end;
  PInstanceInfo = ^TInstanceInfo;
  
var
  InstanceMappingHandle: THandle = 0;
  InstanceInfo: PInstanceInfo = nil;

function GetInstanceMutex(out Mutex: THandle): boolean;
var
  MutexName: string;
begin
  Result := False;
  MutexName := StringReplace(ParamStr(0), '\', '/', [rfReplaceAll]) + ('*mutex' + {$IFDEF WIN64} '*x64' {$ELSE} '*x32' {$ENDIF} );
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if Mutex <> 0 then
    if WaitForSingleObject(Mutex, 1000) = WAIT_FAILED then
      begin
      ReleaseMutex(Mutex);
      CloseHandle(Mutex);
      end
    else
      Result := True;
end;

procedure RegisterMainInstance(const MainFormHandle: THandle);
var
  Dummy, Mutex: THandle;
begin
  if (MainFormHandle = INVALID_HANDLE_VALUE) or (MainFormHandle = 0) then
    Exit;
  FindMainInstance(Dummy);
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        if InstanceInfo^.Count < MAXIMUM_INSTANCE_COUNT then
          begin
          InstanceInfo^.MainFormHandles[InstanceInfo^.Count] := MainFormHandle;
          InstanceInfo^.Count := Succ(InstanceInfo^.Count);
          end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

procedure UnregisterMainInstance(const MainFormHandle: THandle);
var
  Mutex: THandle;
  i: integer;
begin
  if (MainFormHandle = INVALID_HANDLE_VALUE) or (MainFormHandle = 0) then
    Exit;
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        for i := 0 to Pred(InstanceInfo^.Count) do
          if InstanceInfo^.MainFormHandles[i] = MainFormHandle then
            begin
            InstanceInfo^.MainFormHandles[i] := 0;
            Break;
            end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

function FindMainInstance(out MainFormHandle: THandle): boolean;
var
  MappingName: string;
  Mutex: THandle;
  i: integer;
begin
  Result := False;
  if InstanceMappingHandle = 0 then
    begin
    InstanceInfo := nil;
    MappingName := StringReplace(ParamStr(0), '\', '/', [rfReplaceAll]) + ('*mapping' + {$IFDEF WIN64} '*x64' {$ELSE} '*x32' {$ENDIF} );
    InstanceMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, Sizeof(TInstanceInfo), PChar(MappingName));
    if InstanceMappingHandle <> 0 then
      InstanceInfo := MapViewOfFile(InstanceMappingHandle, FILE_MAP_ALL_ACCESS, 0, 0, Sizeof(TInstanceInfo));
    end;
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        for i := 0 to Pred(InstanceInfo^.Count) do
          if InstanceInfo^.MainFormHandles[i] <> 0 then
            begin
            MainFormHandle := InstanceInfo^.MainFormHandles[i];
            Result := True;
            Break;
            end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

{$ENDIF}

initialization

finalization
  {$IFDEF SINGLEINSTANCE}
    if InstanceInfo <> nil then
      if UnmapViewOfFile(InstanceInfo) then
        InstanceInfo := nil;
    if InstanceMappingHandle <> INVALID_HANDLE_VALUE then
      if CloseHandle(InstanceMappingHandle) then
        InstanceMappingHandle := INVALID_HANDLE_VALUE;
  {$ENDIF}

end.
