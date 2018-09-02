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

unit uSystem;
{$INCLUDE 'pepak.inc'}

{.$DEFINE USYSTEM_7Z}
{$DEFINE USYSTEM_CONSOLE}
{$DEFINE USYSTEM_SHELL}
{$DEFINE USYSTEM_FILES}
  {$DEFINE USYSTEM_FILES_CLEANUPTEMP}
{.$DEFINE USYSTEM_NET}
{.$DEFINE USYSTEM_APPLICATION}
{$DEFINE USYSTEM_WOW64}
{$DEFINE USYSTEM_INFO}

{$IFDEF WIN64}
  {$UNDEF USYSTEM_WOW64}
{$ENDIF}

interface

uses
  SysUtils, Classes, Windows,
  {$IFDEF USYSTEM_SHELL}
  ShellApi, ShlObj, ActiveX, 
  {$ENDIF}
  {$IFDEF USYSTEM_NET}
  WinSock,
  {$ENDIF}
  Messages;

{$IFDEF USYSTEM_CONSOLE}
type
  TConsoleOutputEvent = procedure(Handle: THandle; var Data; DataLength: integer) of object;

function GetConsoleOutput(const Command: string; Input, Output, Errors: TStream; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Output, Errors: TStream; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Output, Errors: TStream; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Input: TStream; var Output, Errors: TStringList; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Input: TStream; var Output, Errors: TStringList; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Input, Output, Errors: TStringList; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
function GetConsoleOutput(const Command: string; Input, Output, Errors: TStringList; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
{$ENDIF}

{$IFDEF USYSTEM_7Z}
type
  E7zError = class(EInOutError);

  T7zListItemClass = class of T7zListItem;
  T7zListItem = class
    Name: string;
    Size: int64;
    CompressedSize: int64;
    Date: TDateTime;
    Attr: integer;
    constructor Create; virtual;
    end;

  T7zStream = class(T7zListItem)
    Stream: TStream;
    constructor Create; override;
    destructor Destroy; override;
    end;

function Run7z(Handle: THandle; const Archive, Params: string; InStream, OutStream, ErrStream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function Run7z(Handle: THandle; const Archive, Params: string; OutStream, ErrStream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function Run7z(Handle: THandle; const Archive, Params: string; OutStrings, ErrStrings: TStringList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function Run7z(Handle: THandle; const Archive, Params: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function List7z(Handle: THandle; const Archive: string; List: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil; ItemClass: T7zListItemClass = nil): Boolean;
function Extract7z(Handle: THandle; const Archive: string; Stream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function Extract7z(Handle: THandle; const Archive: string; Streams: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function Extract7z(Handle: THandle; const Archive, DestFile: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function AddTo7z(Handle: THandle; const Archive, StreamName: string; Stream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function AddTo7z(Handle: THandle; const Archive: string; Streams: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function AddTo7z(Handle: THandle; const Archive, FileName: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function AddTo7z(Handle: THandle; const Archive: string; const FileNames: array of string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
function GetLastError7z: string;
{$ENDIF}

{$IFDEF USYSTEM_SHELL}
function RunFile(FileName: string): Integer; overload;
function RunFile(FileName, Params: string): Integer; overload;
function RunFile(FileName, Params, WorkDir: string): Integer; overload;
function RunFile(FileName, Params: string; Command: Word): Integer; overload;
function RunFile(FileName, Params, WorkDir: string; Command: Word): Integer; overload;
function WaitForEnd(FileName: string): Boolean; overload;
function WaitForEnd(FileName: string; out ResultCode: DWORD): Boolean; overload;
function WaitForEnd(FileName: string; Command: Word): Boolean; overload;
function WaitForEnd(FileName: string; Command: Word; out ResultCode: DWORD): Boolean; overload;
function WaitForEnd(FileName: string; Params: string; Command: Word): Boolean; overload;
function WaitForEnd(FileName: string; Params: string; Command: Word; out ResultCode: DWORD): Boolean; overload;
{$IFDEF USYSTEM_APPLICATION}
function WaitForExclusiveAccess(FN: string): Boolean;
{$ENDIF}
function GetComputerName: string;
function FindShortcutByTarget(Dir, Target: string; out FileName: string): boolean;
function CreateShortcut(FileName, Target: string): boolean;
{$ENDIF}

{$IFDEF USYSTEM_NET}
const
  MAX_IP_ADDRESSES = 256;
  IP_ADDRESS_SEPARATOR = ';';

type
  TIpAddressArray = array[0..Pred(MAX_IP_ADDRESSES)] of string;

function GetIpAddress(const Host: string; out Addresses: TIpAddressArray): integer; overload;
function GetIpAddress(const Host: string): string; overload;
function GetIpAddress: string; overload;
function GetMyHostName: string;
function IpAddressIs(const Host: string): boolean;
{$ENDIF}

{$IFDEF USYSTEM_FILES}
type
  TDriveInfo = record
    RootDir: string;
    DriveType: integer;
    Capacity: int64;
    DisplayName: string;
    TypeName: string;
    end;

  TDriveInfoArray = array of TDriveInfo;

function IsExclusiveAccess(FN: string): Boolean;
function IsItLocalDir(Dir: string; var LocalPath: string): Boolean;
function GetSystemDir: string;
function GetSpecialFolder(AFolder: integer): string;
function SystemTempDir: string;
function SystemTempFile(const Dir, Prefix: string): string; overload;
function SystemTempFile(const Dir, Prefix, Extension: string): string; overload;
function AddLastSlash(const Path: string): string;
procedure ForceDeleteDirectory(APath: string);
function GetFileSize(const FileName: string): int64;
function GetFileDateTime(const FileName: string; out CreationTime, ModificationTime, AccessTime: TDateTime): boolean;
function GetFileContent(const FileName: string): string;
function GetFileVersion(const FileName: string; out VersionHigh, VersionLow, BuildNumber: LongWord): boolean;
function GetDrives(out Drives: TDriveInfoArray; OnlyOfType: integer = -1): boolean;
{$ENDIF}

{$IFDEF USYSTEM_WOW64}
function Is64BitWindows: boolean;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
{$ENDIF}

{$IFDEF USYSTEM_INFO}
type
  TWindowsVersion = (wvUnknown, wvWin95, wvWin98, wvWinME, wvWinNT, wvWin2000, wvWinXP, wvWinServer2003, wvWinVista, wvWin7, wvWin8);

function GetWindowsVersion: TWindowsVersion;
{$ENDIF}

implementation

uses
  {$IFDEF USYSTEM_7Z} 
  RegExpr, 
  {$ENDIF}
  {$IFDEF USYSTEM_APPLICATION}
  Forms,
  {$ENDIF}
  uCompatibility;

{$IFDEF USYSTEM_CONSOLE}
function GetConsoleOutput(const Command: string; Input, Output, Errors: TStream; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; 
var
  CommandBuffer: array of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  HavePipes: boolean;
  PipeInputRead: THandle;
  PipeInputWrite: THandle;
  TmpPipeInputWrite: THandle;
  PipeOutputRead: THandle;
  TmpPipeOutputRead: THandle;
  PipeOutputWrite: THandle;
  PipeErrorsRead: THandle;
  TmpPipeErrorsRead: THandle;
  PipeErrorsWrite: THandle;
  ThisProcess: THandle;
  Buffer: Pointer;
  NumberOfBytesRead, NumberOfBytesAvailable: DWORD;

  function ReadFromPipe(Pipe: THandle; Stream: TStream; OnData: TConsoleOutputEvent): Boolean;
  begin
    Result := PeekNamedPipe(Pipe, Buffer, BufferSize, @NumberOfBytesRead, @NumberOfBytesAvailable, nil);
    if Result and (NumberOfBytesRead > 0) then
      begin
      Result := ReadFile(Pipe, Buffer^, BufferSize, NumberOfBytesRead, nil);
      if Result then
        begin
        if (Stream <> nil) then
          Stream.Write(Buffer^, NumberOfBytesRead);
        if (NumberOfBytesRead > 0) and (@OnData <> nil) then
          OnData(ProcessInfo.hProcess, Buffer^, NumberOfBytesRead);
        end;
      end
    else
      Result := False;
  end;

  function WriteToPipe(Pipe: THandle; Stream: TStream): Boolean;
  var n, n2 : Dword;
  begin
    Result := False;
    if Stream <> nil then
      begin
      n := Stream.Read(Buffer^, BufferSize);
      if n > 0 then
        Result := WriteFile(Pipe, Buffer^, n, n2, nil) and (n2 = n)
      end;
  end;

  procedure ClosePipe(var Handle: THandle);
  begin
    if Handle <> 0 then
      begin
      CloseHandle(Handle);
      Handle := 0;
      end;
  end;

  procedure ClosePipes;
  begin
    CloseHandle(PipeInputRead);
    CloseHandle(PipeInputWrite);
    CloseHandle(PipeOutputRead);
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsRead);
    CloseHandle(PipeErrorsWrite);
  end;

begin
  GetMem(Buffer, BufferSize);
  try
    FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);
    FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
    SecurityAttr.nLength := SizeOf(SecurityAttr);
    SecurityAttr.bInheritHandle := True;
    SecurityAttr.lpSecurityDescriptor := nil;
    ThisProcess := GetCurrentProcess;
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.hStdInput := PipeInputRead;
    StartupInfo.hStdOutput := PipeOutputWrite;
    StartupInfo.hStdError := PipeErrorsWrite;
    StartupInfo.wShowWindow := Visibility;
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    HavePipes := (Input <> nil) or (Output <> nil) or (Errors <> nil);
    if HavePipes then
      begin
      StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
      // Je kriticky dulezite vytvaret roury takhle - ze ji vytvorim, pak si z ni
      // udelam kopii a pak tu puvodni zrusim
      CreatePipe(PipeInputRead, TmpPipeInputWrite, @SecurityAttr, 0);
      CreatePipe(TmpPipeOutputRead, PipeOutputWrite, @SecurityAttr, 0);
      CreatePipe(TmpPipeErrorsRead, PipeErrorsWrite, @SecurityAttr, 0);
      DuplicateHandle(ThisProcess, TmpPipeInputWrite, ThisProcess, @PipeInputWrite, 0, false, DUPLICATE_SAME_ACCESS);
      DuplicateHandle(ThisProcess, TmpPipeOutputRead, ThisProcess, @PipeOutputRead, 0, false, DUPLICATE_SAME_ACCESS);
      DuplicateHandle(ThisProcess, TmpPipeErrorsRead, ThisProcess, @PipeErrorsRead, 0, false, DUPLICATE_SAME_ACCESS);
      CloseHandle(TmpPipeInputWrite);
      CloseHandle(TmpPipeOutputRead);
      CloseHandle(TmpPipeErrorsRead);
      StartupInfo.hStdInput := PipeInputRead;
      StartupInfo.hStdOutput := PipeOutputWrite;
      StartupInfo.hStdError := PipeErrorsWrite;
      end
    else
      begin
      PipeInputRead := 0;
      PipeInputWrite := 0;
      PipeOutputRead := 0;
      PipeOutputWrite := 0;
      PipeErrorsRead := 0;
      PipeErrorsWrite := 0;
      end;
    // Pro funkcnost v Unicode je treba zajistit, ze Command je zapisovatelny.
    SetLength(CommandBuffer, Succ(Length(Command)));
    StrPCopy(@CommandBuffer[0], Command);
    try
      if CreateProcess(nil, PChar(CommandBuffer), nil, nil, true,
        CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or Priority, nil, nil,
        StartupInfo, ProcessInfo)
      then
        begin
        Result := true;
        repeat
          GetExitCodeProcess(ProcessInfo.hProcess, ResultCode);
          if Input <> nil then
            if (Input.Size - Input.Position) > 0 then
              begin
              while WriteToPipe(PipeInputWrite, Input) do
                ;
              end
            else
              begin
              Input := nil;
              ClosePipe(PipeInputWrite);
              end;
          if HavePipes then
            while ReadFromPipe(PipeOutputRead, Output, OnStdOutput) do
              ;
          if HavePipes then
            while ReadFromPipe(PipeErrorsRead, Errors, OnErrOutput) do
              ;
          Sleep(200);
        until ResultCode <> STILL_ACTIVE;
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);  // asi zbytecne
        GetExitCodeProcess(ProcessInfo.hProcess, ResultCode); // asi zbytecne
        CloseHandle(ProcessInfo.hProcess);
        end
      else
        begin
        Result := false;
        ResultCode := $ffffff;
        end;
    finally
      ClosePipes;
      end;
  finally
    FreeMem(Buffer);
    end;
end;

function GetConsoleOutput(const Command: string; Output, Errors: TStream; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
begin
  Result := GetConsoleOutput(Command, nil, Output, Errors, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
end;

function GetConsoleOutput(const Command: string; Output, Errors: TStream; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
var ResultCode: Dword;
begin
  Result := GetConsoleOutput(Command, Output, Errors, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
end;

function GetConsoleOutput(const Command: string; Input: TStream; var Output, Errors: TStringList; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
var OutStr, ErrStr : TMemoryStream;
begin
  if Output <> nil then
    Output.Clear;
  if Errors <> nil then
    Errors.Clear;
  OutStr := TMemoryStream.Create;
  try
    ErrStr := TMemoryStream.Create;
    try
      Result := GetConsoleOutput(Command, Input, OutStr, ErrStr, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
      if Result then
        begin
        OutStr.Position := 0;
        if Output <> nil then
          Output.LoadFromStream(OutStr);
        ErrStr.Position := 0;
        if Errors <> nil then
          Errors.LoadFromStream(ErrStr);
        end;
    finally
      ErrStr.Free;
      end;
  finally
    OutStr.Free;
    end;
end;

function GetConsoleOutput(const Command: string; Input: TStream; var Output, Errors: TStringList; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean; overload;
var ResultCode: Dword;
begin
  Result := GetConsoleOutput(Command, Input, Output, Errors, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
end;

function GetConsoleOutput(const Command: string; Input, Output, Errors: TStringList; out ResultCode: Dword; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean;
var InStr: TMemoryStream;
begin
  InStr := TMemoryStream.Create;
  try
    if Input <> nil then
      Input.SaveToStream(InStr);
    InStr.Position := 0;
    Result := GetConsoleOutput(Command, InStr, Output, Errors, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
  finally
    InStr.Free;
    end;
end;

function GetConsoleOutput(const Command: string; Input, Output, Errors: TStringList; Visibility: Integer = SW_SHOW; Priority: integer = NORMAL_PRIORITY_CLASS; OnStdOutput: TConsoleOutputEvent = nil; OnErrOutput: TConsoleOutputEvent = nil; BufferSize: integer = 1048576): Boolean;
var ResultCode: Dword;
begin
  Result := GetConsoleOutput(Command, Input, Output, Errors, ResultCode, Visibility, Priority, OnStdOutput, OnErrOutput, BufferSize);
end;
{$ENDIF}

{$IFDEF USYSTEM_7Z}
constructor T7zListItem.Create;
begin
  inherited Create;
end;

constructor T7zStream.Create;
begin
  inherited Create;
end;

destructor T7zStream.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

var SevenZipError: string = '';

function GetLastError7z: string;
begin
  Result := SevenZipError;
end;

function Run7z(Handle: THandle; const Archive, Params: string; InStream, OutStream, ErrStream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
var ErrCode: Dword;
    ErrStr: TStringStream;
    s : string;
    OnStdOutput : TConsoleOutputEvent;
begin
  s := '7z ' + Params + ' "' + Archive + '"';
  if Length(Password) > 0 then
    s := s + ' -p' + Password;
  OnStdOutput := nil;
  ErrStr := TStringStream.Create('');
  try
    Result := GetConsoleOutput(s, InStream, OutStream, ErrStr, ErrCode, SW_HIDE, NORMAL_PRIORITY_CLASS, OnStdOutput, OnErrOutput);
    ErrStr.Position := 0;
    if ErrStream <> nil then
      ErrStream.CopyFrom(ErrStr, ErrStr.Size);
    SevenZipError := ErrStr.DataString;
    if Result and (ErrCode <> 0) then
      Result := False;
    if not Result then
      Raise E7zError.Create(ErrStr.DataString);
  finally
    ErrStr.Free;
    end;
end;

function Run7z(Handle: THandle; const Archive, Params: string; OutStream, ErrStream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
begin
  Result := Run7z(Handle, Archive, Params, nil, OutStream, ErrStream, ShowProgress, Password, OnErrOutput);
end;

function Run7z(Handle: THandle; const Archive, Params: string; OutStrings, ErrStrings: TStringList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
var OutStr, ErrStr: TMemoryStream;
begin
  if OutStrings <> nil then
    OutStrings.Clear;
  if ErrStrings <> nil then
    ErrStrings.Clear;
  OutStr := TMemoryStream.Create;
  try
    ErrStr := TMemoryStream.Create;
    try
      Result := Run7z(Handle, Archive, Params, OutStr, ErrStr, ShowProgress, Password, OnErrOutput);
      if Result then
        begin
        if OutStrings <> nil then
          begin
          OutStr.Position := 0;
          OutStrings.LoadFromStream(OutStr);
          end;
        if ErrStrings <> nil then
          begin
          ErrStr.Position := 0;
          ErrStrings.LoadFromStream(ErrStr);
          end;
        end;
    finally
      ErrStr.Free;
      end;
  finally
    OutStr.Free;
    end;
end;

function Run7z(Handle: THandle; const Archive, Params: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean; overload;
begin
  Result := Run7z(Handle, Archive, Params, nil, nil, nil, ShowProgress, Password, OnErrOutput);
end;

resourcestring
  re7zListStart = '^-{10}[- ]+$';
  re7zListItem = '^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) ([.DSHRA]{5}) +([0-9]+) +([0-9]+ +)?(.+)$';

function List7z(Handle: THandle; const Archive: string; List: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil; ItemClass: T7zListItemClass = nil): Boolean;
var L: TStringList;
    i : integer;
    s : string;
    RE: TRegExpr;
    Item: T7zListItem;
begin
  List.Clear;
  if ItemClass = nil then
    ItemClass := T7zListItem;
  L := TStringList.Create;
  try
    Result := Run7z(Handle, Archive, 'l', L, nil, ShowProgress, Password, OnErrOutput);
    RE := TRegExpr.Create;
    try
      RE.ModifierI := True;
      RE.ModifierG := True;
      RE.ModifierM := False;
      // Najdu zacatek seznamu souboru
      RE.Expression := re7zListStart;
      i := 1;
      while (i <= L.Count) and (not RE.Exec(L[Pred(i)])) do //
        Inc(i);
      // Ted budu prochazet jednotlive soubory
      RE.Expression := re7zListItem;
      while (i < L.Count) and RE.Exec(L[i]) do
        begin
        Item := ItemClass.Create;
        Item.Name := Trim(RE.Match[10]);
        Item.Size := StrToInt(RE.Match[8]);
        Item.CompressedSize := StrToIntDef(Trim(RE.Match[9]), -1);
        Item.Date := EncodeDate(StrToInt(RE.Match[1]),StrToInt(RE.Match[2]),StrToInt(RE.Match[3])) +
                     EncodeTime(StrToInt(RE.Match[4]),StrToInt(RE.Match[5]),StrToInt(RE.Match[6]), 0);
        Item.Attr := 0;
        s := RE.Match[7];
        if Pos('D', s) > 0 then Inc(Item.Attr, faDirectory);
        if Pos('R', s) > 0 then Inc(Item.Attr, faReadOnly);
        if Pos('S', s) > 0 then Inc(Item.Attr, faSysFile);
        if Pos('H', s) > 0 then Inc(Item.Attr, faHidden);
        if Pos('A', s) > 0 then Inc(Item.Attr, faArchive);
        List.Add(Item);
        Inc(i);
        end;
    finally
      RE.Free;
      end;
  finally
    L.Free;
    end;
end;

function Extract7z(Handle: THandle; const Archive: string; Streams: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
var i : integer;
    Str: TMemoryStream;
begin
  Result := List7z(Handle, Archive, Streams, ShowProgress, Password, OnErrOutput, T7zStream);
  if Result then
    begin
    Str := TMemoryStream.Create;
    try
      Result := Extract7z(Handle, Archive, Str, ShowProgress, Password, OnErrOutput);
      if Result then
        begin
        Str.Position := 0;
        for i := 0 to Pred(Streams.Count) do
          with T7zStream(Streams[i]) do
            if not Longbool(Attr and (faDirectory or faVolumeID)) then
              begin
              Stream := TMemoryStream.Create;
              if Size > 0 then
                Stream.CopyFrom(Str, Size);
              end;
        end;
    finally
      Str.Free;
      end;
    end;
end;

function Extract7z(Handle: THandle; const Archive: string; Stream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
begin
  Result := Run7z(Handle, Archive, 'e -so', Stream, nil, ShowProgress, Password, OnErrOutput);
end;

function Extract7z(Handle: THandle; const Archive, DestFile: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
var OutStr : TMemoryStream;
begin
  OutStr := TMemoryStream.Create;
  try
    Result := Extract7z(Handle, Archive, OutStr, ShowProgress, Password, OnErrOutput);
    if Result then
      with TFileStream.Create(DestFile, fmCreate) do
        try
          CopyFrom(OutStr,0);
        finally
          Free;
          end;
  finally
    OutStr.Free;
    end;
end;

function AddTo7z(Handle: THandle; const Archive, StreamName: string; Stream: TStream; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
var s : string;
begin
  if Length(StreamName) > 0 then
    s := 'a -ms=off -si"' + StreamName + '"'
  else
    s := 'a -ms=off -si';
  Result := Run7z(Handle, Archive, s, Stream, nil, nil, ShowProgress, Password, OnErrOutput);
end;

function AddTo7z(Handle: THandle; const Archive: string; Streams: TList; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
var i : integer;
begin
  // Tohle neni uplne idealni zpusob, ale dokud nebude 7Z umet vic vstupnich streamu, tak to jinak nepujde
  Result := Streams.Count > 0;
  for i := 0 to Pred(Streams.Count) do
    with T7zStream(Streams[i]) do
    Result := Result and AddTo7z(Handle, Archive, Name, Stream, ShowProgress, Password, OnErrOutput);
end;

function AddTo7z(Handle: THandle; const Archive: string; const FileNames: array of string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
var s : string;
    i : integer;
begin
  s := 'a -r';
  for i := 0 to High(FileNames) do
    s := s + ' -i!"' + FileNames[i] + '"';
  Result := Run7z(Handle, Archive, s, ShowProgress, Password, OnErrOutput);
end;

function AddTo7z(Handle: THandle; const Archive, FileName: string; ShowProgress: Boolean = False; const Password: string = ''; OnErrOutput: TConsoleOutputEvent = nil): Boolean;
begin
  Result := AddTo7z(Handle, Archive, [FileName], ShowProgress, Password, OnErrOutput);
end;
{$ENDIF}

{$IFDEF USYSTEM_SHELL}
function RunFile(FileName: string): Integer;
begin
  Result := ShellExecute(hInstance, nil, PChar(FileName), nil, PChar(ExtractFilePath(FileName)), SW_SHOW);
end;

function RunFile(FileName, Params: string): Integer;
begin
  Result := ShellExecute(hInstance, nil, PChar(FileName), PChar(Params), PChar(ExtractFilePath(FileName)), SW_SHOW);
end;

function RunFile(FileName, Params, WorkDir: string): Integer;
begin
  Result := ShellExecute(hInstance, nil, PChar(FileName), PChar(Params), PChar(WorkDir), SW_SHOW);
end;

function RunFile(FileName, Params: string; Command: Word): Integer;
begin
  Result := ShellExecute(hInstance, nil, PChar(FileName), PChar(Params), PChar(ExtractFilePath(FileName)), Command);
end;

function RunFile(FileName, Params, WorkDir: string; Command: Word): Integer; overload;
begin
  Result := ShellExecute(hInstance, nil, PChar(FileName), PChar(Params), PChar(WorkDir), Command);
end;

function WaitForEnd(FileName: string; out ResultCode: DWORD): Boolean;
begin
  Result := WaitForEnd(FileName, SW_SHOW, ResultCode);
end;

function WaitForEnd(FileName: string): Boolean;
begin
  Result := WaitForEnd(FileName, SW_SHOW);
end;

function WaitForEnd(FileName: string; Command: Word; out ResultCode: DWORD): Boolean;
begin
  Result := WaitForEnd(FileName, '', Command, ResultCode);
end;

function WaitForEnd(FileName: string; Command: Word): Boolean;
begin
  Result := WaitForEnd(FileName, '', Command);
end;

function WaitForEnd(FileName: string; Params: string; Command: Word; out ResultCode: DWORD): Boolean;
var
  Dir: string;
begin
  Dir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FileName));
    if Pos(' ', FileName) > 0 then
      FileName := AnsiQuotedStr(FileName, '"');
    Result := GetConsoleOutput(FileName + ' ' + Params, TStream(nil), TStream(nil), TStream(nil), ResultCode, Command);
  finally
    SetCurrentDir(Dir);
    end;
end;

function WaitForEnd(FileName: string; Params: string; Command: Word): Boolean;
var ResultCode: DWORD;
begin
  Result := WaitForEnd(FileName, Params, Command, ResultCode);
end;

{$IFDEF USYSTEM_APPLICATION}
function WaitForExclusiveAccess(FN: string): Boolean;
var
  pokracuj: Boolean;
  cas: Cardinal;
  FileHandle: Integer;
begin
  Result := False;
  if not FileExists(FN) then
    Exit;
  Pokracuj := True;
  cas := GetTickCount;
  while Pokracuj do
  begin
    Application.ProcessMessages;
    if (GetTickCount - cas) > 500 then
    begin
      cas := GetTickCount;
      FileHandle := FileOpen(FN, fmOpenWrite or fmShareExclusive);
      if FileHandle > 0 then
      begin
        FileClose(FileHandle);
        Pokracuj := False;
      end
      else
      begin
      end;
    end
    else
      Sleep(500);
  end;
end;
{$ENDIF}

function GetComputerName: string;
var
  Buffer: array[0..MAX_PATH] of Char;
  n: DWORD;
begin
  n := Length(Buffer);
  if Windows.GetComputerName(Buffer, n) then
    Result := string(Buffer)
  else
    Result := '';
end;

function FindShortcutByTarget(Dir, Target: string; out FileName: string): boolean;
var
  FN, LinkedFN: string;
  FNBuf: array[0..MAX_PATH] of Char;
  IObject: IUnknown;
  SR: TSearchRec;
  PFD: TWin32FindData;
begin
  Result := False;
  Dir := IncludeTrailingPathDelimiter(ExpandFileName(Dir));
  Target := ExpandFileName(Target);
  CoInitialize(nil);
  try
    if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, IObject) and $80000000) = 0 then
      try
        // Zkontroluju vsechny ikonky na plose
        if FindFirst(Dir + '*.lnk', faAnyFile, SR) = 0 then
          try
            repeat
              FN := Dir + SR.Name;
              if not Longbool(SR.Attr and faDirectory) then
                if (IObject as IPersistFile).Load(PWideChar(WideString(FN)), 0) = S_OK then
                  if (IObject as IShellLink).GetPath(FNBuf, Length(FNBuf), PFD, 0) = NOERROR then
                    begin
                    LinkedFN := string(WideString(FNBuf));
                    // Pokud jsem nasel ikonku s pozadovanou cestou, muzu skoncit
                    if AnsiCompareText(ExpandFileName(LinkedFN), Target) = 0 then
                      begin
                      FileName := LinkedFN;
                      Result := True;
                      Exit;
                      end;
                    end;
            until FindNext(SR) <> 0;
          finally
            SysUtils.FindClose(SR);
            end;
      finally
        IObject := nil;
        end;
  finally
    CoUninitialize;
    end;
end;

function CreateShortcut(FileName, Target: string): boolean;
var
  IObject: IUnknown;
begin
  Result := False;
  CoInitialize(nil);
  try
    if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, IObject) and $80000000) = 0 then
      try
        Target := ExpandFileName(Target);
        (IObject as IShellLink).SetPath(PChar(Target));
        (IObject as IShellLink).SetWorkingDirectory(PChar(ExtractFilePath(Target)));
        Result := (IObject as IPersistFile).Save(PWideChar(WideString(FileName)), False) = S_OK;
      finally
        IObject := nil;
        end;
  finally
    CoUninitialize;
    end;
end;
{$ENDIF}

{$IFDEF USYSTEM_NET}
function GetIpAddress(const Host: string; out Addresses: TIpAddressArray): integer;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
//  Buffer: array[0..63] of Char;
  I: Integer;
  GInitData: TWSAData;
begin
  Result := 0;
  WSAStartup($101, GInitData);
  phe := GetHostByName(PAnsiChar( {$IFDEF UNICODE} AnsiString {$ENDIF} (Host)));
  if phe = nil then
    Exit;
  pPtr := PaPInAddr(phe^.h_addr_list);
  I := 0;
  while (pPtr^[I] <> nil) and (I < MAX_IP_ADDRESSES) do
  begin
    Addresses[I] := {$IFDEF UNICODE} string {$ENDIF} ({$IFDEF UNICODE} AnsiString {$ENDIF} (inet_ntoa(pptr^[I]^)));
    Inc(I);
  end;
  WSACleanup;
  Result := I;
end;

function GetIpAddress(const Host: string): string;
var Addresses: TIpAddressArray;
    i, n: integer;
begin
  Result := '';
  n := GetIpAddress(Host, Addresses);
  for i := 0 to Pred(n) do
    if i = 0 then
      Result := Addresses[i]
    else
      Result := Result + IP_ADDRESS_SEPARATOR + Addresses[i];
end;

function GetIpAddress: string;

  function AddressMatches(const Address, AddressGroup, Mask: string): boolean;
    var
      A, AG, M: DWORD;
    begin
      A := inet_addr(PAnsiChar({$IFDEF UNICODE} AnsiString {$ENDIF} (Address)));
      AG := inet_addr(PAnsiChar({$IFDEF UNICODE} AnsiString {$ENDIF} (Address)));
      M := inet_addr(PAnsiChar({$IFDEF UNICODE} AnsiString {$ENDIF} (Mask)));
      Result := (A and M) = AG;
    end;

var Addresses: TIpAddressArray;
    i, n, q, AddressQuality: integer;
begin
  Result := '';
  AddressQuality := 0;
  n := GetIpAddress(GetMyHostName, Addresses);
  for i := 0 to Pred(n) do
    begin
    if AddressMatches(Addresses[i], '127.0.0.0', '255.0.0.0') then
      q := 1
    else if AddressMatches(Addresses[i], '10.0.0.0', '255.0.0.0') then
      q := 2
    else if AddressMatches(Addresses[i], '172.16.0.0', '255.240.0.0') then
      q := 2
    else if AddressMatches(Addresses[i], '192.168.0.0', '255.255.0.0') then
      q := 2
    else
      q := 3;
    if q > AddressQuality then
      begin
      AddressQuality := q;
      Result := Addresses[i];
      end;
    end;
end;

function GetMyHostName: string;
var
  Buffer: array[0..63] of AnsiChar;
  GInitData: TWSAData;
begin
  WSAStartup($101, GInitData);
  GetHostName(Buffer, SizeOf(Buffer));
  WSACleanup;
  Result := {$IFDEF UNICODE} string {$ENDIF} ({$IFDEF UNICODE} AnsiString {$ENDIF} (Buffer));
end;

function IpAddressIs(const Host: string): boolean;
var MyIp, HostIp: TIpAddressArray;
    i, j, n1, n2: integer;
begin
  Result := False;
  n1 := GetIpAddress(GetMyHostName, MyIp);
  n2 := GetIpAddress(Host, HostIp);
  if (n1 > 0) and (n2 > 0) then
    for i := 0 to Pred(n1) do
      for j := 0 to Pred(n2) do
        if AnsiCompareText(MyIp[i], HostIp[j]) = 0 then
          begin
          Result := True;
          Exit;
          end;
end;
{$ENDIF}

{$IFDEF USYSTEM_FILES}
function IsExclusiveAccess(FN: string): Boolean;
var
  FileHandle: Integer;
begin
  Result := False;
  if not FileExists(FN) then
    Exit;
  FileHandle := FileOpen(FN, fmOpenWrite or fmShareExclusive);
  if FileHandle > 0 then
  begin
    Result := True;
    FileClose(FileHandle);
  end;
end;

function IsItLocalDir(Dir: string; var LocalPath: string): Boolean;
var
  res: integer;
  pom, sNetPath: string;
  dwMaxNetPathLen: DWord;
begin
  Result := False;
  LocalPath := '';
  case GetDriveType(PChar(ExtractFileDrive(ExpandFileName(Dir))+'\')) of
    DRIVE_RAMDISK, DRIVE_CDROM, DRIVE_REMOVABLE, DRIVE_FIXED:
      begin
        result := True;
        LocalPath := Dir;
      end;
    DRIVE_REMOTE:
      begin
        try
          dwMaxNetPathLen := MAX_PATH;
          SetLength(sNetPath,
            dwMaxNetPathLen);
          pom := Dir[1] + ':';
          res := Windows.WNetGetConnection(
            PChar(pom),
            PChar(sNetPath),
            dwMaxNetPathLen);
          if Res = ERROR_CONNECTION_UNAVAIL then
          begin
          end;
        except
        end;
      end;
  end;
end;

function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH+1);
  if GetSystemDirectory(@Result[1], MAX_PATH+1) = 0 then
    Result := '';
end;

function GetSpecialFolder(AFolder: integer): string;
var
//  pszPath : pchar;          // an alternative
  pszPath : array[0..MAX_PATH] of char;
  ppidl : PItemIDList; // Needs ShlObj.pas
begin
//  GetMem(pszPath, MAX_PATH);    // required if pszPath is pChar
  if (SHGetSpecialFolderLocation(0, AFolder, ppidl) ) = S_OK then
  begin
    SHGetPathFromIDList(ppidl, pszPath);
    Result := string(pszPath); // cast not strictly needed
                               // but better to be unambiguous
  end
  else
    Result := '';
// FreeMem(pszPath);
end;

procedure ForceDeleteDirectory(APath: string);
var SR: TSearchRec;
    OK: integer;
begin
  if APath = '' then
    Exit;
  APath := IncludeTrailingPathDelimiter(APath);
  OK := FindFirst(APath + '*.*', faAnyFile, SR);
  try
    while OK = 0 do
      begin
      if Longbool(SR.Attr and faDirectory) then
        begin
        if (SR.Name <> '.') and (SR.Name <> '..') then
          ForceDeleteDirectory(APath + SR.Name + '\');
        end
      else
        begin
        {$IFDEF MSWINDOWS}
          {$IFDEF DELPHI7_UP}
            {$WARN SYMBOL_PLATFORM OFF}
          {$ENDIF}
          FileSetAttr(APath + SR.Name, 0);
          {$IFDEF DELPHI7_UP}
            {$WARN SYMBOL_PLATFORM ON}
          {$ENDIF}
        {$ENDIF}
        SysUtils.DeleteFile(APath + SR.Name);
        end;
      OK := FindNext(SR);
      end;
  finally
    SysUtils.FindClose(SR);
    end;
  RemoveDir(ExcludeTrailingPathDelimiter(APath));
end;

function SystemTempDir: string;
var Buf: array[0..MAX_PATH] of Char;
    n: DWORD;
begin
  n := GetTempPath(Length(Buf), Buf);
  if (n > 0) and (n < DWORD(Length(Buf))) then
    Result := string(Buf)
  else
    Result := '';
end;

{$IFDEF USYSTEM_FILES_CLEANUPTEMP}
var
  SystemTempFileList: TStringList = nil;

procedure Init_TempCleanup;
begin
  SystemTempFileList := TStringList.Create;
end;

procedure Done_TempCleanup;
var
  i: integer;
  FN: string;
begin
  if SystemTempFileList <> nil then
    try
      for i := 0 to Pred(SystemTempFileList.Count) do
        begin
        FN := SystemTempFileList[i];
        if FileExists(FN) then
          DeleteFile(PChar(FN));
        end;
    finally
      FreeAndNil(SystemTempFileList);
      end;
end;
{$ENDIF}

function SystemTempFile(const Dir, Prefix: string): string;
var n: integer;
    Buf: array[0..MAX_PATH] of Char;
begin
  n := GetTempFileName(PChar(Dir), PChar(Prefix), 0, Buf);
  if n <> 0 then
    begin
    Result := string(Buf);
    if FileExists(Result) then
      DeleteFile(PChar(Result));
    end
  else
    Result := '';
  {$IFDEF USYSTEM_FILES_CLEANUPTEMP}
  if Result <> '' then
    SystemTempFileList.Add(Result);
  {$ENDIF}
end;

function SystemTempFile(const Dir, Prefix, Extension: string): string;
var
  Base: string;
  i: integer;
begin
  Base := ChangeFileExt(SystemTempFile(Dir, Prefix), '');
  Result := Base + Extension;
  i := 0;
  while FileExists(Result) do
    begin
    Result := Base + IntToStr(i) + Extension;
    Inc(i);
    end;
  {$IFDEF USYSTEM_FILES_CLEANUPTEMP}
  if Result <> '' then
    SystemTempFileList.Add(Result);
  {$ENDIF}
end;

function AddLastSlash(const Path: string): string;
var n: integer;
begin
  n := Length(Path);
  if n = 0 then
    Result := '.\'
  else if Path[n] = '\' then
    Result := Path
  else
    Result := Path + '\';
end;

function GetFileSize(const FileName: string): int64;
var
  Handle: THandle;
  SizeLow, SizeHigh: DWORD;
begin
  Result := -1;
  Handle := CreateFile(PChar(FileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      SizeLow := Windows.GetFileSize(Handle, @SizeHigh);
      if (SizeLow <> INVALID_FILE_SIZE) or (GetLastError = NO_ERROR) then
        Result := int64(SizeLow) + (int64(SizeHigh) shl 32);
    finally
      CloseHandle(Handle);
      end;
end;

function GetFileDateTime(const FileName: string; out CreationTime, ModificationTime, AccessTime: TDateTime): boolean;

  function FileTimeToDateTime(ATime: TFileTime): TDateTime;
    var
      SysTime: TSystemTime;
    begin
      if FileTimeToSystemTime(ATime, SysTime) then
        Result := EncodeDate(SysTime.wYear, SysTime.wMonth, SysTime.wDay) + EncodeTime(SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds)
      else
        Result := 0;
    end;

var
  Handle: THandle;
  CTime, ATime, MTime: TFileTime;
begin
  Result := False;
  CreationTime := 0;
  ModificationTime := 0;
  AccessTime := 0;
  Handle := CreateFile(PChar(FileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      if GetFileTime(Handle, @CTime, @ATime, @MTime) then
        begin
        CreationTime := FileTimeToDateTime(CTime);
        ModificationTime := FileTimeToDateTime(MTime);
        AccessTime := FileTimeToDateTime(ATime);
        Result := True;
        end;
    finally
      CloseHandle(Handle);
      end;
end;

function GetFileContent(const FileName: string): string;
var FS: TFileStream;
    SS: TStringStream;
begin
  Result := '';
  if FileExists(FileName) then
    begin
    SS := TStringStream.Create('');
    try
      FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      try
        SS.CopyFrom(FS, 0);
        Result := SS.DataString;
      finally
        FS.Free;
        end;
    finally
      SS.Free;
      end;
    end;
end;

function GetFileVersion(const FileName: string; out VersionHigh, VersionLow, BuildNumber: LongWord): boolean;
var VerInfoSize: DWORD;
    VerInfo: Pointer;
    dwHandle: DWORD;
    SpecVerInfo: PVsFixedFileInfo;
begin
  Result := False;
  VersionHigh := 0;
  VersionLow := 0;
  BuildNumber := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), dwHandle);
  if VerInfoSize > 0 then
    begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), dwHandle, VerInfoSize, VerInfo) then
        if VerQueryValue(VerInfo, '\', Pointer(SpecVerInfo), VerInfoSize) then
          begin
          VersionHigh := SpecVerInfo^.dwFileVersionMS;
          VersionLow := SpecVerInfo^.dwFileVersionLS shr 16;
          BuildNumber := SpecVerInfo^.dwFileVersionLS and $ffff;
          Result := True;
          end;
    finally
      FreeMem(VerInfo);
      end;
    end;
end;

function GetDrives(out Drives: TDriveInfoArray; OnlyOfType: integer): boolean;
{
  TDriveInfo = record
    RootDir: string;
    DriveType: integer;
    Capacity: int64;
    DisplayName: string;
    TypeName: string;
    end;
}
var n: integer;
    DriveBits: DWORD;
    DriveLetter: Char;
    BytesAvailable, BytesTotal, BytesFree: TLargeInteger;
    ShellInfo: TSHFileInfo;
    ErrorMode: DWORD;
begin
  n := 0;
  SetLength(Drives, 32);
  DriveBits := GetLogicalDrives;
  DriveLetter := 'A';
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    while DriveBits <> 0 do
      begin
      if Longbool(DriveBits and $0001) then
        begin
        Drives[n].RootDir := DriveLetter + ':\';
        Drives[n].DriveType := GetDriveType(PChar(Drives[n].RootDir));
        if (OnlyOfType < 0) or (OnlyOfType = Drives[n].DriveType) then
          begin
          if GetDiskFreeSpaceEx(PChar(Drives[n].RootDir), BytesAvailable, BytesTotal, @BytesFree) then
            Drives[n].Capacity := BytesTotal;
          if ShGetFileInfo(PChar(Drives[n].RootDir), 0, ShellInfo, SizeOf(ShellInfo), SHGFI_DISPLAYNAME or SHGFI_TYPENAME) <> 0 then
            begin
            Drives[n].DisplayName := ShellInfo.szDisplayName;
            Drives[n].TypeName := ShellInfo.szTypeName;
            end
          else
            begin
            Drives[n].DisplayName := '';
            Drives[n].TypeName := '';
            end;
          Inc(n);
          end;
        end;
      DriveBits := DriveBits shr 1;
      Inc(DriveLetter);
      end;
  finally
    SetErrorMode(ErrorMode);
    end;
  SetLength(Drives, n);
  Result := n > 0;
end;
{$ENDIF}

{$IFDEF USYSTEM_WOW64}
type
  TIsWow64ProcessFn = function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  TWow64DisableWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;

var
  Kernel32Dll: THandle = 0;

var
  IsWow64ProcessFn: TIsWow64ProcessFn = nil;
  Wow64DisableWow64FsRedirectionFn: TWow64DisableWow64FsRedirectionFn = nil;
  Wow64RevertWow64FsRedirectionFn: TWow64RevertWow64FsRedirectionFn = nil;

procedure Init_Wow64;
begin
  Kernel32Dll := LoadLibrary('kernel32.dll');
  if Kernel32Dll = 0 then
    begin
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end
  else
    begin
    IsWow64ProcessFn := GetProcAddress(Kernel32Dll, 'IsWow64Process');
    Wow64DisableWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64DisableWow64FsRedirection');
    Wow64RevertWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64RevertWow64FsRedirection');
    end;
end;

procedure Done_Wow64;
begin
  if Kernel32Dll <> 0 then
    begin
    FreeLibrary(Kernel32Dll);
    Kernel32Dll := 0;
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end;
end;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
begin
  if not Assigned(IsWow64ProcessFn) then
    Result := False
  else
    Result := IsWow64ProcessFn(hProcess, Wow64Process);
end;

function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64DisableWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64DisableWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64RevertWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64RevertWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Is64BitWindows: boolean;
var B: BOOL;
begin
  if IsWow64Process(GetCurrentProcess, B) then
    Result := B
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF USYSTEM_INFO}
function GetWindowsVersion: TWindowsVersion;
var
  Ver: TOsVersionInfo;
begin
  {Popis platformy win}
  Result := wvUnknown;
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  if GetVersionEx(Ver) then
    case Ver.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
        if Ver.dwMajorVersion <= 4 then
          Result := wvWinNT
        else if Ver.dwMajorVersion = 5 then
          if Ver.dwMinorVersion = 0 then
            Result := wvWin2000
          else if Ver.dwMinorVersion = 1 then
            Result := wvWinXP
          else
            Result := wvWinServer2003
        else if Ver.dwMajorVersion = 6 then
          if Ver.dwMinorVersion = 0 then
            Result := wvWinVista
          else if Ver.dwMinorVersion = 1 then
            Result := wvWin7
          else
            Result := wvWin8
          ;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          if Ver.dwMajorVersion = 4 then
            if Ver.dwMinorVersion < 10 then
              Result := wvWin95
            else if Ver.dwMinorVersion < 90 then
              Result := wvWin98
            else
              Result := wvWinME
            ;
        end;
      end;
end;
{$ENDIF}

initialization
  {$IFDEF USYSTEM_WOW64}
  Init_Wow64;
  {$ENDIF}
  {$IFDEF USYSTEM_FILES_CLEANUPTEMP}
  Init_TempCleanup;
  {$ENDIF}

finalization
  {$IFDEF USYSTEM_WOW64}
  Done_Wow64;
  {$ENDIF}
  {$IFDEF USYSTEM_FILES_CLEANUPTEMP}
  Done_TempCleanup;
  {$ENDIF}

end.
