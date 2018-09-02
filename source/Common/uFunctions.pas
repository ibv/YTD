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

unit uFunctions;
{$INCLUDE 'ytd.inc'}
{$DEFINE COMOBJ}

interface

uses
  SysUtils, Classes, Windows, {$IFDEF COMOBJ} ComObj, {$ENDIF} ShlObj, ActiveX, ShellApi,
  HttpSend, SynaUtil, Blcksock,
  {$IFDEF SETUP}
  uSetup,
  {$ENDIF}
  uMessages, uCompatibility, uFiles, uStrings, uOptions;

type
  TProgressBarState = (pbsNoProgress, pbsUnknown, pbsNormal, pbsError, pbsPaused);

function PrettySize(Size: int64): string;
function ShowTotalProgressBar(Handle: THandle; State: TProgressBarState): boolean; overload;
function ShowTotalProgressBar(Handle: THandle; State: TProgressBarState; const Current, Total: int64): boolean; overload;
function GetSpecialFolder(FolderID: integer): string;
function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer = 0; const FileName: string = ''; const Parameters: string = ''): boolean;
function DeleteShortcut(const ShortcutName, Where: string; WhereCSIDL: integer = 0; const FileName: string = ''): boolean;
function Run(const FileName, CommandLine, WorkDir: string; OwnerHandle: THandle = 0): boolean; overload;
function Run(const FileName, CommandLine: string; OwnerHandle: THandle = 0): boolean; overload;
function Run(const FileName: string; OwnerHandle: THandle = 0): boolean; overload;
function GetTempDir: string;
function CheckRedirect(Http: THttpSend; var Url: string): boolean;
function CheckProtocol(const Url: string; const Protocols: array of string): integer;
function IsHttpProtocol(const Url: string): boolean;
function IncludeTrailingSlash(const Path: string): string;
function FindHttpHeader(Http: THttpSend; Header: string; out Value: string): boolean;
function IsSSLAvailable: boolean;
function DownloadFromHttp(const Url: string; Options: TYTDOptions; Stream: TStream): boolean;
function SafeRelativeFileName(const FileName: string): string;
function Unzip(const FileName, DestinationDir: string): boolean; overload;
function Unzip(Stream: TStream; const DestinationDir: string): boolean; overload;
function FileContentsSame(const FileName1, FileName2: string): boolean;

implementation

uses
  {$IFNDEF DELPHI7_UP}
  FileCtrl,
  {$ENDIF}
  SciZipFile;

function PrettySize(Size: int64): string;

  function PrettySizeInternal(Size: int64; Shift: integer; const Prefix: char): string;
    begin
      Size := (10 * Size) shr Shift;
      Result := Format('%d.%d %siB', [Size div 10, Size mod 10, Prefix]);
    end;

begin
  if Size <= 0 then
    Result := ''
  else if Size < 10*1e3 then
    Result := IntToStr(Size) + ' B'
  else if Size < 10*1e6 then
    Result := PrettySizeInternal(Size, 10, 'K')
  else if Size < 10*1e9 then
    Result := PrettySizeInternal(Size, 20, 'M')
  else if Size < 10*1e12 then
    Result := PrettySizeInternal(Size, 30, 'G')
  else
    Result := PrettySizeInternal(Size, 40, 'T')
end;

var
  TaskBarListInitialized: boolean = False;
  TaskBarList: ITaskbarList = nil;
  TaskBarList2: ITaskbarList2 = nil;
  TaskBarList3: ITaskbarList3 = nil;
  TaskBarList4: ITaskbarList4 = nil;

function ShowTotalProgressBar(Handle: THandle; State: TProgressBarState): boolean;
begin
  Result := ShowTotalProgressBar(Handle, State, -1, -1);
end;

function ShowTotalProgressBar(Handle: THandle; State: TProgressBarState; const Current, Total: int64): boolean;
const
  States: array[TProgressBarState] of DWORD = (TBPF_NOPROGRESS, TBPF_INDETERMINATE, TBPF_NORMAL, TBPF_ERROR, TBPF_PAUSED);
begin
  Result := False;
  if (Win32MajorVersion > 6) or ((Win32MajorVersion = 6) and (Win32MinorVersion >= 1)) then
    begin
    if not TaskBarListInitialized then
      begin
      TaskBarListInitialized := True;
      CoInitialize(nil);
      {$IFDEF COMOBJ}
      TaskBarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
      {$ELSE}
      if (CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_INPROC_SERVER, ITaskbarList, TaskBarList) and $80000000) <> 0 then
        TaskBarList := nil;
      {$ENDIF}
      if TaskBarList <> nil then
        begin
        Supports(TaskBarList, ITaskBarList2, TaskBarList2);
        Supports(TaskBarList, ITaskBarList3, TaskBarList3);
        Supports(TaskBarList, ITaskBarList4, TaskBarList4);
        end;
      end;
    if TaskBarList3 <> nil then
      begin
      TaskBarList3.SetProgressState(Handle, States[State]);
      if (Current >= 0) and (Total > 0) then
        if Current <= Total then
          TaskBarList3.SetProgressValue(Handle, Current, Total)
        else
          TaskBarList3.SetProgressValue(Handle, Total, Total);
      Result := True;
      end;
    end;
end;

function GetSpecialFolder(FolderID: integer): string;
var PIDL : PItemIDList;
    DirBuf: array[0..MAX_PATH] of char;
begin
  SHGetSpecialFolderLocation(0, FolderID, PIDL);
  try
    SHGetPathFromIDList(PIDL, DirBuf);
    Result := string(DirBuf);
  finally
    CoTaskMemFree(PIDL);
    end;
end;

function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer; const FileName, Parameters: string): boolean;
var
  IObject: IUnknown;
  FN, Dir, ShortcutFile: string;
begin
  CoInitialize(nil);
  try
    {$IFDEF COMOBJ}
    IObject := CreateComObject(CLSID_ShellLink);
    {$ELSE}
    Result := False;
    if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, IObject) and $80000000) = 0 then
    {$ENDIF}
      try
        if FileName = '' then
          FN := ParamStr(0)
        else
          FN := FileName;
        with IObject as IShellLink do
          begin
          SetPath(PChar(FN));
          SetWorkingDirectory(PChar(ExtractFilePath(FN)));
          if Parameters <> '' then
            SetArguments(PChar(Parameters));
          end;
        if WhereCSIDL = 0 then
          Dir := Where
        else
          Dir := GetSpecialFolder(WhereCSIDL);
        if Dir = '' then
          ShortcutFile := ShortcutName
        else
          ShortcutFile := Dir + '\' + ShortcutName;
        with IObject as IPersistFile do
          Save(PWideChar(WideString(ShortcutFile)), False);
        Result := True;
      finally
        IObject := nil;
        end;
  finally
    CoUninitialize;
    end;
end;

function DeleteShortcut(const ShortcutName, Where: string; WhereCSIDL: integer = 0; const FileName: string = ''): boolean;
var
  Dir, ShortcutFile: string;
begin
  if WhereCSIDL = 0 then
    Dir := Where
  else
    Dir := GetSpecialFolder(WhereCSIDL);
  if Dir = '' then
    ShortcutFile := ShortcutName
  else
    ShortcutFile := Dir + '\' + ShortcutName;
  if FileExists(ShortcutFile) then
    Result := SysUtils.DeleteFile(ShortcutFile)
  else
    Result := True;
end;

function Run(const FileName, CommandLine, WorkDir: string; OwnerHandle: THandle): boolean; overload;
begin
  Result := ShellExecute(OwnerHandle, 'open', PChar(FileName), PChar(CommandLine), PChar(WorkDir), SW_SHOWNORMAL) > 32;
end;

function Run(const FileName, CommandLine: string; OwnerHandle: THandle): boolean;
begin
  Result := Run(FileName, CommandLine, '', OwnerHandle);
end;

function Run(const FileName: string; OwnerHandle: THandle): boolean;
begin
  Result := Run(FileName, '', OwnerHandle);
end;

function GetTempDir: string;
const MAX_TEMP_PATH = MAX_PATH + 16;
begin
  SetLength(Result, MAX_TEMP_PATH);
  SetLength(Result, GetTempPath(MAX_TEMP_PATH, @(Result[1])));
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function CheckRedirect(Http: THttpSend; var Url: string): boolean;
const
  Location {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'Location:';
  Localhost {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'localhost';
var
  Redirect: string;
  RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara: string;
  OldURL, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara: string;
begin
  Result := False;
  if (Http.ResultCode >= 300) and (Http.ResultCode < 400) then
    if FindHttpHeader(Http, 'Location', Redirect) then
      begin
      OldUrl := Url;
      ParseUrl(Redirect, RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara);
      if (RedirHost = '') or (AnsiCompareText(RedirHost, Localhost) = 0) then
        begin
        ParseUrl(Url, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara);
        if RedirProtocol = '' then
          RedirProtocol := UrlProtocol;
        if RedirUser = '' then
          RedirUser := UrlUser;
        if RedirPass = '' then
          RedirPass := UrlPass;
        if (RedirHost = '') or (AnsiCompareText(RedirHost, Localhost) = 0) then
          RedirHost := UrlHost;
        if RedirPort = '' then
          RedirPort := UrlPort;
        Url := RedirProtocol + '://';
        if RedirUser <> '' then
          begin
          Url := Url + RedirUser;
          if RedirPass <> '' then
            Url := Url + ':' + RedirPass;
          Url := Url + '@';
          end;
        Url := Url + RedirHost + ':' + RedirPort + RedirPath;
        if RedirPara <> '' then
          Url := Url + '?' + RedirPara ;
        end
      else
        Url := Redirect;
      Result := Url <> OldUrl;
      end;
end;

function CheckProtocol(const Url: string; const Protocols: array of string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Pred(Length(Protocols)) do
    if AnsiCompareText(Protocols[i], Copy(Url, 1, Length(Protocols[i]))) = 0 then
      begin
      Result := i;
      Break;
      end;
end;

function IsHttpProtocol(const Url: string): boolean;
begin
  Result := CheckProtocol(Url, ['http://', 'https://']) >= 0;
end;

function IncludeTrailingSlash(const Path: string): string;
begin
  if Path = '' then
    Result := ''
  else if Path[Length(Path)] = '/' then
    Result := Path
  else
    Result := Path + '/';
end;

function FindHttpHeader(Http: THttpSend; Header: string; out Value: string): boolean;
var
  i: integer;
  HdrLen: integer;
begin
  Result := False;
  Header := Trim(Header) + ':';
  HdrLen := Length(Header);
  for i := 0 to Pred(Http.Headers.Count) do
    if AnsiCompareText(Copy(Http.Headers[i], 1, HdrLen), Header) = 0 then
      begin
      Value := Trim(Copy(Http.Headers[i], Succ(HdrLen), MaxInt));
      Result := True;
      Break;
      end;
end;

function IsSSLAvailable: boolean;
begin
  Result := blcksock.SSLImplementation <> TSSLNone;
end;

function DownloadFromHttp(const Url: string; Options: TYTDOptions; Stream: TStream): boolean;
var
  Http: THttpSend;
  Again: boolean;
  RealUrl: string;
begin
  Result := False;
  try
    Http := Options.CreateHttp;
    try
      RealUrl := Url;
      repeat
        Again := False;
        Http.Clear;
        if Http.HttpMethod('HEAD', RealUrl) then
          if CheckRedirect(HTTP, RealUrl) then
            Again := True
          else if (Http.ResultCode >= 200) and (Http.ResultCode < 300) then
            begin
            Http.Clear;
            if Http.HttpMethod('GET', RealUrl) then
              begin
              Http.Document.Position := 0;
              Stream.CopyFrom(Http.Document, Http.Document.Size);
              Result := True;
              end;
            end;
      until not Again;
    finally
      FreeAndNil(Http);
      end;
  except
    Result := False;
    end;
end;

function SafeRelativeFileName(const FileName: string): string;
var
  Dir: string;
  start, i, n: integer;
begin
  Result := '';
  start := 1;
  i := 1;
  n := Length(FileName);
  while (i <= n) do
    if (FileName[i] = '\') or (FileName[i] = '/') then
      begin
      Dir := Copy(FileName, start, i-start);
      if Dir <> '' then
        if Dir = '' then
          // Nothing to do
        else if Dir = '..' then
          Result := ExtractFilePath(Result)
        else
          Result := Result + Dir + '\';
      Inc(i);
      start := i;
      end
    else
      Inc(i);
  if (i <> start) then
    Result := Result + Copy(FileName, start, MaxInt);
end;

function Unzip(const FileName, DestinationDir: string): boolean;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := Unzip(FS, DestinationDir);
  finally
    FreeAndNil(FS);
    end;
end;

function Unzip(Stream: TStream; const DestinationDir: string): boolean; 
var
  Zip: TZipFile;
  i: integer;
  Dir, FN: string;
  Data: AnsiString;
  FS: TFileStream;
begin
  Result := True;
  Zip := TZipFile.Create;
  try
    Zip.LoadFromStream(Stream);
    if DestinationDir <> '' then
      Dir := IncludeTrailingPathDelimiter(DestinationDir)
    else
      Dir := '';
    for i := 0 to Pred(Zip.Count) do
      begin
      FN := string(Zip.Name[i]);
      if FN <> '' then
        try
          FN := ExcludeTrailingPathDelimiter(Dir + SafeRelativeFileName(FN));
          if (Zip.Attr[i] and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
            ForceDirectories(FN)
          else
            begin
            Data := Zip.Data[i];
            ForceDirectories(ExcludeTrailingPathDelimiter(ExtractFilePath(FN)));
            FS := TFileStream.Create(FN, fmCreate);
            try
              if Data <> '' then
                FS.WriteBuffer(Data[1], Length(Data));
            finally
              FreeAndNil(FS);
              end;
            end;
        except
          on E: Exception do
            Result := False;
          end;
      end;
  finally
    FreeAndNil(Zip);
    end;
end;

function FileContentsSame(const FileName1, FileName2: string): boolean;
const
  BUFFER_SIZE = 32768;
var
  File1, File2: TFileStream;
  Buffer1, Buffer2: array[0..BUFFER_SIZE-1] of byte;
  n1, n2: integer;
begin
  Result := False;
  if FileExists(FileName1) and FileExists(FileName2) then
    begin
    File1 := TFileStream.Create(FileName1, fmOpenRead or fmShareDenyWrite);
    try
      File2 := TFileStream.Create(FileName2, fmOpenRead or fmShareDenyWrite);
      try
        if File1.Size = File2.Size then
          begin
          Result := True;
          while Result do
            begin
            n1 := File1.Read(Buffer1[0], BUFFER_SIZE);
            n2 := File2.Read(Buffer2[0], BUFFER_SIZE);
            if n1 <> n2 then
              Result := False
            else if n1 <= 0 then
              Break
            else
              Result := CompareMem(@Buffer1[0], @Buffer2[0], n1);
            end;
          end;
      finally
        FreeAndNil(File2);
        end;
    finally
      FreeAndNil(File1);
      end;
    end;
end;

end.
