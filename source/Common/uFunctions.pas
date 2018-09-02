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
{.DEFINE COMOBJ}

interface

uses
  SysUtils, Windows, {$IFDEF COMOBJ} ComObj, {$ENDIF} ShlObj, ActiveX, ShellApi,
  HttpSend, SynaUtil,
  uMessages, uCompatibility;

function PrettySize(Size: int64): string;
function IsNewerVersion(OnlineVersion: string): boolean;
function GetSpecialFolder(FolderID: integer): string;
function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer = 0; FileName: string = ''): boolean;
function Run(const FileName, CommandLine, WorkDir: string; OwnerHandle: THandle = 0): boolean; overload;
function Run(const FileName, CommandLine: string; OwnerHandle: THandle = 0): boolean; overload;
function Run(const FileName: string; OwnerHandle: THandle = 0): boolean; overload;
function GetTempDir: string;
function CheckRedirect(Http: THttpSend; var Url: string): boolean;

implementation

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

function IsNewerVersion(OnlineVersion: string): boolean;

  function ExtractVersionPartAsInteger(var Version: string; out Part: integer): boolean;
    var i: integer;
        s: string;
    begin
      i := Pos('.', Version);
      if i > 0 then
        begin
        s := Copy(Version, 1, Pred(i));
        Delete(Version, 1, i);
        end
      else
        begin
        s := Version;
        Version := '';
        end;
      try
        Part := StrToInt(s);
        Result := True;
      except
        on EConvertError do
          begin
          Part := 0;
          Result := False;
          end;
        end;
    end;

var Version: string;
    NumCurrent, NumOnline: integer;
    FoundCurrent, FoundOnline: boolean;
begin
  Result := False;
  Version := APPLICATION_VERSION;
  repeat
    FoundCurrent := ExtractVersionPartAsInteger(Version, NumCurrent);
    FoundOnline := ExtractVersionPartAsInteger(OnlineVersion, NumOnline);
    if not (FoundCurrent or FoundOnline) then
      Break
    else
      if NumCurrent < NumOnline then
        begin
        Result := True;
        Break;
        end
      else if NumCurrent > NumOnline then
        Break;
  until False;
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

function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer; FileName: string): boolean;
var IObject: IUnknown;
    Dir, ShortcutFile: string;
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
          FileName := ParamStr(0);
        with IObject as IShellLink do
          begin
          SetPath(PChar(FileName));
          SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
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
  i: integer;
  Redirect: string;
  RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara: string;
  OldURL, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara: string;
begin
  Result := False;
  if (Http.ResultCode >= 300) and (Http.ResultCode < 400) then
    for i := 0 to Pred(Http.Headers.Count) do
      if AnsiCompareText(Location, Copy(Http.Headers[i], 1, Length(Location))) = 0 then
        begin
        OldUrl := Url;
        Redirect := Trim(Copy(Http.Headers[i], Length(Location)+1, MaxInt));
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
        Break;
        end;
end;

end.
