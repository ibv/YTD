(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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
{.DEFINE COMOBJ}

interface

uses
  SysUtils, Windows, {$IFDEF COMOBJ} ComObj, {$ENDIF} ShlObj, ActiveX, ShellApi,
  SynaCode,
  uFunctions, uDownloadList, uMessages, uStringUtils;

function GetProgressStr(DoneSize, TotalSize: int64): string;
procedure ReportBug(DownloadList: TDownloadList; Index: integer);
function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer = 0): boolean;

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
  ShellExecute(0, 'open', PChar(BugReportUrl), nil, nil, SW_SHOWNORMAL);
end;

function CreateShortcut(const ShortcutName, Where: string; WhereCSIDL: integer): boolean;
var IObject: IUnknown;
    PIDL : PItemIDList;
    Dir, FileName: string;
    DirBuf: array[0..MAX_PATH] of char;
begin
  {$IFDEF COMOBJ}
  IObject := CreateComObject(CLSID_ShellLink);
  {$ELSE}
  Result := False;
  if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, IObject) and $80000000) = 0 then
  {$ENDIF}
    try
      with IObject as IShellLink do
        begin
        SetPath(PChar(ParamStr(0)));
        SetWorkingDirectory(PChar(ExtractFilePath(ParamStr(0))));
        end;
      if WhereCSIDL = 0 then
        Dir := Where
      else
        begin
        SHGetSpecialFolderLocation(0, WhereCSIDL, PIDL);
        SHGetPathFromIDList(PIDL, DirBuf);
        Dir := string(DirBuf);
        end;
      if Dir = '' then
        FileName := ShortcutName
      else
        FileName := Dir + '\' + ShortcutName;
      with IObject as IPersistFile do
        Save(PWideChar(WideString(FileName)), False);
      Result := True;
    finally
      IObject := nil;
      end;
end;

end.
