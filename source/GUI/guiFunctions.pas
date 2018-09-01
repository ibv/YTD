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

interface

uses
  SysUtils, Windows, ShellApi,
  SynaCode,
  uDownloadList, uMessages, uStringUtils;

function PrettySize(Size: int64): string;
function GetProgressStr(DoneSize, TotalSize: int64): string;
procedure ReportBug(DownloadList: TDownloadList; Index: integer);

implementation

function PrettySize(Size: int64): string;
begin
  if Size <= 0 then
    Result := ''
  else if Size < 10*1e3 then
    Result := IntToStr(Size) + ' B'
  else if Size < 10*1e6 then
    Result := IntToStr(Size div (1 shl 10)) + ' KiB'
  else if Size < 10*1e9 then
    Result := IntToStr(Size div (1 shl 20)) + ' MiB'
  else if Size < 10*1e12 then
    Result := IntToStr(Size div (1 shl 30)) + ' GiB'
  else
    Result := IntToStr(Size div (1 shl 40)) + ' TiB';
end;

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
                       [ {$INCLUDE 'YTD.version'} ,
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList.Urls[Index]))),
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList[Index].Downloader.LastErrorMsg)))
                       ]);
  ShellExecute(0, 'open', PChar(BugReportUrl), nil, nil, SW_SHOWNORMAL);
end;

end.
