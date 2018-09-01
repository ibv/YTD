(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit uRtmpDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uDownloader, uCommonDownloader, uExternalDownloader,
  RtmpDump_DLL;

type
  ERtmpDownloaderError = class(EExternalDownloaderError);

  TRtmpDownloader = class(TExternalDownloader)
    private
      fRtmpDumpOptions: TRtmpDumpOptions;
    protected
      procedure ClearRtmpDumpOptions; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure AddRtmpDumpOption(ShortOption: char; const Argument: string = ''); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetRtmpDumpOption(ShortOption: char; const Argument: string = ''); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property RtmpDumpOptions: TRtmpDumpOptions read fRtmpDumpOptions;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
    end;

implementation

uses
  uMessages;

procedure RtmpDumpDownloadProgressCallback(Tag: integer; DownloadedSize: integer; PercentDone: double; var DoAbort: integer); cdecl;
begin
  TRtmpDownloader(Tag).OnRtmpDownloadProgress(DownloadedSize, PercentDone, DoAbort);
end;

{ TRtmpDownloader }

constructor TRtmpDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TRtmpDownloader.Destroy;
begin
  inherited;
end;

procedure TRtmpDownloader.ClearRtmpDumpOptions;
begin
  SetLength(fRtmpDumpOptions, 0);
end;

procedure TRtmpDownloader.AddRtmpDumpOption(ShortOption: char; const Argument: string);
var n: integer;
begin
  n := Length(fRtmpDumpOptions);
  SetLength(fRtmpDumpOptions, Succ(n));
  fRtmpDumpOptions[n].ShortOption := AnsiChar(ShortOption);
  fRtmpDumpOptions[n].Argument := AnsiString(Argument);
end;

procedure TRtmpDownloader.SetRtmpDumpOption(ShortOption: char; const Argument: string);
var i: integer;
begin
  for i := 0 to Pred(Length(fRtmpDumpOptions)) do
    if fRtmpDumpOptions[i].ShortOption = AnsiChar(ShortOption) then
      begin
      fRtmpDumpOptions[i].ShortOption := AnsiChar(ShortOption);
      fRtmpDumpOptions[i].Argument := AnsiString(Argument);
      Exit;
      end;
  AddRtmpDumpOption(ShortOption, Argument);
end;

procedure TRtmpDownloader.OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer);
begin
  DownloadedBytes := DownloadedSize;
  if PercentDone >= 99.9 then
    TotalBytes := DownloadedSize
  else if PercentDone > 0 then
    TotalBytes := Trunc(int64(DownloadedSize) * (100 / PercentDone))
  else
    TotalBytes := -1;
  DoProgress;
  if Aborted then
    DoAbort := 1
  else
    DoAbort := 0;
end;

function TRtmpDownloader.Prepare: boolean;
begin
  ClearRtmpDumpOptions;
  Result := inherited Prepare;
end;

function TRtmpDownloader.Download: boolean;
var LogFileName: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  {$IFDEF DEBUG}
  AddRtmpDumpOption('z');
  {$ENDIF}
  if Options.ProxyActive and (Options.ProxyHost <> '') then
    AddRtmpDumpOption('S', Options.ProxyHost + ':' + Options.ProxyPort);
  AddRtmpDumpOption('o', FileName);
  LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
  if FileExists(LogFileName) then
    DeleteFile(PChar(LogFileName));
  SetLastErrorMsg(Format(ERR_SEE_LOGFILE, [LogFileName]));
  if not RtmpDump_Init then
    Raise ERTMPDownloaderError.CreateFmt(ERR_FAILED_TO_LOAD_DLL, ['rtmpdump_dll.dll']);
  RetCode := RtmpDump_Download(Integer(Self), RtmpDumpDownloadProgressCallback, PAnsiChar(AnsiString(LogFileName)), RtmpDumpOptions);
  case RetCode of
    0: // Download complete
         Result := True;
    2: // Incomplete download
         Result := (100*DownloadedBytes div TotalBytes) > 96; // May report incomplete even though it is not
    end;
end;

end.
