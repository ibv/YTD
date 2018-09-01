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

unit uExternalDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader;

type
  EExternalDownloaderError = class(EDownloaderError);

  TExternalDownloader = class(TCommonDownloader)
    private
      fAborted: boolean;
      fDownloadedBytes: int64;
      fTotalBytes: int64;
    protected
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      procedure SetPrepared(Value: boolean); override;
      function GetTempDir: string; virtual;
      property DownloadedBytes: int64 read fDownloadedBytes write fDownloadedBytes;
      property TotalBytes: int64 read fTotalBytes write fTotalBytes;
      property Aborted: boolean read fAborted write fAborted;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      procedure AbortTransfer; override;
    end;

implementation

uses
  uCompatibility;

constructor TExternalDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TExternalDownloader.Destroy;
begin
  inherited;
end;

procedure TExternalDownloader.AbortTransfer;
begin
  inherited;
  Aborted := True;
end;

function TExternalDownloader.GetDownloadedSize: int64;
begin
  Result := DownloadedBytes;
end;

function TExternalDownloader.GetTotalSize: int64;
begin
  Result := TotalBytes;
end;

procedure TExternalDownloader.SetPrepared(Value: boolean);
begin
  inherited;
  DownloadedBytes := 0;
  TotalBytes := -1;
end;

function TExternalDownloader.GetTempDir: string;
const MAX_TEMP_PATH = MAX_PATH + 16;
begin
  SetLength(Result, MAX_TEMP_PATH);
  SetLength(Result, GetTempPath(MAX_TEMP_PATH, @(Result[1])));
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

end.
