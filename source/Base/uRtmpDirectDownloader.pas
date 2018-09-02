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

unit uRtmpDirectDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, blcksock,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TRtmpDirectDownloader = class(TRtmpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      constructor CreateWithName(const AMovieID, AMovieName: string); virtual;
      destructor Destroy; override;
      function Prepare: boolean; override;
    end;

implementation

uses
  uDownloadClassifier,
  uLanguages, uMessages;

// rtmp://...
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'rtmpt?e?://.+';
  URLREGEXP_AFTER_ID =  '';

{ TRtmpDirectDownloader }

class function TRtmpDirectDownloader.Provider: string;
begin
  Result := 'RTMP direct download';
end;

class function TRtmpDirectDownloader.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TRtmpDirectDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

constructor TRtmpDirectDownloader.CreateWithName(const AMovieID, AMovieName: string);
begin
  Create(AMovieID);
  SetName(AMovieName);
end;

destructor TRtmpDirectDownloader.Destroy;
begin
  inherited;
end;

function TRtmpDirectDownloader.GetMovieInfoUrl: string;
begin
  Result := '';
end;

function TRtmpDirectDownloader.Prepare: boolean;
begin
  inherited Prepare;
  Result := False;
  if MovieID = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    if UnpreparedName = '' then
      SetName(ExtractUrlFileName(MovieID));
    MovieURL := MovieID;
    RtmpUrl := MovieID;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF DIRECTDOWNLOADERS}
  RegisterDownloader(TRtmpDirectDownloader);
  {$ENDIF}

end.
