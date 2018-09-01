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

unit downBlipTvV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBlipTv;

type
  TDownloader_BlipTvV2 = class(TDownloader_BlipTv)
    private
    protected
      MovieIDFromPageRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

// http://blip.tv/file/108391
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*blip\.tv/file/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_PAGE = '<link\s+rel="video_src"\s+href="http://(?:www\.)?blip\.tv/play/(?P<MOVIEID>[^"]+)"';

{ TDownloader_BlipTvV2 }

class function TDownloader_BlipTvV2.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BlipTvV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PAGE, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTvV2.Destroy;
begin
  RegExFreeAndNil(MovieIDFromPageRegExp);
  inherited;
end;

function TDownloader_BlipTvV2.GetMovieInfoUrl: string;
var Http: THttpSend;
    Page, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://blip.tv/file/' + MovieID, Page) then
      if GetRegExpVar(MovieIDFromPageRegExp, Page, 'MOVIEID', ID) then
        begin
        MovieID := ID;
        Result := inherited GetMovieInfoUrl;
        end;
  finally
    Http.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BlipTvV2);

end.
