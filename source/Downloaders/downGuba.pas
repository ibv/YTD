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

unit downGuba;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Guba = class(THttpDownloader)
    private
    protected
      VideoFromPlayerRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.guba.com/watch/3000693318/Pulp-Fiction-Laugh-Track
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*guba\.com/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2 class="title"><a\s[^>]*>(?P<TITLE>.*?)</a></h2>';
  REGEXP_MOVIE_PLAYER = '<link\s+rel="video_src"\s+href="(?P<URL>https?://[^"]+)"';
  REGEXP_VIDEO_FROM_PLAYER = '"url"\s*:\s*"(?P<URL>https?://.*?)"';

{ TDownloader_Guba }

class function TDownloader_Guba.Provider: string;
begin
  Result := 'Guba.com';
end;

class function TDownloader_Guba.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Guba.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  VideoFromPlayerRegExp := RegExCreate(REGEXP_VIDEO_FROM_PLAYER);
end;

destructor TDownloader_Guba.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoFromPlayerRegExp);
  inherited;
end;

function TDownloader_Guba.GetMovieInfoUrl: string;
begin
  Result := 'http://www.guba.com/watch/' + MovieID + '/';
end;

function TDownloader_Guba.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadPage(Http, 'http://www.guba.com/playerConfig?bid=' + MovieID, Page) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(VideoFromPlayerRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    MovieURL := Url;
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_Guba);

end.
