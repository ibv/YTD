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

unit downFacebook;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Facebook = class(THttpDownloader)
    private
    protected
      MovieUrlHQRegExp: TRegExp;
      MovieUrlLQRegExp: TRegExp;
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

// http://www.facebook.com/video/video.php?v=1131482863478
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*facebook\.com/video/.*?[?&]v=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h3\s+class="video_title\s+datawrap">(?P<TITLE>.*?)</h3>';
  REGEXP_MOVIE_URL_HQ = '\bswf_id_[0-9a-f]+\.addVariable\s*\(\s*"highqual_src"\s*,\s*"(?P<URL>.*?)"';
  REGEXP_MOVIE_URL_LQ = '\bswf_id_[0-9a-f]+\.addVariable\s*\(\s*"lowqual_src"\s*,\s*"(?P<URL>.*?)"';

{ TDownloader_Facebook }

class function TDownloader_Facebook.Provider: string;
begin
  Result := 'Facebook.com';
end;

class function TDownloader_Facebook.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Facebook.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieUrlHQRegExp := RegExCreate(REGEXP_MOVIE_URL_HQ, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlLQRegExp := RegExCreate(REGEXP_MOVIE_URL_LQ, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Facebook.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlHQRegExp);
  RegExFreeAndNil(MovieUrlLQRegExp);
  inherited;
end;

function TDownloader_Facebook.GetMovieInfoUrl: string;
begin
  Result := 'http://www.facebook.com/video/video.php?v=' + MovieID;
end;

function TDownloader_Facebook.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not (GetRegExpVar(MovieUrlHQRegExp, Page, 'URL', Url) or GetRegExpVar(MovieUrlLQRegExp, Page, 'URL', Url)) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    MovieURL := UrlDecode(Url);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Facebook);

end.
