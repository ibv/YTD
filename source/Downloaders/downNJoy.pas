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

unit downNJoy;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_NJoy = class(TNestedDownloader)
    private
    protected
      HttpProtocolRegExp: TRegExp;
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
  uStringConsts,
  uHttpDirectDownloader,
  uMSDirectDownloader,
  uDownloadClassifier,
  uMessages;

// http://n-joy.cz/video/supcom-2-zabery-z-hrani-2/oiuhz6e3xgt35e4e
// http://n-joy.cz/right-now/video/honza-b/psvfmt8b61ghlgg4
// http://n-joy.cz/right-now/audio/vera-spinarova/2zpSLkXoR5IteMyC
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*n-joy\.cz/(?:right-now/)?(?:video|audio)/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?:N-JOY.CZ - )?(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_URL = '\sflashvars\.file(?:_url)?\s*=\s*"(?P<URL>.*?)"';
  REGEXP_PROTOCOL_HTTP = '^https?://';

type
  TDownloader_NJoy_HTTP = class(THttpDirectDownloader);
  TDownloader_NJoy_MMS = class(TMSDirectDownloader);


{ TDownloader_NJoy }

class function TDownloader_NJoy.Provider: string;
begin
  Result := 'N-joy.cz';
end;

class function TDownloader_NJoy.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_NJoy.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
  HttpProtocolRegExp := RegExCreate(REGEXP_PROTOCOL_HTTP);
end;

destructor TDownloader_NJoy.Destroy;
begin
  RegExFreeAndNil(HttpProtocolRegExp);
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_NJoy.GetMovieInfoUrl: string;
begin
  Result := 'http://n-joy.cz/video/dummy/' + MovieID;
end;

function TDownloader_NJoy.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := Prepared;
  if Result then
    if HttpProtocolRegExp.Match(MovieURL) then
      Result := CreateNestedDownloaderFromDownloader(TDownloader_NJoy_HTTP.Create(MovieUrl, Name))
    else
      Result := CreateNestedDownloaderFromDownloader(TDownloader_NJoy_MMS.Create(MovieUrl, Name))
end;

initialization
  RegisterDownloader(TDownloader_NJoy);

end.
