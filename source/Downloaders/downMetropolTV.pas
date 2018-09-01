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

unit downMetropolTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MetropolTV = class(THttpDownloader)
    private
    protected
      MovieParamsUrlRegExp: TRegExp; 
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

// http://www.tvpraha11.cz/zastupitelstvo/index.php?file=9285&jednani=66
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tvpraha11.cz/zastupitelstvo/.*?[?&]';
  URLREGEXP_ID =        'file=[0-9]+&jednani=[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>Právì sledujete</h2>\s*</div>\s*<div[^>]*>\s*<p[^>]*>\s*(?P<TITLE>.*?)\s*</';
  REGEXP_MOVIE_PARAMS_URL = '''FlashVars''\s*,\s*''videos=(?P<URL>xml/[^'']+\.xml)''';

const
  PageRoot = 'http://www.tvpraha11.cz/zastupitelstvo/';

{ TDownloader_MetropolTV }

class function TDownloader_MetropolTV.Provider: string;
begin
  Result := 'TVPraha11.cz';
end;

class function TDownloader_MetropolTV.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_MetropolTV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieParamsUrlRegExp := RegExCreate(REGEXP_MOVIE_PARAMS_URL);
end;

destructor TDownloader_MetropolTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieParamsUrlRegExp);
  inherited;
end;

function TDownloader_MetropolTV.GetMovieInfoUrl: string;
begin
  Result := PageRoot + 'index.php?' + MovieID;
end;

function TDownloader_MetropolTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieParamsUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, PageRoot + Url, Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlVar(Xml, 'video/streams', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieURL := PageRoot + Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_MetropolTV);

end.
