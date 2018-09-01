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

unit xxxYouPorn;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_YouPorn = class(THttpDownloader)
    private
    protected
      MovieInfoUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youporn\.com/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>\s*(?:<img[^>]*>)?\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_MOVIE_INFOURL = '\.addVariable\s*\(\s*\''file''\s*,[^;'']*''(?P<URL>https?://[^'']+)''';

{ TDownloader_YouPorn }

class function TDownloader_YouPorn.Provider: string;
begin
  Result := 'YouPorn.com';
end;

class function TDownloader_YouPorn.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_YouPorn.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieInfoUrlRegExp := RegExCreate(REGEXP_MOVIE_INFOURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_YouPorn.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoUrlRegExp);
  inherited;
end;

function TDownloader_YouPorn.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youporn.com/watch/' + MovieID + '/';
end;

function TDownloader_YouPorn.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Url, InfoXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, Url, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'trackList/track/location', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := HtmlDecode(Url);
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

function TDownloader_YouPorn.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  Http.Cookies.Add('age_check=1');
  Result := inherited GetMovieInfoContent(Http, Url, Page, Method);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_YouPorn);
  {$ENDIF}

end.
