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

unit downGameTrailers;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_GameTrailers = class(THttpDownloader)
    private
    protected
      UrlSectionRegExp: TRegExp;
      UrlsRegExp: TRegExp;
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

// http://www.gametrailers.com/video/e3-09-star-wars/49936?type=flv
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*gametrailers\.com/video/';
  URLREGEXP_ID =        '[^/]+/[0-9]+.*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_EXTRACT_URL_SECTION = '<span\s+class="downloads">(?P<DOWNLOADS>.*)</span>';
  REGEXP_EXTRACT_URLS = '<a\s+href="(?P<URL>.+?)"';

{ TDownloader_GameTrailers }

class function TDownloader_GameTrailers.Provider: string;
begin
  Result := 'GameTrailers.com';
end;

class function TDownloader_GameTrailers.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_GameTrailers.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  UrlSectionRegExp := RegExCreate(REGEXP_EXTRACT_URL_SECTION, [rcoIgnoreCase, rcoSingleLine]);
  UrlsRegExp := RegExCreate(REGEXP_EXTRACT_URLS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_GameTrailers.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(UrlSectionRegExp);
  RegExFreeAndNil(UrlsRegExp);
  inherited;
end;

function TDownloader_GameTrailers.GetMovieInfoUrl: string;
begin
  Result := 'http://www.gametrailers.com/video/' + MovieID;
end;

function TDownloader_GameTrailers.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var UrlList, Url, BestUrl, Ext: string;
    BestExt, ExtVal: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(UrlSectionRegExp, Page, 'DOWNLOADS', UrlList) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT))
  else if not UrlsRegExp.Match(UrlList) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    BestUrl := '';
    BestExt := 0;
    repeat
      if UrlsRegExp.SubexpressionByName('URL', Url) then
        begin
        Ext := UpperCase(ExtractUrlExt(Url));
        if Ext = '.FLV' then
          ExtVal := 1
        else if (Ext = '.MOV') or (Ext = '.QT') then
          ExtVal := 2
        else if (Ext = '.WMV') or (Ext = '.ASF') then
          ExtVal := 3
        else
          ExtVal := -1;
        if ExtVal > BestExt then
          begin
          BestExt := ExtVal;
          BestUrl := Url;
          end;
        end;
    until not UrlsRegExp.MatchAgain;
    if BestUrl = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := BestUrl;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_GameTrailers);

end.
