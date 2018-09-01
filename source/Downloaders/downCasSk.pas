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

unit downCasSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_CasSk = class(TNestedDownloader)
    private
    protected
      NestedUrl1RegExp, NestedUrl2RegExp: TRegExp;
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

// http://www.cas.sk/clanok/172890/testovanie-brzd-trochu-inak.html
// http://adam.cas.sk/clanky/7431/moto-aston-martin-rapide-2011.html
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*cas\.sk/(?:clanok|clanky)/[0-9]+/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1 class="article">(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL1 = '<object[^>]*\sdata="(?P<URL>https?://.+?)"';
  REGEXP_EXTRACT_URL2 = '\bso\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>https?://.+?)''';

{ TDownloader_CasSk }

class function TDownloader_CasSk.Provider: string;
begin
  Result := 'Cas.sk';
end;

class function TDownloader_CasSk.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CasSk.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrl1RegExp := RegExCreate(REGEXP_EXTRACT_URL1, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrl2RegExp := RegExCreate(REGEXP_EXTRACT_URL2, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CasSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedUrl1RegExp);
  RegExFreeAndNil(NestedUrl2RegExp);
  inherited;
end;

function TDownloader_CasSk.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CasSk.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  try
    NestedUrlRegExp := NestedUrl1RegExp;
    Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
    if not Result then
      begin
      NestedUrlRegExp := NestedUrl2RegExp;
      Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
      end;
  finally
    NestedUrlRegExp := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CasSk);

end.
