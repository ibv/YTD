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

unit downCestyKSobe;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CestyKSobe = class(THttpDownloader)
    private
    protected
      MovieTitle2RegExp: TRegExp;
      MovieUrl2RegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://www.cestyksobe.cz/novinky/nejnovejsi-a-nejzajimavejsi-porady/642.html?quality=high
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*cestyksobe\.cz/';
  URLREGEXP_ID =        '[^/?&]+/[^/?&]+/[0-9]+\.html';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h3>(?P<TITLE>.*?)</h3>';
  REGEXP_EXTRACT_TITLE2 = '<h1[^>]*>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_MOVIEURL = '\bflashvars\s*:\s*"[^"]*&streamscript=(?P<URL>/[^"&]+)';
  REGEXP_EXTRACT_MOVIEURL2 = '\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>/.+?)''';

{ TDownloader_CestyKSobe }

class function TDownloader_CestyKSobe.Provider: string;
begin
  Result := 'CestyKSobe.sk';
end;

class function TDownloader_CestyKSobe.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_CestyKSobe.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieTitle2RegExp := RegExCreate(REGEXP_EXTRACT_TITLE2);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_MOVIEURL);
  MovieUrl2RegExp := RegExCreate(REGEXP_EXTRACT_MOVIEURL2);
end;

destructor TDownloader_CestyKSobe.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieTitle2RegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(MovieUrl2RegExp);
  inherited;
end;

function TDownloader_CestyKSobe.GetMovieInfoUrl: string;
begin
  Result := 'http://www.cestyksobe.cz/' + MovieID + '?quality=high';
end;

function TDownloader_CestyKSobe.GetFileNameExt: string;
begin
  Result := inherited GetFileNameExt;
  if AnsiCompareText(Result, '.php') = 0 then
    Result := '.flv';
end;

function TDownloader_CestyKSobe.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var s: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if MovieURL = '' then
    if GetRegExpVar(MovieUrl2RegExp, Page, 'URL', s) then
      MovieURL := s;
  if Name = '' then
    if GetRegExpVar(MovieTitle2RegExp, Page, 'TITLE', s) then
      SetName(s);
  if MovieURL <> '' then
    begin
    MovieURL := 'http://www.cestyksobe.cz' + MovieURL;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CestyKSobe);

end.
