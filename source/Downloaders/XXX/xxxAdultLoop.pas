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

unit xxxAdultLoop;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_AdultLoop = class(THttpDownloader)
    private
    protected
      MovieUrlsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function IsBetterUrl(const BestUrl, Url: string): boolean;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*adultloop\.com/v2/video/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="video_title">(?P<TITLE>.*?)</div>';
  REGEXP_MOVIE_URLS = '\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>https?://[^'']+)''';

{ TDownloader_AdultLoop }

class function TDownloader_AdultLoop.Provider: string;
begin
  Result := 'AdultLoop.com';
end;

class function TDownloader_AdultLoop.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_AdultLoop.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlsRegExp := RegExCreate(REGEXP_MOVIE_URLS);
end;

destructor TDownloader_AdultLoop.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlsRegExp);
  inherited;
end;

function TDownloader_AdultLoop.GetMovieInfoUrl: string;
begin
  Result := 'http://www.adultloop.com/v2/video/' + MovieID;
end;

function TDownloader_AdultLoop.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Urls: TStringArray;
    BestUrl: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BestUrl := '';
  if GetRegExpAllVar(MovieUrlsRegExp, Page, 'URL', Urls) then
    begin
    BestUrl := '';
    for i := 0 to Pred(Length(Urls)) do
      if BestUrl = '' then
        BestUrl := Urls[i]
      else if IsBetterUrl(BestUrl, Urls[i]) then
        BestUrl := Urls[i];
    end;
  if BestUrl = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := BestUrl;
    SetPrepared(True);
    Result := True;
    end;
end;

function TDownloader_AdultLoop.IsBetterUrl(const BestUrl, Url: string): boolean;
begin
  Result := AnsiCompareText(ExtractUrlExt(Url), '.mp4') = 0;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_AdultLoop);
  {$ENDIF}

end.
