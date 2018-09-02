(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
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

unit downCT_wrapper;
{$INCLUDE 'ytd.inc'}
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format


interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  uDownloader, uCommonDownloader, uNestedDownloader, downCT;

type
  TDownloader_CT_wrapper = class(TNestedDownloader)
    private
    protected
      IFrameRegExp: TRegExp;
      JavaScriptRegExp: TRegExp;
      IFrameFromAjaxRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function FindNestedUrl(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean; override;
      function GetUrlFromIFRAME(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean;
      function GetUrlFromJS(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uStrings,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?:ceskatelevize|ct24)\.cz/(?!ivysilani/).+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+id="mainPanel".*' + REGEXP_TITLE_H1;
  //REGEXP_IFRAME_URL = '<(?:iframe\b[^>]*\ssrc|a\b[^>]*\shref)="(?P<URL>(https?://[^/]+)?/ivysilani/[^"]+)"';
  REGEXP_IFRAME_URL = '<(?:iframe\b[^>]*\ssrc)="(?P<URL>(https?://[^/]+)?/ivysilani/[^"]+)"';
  REGEXP_JAVASCRIPT_ID = '\shref="javascript:void\s*\(\s*q\s*=\s*''(?P<ID>.+?)''';
  REGEXP_JAVASCRIPT_URL = '"videoPlayerUrl"\s*:\s*"(?P<URL>.+?)"';

{ TDownloader_CT_wrapper }

class function TDownloader_CT_wrapper.Provider: string;
begin
  Result := TDownloader_CT.Provider;
end;

class function TDownloader_CT_wrapper.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_CT_wrapper.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  IFrameRegExp := RegExCreate(REGEXP_IFRAME_URL);
  JavaScriptRegExp := RegExCreate(REGEXP_JAVASCRIPT_ID);
  IFrameFromAjaxRegExp := RegExCreate(REGEXP_JAVASCRIPT_URL);
end;

destructor TDownloader_CT_wrapper.Destroy;
begin
  inherited;
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(IFrameRegExp);
  RegExFreeAndNil(JavaScriptRegExp);
  RegExFreeAndNil(IFrameFromAjaxRegExp);
end;

function TDownloader_CT_wrapper.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CT_wrapper.GetUrlFromIFRAME(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean;
begin
  Result := False;
  if GetRegExpVar(IFrameRegExp, Page, 'URL', Url) and (Url <> '') then
    begin
    Url := GetRelativeUrl(GetMovieInfoUrl, Url);
    Result := True;
    end;
end;

function TDownloader_CT_wrapper.GetUrlFromJS(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean;
var
  ID, IFramePage: string;
begin
  Result := False;
  if GetRegExpVar(JavaScriptRegExp, Page, 'ID', ID) and (ID <> '') then
    if DownloadPage(Http, 'http://www.ceskatelevize.cz/ct24/ajax/', 'cmd=getVideoPlayerUrl&q=' + {$IFDEF UNICODE} AnsiString {$ENDIF} (ID), HTTP_FORM_URLENCODING_UTF8, IFramePage, peUtf8) then
      if GetRegExpVar(IFrameFromAjaxRegExp, IFramePage, 'URL', Url) then
        begin
        Url := GetRelativeUrl(GetMovieInfoUrl, JSDecode(Url));
        Result := True;
        end;
end;

function TDownloader_CT_wrapper.FindNestedUrl(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Url: string): boolean;
begin
  Result := GetUrlFromIFRAME(Page, PageXml, Http, Url) or GetUrlFromJS(Page, PageXml, Http, Url);
end;

initialization
  RegisterDownloader(TDownloader_CT_wrapper);

end.

