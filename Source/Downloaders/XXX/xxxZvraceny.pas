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

unit xxxZvraceny;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Zvraceny = class(THttpDownloader)
    private
    protected
      MovieUrlJSRegExp: TRegExp;
      MovieHDUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
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
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = 'zvraceny\.cz/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>\s*(?P<TITLE>.*?)\s*(?:.\s*video\s*)?(?:\|\s*Zvrácený\.cz\s*)?</title>';
  REGEXP_EXTRACT_URL = '\.addVariable\s*\(\s*(?P<QUOTE1>[''"])file(?P=QUOTE1)\s*,\s*(?P<QUOTE2>[''"])(?P<URL>https?://.+?)(?:(?P=QUOTE2)|&)';
  REGEXP_EXTRACT_HDURL = '\.addVariable\s*\(\s*(?P<QUOTE1>[''"])hd\.file(?P=QUOTE1)\s*,\s*(?P<QUOTE2>[''"])(?P<URL>https?://.+?)(?:(?P=QUOTE2)|&)';
  REGEXP_EXTRACT_URLJS = '<div\s+id="prehravac".*?<script\s+[^>]*\bsrc="(?P<PATH>/js/(?!jquery\.).+?)"';

{ TDownloader_Zvraceny }

class function TDownloader_Zvraceny.Provider: string;
begin
  Result := 'Zvraceny.cz';
end;

class function TDownloader_Zvraceny.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Zvraceny.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
  MovieHDUrlRegExp := RegExCreate(REGEXP_EXTRACT_HDURL);
  MovieUrlJSRegExp := RegExCreate(REGEXP_EXTRACT_URLJS);
end;

destructor TDownloader_Zvraceny.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(MovieHDUrlRegExp);
  RegExFreeAndNil(MovieUrlJSRegExp);
  inherited;
end;

function TDownloader_Zvraceny.GetMovieInfoUrl: string;
begin
  Result := 'http://www.zvraceny.cz/video/' + MovieID;
end;

function TDownloader_Zvraceny.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Http.Cookies.Values['cc_overeni_vstupu'] := '1';
  Result := inherited GetMovieInfoContent(Http, Url, Page, Xml, Method);
end;

function TDownloader_Zvraceny.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Path, Info, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieUrlJSRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, 'http://www.zvraceny.cz' + Path, Info, peAnsi) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not (GetRegExpVar(MovieHDUrlRegExp, Info, 'URL', Url) or GetRegExpVar(MovieUrlRegExp, Info, 'URL', Url)) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_Zvraceny);
  {$ENDIF}

end.
