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

unit xxxRozzlobeniMuzi;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_RozzlobeniMuzi = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
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

// http://www.rozzlobenimuzi.com/?linkid=23073-konec_sikany.htm
const
  URLREGEXP_BEFORE_ID = 'rozzlobenimuzi\.com/.*?[?&]linkid=';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)(?:\s*-\s*RozzlobeniMuzi\.com.*?)?</title>';
  REGEXP_EXTRACT_URL = '\bfile\s*:\s*"(?P<URL>((?!/reklama/).)+?)"';

{ TDownloader_RozzlobeniMuzi }

class function TDownloader_RozzlobeniMuzi.Provider: string;
begin
  Result := 'RozzlobeniMuzi.com';
end;

class function TDownloader_RozzlobeniMuzi.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_RozzlobeniMuzi.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
end;

destructor TDownloader_RozzlobeniMuzi.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_RozzlobeniMuzi.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rozzlobenimuzi.com/?linkid=' + MovieID; // + '&version=' + {$IFDEF XXX} 'hardcore' {$ELSE} 'pinkcore' {$ENDIF} ;
end;

function TDownloader_RozzlobeniMuzi.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  // I need to get the cookie before I get the real page
  Result := False;
  Http.Cookies.Values['version'] := 'hardcore';
  Http.Cookies.Values['rozzlobenimuziacknow'] := 'yes';
  if inherited GetMovieInfoContent(Http, Url, Page, Xml, Method) then
    begin
    FreeAndNil(Xml);
    Result := inherited GetMovieInfoContent(Http, Url, Page, Xml, Method);
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_RozzlobeniMuzi);
  {$ENDIF}

end.
