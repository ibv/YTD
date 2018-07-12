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

unit xxxSlutLoad;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SlutLoad = class(THttpDownloader)
    private
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
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*slutload\.com/watch/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>Video:\s*<b>(?P<TITLE>.*?)</b>';
  REGEXP_MOVIE_URL = '<param\s+name="FlashVars"\s+value="(?:[^"]*&)?flv=(?P<URL>http.+?)["&]';

{ TDownloader_SlutLoad }

class function TDownloader_SlutLoad.Provider: string;
begin
  Result := 'SlutLoad.com';
end;

class function TDownloader_SlutLoad.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_SlutLoad.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_SlutLoad.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_SlutLoad.GetMovieInfoUrl: string;
begin
  Result := 'http://www.slutload.com/watch/' + MovieID + '/';
end;

function TDownloader_SlutLoad.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
  MovieUrl := UrlDecode(MovieUrl);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_SlutLoad);
  {$ENDIF}

end.
