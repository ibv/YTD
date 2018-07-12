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

unit xxxRawTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_RawTube = class(THttpDownloader)
    private
    protected
      MovieIdRegExp: TRegExp;
      MovieSiteRegExp: TRegExp;
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rawtube\.com/videos/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_H1;
  REGEXP_MOVIE_ID =    '\bflashvars\.id\s*=\s*"(?P<ID>.+?)"';
  REGEXP_MOVIE_SITE =  '\bflashvars\.site\s*=\s*"(?P<SITE>.+?)"';

{ TDownloader_RawTube }

class function TDownloader_RawTube.Provider: string;
begin
  Result := 'RawTube.com';
end;

class function TDownloader_RawTube.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_RawTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID);
  MovieSiteRegExp := RegExCreate(REGEXP_MOVIE_SITE);
end;

destructor TDownloader_RawTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdRegExp);
  RegExFreeAndNil(MovieSiteRegExp);
  inherited;
end;

function TDownloader_RawTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rawtube.com/videos/' + MovieID;
end;

function TDownloader_RawTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Site, ID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieSiteRegExp, Page, 'SITE', Site) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else if not GetRegExpVar(MovieIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := 'http://' + Site + '.rawtube.com/flvs/' + ID + '.flv';
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_RawTube);
  {$ENDIF}

end.
