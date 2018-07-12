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

unit downSchoolOfCare;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SchoolOfCare = class(THttpDownloader)
    private
    protected
      MovieIdRegExp: TRegExp;
      MoviePathRegExp: TRegExp;
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

// http://schoolofcare.ru/masha/339/
const
  URLREGEXP_BEFORE_ID = 'schoolofcare\.ru/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<div[^>]*\sclass="head"[^>]*>\s*<p>(?P<TITLE>.*?)</p>';
  REGEXP_MOVIE_ID =     '\bvar\s+id\s*=\s*''(?P<ID>.+?)''';
  REGEXP_MOVIE_PATH =   '<a\s+href="(?P<PATH>/.+?)"';

{ TDownloader_SchoolOfCare }

class function TDownloader_SchoolOfCare.Provider: string;
begin
  Result := 'SchoolOfCare.ru';
end;

class function TDownloader_SchoolOfCare.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_SchoolOfCare.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID);
  MoviePathRegExp := RegExCreate(REGEXP_MOVIE_PATH);
end;

destructor TDownloader_SchoolOfCare.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdRegExp);
  RegExFreeAndNil(MoviePathRegExp);
  inherited;
end;

function TDownloader_SchoolOfCare.GetMovieInfoUrl: string;
begin
  Result := 'http://schoolofcare.ru/' + MovieID;
end;

function TDownloader_SchoolOfCare.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Info, Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not DownloadPage(Http, 'http://schoolofcare.ru/actions/getVideoCode/?ID=' + UrlEncode(ID), Info) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(MoviePathRegExp, Info, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := 'http://schoolofcare.ru' + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_SchoolOfCare);

end.
