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

unit downSoundCloud;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SoundCloud = class(THttpDownloader)
    private
    protected
      ClientUrlRegExp: TRegExp;
      ClientIdRegExp: TRegExp;
      ClientID: string;
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
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://soundcloud.com/ubi_irina/1-campaign-menu
const
  URLREGEXP_BEFORE_ID = 'soundcloud\.com/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '"title"\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_MOVIE_URL =    '"stream_url"\s*:\s*"(?P<URL>https?://.+?)"';
  REGEXP_CLIENT_URL =   '"layouts/stream"\s*:\s*"(?P<URL>.+?)"';
  //REGEXP_CLIENT_URL =   '<script\s+src="(?P<URL>//[^"]+)">';
  REGEXP_CLIENT_ID =    '\bclientId\s*=\s*"(?P<ID>.+?)"';

{ TDownloader_SoundCloud }

class function TDownloader_SoundCloud.Provider: string;
begin
  Result := 'SoundCloud.com';
end;

class function TDownloader_SoundCloud.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_SoundCloud.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  ClientUrlRegExp := RegExCreate(REGEXP_CLIENT_URL);
  ClientIdRegExp := RegExCreate(REGEXP_CLIENT_ID);
end;

destructor TDownloader_SoundCloud.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(ClientUrlRegExp);
  RegExFreeAndNil(ClientIdRegExp);
  inherited;
end;

function TDownloader_SoundCloud.GetMovieInfoUrl: string;
begin
  Result := 'http://soundcloud.com/' + MovieID;
end;

function TDownloader_SoundCloud.GetFileNameExt: string;
begin
  Result := '.mp3';
end;

function TDownloader_SoundCloud.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Url, ClientPage, ID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Exit; {$IFDEF DELPHI7_UP} {$MESSAGE WARN 'Incomplete'} {$ENDIF}
  if not GetRegExpVar(ClientUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, GetRelativeUrl(GetMovieInfoUrl, Url), ClientPage) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(ClientIdRegExp, ClientPage, 'ID', ID) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else
        begin
        ClientID := ID;
        Result := True;
        end;
end;

initialization
  RegisterDownloader(TDownloader_SoundCloud);

end.
