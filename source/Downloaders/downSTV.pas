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

unit downSTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_STV = class(TRtmpDownloader)
    private
    protected
      StreamIdRegExp: TRegExp;
      StreamInfoRegExp: TRegExp;
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

// http://www.stv.sk/online/archiv/nebicko-v-papulke?date=2013-03-09&id=52605&playerType=
const
  URLREGEXP_BEFORE_ID = 'stv\.sk/online/archiv/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  MOVIE_TITLE_REGEXP = '<h2>(?P<TITLE>.*?)<';
  MOVIE_ID_REGEXP = '\bstream_id\s*:\s*"(?P<ID>.+?)"';
  MOVIE_INFO_REGEXP = '\breturn\s+''(?P<SERVER>rtmpt?e?://[^'']+)''.+?\?auth=(?P<AUTH>[^'']+)''';

{ TDownloader_STV }

class function TDownloader_STV.Provider: string;
begin
  Result := 'RTVS.sk';
end;

class function TDownloader_STV.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_STV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP);
  StreamIdRegExp := RegExCreate(MOVIE_ID_REGEXP);
  StreamInfoRegExp := RegExCreate(MOVIE_INFO_REGEXP);
end;

destructor TDownloader_STV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(StreamIdRegExp);
  RegExFreeAndNil(StreamInfoRegExp);
  inherited;
end;

function TDownloader_STV.GetMovieInfoUrl: string;
begin
  Result := 'http://www1.stv.sk/online/archiv/' + MovieID;
end;

function TDownloader_STV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, AuthPage, Auth, Server: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Referer := GetMovieInfoUrl;
  if not GetRegExpVar(StreamIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else if not DownloadPage(Http, 'http://embed.stv.livebox.sk/v1/tv-arch.js', AuthPage) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetRegExpVars(StreamInfoRegExp, AuthPage, ['SERVER', 'AUTH'], [@Server, @Auth]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if Server = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['server']))
  else if Auth = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['auth']))
  else
    begin
    Auth := Auth + IntToStr(UnixTimestamp);
    Server := StringReplace(Server, '/_definst_/', '', []);
    MovieUrl := Server + '?auth=' + Auth;
    Self.RtmpUrl := MovieUrl;
    Self.Playpath := 'mp4:' + ID + '?auth=' + Auth;
    //Self.FlashVer := 'WIN 11,3,300,268'; //FLASH_DEFAULT_VERSION;
    //Self.TcUrl := MovieUrl;
    //Self.SwfVfy := 'http://embed.stv.livebox.sk/v1/LiveboxPlayer.swf';
    //Self.PageUrl := GetMovieInfoUrl;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_STV);

end.
