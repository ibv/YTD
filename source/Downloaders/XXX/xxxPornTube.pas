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

unit xxxPornTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uCompatibility,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornTube = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
      PlaylistItemRegExp: TRegExp;
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
  URLREGEXP_BEFORE_ID = 'porntube\.com/videos/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '\btitle\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_MOVIE_INFO = '\bembedPlayer\s*\(\s*(?P<ID>\d+)\s*,\s*\d+\s*,\s*\[\s*(?P<QUALITIES>(?:\d+,)*\d+)\]';
  REGEXP_PLAYLIST_ITEM = '(?:^\s*\{|,)\s*"(?P<QUALITY>\d+)"\s*:\s*\{\s*"status"\s*:\s*"success"\s*,\s*"token"\s*:\s*"(?P<URL>https?://.+?)"';

{ TDownloader_PornTube }

class function TDownloader_PornTube.Provider: string;
begin
  Result := 'PornTube.com';
end;

class function TDownloader_PornTube.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_PornTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
  PlaylistItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM);
end;

destructor TDownloader_PornTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  RegExFreeAndNil(PlaylistItemRegExp);
  inherited;
end;

function TDownloader_PornTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.porntube.com/videos/' + MovieID;
end;

function TDownloader_PornTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Qualities, Playlist, Url, sQuality, BestUrl: string;
  Quality, BestQuality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(MovieInfoRegExp, Page, ['ID', 'QUALITIES'], [@ID, @Qualities]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if (ID = '') or (Qualities = '') then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, 'http://tkn.porntube.com/' + ID + '/desktop/' + StringReplace(Qualities, ',', '+', [rfReplaceAll]), '{}', HTTP_FORM_URLENCODING, ['Origin: http://www.porntube.com'], Playlist) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVars(PlaylistItemRegExp, Playlist, ['URL', 'QUALITY'], [@Url, @sQuality]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    BestUrl := '';
    BestQuality := -1;
    repeat
      if Url <> '' then
        begin
        Quality := StrToIntDef(sQuality, 0);
        if Quality > BestQuality then
          begin
          BestQuality := Quality;
          BestUrl := Url;
          end;
        end;
    until not GetRegExpVarsAgain(PlaylistItemRegExp, ['URL', 'QUALITY'], [@Url, @sQuality]);
    if BestUrl = '' then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieUrl := BestUrl;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornTube);
  {$ENDIF}

end.
