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

unit downIHned;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  uHttpDirectDownloader, uRtmpDirectDownloader;

type
  TDownloader_IHned = class(TNestedDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
      MovieUrlsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uFunctions,
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://video.ihned.cz/c1-44411910-sef-narodniho-muzea-kdyz-zafouka-vitr-vypadne-nam-za-noc-az-deset-oken
// http://video.ihned.cz/c1-44411910
// http://dialog.ihned.cz/komentare/c1-52351350-vondraczech-at-se-urednici-uci-v-jaderne-elektrarne
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + 'ihned\.cz/.+';
  URLREGEXP_AFTER_ID =  '$';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_H1;
  REGEXP_MOVIE_INFO = '"video_urls"\s*:\s*"(?P<URL>https?:.+?)"';
  REGEXP_MOVIE_URLS = '"(?:alt)?(?P<BITRATE>\d+)"\s*:\s*\[\s*"(?P<URL>(?:https?|rtmpt?e?):[^"]+)"\s*\]';

{ TDownloader_IHned }

class function TDownloader_IHned.Provider: string;
begin
  Result := 'iHned.cz';
end;

class function TDownloader_IHned.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_IHned.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
  MovieUrlsRegExp := RegExCreate(REGEXP_MOVIE_URLS);
end;

destructor TDownloader_IHned.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  RegExFreeAndNil(MovieUrlsRegExp);
  inherited;
end;

function TDownloader_IHned.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_IHned.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
var
  sBitrate, BestUrl, Url, VideoDef: string;
  Bitrate, BestBitrate: integer;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  Result := False;
  if not GetRegExpVar(MovieInfoRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, JSDecode(Url), VideoDef, peAnsi) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if GetRegExpVars(MovieUrlsRegExp, VideoDef, ['BITRATE', 'URL'], [@sBitrate, @Url]) then
    begin
    BestUrl := '';
    BestBitrate := -1;
    repeat
      Url := JSDecode(Url);
      Bitrate := StrToIntDef(sBitrate, 0);
      if IsHttpProtocol(Url) then
        Inc(Bitrate);
      if Bitrate > BestBitrate then
        begin
        BestUrl := Url;
        BestBitrate := Bitrate;
        end;
    until not GetRegExpVarsAgain(MovieUrlsRegExp, ['BITRATE', 'URL'], [@sBitrate, @Url]);
    if BestUrl <> '' then
      begin
      if IsHttpProtocol(BestUrl) then
        Downloader := THttpDirectDownloader.Create(BestUrl)
      else
        Downloader := TRtmpDirectDownloader.Create(BestUrl);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_IHned);

end.
