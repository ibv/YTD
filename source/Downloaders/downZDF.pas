(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit downZDF;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_ZDF = class(TMSDownloader)
    protected
      UrlListRegExp: TRegExp;
      BitrateRegExp: TRegExp;
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

// http://www.zdf.de/ZDFmediathek/beitrag/video/1246826/Vorschau-zur-Sendung-vom-09.03.2011#/beitrag/video/1308766/Ägypten---4-Geheimnis-des-ewigen-Lebens
const
  URLREGEXP_BEFORE_ID = 'zdf\.de/.*(?<=/)video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '[^#]*$';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_H1;
  REGEXP_MOVIE_URLS =   '<a\s+href="(?P<URL>https?://[^/"]*streaming\.zdf\.de/.+?)"';
  REGEXP_MOVIE_BITRATE = '/(?P<BITRATE>[^/]+)/[0-9]+[^/]*$';

{ TDownloader_ZDF }

class function TDownloader_ZDF.Provider: string;
begin
  Result := 'ZDF.de';
end;

class function TDownloader_ZDF.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_ZDF.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  UrlListRegExp := RegExCreate(REGEXP_MOVIE_URLS);
  BitrateRegExp := RegExCreate(REGEXP_MOVIE_BITRATE);
end;

destructor TDownloader_ZDF.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(UrlListRegExp);
  RegExFreeAndNil(BitrateRegExp);
  inherited;
end;

function TDownloader_ZDF.GetMovieInfoUrl: string;
begin
  Result := Format('http://www.zdf.de/ZDFmediathek//beitrag/video/%s/', [MovieID]);
end;

function TDownloader_ZDF.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Urls: TStringArray;
    Url, sBitrate: string;
    i, BestUrlIndex, BestBitrate, Bitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpAllVar(UrlListRegExp, Page, 'URL', Urls) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    BestUrlIndex := -1;
    BestBitrate := -1;
    for i := 0 to Pred(Length(Urls)) do
      if GetRegExpVar(BitrateRegExp, Urls[i], 'BITRATE', sBitrate) then
        begin
        if sBitrate = 'veryhigh' then
          Bitrate := 2000
        else
          Bitrate := StrToIntDef(sBitrate, 0);
        if Bitrate > BestBitrate then
          begin
          BestUrlIndex := i;
          BestBitrate := Bitrate;
          end;
        end;
    if BestUrlIndex < 0 then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
    else if not DownloadXmlAttr(Http, Urls[BestUrlIndex], 'Entry/Ref', 'href', Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieUrl := Url;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_ZDF);

end.
