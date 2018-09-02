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

unit downViki;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Viki = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
      StreamListRegExp: TRegExp;
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

//http://a1.vikiassets.com/channels/1565-secret-garden/videos/32690
//http://www.viki.com/channels/1565-secret-garden/videos/32690
const
  URLREGEXP_BEFORE_ID = 'viki(?:assets)?\.com/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_ID =     '<link\s+rel="media:video"\s+href="[^"]*?/(?P<ID>[0-9]+)/?"';
  REGEXP_MOVIE_STREAMS = '"streams"\s*:\s*\[\s*(?P<LIST>.*?)\s*\]\s*,';
  REGEXP_MOVIE_STREAM = '\{[^}]*"uri"\s*:\s*"(?P<URL>https?://[^"]+)"[^}]*"quality"\s*:\s*"(?P<QUALITY>[0-9]+)p"[^}]*\}';

{ TDownloader_Viki }

class function TDownloader_Viki.Provider: string;
begin
  Result := 'Viki.com';
end;

class function TDownloader_Viki.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Viki.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
  StreamListRegExp := RegExCreate(REGEXP_MOVIE_STREAMS);
  StreamInfoRegExp := RegExCreate(REGEXP_MOVIE_STREAM);
end;

destructor TDownloader_Viki.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(StreamListRegExp);
  RegExFreeAndNil(StreamInfoRegExp);
  inherited;
end;

function TDownloader_Viki.GetMovieInfoUrl: string;
begin
  Result := 'http://www.viki.com/' + MovieID;
end;

function TDownloader_Viki.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Info, StreamList, BestUrl, Url, sQuality: string;
  BestQuality, Quality: integer;
  b: boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not DownloadPage(Http, 'http://www.viki.com/player/medias/' + ID + '/info.json?rtmp=false', Info, peUtf8) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(StreamListRegExp, Info, 'LIST', StreamList) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else
    begin
    BestUrl := '';
    BestQuality := -1;
    b := GetRegExpVars(StreamInfoRegExp, StreamList, ['URL', 'QUALITY'], [@Url, @sQuality]);
    while b do
      begin
      Quality := StrToIntDef(sQuality, 0);
      if Quality > BestQuality then
        begin
        BestUrl := Url;
        BestQuality := Quality;
        end;
      b := GetRegExpVarsAgain(StreamInfoRegExp, ['URL', 'QUALITY'], [@Url, @sQuality]);
      end;
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
  RegisterDownloader(TDownloader_Viki);

end.
