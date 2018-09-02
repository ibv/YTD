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

unit downVideoPortalSfTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_VideoPortalSfTV = class(TRtmpDownloader)
    private
    protected
      UrlListRegExp: TRegExp;
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

// http://www.videoportal.sf.tv/video?id=b78196ea-a1b2-42e2-a554-a9b0ffa4a4bc
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videoportal\.sf\.tv/.*[?&]id=';
  URLREGEXP_ID =        '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '"description_title"\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_URL_LIST = '"bitrate"\s*:\s*(?P<BITRATE>[0-9]+)\s*,.*?"url"\s*:\s*"(?P<URL>rtmpt?e?:[^"]+)"';

{ TDownloader_VideoPortalSfTV }

class function TDownloader_VideoPortalSfTV.Provider: string;
begin
  Result := 'VideoPortal.sf.tv';
end;

class function TDownloader_VideoPortalSfTV.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_VideoPortalSfTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  UrlListRegExp := RegExCreate(REGEXP_URL_LIST);
end;

destructor TDownloader_VideoPortalSfTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(UrlListRegExp);
  inherited;
end;

function TDownloader_VideoPortalSfTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videoportal.sf.tv/cvis/segment/' + MovieID + '/.json?nohttperr=1;omit_video_segments_validity=1;omit_related_segments=1;nearline_data=1';
end;

function TDownloader_VideoPortalSfTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, BestUrl, sBitrate: string;
    Bitrate, BestBitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BestUrl := '';
  BestBitrate := -1;
  if UrlListRegExp.Match(Page) then
    repeat
      if UrlListRegExp.SubexpressionByName('URL', Url) then
        begin
        if UrlListRegExp.SubexpressionByName('BITRATE', sBitrate) then
          Bitrate := StrToIntDef(sBitrate, 0)
        else
          Bitrate := 0;
        if Bitrate > BestBitrate then
          begin
          BestUrl := StripSlashes(Url);
          BestBitrate := Bitrate;
          end;
        end;
    until not UrlListRegExp.MatchAgain;
  if BestUrl <> '' then
    begin
    MovieUrl := BestUrl;
    Self.RtmpUrl := BestUrl;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VideoPortalSfTV);

end.
