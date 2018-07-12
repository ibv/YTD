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

unit downFunnyOrDie;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FunnyOrDie = class(THttpDownloader)
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

// http://www.funnyordie.com/videos/544d80e015/where-it-is-fortunately-not-yet
const
  URLREGEXP_BEFORE_ID = 'funnyordie\.com/videos/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_LIST =   '\bvideo_tag\.attr\s*\(\s*''src''\s*,\s*''(?P<URL>https?://[^'']+?(?P<BITRATE>\d+)\.mp4)''';

{ TDownloader_FunnyOrDie }

class function TDownloader_FunnyOrDie.Provider: string;
begin
  Result := 'FunnyOrDie.com';
end;

class function TDownloader_FunnyOrDie.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_FunnyOrDie.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  UrlListRegExp := RegExCreate(REGEXP_MOVIE_LIST);
end;

destructor TDownloader_FunnyOrDie.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(UrlListRegExp);
  inherited;
end;

function TDownloader_FunnyOrDie.GetMovieInfoUrl: string;
begin
  Result := 'http://www.funnyordie.com/videos/' + MovieID;
end;

function TDownloader_FunnyOrDie.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Url, BestUrl, sBitrate: string;
  Bitrate, BestBitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BestUrl := '';
  BestBitrate := -1;
  if GetRegExpVars(UrlListRegExp, Page, ['URL', 'BITRATE'], [@Url, @sBitrate]) then
    repeat
      Bitrate := StrToIntDef(sBitrate, 0);
      if Bitrate > BestBitrate then
        begin
        BestBitrate := Bitrate;
        BestUrl := Url;
        end;
    until not GetRegExpVarsAgain(UrlListRegExp, ['URL', 'BITRATE'], [@Url, @sBitrate]);
  if BestUrl <> '' then
    begin
    MovieUrl := BestUrl;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_FunnyOrDie);

end.
