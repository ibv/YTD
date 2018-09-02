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

unit downBarrandovTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2007_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions, uCompatibility,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BarrandovTV = class(THttpDownloader)
    private
    protected
      MovieUrlsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

const
  OPTION_BARRANDOV_AVOIDHD {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'avoid_hd';
  OPTION_BARRANDOV_AVOIDHD_DEFAULT = False;

implementation

uses
  uStringConsts,
  uStrings,
  uMessages,
  uDownloadClassifier;

// http://www.barrandov.tv/video/15468-hlavni-zpravy-24-6-2013
const
  URLREGEXP_BEFORE_ID = 'barrandov\.tv/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_H1;
  REGEXP_MOVIE_URLS = '\{\s*file\s*:\s*"(?P<URL>[^"]+\.mp4)"\s*,\s*label\s*:\s*"(?P<QUALITY>\d+)[^"]*"\s*\}';

{ TDownloader_BarrandovTV }

class function TDownloader_BarrandovTV.Provider: string;
begin
  Result := 'Barrandov.tv';
end;

class function TDownloader_BarrandovTV.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_BarrandovTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlsRegExp := RegExCreate(REGEXP_MOVIE_URLS);
end;

destructor TDownloader_BarrandovTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlsRegExp);
  inherited;
end;

function TDownloader_BarrandovTV.GetMovieInfoUrl: string;
begin
  Result := 'http://barrandov.tv/' + MovieID;
end;

function TDownloader_BarrandovTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  BestUrl, Url, sQuality: string;
  BestQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BestUrl := '';
  BestQuality := -1;
  if GetRegExpVars(MovieUrlsRegExp, Page, ['URL', 'QUALITY'], [@Url, @sQuality]) then
    repeat
      if Url <> '' then
        begin
        Quality := StrToIntDef(sQuality, 0);
        if Quality > BestQuality then
          begin
          BestUrl := Url;
          BestQuality := Quality;
          end;
        end;
    until not GetRegExpVarsAgain(MovieUrlsRegExp, ['URL', 'QUALITY'], [@Url, @sQuality]);
  if BestUrl = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := GetRelativeUrl(GetMovieInfoUrl, BestUrl);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BarrandovTV);

end.
