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

unit downSteamPowered;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SteamPowered = class(THttpDownloader)
    private
    protected
      UrlsRegExp: TRegExp;
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

// http://store.steampowered.com/video/220
const
  URLREGEXP_BEFORE_ID = 'store\.steampowered\.com/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_TITLE;
  REGEXP_MOVIE_URLS =   '\brgMovieFlashvars\s*=\s*\{(?P<URLS>.+?)\}\s*;';
  REGEXP_MOVIE_URL =    '\bFILENAME\s*:\s*"(?P<URL>https?://.+?)"';

{ TDownloader_SteamPowered }

class function TDownloader_SteamPowered.Provider: string;
begin
  Result := 'SteamPowered.com';
end;

class function TDownloader_SteamPowered.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_SteamPowered.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  UrlsRegExp := RegExCreate(REGEXP_MOVIE_URLS);
end;

destructor TDownloader_SteamPowered.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(UrlsRegExp);
  inherited;
end;

function TDownloader_SteamPowered.GetMovieInfoUrl: string;
begin
  Result := 'http://store.steampowered.com/video/' + MovieID;
end;

function TDownloader_SteamPowered.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Urls: string;
  UrlArr: TStringArray;
  i: integer;
  {$IFDEF MULTIDOWNLOADS}
  n: integer;
  {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  {$IFDEF MULTIDOWNLOADS}
  n := 0;
  {$ENDIF}
  if GetRegExpVar(UrlsRegExp, Page, 'URLS', Urls) then
    if GetRegExpAllVar(MovieUrlRegExp, Urls, 'URL', UrlArr) then
      for i := 0 to Pred(Length(UrlArr)) do
        begin
        MovieUrl := UrlArr[i];
        SetPrepared(True);
        Result := True;
        {$IFDEF MULTIDOWNLOADS}
        UrlList.Add(UrlArr[i]);
        Inc(n);
        NameList.Add(Format('%s (%d)', [UnpreparedName, n]));
        {$ELSE}
        Break;
        {$ENDIF}
        end;
end;

initialization
  RegisterDownloader(TDownloader_SteamPowered);

end.
