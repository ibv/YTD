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

unit downBreak;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}

  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Break = class(THttpDownloader)
    private
    protected
      MoviePlayerRegExp: TRegExp;
      VideoFromPlayerRegExp: TRegExp;
      TokenFromPlayerRegExp: TRegExp;
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

// http://www.break.com/index/runaway-truck-crashes-and-flips-over.html
// http://www.break.com/usercontent/2007/10/South-Africa-Win-Rugby-World-Cup-385706.html
const
  URLREGEXP_BEFORE_ID = 'break\.com/';
  URLREGEXP_ID =        '[^?]+\.html';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_TITLE;
  REGEXP_MOVIE_PLAYER = REGEXP_URL_LINK_VIDEOSRC;
  REGEXP_VIDEO_FROM_PLAYER = '[?&]sVidLoc=(?P<URL>http[^&]+)';
  REGEXP_TOKEN_FROM_PLAYER = '[?&]icon=(?P<TOKEN>[0-9A-F]+)';

{ TDownloader_Break }

class function TDownloader_Break.Provider: string;
begin
  Result := 'Break.com';
end;

class function TDownloader_Break.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Break.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MoviePlayerRegExp := RegExCreate(REGEXP_MOVIE_PLAYER);
  VideoFromPlayerRegExp := RegExCreate(REGEXP_VIDEO_FROM_PLAYER);
  TokenFromPlayerRegExp := RegExCreate(REGEXP_TOKEN_FROM_PLAYER);
end;

destructor TDownloader_Break.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MoviePlayerRegExp);
  RegExFreeAndNil(VideoFromPlayerRegExp);
  RegExFreeAndNil(TokenFromPlayerRegExp);
  inherited;
end;

function TDownloader_Break.GetMovieInfoUrl: string;
begin
  Result := 'http://www.break.com/' + MovieID;
end;

function TDownloader_Break.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Token: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MoviePlayerRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, Url, hmHead) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(VideoFromPlayerRegExp, LastUrl, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not GetRegExpVar(TokenFromPlayerRegExp, LastUrl, 'TOKEN', Token) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['token']))
  else
    begin
    MovieURL := UrlDecode(Url) + '?' + Token;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Break);

end.
