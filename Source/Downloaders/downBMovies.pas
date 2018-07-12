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

unit downBMovies;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_BMovies = class(TRtmpDownloader)
    private
    protected
      Extension: string;
      MovieInfoRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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
  uStrings,
  uMessages,
  uDownloadClassifier;

// http://bmovies.com/movie/bad-taste
const
  URLREGEXP_BEFORE_ID = 'bmovies\.com/movie/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_INFO =   '<param\s+name="FlashVars"\s+value="(?:[^"]*&)?moviename=(?P<GENRE>[^/]+)/(?P<STREAM>.+?)"';

{ TDownloader_BMovies }

class function TDownloader_BMovies.Provider: string;
begin
  Result := 'bMovies.com';
end;

class function TDownloader_BMovies.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_BMovies.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_BMovies.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_BMovies.GetMovieInfoUrl: string;
begin
  Result := 'http://bmovies.com/movie/' + MovieID;
end;

function TDownloader_BMovies.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_BMovies.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  BMOVIES_APP = 'a4278/o37/bmovies/%s';
  BMOVIES_URL = 'rtmp://dmgx.fcod.llnwd.net/' + BMOVIES_APP;
var
  Genre, Stream: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(MovieInfoRegExp, Page, ['GENRE', 'STREAM'], [@Genre, @Stream]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    Extension := ExtractFileExt(Stream);
    Self.RtmpUrl := Format(BMOVIES_URL, [Genre]);
    Self.RtmpApp := Format(BMOVIES_APP, [Genre]);
    Self.Playpath := ChangeFileExt(Stream, '');
    //Self.SwfVfy := 'http://bmovies.com/test/swf/b.swf';
    //Self.PageUrl := GetMovieInfoUrl;
    //Self.FlashVer := FLASH_DEFAULT_VERSION;
    MovieUrl := Self.RtmpUrl + '/' + Stream;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BMovies);

end.
