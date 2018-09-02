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

unit downCeskeDrahy;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CeskeDrahy = class(THttpDownloader)
    private
    protected
      CurrentVideoIdRegExp: TRegExp;
      MovieListRegExp: TRegExp;
      MovieInfoRegExp: TRegExp;
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

// http://www.cd.cz/tv/aktuality/300_rekonstrukce-nadrazi-v-ceskem-brode
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*cd\.cz/tv/';
  URLREGEXP_ID =        '[^/]+/[0-9]+.*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?:TV portál\s*-\s*)?(?P<TITLE>.*?)</h1>';
  REGEXP_CURRENT_VIDEO = '\bCurrentVideoId\s*=\s*(?P<ID>[0-9]+)\s*;';
  REGEXP_MOVIE_LIST = '\btoFlash\s*=\s*"(?P<MOVIES>.*?)"';
  REGEXP_MOVIE_INFO = '(?P<ID>[0-9]+)~(?P<PAGE>[^~]*)~(?P<TITLE>[^~]*)~(?P<MOVIE>[^~]*)~(?P<DESCRIPTION>[^~]*)~~';

{ TDownloader_CeskeDrahy }

class function TDownloader_CeskeDrahy.Provider: string;
begin
  Result := 'CD.cz';
end;

class function TDownloader_CeskeDrahy.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_CeskeDrahy.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  CurrentVideoIdRegExp := RegExCreate(REGEXP_CURRENT_VIDEO);
  MovieListRegExp := RegExCreate(REGEXP_MOVIE_LIST);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_CeskeDrahy.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(CurrentVideoIdRegExp);
  RegExFreeAndNil(MovieListRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_CeskeDrahy.GetMovieInfoUrl: string;
begin
  Result := 'http://www.cd.cz/tv/' + MovieID;
end;

function TDownloader_CeskeDrahy.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var CurrentID, MovieList, ID, Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(CurrentVideoIdRegExp, Page, 'ID', CurrentID) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['CurrentVideoId']))
  else if not GetRegExpVar(MovieListRegExp, Page, 'MOVIES', MovieList) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['toFlash']))
  else if MovieInfoRegExp.Match(MovieList) then
    repeat
      if MovieInfoRegExp.SubexpressionByName('ID', ID) then
        if ID = CurrentID then
          begin
          if MovieInfoRegExp.SubexpressionByName('MOVIE', Path) then
            begin
            MovieURL := 'http://light.polar.cz/videa/tv.cd.cz/' + Path + '.mp4';
            SetPrepared(True);
            Result := True;
            end;
          Break;
          end;
    until not MovieInfoRegExp.MatchAgain;
end;

initialization
  RegisterDownloader(TDownloader_CeskeDrahy);

end.
