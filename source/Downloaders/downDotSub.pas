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

unit downDotSub;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DotSub = class(THttpDownloader)
    private
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

// http://dotsub.com/view/b9715a32-0bd5-4ad1-80b0-2b1e4832daf2
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*dotsub\.com/[^/]+/';
  URLREGEXP_ID =        '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}';
  URLREGEXP_AFTER_ID =  '(?:[/?]|$)';

const
  REGEXP_MOVIE_TITLE = '[{,]\s*"title"\s*:\s*"(?P<TITLE>[^"]+?)"';
  REGEXP_MOVIE_URL = '[{,]\s*"mediaURI"\s*:\s*"(?P<URL>https?://.+?)"';

{ TDownloader_DotSub }

class function TDownloader_DotSub.Provider: string;
begin
  Result := 'DotSub.com';
end;

class function TDownloader_DotSub.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_DotSub.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_DotSub.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_DotSub.GetMovieInfoUrl: string;
begin
  Result := 'http://dotsub.com/api/media/' + MovieID + '/metadata';
end;

function TDownloader_DotSub.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
{$IFDEF DIRTYHACKS}
var s: string;
    i: integer;
{$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := Prepared;
  if Result then
    begin
    {$IFDEF DIRTYHACKS}
    s := MovieID + '/media/';
    i := Pos(s, MovieUrl);
    if i > 0 then
      MovieUrl := Copy(MovieUrl, 1, i+Length(MovieID)+1) + Copy(MovieUrl, i+Length(s)-1, MaxInt);
    {$ENDIF}
    end;
end;

initialization
  RegisterDownloader(TDownloader_DotSub);

end.
