(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit listYouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_YouTube = class(TPlaylistDownloader)
    private
    protected
      NextPageRegExp: TRegExp;
      function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
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
  uDownloadClassifier;

// http://www.youtube.com/view_play_list?p=90D6E7C4DE68E49E
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=';
  URLREGEXP_ID =        '[^/?&"]+';
  URLREGEXP_AFTER_ID =  '';

const
  //REGEXP_PLAYLIST_ITEM = '<a[^>]*\sid="video-long-title-(?P<ID>[^"]+)[^>]*>(?P<NAME>[^<]+)</a>';
  REGEXP_PLAYLIST_ITEM = '<a\s+href="/watch\?v=(?P<ID>[^&"]+)&amp;p=%s"[^>]*>(?P<NAME>[^<]+)</a>';
  REGEXP_NEXT_PAGE = '<a\s+href="(?P<URL>https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=[^"&]+&sort_field=[^&"]*&page=[0-9]+)"\s+class="yt-uix-pager-link"\s+data-page="(?P<PAGE>[0-9]+)"';

{ TPlaylist_YouTube }

class function TPlaylist_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TPlaylist_YouTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + ClassName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_YouTube.Create(const AMovieID: string);
begin
  inherited;
  //PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM, [rcoIgnoreCase, rcoSingleLine]);
  PlayListItemRegExp := RegExCreate(Format(REGEXP_PLAYLIST_ITEM, [MovieID]), [rcoIgnoreCase, rcoSingleLine]);
  NextPageRegExp := RegExCreate(REGEXP_NEXT_PAGE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TPlaylist_YouTube.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  RegExFreeAndNil(NextPageRegExp);
  inherited;
end;

function TPlaylist_YouTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/view_play_list?p=' + MovieID;
end;

function TPlaylist_YouTube.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Trim(Match.SubexpressionByName('NAME'));
end;

function TPlaylist_YouTube.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := 'http://www.youtube.com/watch?v=' + Match.SubexpressionByName('ID');
end;

function TPlaylist_YouTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Again: boolean;
    Url: string;
    PageNumber, FoundPageNumber: integer;
begin
  PageNumber := 1;
  repeat
    Again := False;
    Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
    if NextPageRegExp.Match(Page) then
      repeat
        FoundPageNumber := StrToIntDef(NextPageRegExp.SubexpressionByName('PAGE'), 0);
        if FoundPageNumber > PageNumber then
          begin
          PageNumber := FoundPageNumber;
          Url := NextPageRegExp.SubexpressionByName('URL');
          Again := DownloadPage(Http, Url, Page);
          Break;
          end;
      until not NextPageRegExp.MatchAgain;
  until not Again;
end;

initialization
  RegisterDownloader(TPlaylist_YouTube);

end.
