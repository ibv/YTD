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

unit listBlipTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_BlipTV = class(TPlaylistDownloader)
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

// http://torrentfreak.blip.tv/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*';
  URLREGEXP_ID =        '[a-z0-9-]+';
  URLREGEXP_AFTER_ID =  '\.blip\.tv/?';

const
  REGEXP_PLAYLIST_ITEM = '<a\s+href="(?P<PATH>/file/[^"]+)"[^>]*>\s*(?P<NAME>[^<]*)\s*</a>';
  REGEXP_NEXT_PAGE = '<div\s+class="view_pages_page">\s*<a\s+href="(?P<PATH>/[^"]+)">Next</a>';

{ TPlaylist_BlipTV }

class function TPlaylist_BlipTV.Provider: string;
begin
  Result := 'BlipTV.com';
end;

class function TPlaylist_BlipTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + ClassName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_BlipTV.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM, [rcoIgnoreCase, rcoSingleLine]);
  NextPageRegExp := RegExCreate(REGEXP_NEXT_PAGE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TPlaylist_BlipTV.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  RegExFreeAndNil(NextPageRegExp);
  inherited;
end;

function TPlaylist_BlipTV.GetMovieInfoUrl: string;
begin
  Result := 'http://' + MovieID + '.blip.tv/posts?view=archive&nsfw=dc';
end;

function TPlaylist_BlipTV.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Trim(Match.SubexpressionByName('NAME'));
end;

function TPlaylist_BlipTV.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := 'http://blip.tv' + Match.SubexpressionByName('PATH');
end;

function TPlaylist_BlipTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url: string;
begin
  repeat
    Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
    if not GetRegExpVar(NextPageRegExp, Page, 'PATH', Url) then
      Break
    else if not DownloadPage(Http, 'http://blip.tv' + Url, Page) then
      Break;
  until False;
end;

initialization
  RegisterDownloader(TPlaylist_BlipTV);

end.
