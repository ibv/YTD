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

unit listCT24;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader,
  downCT24;

type
  TPlaylist_CT24 = class(TPlaylistDownloader)
    private
    protected
      //function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

// http://www.ct24.cz/regionalni/87267-vrchlabsky-zamek-ma-vlastni-miniaturu/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ct24\.cz/';
  URLREGEXP_ID =        '[^/?&]+/[0-9]+-[^/?&]*';
  URLREGEXP_AFTER_ID =  '(?!/video)';

const
  REGEXP_PLAYLIST_ITEM = '<a\s+href="video/(?P<ID>[0-9]+)';

{ TPlaylist_CT24 }

class function TPlaylist_CT24.Provider: string;
begin
  Result := TDownloader_CT24.Provider;
end;

class function TPlaylist_CT24.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + ClassName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_CT24.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TPlaylist_CT24.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  inherited;
end;

function TPlaylist_CT24.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ct24.cz/' + MovieID;
end;

function TPlaylist_CT24.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := GetMovieInfoUrl + '/video/' + Match.SubexpressionByName('ID');
end;

initialization
  RegisterDownloader(TPlaylist_CT24);

end.
