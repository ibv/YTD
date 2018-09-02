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

unit listGameAnyone;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_GameAnyone = class(TPlaylistDownloader)
    private
    protected
      function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; override;
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

// http://www.gameanyone.com/game/PC/936.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*gameanyone\.com/game/[^/]+/';
  URLREGEXP_ID =        '[0-9]+\.html?';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PLAYLIST_ITEM = '<a\s+href="https?://(?:[a-z0-9-]+\.)*gameanyone\.com/video/(?P<ID>[0-9]+)">(?P<NAME>[^<]+)</a>\s+\([0-9]+:[0-9]{2}\)';

{ TPlaylist_GameAnyone }

class function TPlaylist_GameAnyone.Provider: string;
begin
  Result := 'GameAnyone.com';
end;

class function TPlaylist_GameAnyone.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TPlaylist_GameAnyone.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM);
end;

destructor TPlaylist_GameAnyone.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  inherited;
end;

function TPlaylist_GameAnyone.GetMovieInfoUrl: string;
begin
  Result := 'http://www.gameanyone.com/game/dummy/' + MovieID;
end;

function TPlaylist_GameAnyone.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Trim(Match.SubexpressionByName('NAME'));
end;

function TPlaylist_GameAnyone.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := 'http://www.gameanyone.com/video/' + Match.SubexpressionByName('ID');
end;

initialization
  RegisterDownloader(TPlaylist_GameAnyone);

end.
