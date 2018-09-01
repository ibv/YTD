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

unit downTVSpl;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, {$IFDEF DIRTYHACKS} SynaUtil, {$ENDIF}
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_TVSpl = class(TRtmpDownloader)
    private
    protected
      MovieIdRegExp: TRegExp;
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

// http://www.tvs.pl/informacje/32172,szpitalna_kapsula__szybsze_diagnozowanie_pacjentow_i_oszczednosc_czasu_dla_pracownikow_szpitala.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tvs\.pl/';
  URLREGEXP_ID =        '[^/]+/[0-9]+,.+';
  URLREGEXP_AFTER_ID =  '';

const
  MOVIE_ID_REGEXP = '^[^/]+/(?P<ID>[0-9]+),';

{ TDownloader_TVSpl }

class function TDownloader_TVSpl.Provider: string;
begin
  Result := 'TVS.pl';
end;

class function TDownloader_TVSpl.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_TVSpl.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peXml;
  InfoPageIsXml := True;
  MovieIdRegExp := RegExCreate(MOVIE_ID_REGEXP);
end;

destructor TDownloader_TVSpl.Destroy;
begin
  RegExFreeAndNil(MovieIdRegExp);
  inherited;
end;

function TDownloader_TVSpl.GetMovieInfoUrl: string;
var ID: string;
begin
  if not GetRegExpVar(MovieIdRegExp, MovieID, 'ID', ID) then
    Result := ''
  else
    Result := 'http://www.tvs.pl/playlist/' + ID + '.xml';
end;

function TDownloader_TVSpl.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Node: TXmlNode;
    Title, Location, Streamer: string;
    {$IFDEF DIRTYHACKS}
    Protocol, User, Password, Host, Port, Path, Para: string;
    {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not PageXml.NodeByPath('channel/item', Node) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetXmlVar(Node, 'jwplayer:file', Location) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not GetXmlVar(Node, 'jwplayer:streamer', Streamer) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    if GetXmlVar(Node, 'title', Title) then
      SetName(Title);
    {$IFDEF DIRTYHACKS}
    ParseUrl(Streamer, Protocol, User, Password, Host, Port, Path, Para);
    Streamer := 'rtmp://' + Host + ':1935' + Path;
    {$ENDIF}
    MovieUrl := Streamer + '/mp4:' + Location;
    AddRtmpDumpOption('r', Streamer);
    AddRtmpDumpOption('y', 'mp4:' + Location);
    AddRtmpDumpOption('f', 'WIN 10,1,82,76');
    AddRtmpDumpOption('W', 'http://www.tvs.pl/gfx/mediaplayer-5.3-licensed/player.swf');
    AddRtmpDumpOption('p', 'http://www.tvs.pl/' + MovieID);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TVSpl);

end.
