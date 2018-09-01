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

unit downSTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_STV = class(TRtmpDownloader)
    private
    protected
      PlayListRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

// http://www.stv.sk/online/archiv/spravy-stv?id=43633&scroll=
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stv\.sk/online/archiv/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  MOVIE_TITLE_REGEXP = '<h2>(?P<TITLE>.*?)<';
  PLAYLIST_REGEXP = '\.addParam\s*\(\s*''flashvars''\s*,\s*''(?:[^''&]*&)*playlistfile=(?P<PLAYLIST>https?://[^''&]+)';

{ TDownloader_STV }

class function TDownloader_STV.Provider: string;
begin
  Result := 'STV.sk';
end;

class function TDownloader_STV.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_STV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP);
  PlayListRegExp := RegExCreate(PLAYLIST_REGEXP);
end;

destructor TDownloader_STV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(PlayListRegExp);
  inherited;
end;

function TDownloader_STV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.stv.sk/online/archiv/' + MovieID;
end;

function TDownloader_STV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Playlist, Streamer, Location: string;
    Xml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(PlayListRegExp, Page, 'PLAYLIST', Playlist) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, UrlDecode(Playlist), Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not Xml.NodeByPath('tracklist/track', Node) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Node, 'location', Location) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not XmlNodeByPathAndAttr(Node, 'meta', 'rel', 'streamer', Node) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Node, '', Streamer) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := Streamer + '/mp4:' + Location;
        AddRtmpDumpOption('r', Streamer);
        AddRtmpDumpOption('y', 'mp4:' + Location);
        AddRtmpDumpOption('f', 'WIN 10,1,82,76');
        AddRtmpDumpOption('W', 'http://www.streamhosting.cz/flash/recent/player-licensed.swf');
        AddRtmpDumpOption('p', GetMovieInfoUrl);
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_STV);

end.
