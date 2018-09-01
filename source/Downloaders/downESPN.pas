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

unit downESPN;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ESPN = class(THttpDownloader)
    private
    protected
      PlayerIDRegExp: TRegExp;
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

// http://espn.go.com/video/clip?id=5570038
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*espn\.go\.com/video/clip\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h2>\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_PLAYERID = '\bvideoPlayers\s*:\s*\{\s*(?P<PLAYER>[a-z_][a-z0-9_]*)';

{ TDownloader_ESPN }

class function TDownloader_ESPN.Provider: string;
begin
  Result := 'ESPN.com';
end;

class function TDownloader_ESPN.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_ESPN.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  PlayerIDRegExp := RegExCreate(REGEXP_PLAYERID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_ESPN.Destroy;
begin
  RegExFreeAndNil(PlayerIDRegExp);
  inherited;
end;

function TDownloader_ESPN.GetMovieInfoUrl: string;
begin
  Result := 'http://espn.go.com/video/clip?id=' + MovieID;
end;

function TDownloader_ESPN.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Player, MediaUrl, PlaylistUrl, Title, FileName: string;
    InfoXml, PlaylistXml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(PlayerIDRegExp, Page, 'PLAYER', Player) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, 'http://espn.go.com/videohub/mpf/config.prodXml?player=' + Player + '&adminOver=none', InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlVar(InfoXml, 'globalPlayerConfig/mediaUrl', MediaUrl) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['mediaUrl']))
      else if not GetXmlVar(InfoXml, 'globalPlayerConfig/playlistURL', PlaylistUrl) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['playlistURL']))
      else if not DownloadXml(Http, PlaylistUrl + '?id=' + MovieID + '&player=' + Player, PlaylistXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        try
          if not GetXmlVar(PlaylistXml, 'channel/item/headline', Title) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
          else if not GetXmlVar(PlaylistXml, 'channel/item/asseturl', FileName) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
          else
            begin
            SetName(Title);
            MovieURL := MediaUrl + FileName;
            SetPrepared(True);
            Result := True;
            end;
        finally
          PlaylistXml.Free;
          end;
    finally
      InfoXml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_ESPN);

end.
