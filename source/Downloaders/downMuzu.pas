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

unit downMuzu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Muzu = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarsVariablesRegExp: TRegExp;
      NetworkID, VideoID, ChannelID: string;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetMuzuMediaUrl(out Url: string): boolean; virtual;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.muzu.tv/elizarickman/cinnamon-bone-music-video/670078
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*muzu\.tv/';
  URLREGEXP_ID =        '(?:[^/]+/){2}[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1\s+id="videosPageMainTitleH1">\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_FLASHVARS = '\bflashvars\s*:[^"]*"(?P<FLASHVARS>&[^"]+)"';
  REGEXP_FLASHVARS_VARIABLES = '&(?P<VARNAME>[^=&]+)(?:=(?P<VARVALUE>[^&]*))?';

{ TDownloader_Muzu }

class function TDownloader_Muzu.Provider: string;
begin
  Result := 'Muzu.tv';
end;

class function TDownloader_Muzu.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Muzu.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashVarsVariablesRegExp := RegExCreate(REGEXP_FLASHVARS_VARIABLES);
end;

destructor TDownloader_Muzu.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarsVariablesRegExp);
  inherited;
end;

function TDownloader_Muzu.GetMovieInfoUrl: string;
begin
  Result := 'http://www.muzu.tv/' + MovieID;
end;

function TDownloader_Muzu.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVarsInfo, CountryID, NetworkVersion, Url: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVarsInfo) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT))
  else
    begin
    GetRegExpVarPairs(FlashVarsVariablesRegExp, FlashVarsInfo, ['networkId', 'vidId', 'countryIdentity', 'networkVersion'], [@NetworkID, @VideoID, @CountryID, @NetworkVersion]);
    if NetworkID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['networkId']))
    else if VideoID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['vidId']))
    else if CountryID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['countryIdentity']))
    else if NetworkVersion = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['networkVersion']))
    else if not DownloadXml(Http, 'http://www.muzu.tv/player/networkVideos/' + NetworkID + '?countryIdentity=' + CountryID + '&networkVersion=' + NetworkVersion + '&hostName=http%3A%2F%2Fwww%2Emuzu%2Etv', Xml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      try
        if not GetXmlAttr(Xml, 'channels/channel', 'id', ChannelID) then
          SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['channelId']))
        else if not GetMuzuMediaUrl(Url) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          MovieUrl := Url;
          SetPrepared(True);
          Result := True;
          end;
      finally
        Xml.Free;
        end;
    end;
end;

function TDownloader_Muzu.GetMuzuMediaUrl(out Url: string): boolean;
var Http: THttpSend;
    Xml: TXmlDoc;
    Src: string;
begin
  Result := False;
  Url := '';
  if (NetworkID <> '') and (VideoID <> '') and (ChannelID <> '') then
    begin
    Http := CreateHttp;
    try
      if DownloadXml(Http, 'http://www.muzu.tv/player/playAsset?id=' + NetworkID + '&assetId=' + VideoID + '&videoType=1&playlistId=' + ChannelID, Xml) then
        try
          if GetXmlAttr(Xml, 'body/video', 'src', Src) then
            if Src <> '' then
              begin
              Url := Src;
              Result := True;
              end;
        finally
          Xml.Free;
          end;
    finally
      Http.Free;
      end;
    end;
end;

function TDownloader_Muzu.Download: boolean;
var Url: string;
begin
  if GetMuzuMediaUrl(Url) then
    begin
    MovieUrl := Url;
    Result := inherited Download;
    end
  else
    Result := False;
end;

initialization
  RegisterDownloader(TDownloader_Muzu);

end.
