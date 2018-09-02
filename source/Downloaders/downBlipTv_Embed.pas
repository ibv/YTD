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

unit downBlipTv_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BlipTv_Embed = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
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

// http://blip.tv/play/hIVV4sNUAg
const
  URLREGEXP_BEFORE_ID = 'blip\.tv/play/';
  URLREGEXP_ID =        REGEXP_PATH_COMPONENT;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_URL = '[?&#]file=http(?:%3A%2F%2F|://)(?:www\.)?blip\.tv(?:%2F|/)rss(?:%2F|/)flash(?:%2F|/)(?P<ID>[0-9]+)';

{ TDownloader_BlipTv_Embed }

class function TDownloader_BlipTv_Embed.Provider: string;
begin
  Result := 'Blip.tv';
end;

class function TDownloader_BlipTv_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_BlipTv_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  InfoPageIsXml := True;
  MovieIdFromUrlRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_URL);
end;

destructor TDownloader_BlipTv_Embed.Destroy;
begin
  RegExFreeAndNil(MovieIdFromUrlRegExp);
  inherited;
end;

function TDownloader_BlipTv_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://blip.tv/play/' + MovieID;
end;

function TDownloader_BlipTv_Embed.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
var ID: string;
begin
  Result := False;
  if DownloadPage(Http, Url, hmHead) then
    if GetRegExpVar(MovieIdFromUrlRegExp, LastUrl, 'ID', ID) then
      Result := inherited GetMovieInfoContent(Http, 'http://blip.tv/rss/flash/' + ID, Page, Xml, Method);
end;

function TDownloader_BlipTv_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Node: TXmlNode;
  Title, Url, BestUrl, sSize: string;
  i, Size, BestSize: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'channel/item/media:title', Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else if not XmlNodeByPath(PageXml, 'channel/item/media:group', Node) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    BestUrl := '';
    BestSize := -1;
    for i := 0 to Pred(Node.NodeCount) do
      if Node[i].Name = 'media:content' then
        if GetXmlAttr(Node[i], '', 'url', Url) then
          begin
          if GetXmlAttr(Node[i], '', 'fileSize', sSize) then
            Size := StrToIntDef(sSize, 0)
          else
            Size := 0;
          if Size > BestSize then
            begin
            BestUrl := Url;
            BestSize := Size;
            end;
          end;
    if BestUrl = '' then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      SetName(Title);
      MovieURL := BestUrl;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BlipTv_Embed);

end.
