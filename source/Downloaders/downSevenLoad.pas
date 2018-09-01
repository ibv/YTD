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

unit downSevenLoad;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SevenLoad = class(THttpDownloader)
    private
    protected
      ConfigUrlRegExp: TRegExp;
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

// http://en.sevenload.com/shows/Food-Drink/episodes/4YXLHBt-How-To-Make-Elegant-No-Bake-Cheesecake
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*sevenload\.com/shows/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG_URL = '<param\s+name="flashVars"\s+value="(?:[^"&]+&)*configPath=(?P<URL>https?[^"]+?)(?:&amp;|")';

{ TDownloader_SevenLoad }

class function TDownloader_SevenLoad.Provider: string;
begin
  Result := 'SevenLoad.com';
end;

class function TDownloader_SevenLoad.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_SevenLoad.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  ConfigUrlRegExp := RegExCreate(REGEXP_CONFIG_URL);
end;

destructor TDownloader_SevenLoad.Destroy;
begin
  RegExFreeAndNil(ConfigUrlRegExp);
  inherited;
end;

function TDownloader_SevenLoad.GetMovieInfoUrl: string;
begin
  Result := 'http://en.sevenload.com/shows/' + MovieID;
end;

function TDownloader_SevenLoad.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Title, BestUrl, StreamWidth, StreamHeight: string;
    i, BestQuality, Quality: integer;
    Xml: TXmlDoc;
    Node, Streams: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(ConfigUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, UrlDecode(Url), Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not Xml.NodeByPath('playlists/playlist/items/item', Node) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlVar(Node, 'title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else
        begin
        BestUrl := '';
        BestQuality := 0;
        if XmlNodeByPath(Node, 'videos/video/streams', Streams) then
          for i := 0 to Pred(Streams.NodeCount) do
            if Streams.Nodes[i].Name = 'stream' then
              if (GetXmlAttr(Streams.Nodes[i], '', 'width', StreamWidth) and GetXmlAttr(Streams.Nodes[i], '', 'height', StreamHeight)) or (BestQuality = 0) then
                begin
                Quality := StrToIntDef(StreamWidth, 0) * StrToIntDef(StreamHeight, 0);
                if Quality >= BestQuality then
                  if GetXmlVar(Streams.Nodes[i], 'locations/location', Url) then
                    begin
                    BestQuality := Quality;
                    BestUrl := Url;
                    end;
                end;
        if BestUrl = '' then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieUrl := BestUrl;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_SevenLoad);

end.
