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

unit downMTVEmbed;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_MTVEmbed = class(TRtmpDownloader)
    private
    protected
      MovieParamsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoUrlForID(const ID: string): string; virtual;
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

// http://media.mtvnservices.com/mgid:uma:video:mtv.com:602324
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*media\.mtvnservices\.com/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_MTVEmbed }

class function TDownloader_MTVEmbed.Provider: string;
begin
  Result := 'MTV.com';
end;

class function TDownloader_MTVEmbed.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MTVEmbed.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peXml;
end;

destructor TDownloader_MTVEmbed.Destroy;
begin
  inherited;
end;

function TDownloader_MTVEmbed.GetMovieInfoUrlForID(const ID: string): string;
begin
  Result := 'http://media.mtvnservices.com/player/config.jhtml?uri=' + UrlEncode(ID);
end;

function TDownloader_MTVEmbed.GetMovieInfoUrl: string;
begin
  Result := GetMovieInfoUrlForID(MovieID);
end;

function TDownloader_MTVEmbed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Title, InfoUrl: string;
    InfoXml: TXmlDoc;
    InfoNode: TXmlNode;
    i, BestResolution, BestBitrate, Resolution, Bitrate: integer;
    sWidth, sHeight, sBitrate, Url, BestUrl: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Title := '';
  if not GetXmlVar(PageXml, 'player/feed/rss/channel/item/media:group/media:title', Title) then
    if not GetXmlVar(PageXml, 'player/feed/rss/channel/item/title', Title) then
      if not GetXmlVar(PageXml, 'player/feed/rss/channel/title', Title) then
        begin
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE));
        Exit;
        end;
  if not GetXmlAttr(PageXml, 'player/feed/rss/channel/item/media:group/media:content', 'url', InfoUrl) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, StringReplace(InfoUrl, '{ref}', 'www.mtv.com', [rfReplaceAll]), InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not XmlNodeByPath(InfoXml, 'video/item', InfoNode) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
      else
        begin
        BestResolution := 0;
        BestBitrate := 0;
        BestUrl := '';
        for i := 0 to Pred(InfoNode.NodeCount) do
          if InfoNode.Nodes[i].Name = 'rendition' then
            if GetXmlAttr(InfoNode.Nodes[i], '', 'width', sWidth) then
              if GetXmlAttr(InfoNode.Nodes[i], '', 'height', sHeight) then
                if GetXmlAttr(InfoNode.Nodes[i], '', 'bitrate', sBitrate) then
                  begin
                  try
                    Resolution := StrToInt(sWidth) * StrToInt(sHeight);
                    Bitrate := StrToInt(sBitrate);
                  except
                    Resolution := -1;
                    Bitrate := -1;
                    end;
                  if (Resolution > BestResolution) or ((Resolution = BestResolution) and (Bitrate > BestBitrate)) then
                    if GetXmlVar(InfoNode.Nodes[i], 'src', Url) then
                      begin
                      BestResolution := Resolution;
                      BestBitrate := Bitrate;
                      BestUrl := Url;
                      end;
                  end;
        if BestUrl = '' then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieUrl := BestUrl;
          AddRtmpDumpOption('r', BestUrl);
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      InfoXml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_MTVEmbed);

end.
