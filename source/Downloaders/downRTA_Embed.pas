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

unit downRTA_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uCompatibility, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_RTA_Embed = class(TMSDownloader)
    private
    protected
      ConfigXmlRegExp: TRegExp;
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
  uStringUtils,
  uDownloadClassifier,
  uMessages;

// http://www.rta.cz/rta_stream_wmv/tvframe1.html?id_item=10046739
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rta\.cz/rta_stream_wmv/.*?[?&]id_item=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG_XML = '<param\s+value="(?P<PATH>[^"]+)"\s+name="URL"';

{ TDownloader_RTA_Embed }

class function TDownloader_RTA_Embed.Provider: string;
begin
  Result := 'RTA.cz';
end;

class function TDownloader_RTA_Embed.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_RTA_Embed.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  ConfigXmlRegExp := RegExCreate(REGEXP_CONFIG_XML);
end;

destructor TDownloader_RTA_Embed.Destroy;
begin
  RegExFreeAndNil(ConfigXmlRegExp);
  inherited;
end;

function TDownloader_RTA_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rta.cz/rta_stream_wmv/tvframe1.html?id_item=' + MovieID;
end;

function TDownloader_RTA_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Path, Url, Title: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(ConfigXmlRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://www.rta.cz' + Path, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(Xml, 'entry/title', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
      else if not GetXmlAttr(Xml, 'entry/ref', 'href', Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        SetName(Title);
        MovieURL := URL;
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_RTA_Embed);

end.
