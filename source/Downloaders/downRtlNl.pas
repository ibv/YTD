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

unit downRtlNl;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uCompatibility, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_RtlNl = class(TMSDownloader)
    private
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
  uStringUtils,
  uDownloadClassifier,
  uMessages;

// http://www.rtl.nl/xl/#/u/e6820785-1c71-381e-a3fa-37c6acc343b9/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rtl\.nl/(?:[^/]+/)*';
  URLREGEXP_ID =        '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}';
  URLREGEXP_AFTER_ID =  '(?:[^0-9a-f]|$)';

{ TDownloader_RtlNl }

class function TDownloader_RtlNl.Provider: string;
begin
  Result := 'RTL.nl';
end;

class function TDownloader_RtlNl.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_RtlNl.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peXml;
  InfoPageIsXml := True;
end;

destructor TDownloader_RtlNl.Destroy;
begin
  inherited;
end;

function TDownloader_RtlNl.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rtl.nl/system/s4m/xldata/uuid/' + MovieID + '.xml';
end;

function TDownloader_RtlNl.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Node: TXmlNode;
    Xml: TXmlDoc;
    Title, Path, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not PageXml.NodeByPathAndAttr('material_list/material', 'key', MovieID, Node) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not GetXmlVar(Node, 'component_uri', Path) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, 'http://www.rtl.nl/system/video/wvx' + Path + '/1500.wvx?utf8=ok', Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlAttr(Xml, 'ENTRY/REF', 'HREF', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        if GetXmlVar(Xml, 'TITLE', Title) then
          SetName(Title);
        MovieUrl := Url;
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_RtlNl);

end.
