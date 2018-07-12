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

unit downSpike;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Spike = class(THttpDownloader)
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
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.spike.com/video/prince-of-persia/3355664
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*spike\.com/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG_URL = '<param\s+name="flashVars"\s+value="CONFIG_URL=(?P<URL>/[^"]+)"';

{ TDownloader_Spike }

class function TDownloader_Spike.Provider: string;
begin
  Result := 'Spike.com';
end;

class function TDownloader_Spike.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Spike.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  ConfigUrlRegExp := RegExCreate(REGEXP_CONFIG_URL);
end;

destructor TDownloader_Spike.Destroy;
begin
  RegExFreeAndNil(ConfigUrlRegExp);
  inherited;
end;

function TDownloader_Spike.GetMovieInfoUrl: string;
begin
  Result := 'http://www.spike.com/' + MovieID;
end;

function TDownloader_Spike.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var TitleXml, Xml: TXmlDoc;
    Node: TXmlNode;
    Url, Title, BitrateStr, BestUrl: string;
    i, Bitrate, BestBitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(ConfigUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://www.spike.com' + UrlDecode(Url), TitleXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(TitleXml, 'gui/share/embed/title', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
      else if not DownloadXml(Http, 'http://www.spike.com/ui/xml/mediaplayer/mediagen.groovy?videoId=' + MovieID + '&royaltyReport=true&duration=152&width=640&height=391&impressiontype=18', Xml) then
        SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
      else
        try
          BestUrl := '';
          BestBitrate := 0;
          if Xml.NodeByPath('video/item', Node) then
            for i := 0 to Pred(Node.NodeCount) do
              if (Node.Nodes[i].Name = 'rendition') and GetXmlAttr(Node.Nodes[i], '', 'bitrate', BitrateStr) then
                begin
                Bitrate := StrToIntDef(BitrateStr, 0);
                if Bitrate > BestBitrate then
                  if GetXmlVar(Node.Nodes[i], 'src', Url) then
                    begin
                    BestUrl := Url;
                    BestBitrate := Bitrate;
                    end;
                end;
          if BestUrl = '' then
            SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
          else
            begin
            Name := Title;
            MovieUrl := BestUrl;
            SetPrepared(True);
            Result := True;
            end;
        finally
          Xml.Free;
          end;
    finally
      TitleXml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_Spike);

end.
