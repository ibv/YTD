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

unit downEyeneer;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Eyeneer = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
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

// http://www.eyeneer.com/video/rock/bee-gees/to-love-somebody
const
  URLREGEXP_BEFORE_ID = 'eyeneer\.com/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID =     '<meta\s+property="og:video"\s+content="https?://content\.bitsontherun\.com/players/(?P<ID>[^"-.]+)-[^."]+\.swf"';

{ TDownloader_Eyeneer }

class function TDownloader_Eyeneer.Provider: string;
begin
  Result := 'Eyeneer.com';
end;

class function TDownloader_Eyeneer.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Eyeneer.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
end;

destructor TDownloader_Eyeneer.Destroy;
begin
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_Eyeneer.GetMovieInfoUrl: string;
begin
  Result := 'http://www.eyeneer.com/video/' + MovieID;
end;

function TDownloader_Eyeneer.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Xml: TXmlDoc;
  ChannelNode, ItemNode: TXmlNode;
  ID, Title, Url, Ext: string;
  i, j: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://content.bitsontherun.com/jw6/' + ID + '.xml', Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if XmlNodeByPath(Xml, 'channel', ChannelNode) then
        for i := 0 to Pred(ChannelNode.NodeCount) do
          if ChannelNode.Nodes[i].Name = 'item' then
            begin
            ItemNode := ChannelNode.Nodes[i];
            if GetXmlVar(ItemNode, 'title', Title) then
              for j := 0 to Pred(ItemNode.NodeCount) do
                if ItemNode.Nodes[j].Name = 'jwplayer:source' then
                  if GetXmlAttr(ItemNode.Nodes[j], '', 'file', Url) then
                    begin
                    Ext := ExtractUrlExt(Url);
                    if AnsiCompareText(Ext, '.mp4') = 0 then
                      begin
                      {$IFDEF MULTIDOWNLOADS}
                      NameList.Add(Title);
                      UrlList.Add(Url);
                      {$ELSE}
                      MovieUrl := Url;
                      Name := Title;
                      {$ENDIF}
                      SetPrepared(True);
                      Result := True;
                      Break;
                      end;
                    end;
            end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_Eyeneer);

end.
