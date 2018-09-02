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

unit downNHL;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NHL = class(THttpDownloader)
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
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://video.nhl.com/videocenter/console?hlp=8474138&fr=false
const
  URLREGEXP_BEFORE_ID = 'video\.nhl\.com/videocenter/.*[?&]hlp=';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_NHL }

class function TDownloader_NHL.Provider: string;
begin
  Result := 'NHL.com';
end;

class function TDownloader_NHL.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_NHL.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peXml;
end;

destructor TDownloader_NHL.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_NHL.GetMovieInfoUrl: string;
begin
  Result := 'http://video.nhl.com/videocenter/console?hlp=' + MovieID;
end;

function TDownloader_NHL.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Xml: TXmlDoc;
  Node: TXmlNode;
  NodeName, Url: string;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadXml(Http, 'http://video.nhl.com/videocenter/highlights', 'xml=2&id=' + MovieID + '&ps=9&pn=1&pm=0&ptrs=3', HTTP_FORM_URLENCODING, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      Node := nil;
      NodeName := '';
      if Xml.Root.Name = 'games' then
        begin
        Node := Xml.Root;
        NodeName := 'game';
        end
      else if (Xml.Root.Name = 'player') and (XmlNodeByPath(Xml, 'goals', Node)) then
        NodeName := 'goal';
      if Node <> nil then
        for i := 0 to Pred(Node.NodeCount) do
          if (NodeName = '') or (Node.Nodes[i].Name = NodeName) then
            if GetXmlVar(Node.Nodes[i], 'alt-video-clip', Url) then
              // Je tam i 'video-clip', ktery bezi na RTMP
              begin
              MovieUrl := Url;
              SetPrepared(True);
              Result := True;
              {$IFDEF MULTIDOWNLOADS}
              UrlList.Add(Url);
              NameList.Add('');
              {$ELSE}
              Break;
              {$ENDIF}
              end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_NHL);

end.
