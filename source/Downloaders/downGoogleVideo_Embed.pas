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

unit downGoogleVideo_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_GoogleVideo_Embed = class(THttpDownloader)
    private
    protected
      Extension: string;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://video.google.com/googleplayer.swf?docid=-3219629169575348946&hl=cs&fs=true
const
  URLREGEXP_BEFORE_ID = 'video\.google\.com/googleplayer\.swf\?';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_GoogleVideo_Embed }

class function TDownloader_GoogleVideo_Embed.Provider: string;
begin
  Result := 'Google.com';
end;

class function TDownloader_GoogleVideo_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_GoogleVideo_Embed.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := True;
end;

destructor TDownloader_GoogleVideo_Embed.Destroy;
begin
  inherited;
end;

function TDownloader_GoogleVideo_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://video.google.com/videofeed?fgvns=1&fai=1&' + MovieID;
end;

function TDownloader_GoogleVideo_Embed.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_GoogleVideo_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Node: TXmlNode;
  Title, Url, ContentType: string;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not XmlNodeByPath(PageXml, 'channel/item/media:group', Node) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    if GetXmlVar(Node, 'media:title', Title) then
      if Title <> '' then
        Name := Title;
    for i := 0 to Pred(Node.NodeCount) do
      if Node[i].Name = 'media:content' then
        if GetXmlAttr(Node[i], '', 'url', Url) then
          if GetXmlAttr(Node[i], '', 'type', ContentType) then
            begin
            Extension := ContentTypeToExtension(ContentType);
            if Extension <> '.swf' then
              begin
              MovieUrl := Url;
              if UnpreparedName = '' then
                Name := 'Google Video ' + MovieID;
              SetPrepared(True);
              Result := True;
              Exit;
              end;
            end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_GoogleVideo_Embed);

end.

