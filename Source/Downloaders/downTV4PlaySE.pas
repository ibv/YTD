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

unit downTV4PlaySE;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_TV4PlaySE = class(TRtmpDownloader)
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
  uStrings,
  uMessages,
  uDownloadClassifier;

// http://www.tv4play.se/sport/alpint?title=inga_svenska_skidskyttemedaljer&videoid=1295009
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tv4play\.se/.*[?&]videoid=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '"name"\s*:\s*"(?P<TITLE>.*?)"';

{ TDownloader_TV4PlaySE }

class function TDownloader_TV4PlaySE.Provider: string;
begin
  Result := 'TV4Play.se';
end;

class function TDownloader_TV4PlaySE.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_TV4PlaySE.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
end;

destructor TDownloader_TV4PlaySE.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_TV4PlaySE.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tv4play.se/search/search.json?vmanid=' + MovieID;
end;

function TDownloader_TV4PlaySE.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Node: TXmlNode;
    Url, BaseUrl, BestUrl: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadXml(Http, 'http://anytime.tv4.se/webtv/metafileFlash.smil?p=' + MovieID + '&bw=1800&emulate=true&sl=true&', Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      BaseUrl := '';
      if Xml.NodeByPath('head', Node) then
        for i := 0 to Pred(Node.NodeCount) do
          if Node.Nodes[i].Name = 'meta' then
            if GetXmlAttr(Node.Nodes[i], '', 'base', Url) then
              begin
              BaseUrl := Url;
              Break;
              end;
      if BaseUrl = '' then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND , ['base']))
      else
        begin
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND , ['video']));
        if Xml.NodeByPath('body/switch', Node) then
          if Smil_FindBestVideo(Node, BestUrl) then
            begin
            MovieUrl := BaseUrl + '/' + BestUrl;
            Self.RtmpUrl := BaseUrl;
            Self.Playpath := BestUrl;
            SetPrepared(True);
            Result := True;
            end;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_TV4PlaySE);

end.
