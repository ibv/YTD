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

unit downEKucharkaNet;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uStrings, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EKucharkaNet = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
      InfoXmlRegExp: TRegExp;
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

// http://ekucharka.net/jablka-v-zupanu
const
  URLREGEXP_BEFORE_ID = 'ekucharka\.net/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<title>(?P<TITLE>.*?)(?:\s*\||</title>)';
  REGEXP_MOVIE_ID =     '\bvar\s+videoID\s*=\s*(?P<QUOTE>"?)(?P<ID>\d+)(?P=QUOTE)';
  REGEXP_INFO_XML =     '(?<!\\)"(?P<XML><\?xml.+?>)"';

{ TDownloader_EKucharkaNet }

class function TDownloader_EKucharkaNet.Provider: string;
begin
  Result := 'eKucharka.net';
end;

class function TDownloader_EKucharkaNet.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_EKucharkaNet.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
  InfoXmlRegExp := RegExCreate(REGEXP_INFO_XML);
end;

destructor TDownloader_EKucharkaNet.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(InfoXmlRegExp);
  inherited;
end;

function TDownloader_EKucharkaNet.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ekucharka.net/' + MovieID;
end;

function TDownloader_EKucharkaNet.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, InfoPage, InfoXml: string;
  Xml: TXmlDoc;
  VideoNode, SourcesNode: TXmlNode;
  i, Quality, BestQuality: integer;
  BestUrl, sUrl, sQuality: string; 
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, Format('http://ekucharka.net/prehravac/players/requests/darkonyx_datafeed.php?videoID=%s,', [ID]), InfoPage, peUtf8) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(InfoXmlRegExp, InfoPage, 'XML', InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
  else
    begin
    Xml := TXmlDoc.Create;
    try
      InfoXml := JSDecode(InfoXml);
      Xml.LoadFromBinaryString( {$IFDEF UNICODE} AnsiString {$ENDIF} (InfoXml));
      if not XmlNodeByPathAndAttr(Xml, 'videos/video', 'id', ID, VideoNode) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
      else if not XmlNodeByPath(VideoNode, 'sources', SourcesNode) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
      else
        begin
        BestUrl := '';
        BestQuality := -1;
        for i := 0 to Pred(SourcesNode.NodeCount) do
          if SourcesNode.Nodes[i].Name = 'source' then
            begin
            sUrl := XmlValueIncludingCData(SourcesNode.Nodes[i]);
            if sUrl <> '' then
              begin
              if not GetXmlAttr(SourcesNode.Nodes[i], '', 'type', sQuality) then
                Quality := 0
              else if sQuality = 'SD' then
                Quality := 1
              else if sQuality = 'HD' then
                Quality := 2
              else
                Quality := 0;
              if Quality > BestQuality then
                if DownloadPage(Http, sUrl, hmHead) and (Http.ResultCode >= 200) and (Http.ResultCode < 300) then
                  begin
                  BestUrl := sUrl;
                  BestQuality := Quality;
                  end;
              end;
            end;
        if BestUrl <> '' then
          begin
          MovieUrl := BestUrl;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      FreeAndNil(Xml);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_EKucharkaNet);

end.
