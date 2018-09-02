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

unit downZDF;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ZDF = class(THttpDownloader)
    protected
      AssetIdRegExp: TRegExp;
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

// http://www.zdf.de/ZDFmediathek/beitrag/video/1246826/Vorschau-zur-Sendung-vom-09.03.2011#/beitrag/video/1308766/Ägypten---4-Geheimnis-des-ewigen-Lebens
const
  URLREGEXP_BEFORE_ID = 'zdf\.de/ZDFmediathek/(?:hauptnavigation/.*?#/|(?!hauptnavigation/))';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_ASSET_ID = '<div\s+id="playerContainer"\s+class="(?P<ID>\d+)"';

{ TDownloader_ZDF }

class function TDownloader_ZDF.Provider: string;
begin
  Result := 'ZDF.de';
end;

class function TDownloader_ZDF.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_ZDF.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  AssetIdRegExp := RegExCreate(REGEXP_ASSET_ID);
end;

destructor TDownloader_ZDF.Destroy;
begin
  RegExFreeAndNil(AssetIdRegExp);
  inherited;
end;

function TDownloader_ZDF.GetMovieInfoUrl: string;
begin
  Result := 'http://www.zdf.de/ZDFmediathek/' + MovieID;
end;

function TDownloader_ZDF.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  ServiceUrl = 'http://www.zdf.de/ZDFmediathek/xmlservice/web/beitragsDetails?id=%s&ak=web';
var
  AssetID: string;
  InfoXml: TXmlDoc;
  FormatsNode: TXmlNode;
  i, Size, BestSize: integer;
  Status, Title, BestUrl, Url, sSize: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(AssetIdRegExp, Page, 'ID', AssetID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, Format(ServiceUrl, [AssetID]), InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(InfoXml, 'status/statuscode', Status) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else if Status <> 'ok' then
        SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Status]))
      else if not GetXmlVar(InfoXml, 'video/information/title', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
      else if not XmlNodeByPath(InfoXml, 'video/formitaeten', FormatsNode) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        BestUrl := '';
        BestSize := -1;
        for i := 0 to Pred(FormatsNode.NodeCount) do
          if FormatsNode.Nodes[i].Name = 'formitaet' then
            if GetXmlVar(FormatsNode.Nodes[i], 'url', Url) then
              if Url <> '' then
                if GetXmlVar(FormatsNode.Nodes[i], 'filesize', sSize) then
                  begin
                  Size := StrToIntDef(sSize, 0);
                  if Size > BestSize then
                    begin
                    BestUrl := Url;
                    BestSize := Size;
                    end;
                  end;
        if BestUrl <> '' then
          begin
          MovieUrl := Url;
          SetName(Title);
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      FreeAndNil(InfoXml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_ZDF);

end.
