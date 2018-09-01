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

unit downJoj_Porady;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

// Note: This is not a standalone downloader. It is intended to be used as
// a parent for actual downloaders, e.g. CSmaTalent or Sefka.

type
  TDownloader_Joj_Porady = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      VariablesRegExp: TRegExp;
    protected
      function TheServer: string; virtual; abstract;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

const
  REGEXP_FLASHVARS = '\.addParam\s*\(\s*"FlashVars"\s*,\s*"(?P<FLASHVARS>.+?)"';
  REGEXP_VARIABLES = '(?P<VARNAME>[^=]+)=(?P<VARVALUE>.*?)(?:&|$)';

{ TDownloader_Joj_Porady }

class function TDownloader_Joj_Porady.Provider: string;
begin
  Result := 'Joj.sk';
end;

constructor TDownloader_Joj_Porady.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  VariablesRegExp := RegExCreate(REGEXP_VARIABLES);
end;

destructor TDownloader_Joj_Porady.Destroy;
begin
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(VariablesRegExp);
  inherited;
end;

function TDownloader_Joj_Porady.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, BasePath, VideoID, Title, Path: string;
    InfoXml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetRegExpVarPairs(VariablesRegExp, HtmlDecode(FlashVars), ['basePath', 'videoId'], [@BasePath, @VideoID]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, UrlDecode(BasePath) + 'services/Video.php?clip=' + VideoID, InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetXmlAttr(InfoXml, '', 'title', Title) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else if not (XmlNodeByPathAndAttr(InfoXml, 'files/file', 'quality', 'hi', Node) or XmlNodeByPath(InfoXml, 'files/file', Node)) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not GetXmlAttr(Node, '', 'path', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    SetName(Title);
    MovieUrl := 'http://' + TheServer + '/' + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  // Not a standalone downloader
  //RegisterDownloader(TDownloader_Joj_Porady);

end.
