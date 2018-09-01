(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit downCSmaTalent;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CSmaTalent = class(THttpDownloader)
    private
    protected
      VideoIdRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.csmatalent.cz/epizody-cz/detail/divadlo-29-8-2010.html
// http://www.csmatalent.sk/epizody/detail/divadlo-29-8-2010.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*csmatalent\.';
  URLREGEXP_ID =        '(cz|sk)/.+?\.html';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_VIDEO_ID = '<link\s+rel="video_src"\s+href="(?:[^"]*&amp;)?videoId=(?P<ID>.+?)(?:&amp;|")';

{ TDownloader_CSmaTalent }

class function TDownloader_CSmaTalent.Provider: string;
begin
  Result := 'CSmaTalent.cz';
end;

class function TDownloader_CSmaTalent.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CSmaTalent.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CSmaTalent.Destroy;
begin
  RegExFreeAndNil(VideoIdRegExp);
  inherited;
end;

function TDownloader_CSmaTalent.GetMovieInfoUrl: string;
begin
  Result := 'http://www.csmatalent.' + MovieID;
end;

function TDownloader_CSmaTalent.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var ID, Title, Path: string;
    InfoXml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['videoId']))
  else if not DownloadXml(Http, 'http://www.csmatalent.cz/services/Video.php?clip=' + ID, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else if not GetXmlAttr(InfoXml, '', 'title', Title) then
    SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
  else if not (XmlNodeByPathAndAttr(InfoXml, 'files/file', 'quality', 'hi', Node) or XmlNodeByPath(InfoXml, 'files/file', Node)) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else if not GetXmlAttr(Node, '', 'path', Path) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    SetName(Title);
    MovieUrl := 'http://n06.joj.sk/' + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CSmaTalent);

end.
