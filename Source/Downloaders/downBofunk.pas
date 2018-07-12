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

unit downBofunk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Bofunk = class(THttpDownloader)
    private
    protected
      InfoUrlRegExp: TRegExp;
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

// http://www.bofunk.com/video/10444/ingenious_way_to_mow_your_grass.html
const
  URLREGEXP_BEFORE_ID = 'bofunk\.com/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1\s+class=''title''>(?P<TITLE>.*?)</h1>';
  REGEXP_INFO_URL = '<embed\s+src="(?P<URL>/.+?)"';

{ TDownloader_Bofunk }

class function TDownloader_Bofunk.Provider: string;
begin
  Result := 'Bofunk.com';
end;

class function TDownloader_Bofunk.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Bofunk.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUnknown;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL);
end;

destructor TDownloader_Bofunk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_Bofunk.GetMovieInfoUrl: string;
begin
  Result := 'http://www.bofunk.com/video/' + MovieID;
end;

function TDownloader_Bofunk.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url: string;
    Xml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://flv.bofunk.com' + Url, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not Xml.NodeByPathAndAttr('SETTINGS/PLAYER_SETTINGS', 'Name', 'FLVPath', Node) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else if not GetXmlAttr(Node, '', 'Value', Url) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else
        begin
        MovieUrl := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_Bofunk);

end.
