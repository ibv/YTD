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

unit downEuroVisionSports;
{$INCLUDE 'ytd.inc'}

// Chybi token

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EuroVisionSports = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
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

// http://www.eurovisionsports.tv/london2012/index.html?video_id=8145
const
  URLREGEXP_BEFORE_ID = 'eurovisionsports\.tv/';
  URLREGEXP_ID =        '.*?[?&]video_id=(?<ID>\d+).*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_INFO =   '\bmrss\s*:\s*"(?P<URL>.+?)"';

{ TDownloader_EuroVisionSports }

class function TDownloader_EuroVisionSports.Provider: string;
begin
  Result := 'EuroVisionSports.tv';
end;

class function TDownloader_EuroVisionSports.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_EuroVisionSports.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUnknown;
  //MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  //MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  MovieIDRegExp := RegExCreate(URLREGEXP_ID);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_EuroVisionSports.Destroy;
begin
  //RegExFreeAndNil(MovieTitleRegExp);
  //RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_EuroVisionSports.GetMovieInfoUrl: string;
begin
  Result := 'http://www.eurovisionsports.tv/' + MovieID;
end;

function TDownloader_EuroVisionSports.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Url, Title, Server, Stream: string;
  InfoXml, SmilXml: TXmlDoc;
  InfoNode, SmilNode: TXmlNode;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, MovieID, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetRegExpVar(MovieInfoRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, GetRelativeUrl(GetMovieInfoUrl, Url), InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      for i := 0 to Pred(InfoXml.Root.NodeCount) do
        if InfoXml.Root.Nodes[i].Name = 'channel' then
          if XmlNodeByPathAndAttr(InfoXml.Root.Nodes[i], 'item', 'id', ID, InfoNode) then
            begin
            if not GetXmlAttr(InfoNode, 'enclosure', 'url', Url) then
              SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
            else
              begin
              if not GetXmlVar(InfoNode, 'title', Title) then
                GetXmlVar(InfoNode, 'media:title', Title);
              if not DownloadXml(Http, Url, SmilXml) then
                SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
              else
                try
                  Server := '';
                  if XmlNodeByPathAndAttr(SmilXml, 'head/meta', 'name', 'httpBase', SmilNode) then
                    GetXmlAttr(SmilNode, '', 'content', Server);
                  if not GetXmlAttr(SmilXml, 'body/switch/video', 'src', Stream) then
                    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
                  else
                    begin
                    Name := Title;
                    MovieUrl := Server + Stream;
                    SetPrepared(True);
                    Result := True;
                    end;
                finally
                  FreeAndNil(SmilXml);
                  end;
              end;
            Break;
            end;
    finally
      FreeAndNil(InfoXml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_EuroVisionSports);

end.
