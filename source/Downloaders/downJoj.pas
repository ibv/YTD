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

unit downJoj;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uCompatibility, HttpSend,
  uOptions, 
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_Joj,
    {$ELSE}
      guiOptionsVCL_Joj,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Joj = class(TRtmpDownloader)
    private
    protected
      Server: string;
      MovieIdRegExp: TRegExp;
      PageIdRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

const
  OPTION_JOJ_SERVER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'server';
  OPTION_JOJ_SERVER_DEFAULT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'n15.joj.sk';

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.joj.sk/anosefe/anosefe-epizody/2011-05-09-ano-sefe-.html
// http://www.joj.sk/sudna-sien/sudna-sien-archiv/2011-05-03-sudna-sien.html
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + 'joj\.sk/' + REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_TITLE;
  REGEXP_MOVIE_ID = '&amp;videoId=(?P<ID>\d+)';
  REGEXP_PAGE_ID = '&amp;pageId=(?P<ID>\d+)';

{ TDownloader_Joj }

class function TDownloader_Joj.Provider: string;
begin
  Result := 'Joj.sk';
end;

class function TDownloader_Joj.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

{$IFDEF GUI}
class function TDownloader_Joj.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_Joj;
end;
{$ENDIF}

constructor TDownloader_Joj.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  Server := OPTION_JOJ_SERVER_DEFAULT;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID);
  PageIdRegExp := RegExCreate(REGEXP_PAGE_ID);
end;

destructor TDownloader_Joj.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdRegExp);
  RegExFreeAndNil(PageIdRegExp);
  inherited;
end;

function TDownloader_Joj.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Joj.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Files: TXmlNode;
    VideoID, PageID, BestPath, Path, sQuality: string;
    i, j, BestQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIdRegExp, Page, 'ID', VideoID) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['videoId']))
  else if not GetRegExpVar(PageIdRegExp, Page, 'ID', PageID) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['pageId']))
  else if not DownloadXml(Http, 'http://www.joj.sk/services/Video.php?clip=' + VideoID + '&pageId=' + PageID, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      //if GetXmlAttr(Xml, '', 'title', Title) then
      //  SetName(Title);
      if not XmlNodeByPath(Xml, 'files', Files) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else
        begin
        BestQuality := -1;
        BestPath := '';
        for i := 0 to Pred(Files.NodeCount) do
          if Files.Nodes[i].Name = 'file' then
            if GetXmlAttr(Files.Nodes[i], '', 'path', Path) then
              begin
              Quality := 0;
              if GetXmlAttr(Files.Nodes[i], '', 'label', sQuality) then
                for j := 1 to Length(sQuality) do
                  if CharInSet(sQuality[j], ['0'..'9']) then
                    Quality := 10 * Quality + Ord(sQuality[j]) - Ord('0')
                  else
                    Break;
              if Quality > BestQuality then
                begin
                BestPath := Path;
                BestQuality := Quality;
                end;
              end;
        if BestPath <> '' then
          begin
          Self.RtmpUrl := 'rtmp://' + Server;
          Self.Playpath := BestPath;
          MovieUrl := RtmpUrl + BestPath;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      FreeAndNil(Xml);
      end;
end;

procedure TDownloader_Joj.SetOptions(const Value: TYTDOptions);
var
  s: string;
begin
  inherited;
  s := Value.ReadProviderOptionDef(Provider, OPTION_JOJ_SERVER, OPTION_JOJ_SERVER_DEFAULT);
  if s <> '' then
    Server := s;
end;

initialization
  RegisterDownloader(TDownloader_Joj);

end.
