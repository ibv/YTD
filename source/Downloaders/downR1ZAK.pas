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

unit downR1ZAK;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_R1ZAK = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
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

// http://www.r1zak.cz/porady/sumava_na_dlani/
const
  URLREGEXP_BEFORE_ID = 'r1zak\.cz';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_INFO = '<iframe\s+src="https?://(?:[a-z0-9-]+\.)*regionplzen\.cz/video/export/\?jmeno=(?P<PATH>[^"&]+)';

{ TDownloader_R1ZAK }

class function TDownloader_R1ZAK.Provider: string;
begin
  Result := 'R1ZAK.cz';
end;

class function TDownloader_R1ZAK.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_R1ZAK.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peAnsi;
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_R1ZAK.Destroy;
begin
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_R1ZAK.GetMovieInfoUrl: string;
begin
  Result := 'http://www.r1zak.cz/' + MovieID;
end;

function TDownloader_R1ZAK.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Title, Path: string;
    InfoXml: TXmlDoc;
    ListNode: TXmlNode;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://www.regionplzen.cz/video/export/' + Path, InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if XmlNodeByPath(InfoXml, 'trackList', ListNode) then
        for i := 0 to Pred(ListNode.NodeCount) do
          if ListNode[i].Name = 'track' then
            if GetXmlVar(ListNode[i], 'title', Title) then
              if GetXmlVar(ListNode[i], 'location', Path) then
                begin
                Path := UrlEncode(Path);
                {$IFDEF MULTIDOWNLOADS}
                NameList.Add(Title);
                UrlList.Add('http://www.regionplzen.cz' + Path);
                {$ELSE}
                SetName(Title);
                MovieUrl := 'http://www.regionplzen.cz' + Path;
                SetPrepared(True);
                Result := True;
                Exit;
                {$ENDIF}
                end;
      {$IFDEF MULTIDOWNLOADS}
      if UrlList.Count > 0 then
        begin
        SetPrepared(True);
        Result := First;
        end;
      {$ENDIF}
    finally
      FreeAndNil(InfoXml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_R1ZAK);

end.
