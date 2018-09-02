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

unit downRajce;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Rajce = class(THttpDownloader)
    private
    protected
      PlaylistRegExp: TRegExp;
      {$IFDEF MULTIDOWNLOADS}
      ExtList: TStringList;
      {$ELSE}
      Extension: string;
      {$ENDIF}
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

// http://pso.rajce.idnes.cz/Koncert_28._10._2011_v_Semilech/#PSO-Semily-2011-sestrih.jpg
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*rajce\.idnes\.cz/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<span\s+id="albumName">(?P<TITLE>.*?)</span>';
  REGEXP_PLAYLIST = '\{\s*photoID\s*:\s*"(?P<ID>[0-9]+)"\s*,\s*date\s*:\s*"[^"]*"\s*,\s*name\s*:\s*"(?P<TITLE>[^"]*)"\s*,\s*isVideo\s*:\s*true\s*,\s*desc\s*:\s*"[^"]*"\s*,\s*info\s*:\s*"(?P<FILENAME>[^"|\s]+)[^"]*"';

{ TDownloader_Rajce }

class function TDownloader_Rajce.Provider: string;
begin
  Result := 'Rajce.iDnes.cz';
end;

class function TDownloader_Rajce.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Rajce.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  PlaylistRegExp := RegExCreate(REGEXP_PLAYLIST);
  {$IFDEF MULTIDOWNLOADS}
  ExtList := TStringList.Create;
  {$ENDIF}
end;

destructor TDownloader_Rajce.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(PlaylistRegExp);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(ExtList);
  {$ENDIF}
  inherited;
end;

function TDownloader_Rajce.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Rajce.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Title, FoundID, Server, Path, FileName: string;
  Xml: TXmlDoc;
  Node: TXmlNode;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  {$IFDEF MULTIDOWNLOADS}
  if PlayListRegExp.Match(Page) then
    repeat
      if PlayListRegExp.SubexpressionByName('ID', ID) then
        if PlayListRegExp.SubexpressionByName('TITLE', Title) then
          if PlayListRegExp.SubexpressionByName('FILENAME', FileName) then
  {$ELSE}
  if GetRegExpVars(PlayListRegExp, Page, ['ID', 'TITLE', 'FILENAME'], [@ID, @Title, @FileName]) then
  {$ENDIF}
            if DownloadXml(Http, 'http://www.rajce.idnes.cz/ajax/videoxml.php?id=' + ID, Xml) then
              try
                if XmlNodeByPath(Xml, 'items', Node) then
                  for i := 0 to Pred(Node.NodeCount) do
                    if Node[i].Name = 'item' then
                      if GetXmlVar(Node[i], 'idvideo', FoundID) then
                        if FoundID = ID then
                          begin
                          if GetXmlVar(Node[i], 'linkvideo/server', Server) then
                            if GetXmlVar(Node[i], 'linkvideo/path', Path) then
                              begin
                              MovieUrl := Server + Path + ID;
                              {$IFDEF MULTIDOWNLOADS}
                              if Title = '' then
                                Title := Format('%s (%d)', [UnpreparedName, Succ(UrlList.Count)]);
                              UrlList.Add(MovieUrl);
                              NameList.Add(Title);
                              ExtList.Add(ExtractFileExt(FileName));
                              {$ELSE}
                              if Title <> '' then
                                SetName(Title);
                              Extension := ExtractFileExt(FileName);
                              {$ENDIF}
                              SetPrepared(True);
                              Result := True;
                              end;
                          Break;
                          end;
              finally
                FreeAndNil(Xml);
                end;
  {$IFDEF MULTIDOWNLOADS}
    until not PlayListRegExp.MatchAgain;
  {$ENDIF}
end;

function TDownloader_Rajce.GetFileNameExt: string;
begin
  {$IFDEF MULTIDOWNLOADS}
  Result := ExtList[DownloadIndex];
  {$ELSE}
  Result := Extension;
  {$ENDIF}
end;

initialization
  RegisterDownloader(TDownloader_Rajce);

end.

