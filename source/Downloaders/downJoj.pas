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

unit downJoj;
{$INCLUDE 'ytd.inc'}
{.DEFINE ALLOW_MDY_DATE} // Allow switching of day and month. Not recommended!

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Joj = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function ProcessCalendar(Http: THttpSend; const CalendarUrl, RelationID: string; Day, Month, Year: integer): boolean; virtual;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://televizia.joj.sk/tv-archiv/krimi-noviny/22-05-2010.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*joj\.sk/tv-archiv/';
  URLREGEXP_ID =        '.+?';
  URLREGEXP_AFTER_ID =  '/?$';

const
  REGEXP_FLASHVARS = '\.addParam\s*\(\s*"FlashVars"\s*,\s*"basePath=[^"]*?&amp;relationId=(?P<RELATIONID>[0-9]+)&amp;date=(?P<DAY>[0-9]{2})-(?P<MONTH>[0-9]{2})-(?P<YEAR>[0-9]{4})&amp;calendar=(?P<CALENDARURL>https?%3A%2F%2F.*?)&amp;';

const
  CALENDAR_URL_BY_RELATIONID = 'http://www.joj.sk//services/ArchivCalendar.xml?channel=1&relationId=';
  {$IFDEF ALLOW_MDY_DATE}
  CALENDAR_URL_BY_DATE = 'http://www.joj.sk//services/ArchivCalendar.xml?channel=1&date=';
  {$ENDIF}

{ TDownloader_Joj }

class function TDownloader_Joj.Provider: string;
begin
  Result := 'Joj.sk';
end;

class function TDownloader_Joj.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Joj.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Joj.Destroy;
begin
  RegExFreeAndNil(FlashVarsRegExp);
  inherited;
end;

function TDownloader_Joj.GetMovieInfoUrl: string;
begin
  Result := 'http://televizia.joj.sk/tv-archiv/' + MovieID;
end;

function TDownloader_Joj.ProcessCalendar(Http: THttpSend; const CalendarUrl, RelationID: string; Day, Month, Year: integer): boolean;
var Xml: TXmlDoc;
    Node, MonthNode, DayNode, FileNode: TXmlNode;
    i: integer;
    WantedDay, WantedMonth, Title, Path: string;
begin
  Result := False;
  if DownloadXml(Http, CalendarUrl, Xml) then
    try
      WantedMonth := Format('%04.4d-%02.2d', [Year, Month]);
      WantedDay := IntToStr(Day); //Format('%02.2d', [Day]);
      if Xml.NodeByPathAndAttr('month', 'date', WantedMonth, MonthNode) then
        if XmlNodeByPathAndAttr(MonthNode, 'day', 'date', WantedDay, DayNode) then
          for i := 0 to Pred(DayNode.NodeCount) do
            if DayNode.Nodes[i].Name = 'episode' then
              if XmlNodeByPathAndAttr(DayNode.Nodes[i], 'relation', 'id', RelationId, Node) then
                begin
                Node := DayNode.Nodes[i];
                if not XmlNodeByPathAndAttr(Node, 'files/file', 'quality', 'hi', FileNode) then
                  XmlNodeByPath(Node, 'files/file', FileNode);
                if FileNode <> nil then
                  if GetXmlAttr(Node, '', 'title', Title) then
                    if GetXmlAttr(FileNode, '', 'path', Path) then
                      begin
                      if Copy(Path, 1, 5) = 'data/' then
                        Path := Copy(Path, 6, MaxInt);
                      if (Title <> '') and (Path <> '') then
                        begin
                        SetName(Format('%s (%04.4d-%02.2d-%02.2d)', [Title, Year, Month, Day]));
                        {$IFDEF DIRTYHACKS}
                        // Note: I don't really know whether the domain is fixed or not! It seems to be, though
                        MovieURL := 'http://n03.joj.sk' + Base64Decode(Path);
                        SetPrepared(True);
                        Result := True;
                        {$ENDIF}
                        Exit;
                        end;
                      end;
                end;
    finally
      Xml.Free;
      end;
end;

function TDownloader_Joj.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var RelationID, CalendarUrl: string;
    Day, Month, Year: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not FlashVarsRegExp.Match(Page) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else
    begin
    RelationId := FlashVarsRegExp.SubexpressionByName('RELATIONID');
    Day := StrToInt(FlashVarsRegExp.SubexpressionByName('DAY'));
    Month := StrToInt(FlashVarsRegExp.SubexpressionByName('MONTH'));
    Year := StrToInt(FlashVarsRegExp.SubexpressionByName('YEAR'));
    CalendarUrl := FlashVarsRegExp.SubexpressionByName('CALENDARURL');
    Result := False
      or ProcessCalendar(Http, UrlDecode(CalendarUrl), RelationID, Day, Month, Year)
      or ProcessCalendar(Http, CALENDAR_URL_BY_RELATIONID + RelationID, RelationID, Day, Month, Year)
      {$IFDEF ALLOW_MDY_DATE}
      or ProcessCalendar(Http, CALENDAR_URL_BY_DATE + Format('%04.4d-%02.2d', [Year, Day]), RelationID, Month, Day, Year)
      {$ENDIF}
      ;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Joj);

end.
