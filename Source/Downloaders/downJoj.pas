unit downJoj;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Joj = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: IRegEx;
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://televizia.joj.sk/tv-archiv/krimi-noviny/22-05-2010.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*joj\.sk/tv-archiv/';
  URLREGEXP_ID =        '.+?';
  URLREGEXP_AFTER_ID =  '/?$';

const
  REGEXP_FLASHVARS = '\.addParam\s*\(\s*"FlashVars"\s*,\s*"basePath=[^"]*?&amp;relationId=(?P<RELATIONID>[0-9]+)&amp;date=(?P<MONTH>[0-9]{2})-(?P<DAY>[0-9]{2})-(?P<YEAR>[0-9]{4})&amp;';

const
  CALENDAR_URL = 'http://www.joj.sk/services/ArchivCalendar.xml?channel=1&relation=';

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
  SetInfoPageEncoding(peUTF8);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Joj.Destroy;
begin
  FlashVarsRegExp := nil;
  inherited;
end;

function TDownloader_Joj.GetMovieInfoUrl: string;
begin
  Result := 'http://televizia.joj.sk/tv-archiv/' + MovieID;
end;

function TDownloader_Joj.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Match: IMatch;
    RelationID, CalendarXml: string;
    Day, Month, Year: integer;
    WantedDay, WantedMonth, Title, Path: string;
    Xml: TjanXmlParser2;
    Node, Files, FileNode: TjanXmlNode2;
    i, j, k: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Match := FlashVarsRegExp.Match(Page);
  try
    if not Match.Matched then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
    else
      begin
      RelationId := Match.Groups.ItemsByName['RELATIONID'].Value;
      Day := StrToInt(Match.Groups.ItemsByName['DAY'].Value);
      Month := StrToInt(Match.Groups.ItemsByName['MONTH'].Value);
      Year := StrToInt(Match.Groups.ItemsByName['YEAR'].Value);
      if not DownloadPage(Http, CALENDAR_URL + RelationId, CalendarXml, peUTF8) then
        SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
      else
        begin
        Xml := TjanXmlParser2.Create;
        try
          Xml.Xml := CalendarXml;
          WantedMonth := Format('%04.4d-%02.2d', [Year, Month]);
          WantedDay := Format('%02.2d', [Day]);
          for i := 0 to Pred(Xml.childCount) do
            if (Xml.childNode[i].name = 'month') and (Xml.childNode[i].attribute['date'] = WantedMonth) then
              for j := 0 to Pred(Xml.childNode[i].childCount) do
                if (Xml.childNode[i].childNode[j].name = 'day') and (Xml.childNode[i].childNode[j].attribute['date'] = WantedDay) then
                  begin
                  Node := Xml.childNode[i].childNode[j].getChildByPath('episode');
                  if Node <> nil then
                    begin
                    FileNode := Node.getChildByPath('files/file');
                    Files := Node.getChildByPath('files');
                    if Files <> nil then
                      for k := 0 to Pred(Files.childCount) do
                        if (Files.childNode[k].name = 'file') and (Files.childNode[k].attribute['quality'] = 'hi') then
                          begin
                          FileNode := Files.childNode[k];
                          Break;
                          end;
                    if FileNode <> nil then
                      begin
                      Title := Node.attribute['title'];
                      Path := FileNode.attribute['path'];
                      if Copy(Path, 1, 5) = 'data/' then
                        Path := Copy(Path, 6, MaxInt);
                      if (Title <> '') and (Path <> '') then
                        begin
                        SetName(Format('%s (%04.4d-%02.2d-%02.2d)', [Title, Year, Month, Day]));
                        // Note: This is a VERY DIRTY HACK! I don't really know whether the domain is fixed or not!
                        MovieURL := 'http://n03.joj.sk' + Base64Decode(Path);
                        SetPrepared(True);
                        Result := True;
                        Exit;
                        end;
                      end;
                    end;
                  end;
        finally
          Xml.Free;
          end;
        end;
      end;
  finally
    Match := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Joj);

end.
