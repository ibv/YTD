unit downRozhlas;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Rozhlas = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: IRegEx;
      TableRowsRegExp: IRegEx;
      StreamIdRegExp: IRegEx;
      StreamTitleRegExp: IRegEx;
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
  uDownloadClassifier,
  uMessages;

// http://www.rozhlas.cz/vltava/porady/_zprava/676996
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rozhlas\.cz/vltava/porady/_zprava/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_TABLE_ROWS = '<tr>(?P<ROW>.*?)</tr>';
  REGEXP_STREAM_ID = '"https?://(?:www\.)?rozhlas\.cz/default/default/rnp-player\.php\?id=(?P<ID>[0-9]+)"';
  REGEXP_STREAM_TITLE = '<strong>(?P<TITLE>.*?)</strong>';

{ TDownloader_Rozhlas }

class function TDownloader_Rozhlas.Provider: string;
begin
  Result := 'Rozhlas.cz';
end;

class function TDownloader_Rozhlas.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Rozhlas.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  TableRowsRegExp := RegExCreate(REGEXP_TABLE_ROWS, [rcoSingleLine, rcoIgnoreCase]);
  StreamIdRegExp := RegExCreate(REGEXP_STREAM_ID, [rcoIgnoreCase]);
  StreamTitleRegExp := RegExCreate(REGEXP_STREAM_TITLE, [rcoIgnoreCase]);
end;

destructor TDownloader_Rozhlas.Destroy;
begin
  TableRowsRegExp := nil;
  StreamIdRegExp := nil;
  StreamTitleRegExp := nil;
  inherited;
end;

function TDownloader_Rozhlas.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rozhlas.cz/vltava/porady/_zprava/' + MovieID;
end;

function TDownloader_Rozhlas.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const
  MediaUrl = 'http://media.rozhlas.cz/_audio/%s.mp3';
var TableRows: IMatchCollection;
    Row, Title, ID: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  TableRows := TableRowsRegExp.Matches(Page);
  try
    for i := 0 to Pred(TableRows.Count) do
      begin
      Row := TableRows[i].Groups.ItemsByName['ROW'].Value;
      if GetRegExpVar(StreamIdRegExp, Row, 'ID', ID) and GetRegExpVar(StreamTitleRegExp, Row, 'TITLE', Title) then
        begin
        {$IFDEF MULTIDOWNLOADS}
        NameList.Add(Title);
        UrlList.Add(Format(MediaUrl, [ID]));
        {$ELSE}
        SetName(Title);
        MovieUrl := Format(MediaUrl, [ID]);
        Result := True;
        SetPrepared(True);
        Exit;
        {$ENDIF}
        end;
      end;
    {$IFDEF MULTIDOWNLOADS}
    if UrlList.Count <= 0 then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      SetPrepared(True);
      Result := First;
      end;
    {$ELSE}
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);
    {$ENDIF}
  finally
    TableRows := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Rozhlas);

end.
