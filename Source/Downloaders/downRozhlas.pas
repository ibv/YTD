unit downRozhlas;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Rozhlas = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: IRegEx;
      TableRowsRegExp: IRegEx;
      StreamIdRegExp: IRegEx;
      StreamTitleRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uStringUtils;

const TABLE_ROWS_REGEXP = '<tr>(?P<ROW>.*?)</tr>';
const STREAM_ID_REGEXP = '"https?://(?:www\.)?rozhlas\.cz/default/default/rnp-player\.php\?id=(?P<ID>[0-9]+)"';
const STREAM_TITLE_REGEXP = '<strong>(?P<TITLE>.*?)</strong>';

{ TDownloader_Rozhlas }

constructor TDownloader_Rozhlas.Create(const AMovieID: string);
begin
  inherited;
  TableRowsRegExp := RegExCreate(TABLE_ROWS_REGEXP, [rcoSingleLine, rcoIgnoreCase]);
  StreamIdRegExp := RegExCreate(STREAM_ID_REGEXP, [rcoIgnoreCase]);
  StreamTitleRegExp := RegExCreate(STREAM_TITLE_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_Rozhlas.Destroy;
begin
  TableRowsRegExp := nil;
  StreamIdRegExp := nil;
  StreamTitleRegExp := nil;
  inherited;
end;

class function TDownloader_Rozhlas.Provider: string;
begin
  Result := 'rozhlas.cz';
end;

class function TDownloader_Rozhlas.MovieIDParamName: string;
begin
  Result := 'ROZHLAS';
end;

class function TDownloader_Rozhlas.UrlRegExp: string;
begin
  // http://www.rozhlas.cz/vltava/porady/_zprava/676996
  Result := '^https?://(?:[a-z0-9-]+\.)?rozhlas\.cz/vltava/porady/_zprava/(?P<' + MovieIDParamName + '>[0-9]+)';
end;

function TDownloader_Rozhlas.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rozhlas.cz/vltava/porady/_zprava/' + MovieID;
end;

function TDownloader_Rozhlas.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const MediaUrl = 'http://media.rozhlas.cz/_audio/%s.mp3';
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
      SetLastErrorMsg('Failed to locate useful stream.')
    else
      begin
      SetPrepared(True);
      Result := First;
      end;
    {$ELSE}
    SetLastErrorMsg('Failed to locate useful stream.');
    {$ENDIF}
  finally
    TableRows := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Rozhlas);

end.
