unit downCekniTo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CekniTo = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: IRegEx;
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

// http://www.ceknito.cz/video/446074
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ceknito\.(?:cz|sk)/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_MOVIEID = '<param\s+name="flashvars"\s+value="fid=(?P<ID>[^&"]+)';

{ TDownloader_CekniTo }

class function TDownloader_CekniTo.Provider: string;
begin
  Result := 'CekniTo.sk';
end;

class function TDownloader_CekniTo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CekniTo.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIDRegExp := RegExCreate(REGEXP_EXTRACT_MOVIEID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CekniTo.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieIDRegExp := nil;
  inherited;
end;

function TDownloader_CekniTo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ceknito.sk/video/' + MovieID;
end;

function TDownloader_CekniTo.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var ID: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not DownloadPage(Http, 'http://vid4.ceknito.sk/v.php?id=' + ID, hmHEAD) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    MovieURL := LastURL;
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_CekniTo);

end.
