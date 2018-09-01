unit xxxDachix;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Dachix = class(THttpDownloader)
    private
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*dachix\.com/Media-';
  URLREGEXP_ID =        '[0-9]+.*\.html?';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_URL = '\s"shows"\s*:\s*\{\s*"streams"\s*:\s*\[\s*\{\s*"file"\s*:"(?P<URL>http.+?)"';

{ TDownloader_Dachix }

class function TDownloader_Dachix.Provider: string;
begin
  Result := 'Dachix.com';
end;

class function TDownloader_Dachix.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Dachix.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Dachix.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Dachix.GetMovieInfoUrl: string;
begin
  Result := 'http://www.dachix.com/Media-' + MovieID;
end;

function TDownloader_Dachix.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := UrlDecode(MovieUrl);
end;

initialization
  RegisterDownloader(TDownloader_Dachix);

end.
