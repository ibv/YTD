unit uNJoyDownloader;

interface

uses
  SysUtils, Classes,
  HttpSend,
  uDownloader, uCommonDownloader;

type
  TNJoyDownloader = class(TCommonDownloader)
    private
    protected
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function GetMovieInfoUrl: string; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  PCRE, uStringUtils;
  
const EXTRACT_TITLE_REGEXP = '<title>(?:N-JOY.CZ - )?(?P<TITLE>.*?)</title>';
      EXTRACT_URL_REGEXP = '\sflashvars\.file_url\s*=\s*"(?P<URL>.*?)"';
      
{ TNJoyDownloader }

constructor TNJoyDownloader.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  MovieTitleRegExp := RegExCreate(EXTRACT_TITLE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(EXTRACT_URL_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TNJoyDownloader.Destroy;
begin
  inherited;
end;

function TNJoyDownloader.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Page := WideToAnsi(Utf8ToWide(Page));
end;

function TNJoyDownloader.GetMovieInfoUrl: string;
begin
  Result := 'http://n-joy.cz/video/dummy/' + MovieID;
end;

end.
