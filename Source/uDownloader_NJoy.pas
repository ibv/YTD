unit uDownloader_NJoy;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NJoy = class(THttpDownloader)
    private
    protected
      function GetInfoPageEncoding: TPageEncoding; override;
      function GetMovieInfoUrl: string; override;
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
  PCRE;
  
const EXTRACT_TITLE_REGEXP = '<title>(?:N-JOY.CZ - )?(?P<TITLE>.*?)</title>';
      EXTRACT_URL_REGEXP = '\sflashvars\.file_url\s*=\s*"(?P<URL>.*?)"';
      
{ TDownloader_NJoy }

constructor TDownloader_NJoy.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  MovieTitleRegExp := RegExCreate(EXTRACT_TITLE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(EXTRACT_URL_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_NJoy.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_NJoy.GetMovieInfoUrl: string;
begin
  Result := 'http://n-joy.cz/video/dummy/' + MovieID;
end;

class function TDownloader_NJoy.Provider: string;
begin
  Result := 'N-joy.cz';
end;

class function TDownloader_NJoy.MovieIDParamName: string;
begin
  Result := 'NJOY';
end;

class function TDownloader_NJoy.UrlRegExp: string;
begin
  // http://n-joy.cz/video/supcom-2-zabery-z-hrani-2/oiuhz6e3xgt35e4e
  Result := '^https?://(?:[a-z0-9-]+\.)?n-joy\.cz/video/[^/]+/(?P<' + MovieIDParamName + '>[^/&?]+)';
end;

function TDownloader_NJoy.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

initialization
  RegisterDownloader(TDownloader_NJoy);

end.
