unit downNJoy;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NJoy = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
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

// http://n-joy.cz/video/supcom-2-zabery-z-hrani-2/oiuhz6e3xgt35e4e
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*n-joy\.cz/video/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?:N-JOY.CZ - )?(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_URL = '\sflashvars\.file_url\s*=\s*"(?P<URL>.*?)"';

{ TDownloader_NJoy }

class function TDownloader_NJoy.Provider: string;
begin
  Result := 'N-joy.cz';
end;

class function TDownloader_NJoy.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_NJoy.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
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

initialization
  RegisterDownloader(TDownloader_NJoy);

end.
