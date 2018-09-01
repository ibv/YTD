unit downKukaj;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Kukaj = class(THttpDownloader)
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

// http://www.kukaj.sk/videa/5860/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*kukaj\.sk/videa/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_MOVIE_URL = '\.addVariable\s*\(\s*(["''])file\1\s*,\s*(["''])(?P<URL>https?://static\.kukaj\.sk/[^&\2]+)';

{ TDownloader_Kukaj }

class function TDownloader_Kukaj.Provider: string;
begin
  Result := 'Kukaj.sk';
end;

class function TDownloader_Kukaj.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Kukaj.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Kukaj.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Kukaj.GetMovieInfoUrl: string;
begin
  Result := 'http://www.kukaj.sk/videa/' + MovieID + '/';
end;

initialization
  RegisterDownloader(TDownloader_Kukaj);

end.
