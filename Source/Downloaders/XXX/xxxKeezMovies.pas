unit xxxKeezMovies;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_KeezMovies = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*keezmovies\.com/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_MOVIE_URL = '\bflashvars\.video_url\s*=\s*''(?P<URL>https?://.+?)''\s*;';

{ TDownloader_KeezMovies }

class function TDownloader_KeezMovies.Provider: string;
begin
  Result := 'KeezMovies.com';
end;

class function TDownloader_KeezMovies.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_KeezMovies.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_KeezMovies.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_KeezMovies.GetMovieInfoUrl: string;
begin
  Result := 'http://www.keezmovies.com/video/' + MovieID;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_KeezMovies);
  {$ENDIF}

end.
