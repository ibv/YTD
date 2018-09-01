unit downLibimSeTi;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LibimSeTi = class(THttpDownloader)
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

// http://video.libimseti.cz/basket-trosku-jinak/f1ea1c7fcc03?uid=
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*libimseti\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h3\s+class="big">(?P<TITLE>.*?)</h3>';
  REGEXP_MOVIE_URL = '\.addParam\s*\(\s*''file''\s*,\s*''(?P<URL>https?://videostream\.libimseti\.cz/[^'']+)''';

{ TDownloader_LibimSeTi }

class function TDownloader_LibimSeTi.Provider: string;
begin
  Result := 'LibimSeTi.cz';
end;

class function TDownloader_LibimSeTi.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_LibimSeTi.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_LibimSeTi.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_LibimSeTi.GetMovieInfoUrl: string;
begin
  Result := 'http://video.libimseti.cz/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_LibimSeTi);

end.
