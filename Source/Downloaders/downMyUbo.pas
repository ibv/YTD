unit downMyubo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Myubo = class(THttpDownloader)
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

// http://www.myubo.sk/page/media_detail.html?movieid=deac5b36-9efe-4176-a1e9-01088aa24696
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*myubo\.sk/page/media_detail\.html\?movieid=';
  URLREGEXP_ID =        '[0-9a-f-]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+id="movieDetail">\s*<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_MOVIE_URL = '&lt;param\s+name=movie\s+value=&quot;https?://(?:[a-z0-9-]+\.)*myubo\.com/.*?[?&]movieURL=(?P<URL>https?://[^&>]+)';

{ TDownloader_Myubo }

class function TDownloader_Myubo.Provider: string;
begin
  Result := 'Myubo.sk';
end;

class function TDownloader_Myubo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Myubo.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Myubo.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Myubo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.myubo.sk/page/media_detail.html?movieid=' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Myubo);

end.
