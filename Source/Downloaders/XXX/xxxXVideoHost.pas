unit xxxXVideoHost;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XVideoHost = class(THttpDownloader)
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xvideohost\.com/video\.php\?id=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?:[^<]*--\s+)?(?P<TITLE>.*?)</title>';
  REGEXP_MOVIE_URL = '<iframe\s+src="ifr_pl\.php[^"]*[?&]url=(?P<URL>http%3A%2F%2F[^&"]+)';

{ TDownloader_PornoTube }

class function TDownloader_XVideoHost.Provider: string;
begin
  Result := 'XVideoHost.com';
end;

class function TDownloader_XVideoHost.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_XVideoHost.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_XVideoHost.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_XVideoHost.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xvideohost.com/video.php?id=' + MovieID;
end;

function TDownloader_XVideoHost.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := Prepared;
  MovieURL := UrlDecode(MovieUrl);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XVideoHost);
  {$ENDIF}

end.
