unit xxxXNXX;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XNXX = class(THttpDownloader)
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xnxx\.com/video';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<!--\s*TITRE\s*-->\s*<span class="style5">\s*<strong>\s*(?P<TITLE>.*?)\s*</strong>\s*</span>';
  REGEXP_MOVIE_URL = '<embed(?:\s+.*)?\sflashvars="id_video=[^"]*&amp;flv_url=(?P<URL>https?%3A%2F%2F[^&"]+)';

{ TDownloader_XNXX }

class function TDownloader_XNXX.Provider: string;
begin
  Result := 'XNXX.com';
end;

class function TDownloader_XNXX.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_XNXX.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_XNXX.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_XNXX.GetMovieInfoUrl: string;
begin
  Result := 'http://video.xnxx.com/video' + MovieID;
end;

function TDownloader_XNXX.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieURL := UrlDecode(MovieURL);
  Result := True;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XNXX);
  {$ENDIF}

end.
