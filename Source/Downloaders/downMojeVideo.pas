unit downMojeVideo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MojeVideo = class(THttpDownloader)
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

// http://mojevideo.cz/view.php?id=5332
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*mojevideo\.cz/view\.php\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class=''nadpis2''[^>]*>\s*(?P<TITLE>.*?)\s*</div>';
  REGEXP_MOVIE_URL = '\.addVariable\s*\(\s*"file"\s*,\s*"(?:\.\./)?(?P<URL>videofiles/[^"]+)"';

{ TDownloader_MojeVideo }

class function TDownloader_MojeVideo.Provider: string;
begin
  Result := 'MojeVideo.cz';
end;

class function TDownloader_MojeVideo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MojeVideo.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_MojeVideo.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_MojeVideo.GetMovieInfoUrl: string;
begin
  Result := 'http://mojevideo.cz/view.php?id=' + MovieID;
end;

function TDownloader_MojeVideo.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieURL := 'http://mojevideo.cz/' + MovieURL;
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_MojeVideo);

end.
