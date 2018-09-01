unit downAktualne;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Aktualne = class(THttpDownloader)
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

// http://aktualne.centrum.cz/video/?id=316586
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*aktualne(?:\.centrum)?\.cz/video/?\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h3 class="titulek">(?P<TITLE>.*?)</h3>';
  REGEXP_EXTRACT_URL = '<param\s+name="movie"\s+value="[^"]*[?&]adresa[0-9]+=(?P<URL>https?://video\.aktualne.+?)[&"]';

{ TDownloader_Aktualne }

class function TDownloader_Aktualne.Provider: string;
begin
  Result := 'Aktualne.cz';
end;

class function TDownloader_Aktualne.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Aktualne.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Aktualne.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Aktualne.GetMovieInfoUrl: string;
begin
  Result := 'http://aktualne.centrum.cz/video/?id=' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Aktualne);

end.
