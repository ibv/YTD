unit downTontuyau;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Tontuyau = class(THttpDownloader)
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

// http://www.tontuyau.com/v.asp?id=21715
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tontuyau\.com/v\.asp\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1\s+class="otb"[^>]*><B>(?P<TITLE>[^<]+)</td>';
  REGEXP_EXTRACT_URL = '\bfo\.addVariable\s*\(\s*"flvList"\s*,\s*"(?P<URL>https?://[^"]+\.flv)"';

{ TDownloader_Tontuyau }

class function TDownloader_Tontuyau.Provider: string;
begin
  Result := 'Tontuyau.com';
end;

class function TDownloader_Tontuyau.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Tontuyau.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Tontuyau.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Tontuyau.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tontuyau.com/v.asp?id=' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Tontuyau);

end.
