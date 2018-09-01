unit downWimp;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Wimp = class(THttpDownloader)
    private
    protected
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

// http://www.wimp.com/frazilice/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*wimp\.com/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="description"\s+content="(?P<TITLE>.*?)"';
  REGEXP_MOVIE_URL = '\.addVariable\s*\(\s*"file"\s*,\s*"(?P<URL>https?://.*?)"';

{ TDownloader_Wimp }

class function TDownloader_Wimp.Provider: string;
begin
  Result := 'Wimp.com';
end;

class function TDownloader_Wimp.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Wimp.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Wimp.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Wimp.GetMovieInfoUrl: string;
begin
  Result := 'http://www.wimp.com/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Wimp);

end.
