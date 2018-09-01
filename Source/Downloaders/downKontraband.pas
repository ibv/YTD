unit downKontraband;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Kontraband = class(THttpDownloader)
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

// http://www.kontraband.com/videos/23388/Failed-Slam-Dunks/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*kontraband\.com/videos/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)\s+at\s+Kontraband\s*</title>';
  REGEXP_EXTRACT_URL = '\bso\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>https?://.+?)''';

{ TDownloader_Kontraband }

class function TDownloader_Kontraband.Provider: string;
begin
  Result := 'Kontraband.com';
end;

class function TDownloader_Kontraband.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Kontraband.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Kontraband.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Kontraband.GetMovieInfoUrl: string;
begin
  Result := 'http://www.kontraband.com/videos/' + MovieID + '/';
end;

initialization
  RegisterDownloader(TDownloader_Kontraband);

end.
