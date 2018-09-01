unit downBomba;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downYouTube;

type
  TDownloader_Bomba = class(TNestedDownloader)
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

// http://www.bomba.cz/video/simpsnovi-vs-cesti-politici/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bomba\.cz/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '<param\s+name="movie"\s+value="(?P<URL>https?://[^"]+)';

{ TDownloader_Bomba }

class function TDownloader_Bomba.Provider: string;
begin
  Result := 'Bomba.cz';
end;

class function TDownloader_Bomba.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Bomba.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Bomba.Destroy;
begin
  MovieTitleRegExp := nil;
  NestedUrlRegExp := nil;
  inherited;
end;

function TDownloader_Bomba.GetMovieInfoUrl: string;
begin
  Result := 'http://www.bomba.cz/video/' + MovieID + '/';
end;

initialization
  RegisterDownloader(TDownloader_Bomba);

end.
