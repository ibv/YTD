unit xxxShufuni;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Shufuni = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*shufuni\.com/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>(?P<TITLE>.+?)</h1>';
  REGEXP_MOVIE_URL = '\bembededFlashContainer\.innerHTML\s*=\s*''(?P<URL>https?://media\.shufuni\.com/[^'']+)''';

{ TDownloader_PornoTube }

class function TDownloader_Shufuni.Provider: string;
begin
  Result := 'Shufuni.com';
end;

class function TDownloader_Shufuni.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Shufuni.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Shufuni.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_Shufuni.GetMovieInfoUrl: string;
begin
  Result := 'http://www.shufuni.com/' + MovieID;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_Shufuni);
  {$ENDIF}

end.
