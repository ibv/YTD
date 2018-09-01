unit xxxExtremeTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ExtremeTube = class(THttpDownloader)
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*extremetube\.com/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1[^>]*>\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_MOVIE_URL = '\bflashvars\.video_url\s*=\s*''(?P<URL>https?://.+?)''\s*;';

{ TDownloader_ExtremeTube }

class function TDownloader_ExtremeTube.Provider: string;
begin
  Result := 'ExtremeTube.com';
end;

class function TDownloader_ExtremeTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_ExtremeTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_ExtremeTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_ExtremeTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.extremetube.com/video/' + MovieID;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_ExtremeTube);
  {$ENDIF}

end.
