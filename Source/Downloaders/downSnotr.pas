unit downSnotr;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Snotr = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function BuildMovieUrl(out Url: string): boolean; override;
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

// http://www.snotr.com/video/4280
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*snotr\.com/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';

{ TDownloader_Snotr }

class function TDownloader_Snotr.Provider: string;
begin
  Result := 'Snotr.com';
end;

class function TDownloader_Snotr.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Snotr.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
end;

destructor TDownloader_Snotr.Destroy;
begin
  MovieTitleRegExp := nil;
  inherited;
end;

function TDownloader_Snotr.GetMovieInfoUrl: string;
begin
  Result := 'http://www.snotr.com/video/' + MovieID;
end;

function TDownloader_Snotr.BuildMovieUrl(out Url: string): boolean;
begin
  URL := 'http://videos.snotr.com/' + MovieID + '.flv';
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_Snotr);

end.
