unit uDownloader_Snotr;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Snotr = class(THttpDownloader)
    private
    protected
      function GetInfoPageEncoding: TPageEncoding; override;
      function GetMovieInfoUrl: string; override;
      function BuildMovieUrl(out Url: string): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

const MOVIE_TITLE_REGEXP = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';

{ TDownloader_Snotr }

constructor TDownloader_Snotr.Create(const AMovieID: string);
begin
  inherited;
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_Snotr.Destroy;
begin
  MovieTitleRegExp := nil;
  inherited;
end;

class function TDownloader_Snotr.Provider: string;
begin
  Result := 'Snotr.com';
end;

class function TDownloader_Snotr.MovieIDParamName: string;
begin
  Result := 'SNOTR';
end;

class function TDownloader_Snotr.UrlRegExp: string;
begin
  // http://www.snotr.com/video/4280
  Result := '^https?://(?:[a-z0-9-]+\.)?snotr\.com/video/(?P<' + MovieIDParamName + '>[0-9]+)';
end;

function TDownloader_Snotr.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
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
