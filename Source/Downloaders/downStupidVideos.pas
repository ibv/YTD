unit downStupidVideos;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader,
  downGrindTV;

type
  TDownloader_StupidVideos = class(TDownloader_GrindTV)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function BaseUrl: string; override;
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

// http://www.stupidvideos.com/tv/homepage_1/Amazing_Aquarium/
// http://www.stupidvideos.com/video/Fliers_Fall_Under_Friendly_Fire/?m=new
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stupidvideos\.com/(?:video|tv/[^/]+)/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<META\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_MOVIE_ID = '\bvar\s+videoID\s*=\s*''(?P<VIDEOID>[0-9]+)''\s*;';

{ TDownloader_StupidVideos }

class function TDownloader_StupidVideos.Provider: string;
begin
  Result := 'StupidVideos.com';
end;

class function TDownloader_StupidVideos.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_StupidVideos.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := nil;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIdRegExp := nil;
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_StupidVideos.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieIdRegExp := nil;
  inherited;
end;

function TDownloader_StupidVideos.GetMovieInfoUrl: string;
begin
  Result := 'http://www.stupidvideos.com/video/' + MovieID + '/';
end;

function TDownloader_StupidVideos.BaseUrl: string;
begin
  Result := 'http://videos.stupidvideos.com/2/';
end;

initialization
  RegisterDownloader(TDownloader_StupidVideos);

end.
