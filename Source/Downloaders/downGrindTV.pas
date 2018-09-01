unit downGrindTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_GrindTV = class(THttpDownloader)
    private
    protected
      MovieIdRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function BaseUrl: string; virtual;
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

// http://www.grindtv.com/surf/video/surfing_ancientstyle_surfboards_in_peru_wred_bull_team/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*grindtv\.com/';
  URLREGEXP_ID =        '[^/]+/video/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_MOVIE_ID = '<link\s+rel="video_src"\s+href="[^"]*[?&]i=(?P<VIDEOID>[0-9]+)';

{ TDownloader_GrindTV }

class function TDownloader_GrindTV.Provider: string;
begin
  Result := 'GrindTV.com';
end;

class function TDownloader_GrindTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_GrindTV.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_GrindTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdRegExp);
  inherited;
end;

function TDownloader_GrindTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.grindtv.com/' + MovieID;
end;

function TDownloader_GrindTV.BaseUrl: string;
begin
  Result := 'http://videos.grindtv.com/1/';
end;

function TDownloader_GrindTV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var VideoId, VideoId8: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieIdRegExp, Page, 'VIDEOID', VideoId) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    VideoId8 := Format('%08.8d', [StrToInt(VideoId)]);
    MovieUrl := BaseUrl
      + Copy(VideoId8, 1, 2) + '/'
      + Copy(VideoId8, 3, 2) + '/'
      + Copy(VideoId8, 5, 2) + '/'
      + Copy(VideoId8, 7, 2) + '/'
      + VideoId + '.flv';
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_GrindTV);

end.
