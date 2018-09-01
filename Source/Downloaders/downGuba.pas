unit downGuba;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Guba = class(THttpDownloader)
    private
    protected
      VideoFromPlayerRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
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

// http://www.guba.com/watch/3000693318/Pulp-Fiction-Laugh-Track
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*guba\.com/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2 class="title"><a\s[^>]*>(?P<TITLE>.*?)</a></h2>';
  REGEXP_MOVIE_PLAYER = '<link\s+rel="video_src"\s+href="(?P<URL>https?://[^"]+)"';
  REGEXP_VIDEO_FROM_PLAYER = '"url"\s*:\s*"(?P<URL>https?://.*?)"';

{ TDownloader_Guba }

class function TDownloader_Guba.Provider: string;
begin
  Result := 'Guba.com';
end;

class function TDownloader_Guba.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Guba.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoFromPlayerRegExp := RegExCreate(REGEXP_VIDEO_FROM_PLAYER, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Guba.Destroy;
begin
  MovieTitleRegExp := nil;
  VideoFromPlayerRegExp := nil;
  inherited;
end;

function TDownloader_Guba.GetMovieInfoUrl: string;
begin
  Result := 'http://www.guba.com/watch/' + MovieID + '/';
end;

function TDownloader_Guba.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not DownloadPage(Http, 'http://www.guba.com/playerConfig?bid=' + MovieID, Page) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(VideoFromPlayerRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    MovieURL := Url;
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_Guba);

end.
