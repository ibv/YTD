unit downFacebook;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Facebook = class(THttpDownloader)
    private
    protected
      MovieUrlHQRegExp: TRegExp;
      MovieUrlLQRegExp: TRegExp;
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

// http://www.facebook.com/video/video.php?v=1131482863478
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*facebook\.com/video/.*?[?&]v=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h3\s+class="video_title\s+datawrap">(?P<TITLE>.*?)</h3>';
  REGEXP_MOVIE_URL_HQ = '\bswf_id_[0-9a-f]+\.addVariable\s*\(\s*"highqual_src"\s*,\s*"(?P<URL>.*?)"';
  REGEXP_MOVIE_URL_LQ = '\bswf_id_[0-9a-f]+\.addVariable\s*\(\s*"lowqual_src"\s*,\s*"(?P<URL>.*?)"';

{ TDownloader_Facebook }

class function TDownloader_Facebook.Provider: string;
begin
  Result := 'Facebook.com';
end;

class function TDownloader_Facebook.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Facebook.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieUrlHQRegExp := RegExCreate(REGEXP_MOVIE_URL_HQ, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlLQRegExp := RegExCreate(REGEXP_MOVIE_URL_LQ, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Facebook.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlHQRegExp);
  RegExFreeAndNil(MovieUrlLQRegExp);
  inherited;
end;

function TDownloader_Facebook.GetMovieInfoUrl: string;
begin
  Result := 'http://www.facebook.com/video/video.php?v=' + MovieID;
end;

function TDownloader_Facebook.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not (GetRegExpVar(MovieUrlHQRegExp, Page, 'URL', Url) or GetRegExpVar(MovieUrlLQRegExp, Page, 'URL', Url)) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    MovieURL := UrlDecode(Url);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Facebook);

end.
