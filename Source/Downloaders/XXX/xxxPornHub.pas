unit xxxPornHub;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader, xxxPornHubEmbed;

type
  TDownloader_PornHub = class(TDownloader_PornHubEmbed)
    private
    protected
      MovieIDRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornhub\.com/view_video\.php\?(?:.*&)?viewkey=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="video-title-nf"[^>]+>\s*<h1>(?P<TITLE>.+?)</h1>';
  REGEXP_MOVIE_ID = '<input\s+type="hidden"\s+id="video_[0-9+]"\s+value="(?P<ID>[0-9]+)"';

{ TDownloader_PornHub }

class function TDownloader_PornHub.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornHub.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PornHub.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_PornHub.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhub.com/view_video.php?viewkey=' + MovieID;
end;

function TDownloader_PornHub.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var ID, Title, InfoXml: string;
begin
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.pornhub.com/embed_player.php?id=' + ID, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Result := inherited AfterPrepareFromPage(InfoXml, Http);
    if Result and GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', Title) then
      SetName(Title);
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornHub);
  {$ENDIF}

end.
