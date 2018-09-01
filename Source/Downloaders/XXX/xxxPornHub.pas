unit xxxPornHub;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornHub = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: IRegEx;
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornhub\.com/.*[?&]viewkey=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="video-title-nf"[^>]+>\s*<h1>(?P<TITLE>.+?)</h1>';
  REGEXP_MOVIE_ID = '<input\s+type="hidden"\s+id="video_[0-9+]"\s+value="(?P<ID>[0-9]+)"';

{ TDownloader_PornoTube }

class function TDownloader_PornHub.Provider: string;
begin
  Result := 'PornHub.com';
end;

class function TDownloader_PornHub.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornHub.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PornHub.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieIDRegExp := nil;
  inherited;
end;

function TDownloader_PornHub.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhub.com/view_video.php?viewkey=' + MovieID;
end;

function TDownloader_PornHub.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    ID, Url, InfoXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, 'http://www.pornhub.com/embed_player.php?id=' + ID, InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'flv_url', Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        MovieUrl := HtmlDecode(Url);
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornHub);
  {$ENDIF}

end.
