unit xxxXVideos;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XVideos = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: IRegEx;
      FlashMovieUrlRegExp: IRegEx;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xvideos\.com/video';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>\s*(?P<TITLE>.*?)\s*-\s*XVIDEOS.COM\s*</title>';
  REGEXP_FLASHVARS = '<embed\s(?:[^>]+\s)*flashvars="(?P<VARS>.*?)"';
  REGEXP_FLASHMOVIEURL = '[?&]flv_url=(?P<URL>[^&]+)';

{ TDownloader_XVideos }

class function TDownloader_XVideos.Provider: string;
begin
  Result := 'XVideos.com';
end;

class function TDownloader_XVideos.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_XVideos.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
  FlashMovieUrlRegExp := RegExCreate(REGEXP_FLASHMOVIEURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_XVideos.Destroy;
begin
  MovieTitleRegExp := nil;
  FlashVarsRegExp := nil;
  FlashMovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_XVideos.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xvideos.com/video' + MovieID + '/';
end;

function TDownloader_XVideos.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashVars, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'VARS', FlashVars) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else
    begin
    FlashVars := HtmlDecode(FlashVars);
    if not GetRegExpVar(FlashMovieUrlRegExp, FlashVars, 'URL', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := UrlDecode(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XVideos);
  {$ENDIF}

end.
