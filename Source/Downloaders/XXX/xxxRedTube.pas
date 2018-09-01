unit xxxRedTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_RedTube = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashMovieUrlRegExp: TRegExp;
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*redtube\.com/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1\s[^>]*class="videoTitle"[^>]*>(?P<TITLE>.*?)</h1>';
  REGEXP_FLASHVARS = '\bso\.addParam\s*\(\s*"flashvars"\s*,\s*"(?P<VARS>.*?)"';
  REGEXP_FLASHMOVIEURL = '[?&]hashlink=(?P<URL>[^&]+)';

{ TDownloader_RedTube }

class function TDownloader_RedTube.Provider: string;
begin
  Result := 'RedTube.com';
end;

class function TDownloader_RedTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_RedTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
  FlashMovieUrlRegExp := RegExCreate(REGEXP_FLASHMOVIEURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_RedTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashMovieUrlRegExp);
  inherited;
end;

function TDownloader_RedTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.redtube.com/' + MovieID;
end;

function TDownloader_RedTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
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
  RegisterDownloader(TDownloader_RedTube);
  {$ENDIF}

end.
