unit downEVTV1;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EVTV1 = class(THttpDownloader)
    private
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

// http://www.evtv1.com/player.aspx?itemnum=15099&aid=
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*evtv1\.com/player\.aspx?\?(?:[^&]*&)*itemnum=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>\s*(?P<TITLE>.*?)\s*</title>';
  REGEXP_EXTRACT_URL = '<param\s+name="flashVars"\s+value="(?:[^"]*&)*ClipURL=(?P<URL>http.+?)[&"]';

{ TDownloader_EVTV1 }

class function TDownloader_EVTV1.Provider: string;
begin
  Result := 'EVTV1.com';
end;

class function TDownloader_EVTV1.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_EVTV1.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_EVTV1.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_EVTV1.GetMovieInfoUrl: string;
begin
  Result := 'http://www.evtv1.com/player.aspx?itemnum=' + MovieID + '&aid=';
end;

function TDownloader_EVTV1.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := UrlDecode(MovieUrl);
end;

initialization
  RegisterDownloader(TDownloader_EVTV1);

end.
