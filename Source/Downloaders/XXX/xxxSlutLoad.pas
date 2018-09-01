unit xxxSlutLoad;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SlutLoad = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*slutload\.com/watch/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>Video:\s*<b>(?P<TITLE>.*?)</b>';
  REGEXP_MOVIE_URL = '<param\s+name="FlashVars"\s+value="(?:[^"]*&)?flv=(?P<URL>http.+?)["&]';

{ TDownloader_SlutLoad }

class function TDownloader_SlutLoad.Provider: string;
begin
  Result := 'SlutLoad.com';
end;

class function TDownloader_SlutLoad.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_SlutLoad.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_SlutLoad.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_SlutLoad.GetMovieInfoUrl: string;
begin
  Result := 'http://www.slutload.com/watch/' + MovieID + '/';
end;

function TDownloader_SlutLoad.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := UrlDecode(MovieUrl);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_SlutLoad);
  {$ENDIF}

end.
