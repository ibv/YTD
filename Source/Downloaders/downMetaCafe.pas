unit downMetaCafe;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MetaCafe = class(THttpDownloader)
    private
    protected
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

// http://www.metacafe.com/watch/4577253/kick_ass_release_trailer/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*metacafe\.com/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';
  REGEXP_MOVIE_URL = '<param\s+id="flashVars"\s+name="flashvars"\s+value="[^"]*%22mediaURL%22%3A%22(?P<URL>[^"]+?)%22';

{ TDownloader_MetaCafe }

class function TDownloader_MetaCafe.Provider: string;
begin
  Result := 'MetaCafe.com';
end;

class function TDownloader_MetaCafe.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MetaCafe.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_MetaCafe.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_MetaCafe.GetMovieInfoUrl: string;
begin
  Result := 'http://www.metacafe.com/watch/' + MovieID;
end;

function TDownloader_MetaCafe.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieURL := StripSlashes(UrlDecode(MovieURL));
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_MetaCafe);

end.
