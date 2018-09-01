unit down5min;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_5min = class(THttpDownloader)
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

// http://www.5min.com/Video/Customizable-Xbox-360-Controller-Brings-Gaming-to-the-Disabled-299179952
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*5min\.com/Video/[^/]*-';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_EXTRACT_URL = '<param\s+value="[^"]*&videoUrl=(?P<URL>https?[^"&]+)[^"]*"\s+name="movie"';

{ TDownloader_5min }

class function TDownloader_5min.Provider: string;
begin
  Result := '5min.com';
end;

class function TDownloader_5min.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_5min.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_5min.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_5min.GetMovieInfoUrl: string;
begin
  Result := 'http://www.5min.com/Video/-' + MovieID;
end;

function TDownloader_5min.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := UrlDecode(MovieUrl);
  SetPrepared(True);
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_5min);

end.
