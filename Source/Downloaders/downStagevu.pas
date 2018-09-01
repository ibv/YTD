unit downStagevu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Stagevu = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
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

// http://stagevu.com/video/jomxdgbvxnip
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stagevu\.com/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '\burl\s*\[\s*[0-9]+\s*\]\s*=\s*''(?P<URL>https?://(?:[a-z0-9-]+\.)*stagevu\.com/[^'']+)''\s*;';

{ TDownloader_Stagevu }

class function TDownloader_Stagevu.Provider: string;
begin
  Result := 'Stagevu.com';
end;

class function TDownloader_Stagevu.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Stagevu.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Stagevu.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Stagevu.GetMovieInfoUrl: string;
begin
  Result := 'http://stagevu.com/video/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Stagevu);

end.
