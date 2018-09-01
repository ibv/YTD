unit downRevver;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Revver = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.revver.com/video/2378230/super-mario-bros-2-on-the-nes-level-x/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*revver\.com/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '/';

const
  REGEXP_EXTRACT_TITLE = '<h1\s+id="video-title">(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '<a\s+href="(?P<URL>http://[^"]+)"\s+id="share-download"';

{ TDownloader_Revver }

class function TDownloader_Revver.Provider: string;
begin
  Result := 'Revver.com';
end;

class function TDownloader_Revver.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Revver.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Revver.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Revver.GetMovieInfoUrl: string;
begin
  Result := 'http://www.revver.com/video/' + MovieID + '/';
end;

function TDownloader_Revver.Download: boolean;
begin
  Referer := GetMovieInfoUrl;
  Result := inherited Download;
end;

initialization
  RegisterDownloader(TDownloader_Revver);

end.
