unit downNovinky;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Novinky = class(THttpDownloader)
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

// http://video.novinky.cz/video/doporucujeme/?videoId=10869&page=1
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*video\.novinky\.cz/video/';
  URLREGEXP_ID =        '[^/?&]+/\?videoId=[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h3>(?P<TITLE>.*?)</h3>';
  REGEXP_EXTRACT_URL = '<param\s+name="FlashVars"\s+value="(?:[^"]+&amp;)?video_src=(?P<URL>https?://.+?)(?:&amp|")';

{ TDownloader_Novinky }

class function TDownloader_Novinky.Provider: string;
begin
  Result := 'Novinky.cz';
end;

class function TDownloader_Novinky.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Novinky.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Novinky.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Novinky.GetMovieInfoUrl: string;
begin
  Result := 'http://video.novinky.cz/video/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_Novinky);

end.
