unit listBing;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader, listHTML;

type
  TPlaylist_Bing = class(TPlaylist_HTML)
    private
    protected
      function GetUrlRegExp: string; override;
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

// http://www.bing.com/videos/watch/video/surfer-mounts-hd-camera-to-board/1abwgqhhl
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bing\.com/videos/watch/video/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '\.writeFlash\s*\(\s*''[^'']+''\s*,\s*''(?P<URL>https?://.*?)''';

{ TPlaylist_Bing }

class function TPlaylist_Bing.Provider: string;
begin
  Result := 'Bing.com';
end;

class function TPlaylist_Bing.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_Bing.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
end;

destructor TPlaylist_Bing.Destroy;
begin
  inherited;
end;

function TPlaylist_Bing.GetUrlRegExp: string;
begin
  Result := REGEXP_EXTRACT_URL;
end;

function TPlaylist_Bing.GetMovieInfoUrl: string;
begin
  Result := 'http://www.bing.com/videos/watch/video/dummy/' + MovieID;
end;

initialization
  RegisterDownloader(TPlaylist_Bing);

end.
