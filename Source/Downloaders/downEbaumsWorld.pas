unit downEbaumsWorld;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EbaumsWorld = class(THttpDownloader)
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

// http://www.ebaumsworld.com/video/watch/80973186/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ebaumsworld\.com/video/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_EXTRACT_URL = '<link\s+rel="video_src"\s+href="[^"]*?[?&]file=(?P<URL>https?://[^&"]+)';

{ TDownloader_EbaumsWorld }

class function TDownloader_EbaumsWorld.Provider: string;
begin
  Result := 'EbaumsWorld.com';
end;

class function TDownloader_EbaumsWorld.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_EbaumsWorld.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_EbaumsWorld.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_EbaumsWorld.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ebaumsworld.com/video/watch/' + MovieID + '/';
end;

initialization
  RegisterDownloader(TDownloader_EbaumsWorld);

end.
