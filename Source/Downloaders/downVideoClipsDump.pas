unit downVideoClipsDump;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VideoClipsDump = class(THttpDownloader)
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

// http://www.videoclipsdump.com/media/1951/Karate_Fail/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videoclipsdump\.com/media/';
  URLREGEXP_ID =        '[0-9]+/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>\s*Video\s*Clips\s*Dump\s*-\s*(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_URL = '\.videoclipsdump\.com/player/simple\.swf\?url=(?P<URL>https?://[^?&"]+)';

{ TDownloader_VideoClipsDump }

class function TDownloader_VideoClipsDump.Provider: string;
begin
  Result := 'VideoClipsDump.com';
end;

class function TDownloader_VideoClipsDump.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_VideoClipsDump.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_VideoClipsDump.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_VideoClipsDump.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videoclipsdump.com/media/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_VideoClipsDump);

end.
