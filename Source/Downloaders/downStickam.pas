unit downStickam;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Stickam = class(THttpDownloader)
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

// http://www.stickam.com/viewMedia.do?mId=188284575&rf=hvthumb
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stickam\.com/viewMedia\.do\?mId=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div\s+class="mcTitle">(?P<TITLE>.*?)</div>';
  REGEXP_EXTRACT_URL = '''movieName=(?P<URL>[^&'']+)';

{ TDownloader_Stickam }

class function TDownloader_Stickam.Provider: string;
begin
  Result := 'Stickam.com';
end;

class function TDownloader_Stickam.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Stickam.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Stickam.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_Stickam.GetMovieInfoUrl: string;
begin
  Result := 'http://www.stickam.com/viewMedia.do?mId=' + MovieID + '&rf=hvthumb';
end;

function TDownloader_Stickam.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := Prepared;
  if Result then
    MovieURL := 'http://static.videos.stickam.com/' + UrlDecode(MovieURL);
end;

initialization
  RegisterDownloader(TDownloader_Stickam);

end.
