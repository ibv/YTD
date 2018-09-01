unit downBreakEmbedV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBreakEmbed;

type
  TDownloader_BreakEmbedV2 = class(TDownloader_BreakEmbed)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://embed.break.com/MzU5NDE4
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*embed\.break\.com/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '$';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';

{ TDownloader_BreakEmbedV2 }

class function TDownloader_BreakEmbedV2.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BreakEmbedV2.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_BreakEmbedV2.Destroy;
begin
  inherited;
end;

function TDownloader_BreakEmbedV2.GetMovieInfoUrl: string;
begin
  Result := 'http://embed.break.com/' + MovieID;
end;

function TDownloader_BreakEmbedV2.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  // I don't need to download anything, but do I need to get the redirected URL
  // which contains all required info
  Result := DownloadPage(Http, Url, hmHEAD);
  if Result then
    Page := LastUrl;
end;

initialization
  RegisterDownloader(TDownloader_BreakEmbedV2);

end.
