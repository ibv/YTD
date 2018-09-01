unit downDoubleAgent;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DoubleAgent = class(THttpDownloader)
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

// http://www.doubleagent.com/agent/tiffany/video-blog/video-28
// http://www.doubleagent.com/play/are-you-a-tweetbook-twit
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*doubleagent\.com/';
  URLREGEXP_ID =        '(?:agent|play)/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '\btmObj\.set\s*\(\s*"VideoTitle"\s*,\s*"(?P<TITLE>.+?)"';
  REGEXP_EXTRACT_URL = '\btmObj\.set\s*\(\s*"VideoURL"\s*,\s*"(?P<URL>https?://.+?)"';

{ TDownloader_DoubleAgent }

class function TDownloader_DoubleAgent.Provider: string;
begin
  Result := 'DoubleAgent.com';
end;

class function TDownloader_DoubleAgent.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_DoubleAgent.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_DoubleAgent.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_DoubleAgent.GetMovieInfoUrl: string;
begin
  Result := 'http://www.doubleagent.com/' + MovieID;
end;

function TDownloader_DoubleAgent.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := UrlDecode(MovieUrl);
  SetPrepared(True);
  SetName(HtmlDecode(Name)); // Must be AFTER SetPrepared, because Name results in an error otherwise
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_DoubleAgent);

end.
