unit downIHned;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_IHned = class(TRtmpDownloader)
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

// http://video.ihned.cz/c1-44411910-sef-narodniho-muzea-kdyz-zafouka-vitr-vypadne-nam-za-noc-az-deset-oken
// http://video.ihned.cz/c1-44411910
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*video\.ihned\.cz/';
  URLREGEXP_ID =        '[^/?&]+?-[0-9]+';
  URLREGEXP_AFTER_ID =  '($|-)';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '\bvar\s+mm_str\s*=\s*"[^"]*?\bfilename1\s*:\s*''(?P<URL>rtmpt?e?://.+?)''';

{ TDownloader_IHned }

class function TDownloader_IHned.Provider: string;
begin
  Result := 'IHned.com';
end;

class function TDownloader_IHned.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_IHned.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_IHned.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_IHned.GetMovieInfoUrl: string;
begin
  Result := 'http://video.ihned.cz/' + MovieID;
end;

function TDownloader_IHned.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := Prepared;
  if Result then
    begin
    AddRtmpDumpOption('r', MovieURL);
    end;
end;

initialization
  RegisterDownloader(TDownloader_IHned);

end.
