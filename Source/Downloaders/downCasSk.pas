unit downCasSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_CasSk = class(TNestedDownloader)
    private
    protected
      NestedUrl1RegExp, NestedUrl2RegExp: TRegExp;
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

// http://www.cas.sk/clanok/172890/testovanie-brzd-trochu-inak.html
// http://adam.cas.sk/clanky/7431/moto-aston-martin-rapide-2011.html
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*cas\.sk/(?:clanok|clanky)/[0-9]+/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1 class="article">(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL1 = '<object[^>]*\sdata="(?P<URL>https?://.+?)"';
  REGEXP_EXTRACT_URL2 = '\bso\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>https?://.+?)''';

{ TDownloader_CasSk }

class function TDownloader_CasSk.Provider: string;
begin
  Result := 'Cas.sk';
end;

class function TDownloader_CasSk.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CasSk.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrl1RegExp := RegExCreate(REGEXP_EXTRACT_URL1, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrl2RegExp := RegExCreate(REGEXP_EXTRACT_URL2, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CasSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedUrl1RegExp);
  RegExFreeAndNil(NestedUrl2RegExp);
  inherited;
end;

function TDownloader_CasSk.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CasSk.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  try
    NestedUrlRegExp := NestedUrl1RegExp;
    Result := inherited AfterPrepareFromPage(Page, Http);
    if not Result then
      begin
      NestedUrlRegExp := NestedUrl2RegExp;
      Result := inherited AfterPrepareFromPage(Page, Http);
      end;
  finally
    NestedUrlRegExp := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CasSk);

end.
