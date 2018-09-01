unit downCestyKSobe;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CestyKSobe = class(THttpDownloader)
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

// http://www.cestyksobe.cz/novinky/nejnovejsi-a-nejzajimavejsi-porady/642.html?quality=high
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*cestyksobe\.cz/';
  URLREGEXP_ID =        '[^/?&]+/[^/?&]+/[0-9]+\.html';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h3>(?P<TITLE>.*?)</h3>';
  REGEXP_EXTRACT_MOVIEURL = '\bflashvars\s*:\s*"[^"]*&streamscript=(?P<URL>/[^"&]+)';

{ TDownloader_CestyKSobe }

class function TDownloader_CestyKSobe.Provider: string;
begin
  Result := 'CestyKSobe.sk';
end;

class function TDownloader_CestyKSobe.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CestyKSobe.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_MOVIEURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CestyKSobe.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_CestyKSobe.GetMovieInfoUrl: string;
begin
  Result := 'http://www.cestyksobe.cz/' + MovieID + '?quality=high';
end;

function TDownloader_CestyKSobe.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if MovieURL <> '' then
    begin
    MovieURL := 'http://www.cestyksobe.cz' + MovieURL;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CestyKSobe);

end.
