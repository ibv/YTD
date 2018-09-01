unit downPublicTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PublicTV = class(THttpDownloader)
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

// http://www.publictv.cz/cz/menu/3/videoarchiv/clanek-535-re-play/1718/
// http://www.publictv.cz/videoarchiv/535/1718/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*publictv\.cz/';
  URLREGEXP_ID =        '(?:[^/?&]+/)*videoarchiv/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<td><p\s+style="text-align:\s*justify;">(?P<TITLE>.*?)</p></td>';
  REGEXP_EXTRACT_URL = '\bvar\s+cfg\s*=\s*\{\s*file\s*:\s*''(?P<URL>.+?)''';

{ TDownloader_PublicTV }

class function TDownloader_PublicTV.Provider: string;
begin
  Result := 'PublicTV.com';
end;

class function TDownloader_PublicTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PublicTV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PublicTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_PublicTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.publictv.cz/' + MovieID;
end;

function TDownloader_PublicTV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  MovieUrl := 'http://www.publictv.cz' + MovieUrl;
  SetPrepared(True);
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_PublicTV);

end.
