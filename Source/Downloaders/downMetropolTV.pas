unit downMetropolTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MetropolTV = class(THttpDownloader)
    private
    protected
      MovieParamsUrlRegExp: TRegExp; 
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://www.tvpraha11.cz/zastupitelstvo/index.php?file=9285&jednani=66
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tvpraha11.cz/zastupitelstvo/.*?[?&]';
  URLREGEXP_ID =        'file=[0-9]+&jednani=[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>Právì sledujete</h2>\s*</div>\s*<div[^>]*>\s*<p[^>]*>\s*(?P<TITLE>.*?)\s*</';
  REGEXP_MOVIE_PARAMS_URL = '''FlashVars''\s*,\s*''videos=(?P<URL>xml/[^'']+\.xml)''';

const
  PageRoot = 'http://www.tvpraha11.cz/zastupitelstvo/';

{ TDownloader_MetropolTV }

class function TDownloader_MetropolTV.Provider: string;
begin
  Result := 'TVPraha11.cz';
end;

class function TDownloader_MetropolTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MetropolTV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieParamsUrlRegExp := RegExCreate(REGEXP_MOVIE_PARAMS_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_MetropolTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieParamsUrlRegExp);
  inherited;
end;

function TDownloader_MetropolTV.GetMovieInfoUrl: string;
begin
  Result := PageRoot + 'index.php?' + MovieID;
end;

function TDownloader_MetropolTV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    InfoXml, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieParamsUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, PageRoot + Url, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'video/streams', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieURL := PageRoot + Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_MetropolTV);

end.
