unit downFreeRide;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeRide = class(THttpDownloader)
    private
    protected
      InfoUrlRegExp: TRegExp;
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
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.freeride.cz/snowboard/clanky/trix/tricktip-fs-grind-fs-revert-by-honza-smekal--9323/
// http://www.freeride.cz/snowboard/video/freeridecz-karneval-2010-by-shymonkey--9121/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*freeride\.cz/snowboard/';
  URLREGEXP_ID =        '(?:clanky|video)/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div\s+class="article">\s*<h([12])>\s*<span>[^<]+:</span>\s*<br\s*/>\s*(?P<TITLE>.*?)</h\1>';
  REGEXP_INFO_URL = '\.addVariable\s*\(\s*([''"])xml_data_url\1\s*,\s*([''"])(?P<URL>.*?)\2';

{ TDownloader_FreeRide }

class function TDownloader_FreeRide.Provider: string;
begin
  Result := 'FreeRide.com';
end;

class function TDownloader_FreeRide.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FreeRide.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_FreeRide.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_FreeRide.GetMovieInfoUrl: string;
begin
  Result := 'http://www.freeride.cz/snowboard/' + MovieID;
end;

function TDownloader_FreeRide.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, InfoXml: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.freeride.cz' + UrlDecode(Url), InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'video_url', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieURL := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_FreeRide);

end.
