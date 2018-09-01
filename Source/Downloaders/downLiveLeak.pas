unit downLiveLeak;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LiveLeak = class(THttpDownloader)
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
  uXml,
  uDownloadClassifier,
  uMessages;

// http://www.liveleak.com/view?i=6f4_1272904024
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*liveleak\.com/view\?i=';
  URLREGEXP_ID =        '[^?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h4\s+id="s_hd">(?P<TITLE>.*?)</h4>';
  REGEXP_INFO_URL = '\.addVariable\s*\(\s*''config''\s*,\s*''(?P<URL>.*?)''';

{ TDownloader_LiveLeak }

class function TDownloader_LiveLeak.Provider: string;
begin
  Result := 'LiveLeak.com';
end;

class function TDownloader_LiveLeak.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_LiveLeak.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_LiveLeak.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_LiveLeak.GetMovieInfoUrl: string;
begin
  Result := 'http://www.liveleak.com/view?i=' + MovieID;
end;

function TDownloader_LiveLeak.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, InfoXml: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, UrlDecode(Url), InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'file', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
      else if not DownloadPage(Http, Url, InfoXml, peXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        begin
        Xml.Xml := InfoXml;
        if not GetXmlVar(Xml, 'trackList/track/location', Url) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          MovieURL := Url;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_LiveLeak);

end.
