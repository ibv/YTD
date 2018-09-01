unit downTodaysBigThing;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_TodaysBigThing = class(THttpDownloader)
    private
    protected
      VideoIdRegExp: TRegExp;
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

// http://www.todaysbigthing.com/2010/06/01
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*todaysbigthing\.com/';
  URLREGEXP_ID =        '[0-9]{4}/[0-6]{2}/[0-9]{2}';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_VIDEO_ID = '\.addVariable\s*\(\s*"item_id"\s*,\s*(?P<ID>[0-9]+)\s*\)\s*;';

{ TDownloader_TodaysBigThing }

class function TDownloader_TodaysBigThing.Provider: string;
begin
  Result := 'TodaysBigThing.com';
end;

class function TDownloader_TodaysBigThing.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TodaysBigThing.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_TodaysBigThing.Destroy;
begin
  RegExFreeAndNil(VideoIdRegExp);
  inherited;
end;

function TDownloader_TodaysBigThing.GetMovieInfoUrl: string;
begin
  Result := 'http://www.todaysbigthing.com/' + MovieID;
end;

function TDownloader_TodaysBigThing.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var VideoID, InfoXml, Url, Title: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', VideoID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.todaysbigthing.com/betamax:' + VideoID, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else if not GetXmlVar(Xml, 'flv', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetName(Title);
        MovieUrl := Url;
        Result := True;
        SetPrepared(True);
        Exit;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TodaysBigThing);

end.
