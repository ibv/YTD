unit downVitalMtb;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VitalMtb = class(THttpDownloader)
    private
    protected
      XmlPathRegExp: TRegExp;
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

// http://www.vitalmtb.com/videos/member/Practice-at-the-bmx-track,2631/bubb120491,1836
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*vitalmtb\.com/videos/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_XML_PATH = '<link\s+href="[^"]*[?&]xml_path=(?P<URL>https?://[^"&]+)[^"]*"\s+rel="video_src"';

{ TDownloader_VitalMtb }

class function TDownloader_VitalMtb.Provider: string;
begin
  Result := 'VitalMtb.com';
end;

class function TDownloader_VitalMtb.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_VitalMtb.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  XmlPathRegExp := RegExCreate(REGEXP_XML_PATH, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_VitalMtb.Destroy;
begin
  RegExFreeAndNil(XmlPathRegExp);
  inherited;
end;

function TDownloader_VitalMtb.GetMovieInfoUrl: string;
begin
  Result := 'http://www.vitalmtb.com/videos/' + MovieID;
end;

function TDownloader_VitalMtb.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title, InfoXml: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(XmlPathRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, Url, InfoXml, peUTF8) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      Url := '';
      if not GetXmlVar(Xml, 'title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else if not GetXmlVar(Xml, 'hd_video_url', Url) then
        if not GetXmlVar(Xml, 'standard_video_url', Url) then
          if not GetXmlVar(Xml, 'mobile_video_url', Url) then
            Url := '';
      if Url = '' then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetName(Title);
        MovieUrl := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VitalMtb);

end.
