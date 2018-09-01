unit downCollegeHumor;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_CollegeHumor = class(THttpDownloader)
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://www.collegehumor.com/video:1934920
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*collegehumor\.com/video:';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_CollegeHumor }

class function TDownloader_CollegeHumor.Provider: string;
begin
  Result := 'CollegeHumor.com';
end;

class function TDownloader_CollegeHumor.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CollegeHumor.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
end;

destructor TDownloader_CollegeHumor.Destroy;
begin
  inherited;
end;

function TDownloader_CollegeHumor.GetMovieInfoUrl: string;
begin
  Result := 'http://www.collegehumor.com/moogaloop/video:' + MovieID + '/';
end;

function TDownloader_CollegeHumor.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'video/file', Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else if not GetXmlVar(Xml, 'video/caption', Title) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
    else
      begin
      SetName(Title);
      MovieUrl := Url;
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CollegeHumor);

end.
