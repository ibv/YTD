unit downTeacherTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_TeacherTube = class(THttpDownloader)
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
  uXML,
  uDownloadClassifier,
  uMessages;

// http://teachertube.com/viewVideo.php?video_id=177149&title=Top_10_Mistakes_New_Teachers_Make
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*teachertube*\.com/viewVideo\.php.*?[?&]video_id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_TeacherTube }

class function TDownloader_TeacherTube.Provider: string;
begin
  Result := 'TeacherTube.com';
end;

class function TDownloader_TeacherTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TeacherTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_TeacherTube.Destroy;
begin
  inherited;
end;

function TDownloader_TeacherTube.GetMovieInfoUrl: string;
begin
  Result := 'http://teachertube.com/embedFLV.php?pg=video_' + MovieID;
end;

function TDownloader_TeacherTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Title, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TXmlDoc.create;
  try
    Xml.xml := Page;
    if not GetXmlVar(Xml, 'channel/item/title', Title) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else if not GetXmlAttr(Xml, 'channel/item/media:content', 'url', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      SetName(Title);
      MovieURL := Url;
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TeacherTube);

end.
