unit downClipfish;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Clipfish = class(THttpDownloader)
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

// http://www.clipfish.de/video/3306089/baby-kaenguruh-versucht-zu-schwimmen/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*clipfish\.de/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Clipfish }

class function TDownloader_Clipfish.Provider: string;
begin
  Result := 'Clipfish.de';
end;

class function TDownloader_Clipfish.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Clipfish.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_Clipfish.Destroy;
begin
  inherited;
end;

function TDownloader_Clipfish.GetMovieInfoUrl: string;
begin
  Result := 'http://www.clipfish.de/devxml/videoinfo/' + MovieID;
end;

function TDownloader_Clipfish.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TXmlDoc.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'filename', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else if not GetXmlVar(Xml, 'title', Title) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
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

initialization
  RegisterDownloader(TDownloader_Clipfish);

end.
