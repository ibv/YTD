unit downTangle;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Tangle = class(THttpDownloader)
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

// http://www.tangle.com/view_video?viewkey=fe586286e688a7cb197e
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tangle\.com/.*?[?&]viewkey=';
  URLREGEXP_ID =        '[0-9a-f]{20}';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Tangle }

class function TDownloader_Tangle.Provider: string;
begin
  Result := 'Tangle.com';
end;

class function TDownloader_Tangle.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Tangle.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_Tangle.Destroy;
begin
  inherited;
end;

function TDownloader_Tangle.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tangle.com/playlist/media/video/' + MovieID;
end;

function TDownloader_Tangle.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'playlist/item/title', Title) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else if not GetXmlVar(Xml, 'playlist/item/filelocation', Url) then
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

initialization
  RegisterDownloader(TDownloader_Tangle);

end.
