unit downMySpace;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_MySpace = class(TRtmpDownloader)
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

// http://vids.myspace.com/index.cfm?fuseaction=vids.individual&videoid=63620005
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*vids\.myspace\.com/.*?[?&]videoid=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_MySpace }

class function TDownloader_MySpace.Provider: string;
begin
  Result := 'MySpace.com';
end;

class function TDownloader_MySpace.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MySpace.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_MySpace.Destroy;
begin
  inherited;
end;

function TDownloader_MySpace.GetMovieInfoUrl: string;
begin
  Result := 'http://mediaservices.myspace.com/services/rss.ashx?type=video&videoID=' + MovieID;
end;

function TDownloader_MySpace.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Title, Url: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'channel/item/title', Title) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
    else if not GetXmlAttr(Xml, 'channel/item/myspace:RTMPE', 'url', Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      SetName(Title);
      // Note: Url is somewhat incorrect, MySpace uses protocol "rtmp" while in fact it should be "rtmpe"
      MovieURL := StringReplace(Url, 'rtmp://', 'rtmpe://', [rfIgnoreCase]);
      // Download
      AddRtmpDumpOption('r', MovieURL);
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_MySpace);

end.
