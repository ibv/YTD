unit downTVNoe;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_TVNoe = class(THttpDownloader)
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

// http://tvnoe.tbsystem.cz/index.php?cs/videoarchiv/hlubinami-vesmiru-2010-04-12-mikulasek
// http://tvnoe.tbsystem.cz/index.php?cs/videoarchiv/hlubinami-vesmiru-2010-04-12-mikulasek/quality/high
// http://tvnoe.tbsystem.cz/asx/hlubinami-vesmiru-2010-04-12-mikulasek-low.asx
// http://tvnoe.tbsystem.cz/asx/hlubinami-vesmiru-2010-04-12-mikulasek-high.asx
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tvnoe\.tbsystem\.cz/(?:asx/|.*?/videoarchiv/)';
  URLREGEXP_ID =        '[^/?&]+?';
  URLREGEXP_AFTER_ID =  '(?:-low|-high|/|$)';

{ TDownloader_TVNoe }

class function TDownloader_TVNoe.Provider: string;
begin
  Result := 'TVNoe.tbsystem,cz';
end;

class function TDownloader_TVNoe.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TVNoe.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peXml);
end;

destructor TDownloader_TVNoe.Destroy;
begin
  inherited;
end;

function TDownloader_TVNoe.GetMovieInfoUrl: string;
begin
  Result := 'http://tvnoe.tbsystem.cz/asx/' + MovieID + '-high.asx';
end;

function TDownloader_TVNoe.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TXmlDoc.Create;
  try
    Xml.Xml := Page;
    if not GetXmlAttr(Xml, 'entry/ref', 'href', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else if not GetXmlVar(Xml, 'entry/Title', Title) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else
      begin
      SetName(Trim(Title));
      MovieUrl := Url;
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TVNoe);

end.
