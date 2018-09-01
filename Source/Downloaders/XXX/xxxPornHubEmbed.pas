unit xxxPornHubEmbed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornHubEmbed = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornhub\.com/embed_player\.php\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_PornHubEmbed }

class function TDownloader_PornHubEmbed.Provider: string;
begin
  Result := 'PornHub.com';
end;

class function TDownloader_PornHubEmbed.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornHubEmbed.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peXml);
end;

destructor TDownloader_PornHubEmbed.Destroy;
begin
  inherited;
end;

function TDownloader_PornHubEmbed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhub.com/embed_player.php?id=' + MovieID;
end;

function TDownloader_PornHubEmbed.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Url, Title: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TXmlDoc.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'flv_url', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      if GetXmlVar(Xml, 'post_roll/embed/a', Title) then
        SetName(Title);
      MovieUrl := HtmlDecode(Url);
      SetPrepared(True);
      Result := True;
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornHubEmbed);
  {$ENDIF}

end.
