unit downCT_Port;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT_Port = class(TDownloader_CT)
    private
    protected
      PortToIVysilaniRegExp: IRegEx;
      PortTitleRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.ceskatelevize.cz/program/port/541-elektronicke-knihy/video/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ceskatelevize\.cz/program/port/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PORT_TO_IVYSILANI = '<iframe\s+src="(?P<PATH>/ivysilani/embed/.*?)"';
  REGEXP_PORT_TITLE = '<div id="heading">\s*<h2>(?P<TITLE>.*?)</h2>';

{ TDownloader_CT_Port }

class function TDownloader_CT_Port.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT_Port.Create(const AMovieID: string);
begin
  inherited;
  PortToIVysilaniRegExp := RegExCreate(REGEXP_PORT_TO_IVYSILANI, [rcoIgnoreCase, rcoSingleLine]);
  PortTitleRegExp := RegExCreate(REGEXP_PORT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT_Port.Destroy;
begin
  PortToIVysilaniRegExp := nil;
  PortTitleRegExp := nil;
  inherited;
end;

function TDownloader_CT_Port.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ceskatelevize.cz/program/port/' + MovieID + '/video/';
end;

function TDownloader_CT_Port.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Path, Url, EmbeddedPlayer, Title: string;
begin
  Result := False;
  if not GetRegExpVar(PortToIVysilaniRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else
    begin
    Url := 'http://www.ceskatelevize.cz' + EncodeUrl(HtmlDecode(Path));
    if not DownloadPage(Http, Url, EmbeddedPlayer, peUTF8) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      begin
      Result := inherited AfterPrepareFromPage(EmbeddedPlayer, Http);
      if Result then
        if GetRegExpVar(PortTitleRegExp, Page, 'TITLE', Title) then
          SetName(Title);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CT_Port);

end.
