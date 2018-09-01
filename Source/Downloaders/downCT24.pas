unit downCT24;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT24 = class(TDownloader_CT)
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

// http://www.ct24.cz/regionalni/87267-vrchlabsky-zamek-ma-vlastni-miniaturu/video/1/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ct24\.cz/';
  URLREGEXP_ID =        '[^/]+/[0-9]+[^/?&]*/video/[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXPCT24_TO_IVYSILANI = '<iframe\s+src="(?P<PATH>https?://(?:[a-z0-9-]+\.)ct24\.cz/embed/iFramePlayer\.php\?.+?)"';
  REGEXPCT24_TITLE = '<h1>(?P<TITLE>.*?)</h1>';

{ TDownloader_CT24 }

class function TDownloader_CT24.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT24.Create(const AMovieID: string);
begin
  inherited;
  PortToIVysilaniRegExp := RegExCreate(REGEXPCT24_TO_IVYSILANI, [rcoIgnoreCase, rcoSingleLine]);
  PortTitleRegExp := RegExCreate(REGEXPCT24_TITLE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT24.Destroy;
begin
  PortToIVysilaniRegExp := nil;
  PortTitleRegExp := nil;
  inherited;
end;

function TDownloader_CT24.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ct24.cz/' + MovieID;
end;

function TDownloader_CT24.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Path, Url, EmbeddedPlayer, Title: string;
begin
  Result := False;
  if not GetRegExpVar(PortToIVysilaniRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else
    begin
    Url := HtmlDecode(Path);
    if not DownloadPage(Http, Url, EmbeddedPlayer, peUTF8) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_EMBEDDED_OBJECT)
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
  RegisterDownloader(TDownloader_CT24);

end.
