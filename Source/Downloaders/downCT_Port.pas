unit downCT_Port;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT_Port = class(TDownloader_CT)
    private
      PortToIVysilaniRegExp: IRegEx;
      PortTitleRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
    end;

implementation

uses
  SynaCode,
  uDownloadClassifier,
  uStringUtils;

const PORT_TO_IVYSILANI_REGEXP = '<iframe\s+src="(?P<PATH>/ivysilani/embed/.*?)"';
const PORT_TITLE_REGEXP = '<div id="heading">\s*<h2>(?P<TITLE>.*?)</h2>';

{ TDownloader_CT_Port }

constructor TDownloader_CT_Port.Create(const AMovieID: string);
begin
  inherited;
  PortToIVysilaniRegExp := RegExCreate(PORT_TO_IVYSILANI_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  PortTitleRegExp := RegExCreate(PORT_TITLE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT_Port.Destroy;
begin
  PortToIVysilaniRegExp := nil;
  PortTitleRegExp := nil;
  inherited;
end;

class function TDownloader_CT_Port.MovieIDParamName: string;
begin
  Result := inherited MovieIDParamName + 'PORT';
end;

class function TDownloader_CT_Port.UrlRegExp: string;
begin
  // http://www.ceskatelevize.cz/program/port/541-elektronicke-knihy/video/
  Result := '^https?://(?:[a-z0-9-]+\.)?ceskatelevize\.cz/program/port/(?P<' + MovieIDParamName + '>[^/]+)(?:/|$)';
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
    SetLastErrorMsg('Failed to find embedded player.')
  else
    begin
    Url := 'http://www.ceskatelevize.cz' + EncodeUrl(StringReplace(Path, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]));
    if not DownloadPage(Http, Url, EmbeddedPlayer) then
      SetLastErrorMsg('Failed to download embedded player.')
    else
      begin
      EmbeddedPlayer := WideToAnsi(Utf8ToWide(EmbeddedPlayer));
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
