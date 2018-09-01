unit downDeutscheBahn;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DeutscheBahn = class(THttpDownloader)
    private
    protected
      BaseNameRegExp: IRegEx;
      SwfObjectRegExp: IRegEx;
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
  uDownloadClassifier,
  uMessages;

// http://bewegtbild.deutschebahn.com/btvo/site/index.php?s=5600&ids=143306
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bewegtbild\.deutschebahn\.com/btvo/site/(?:index\.php)\?(?:[^&]+&)*ids=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<td\s+class="ueberschrift2">\s*(?P<TITLE>.*?)\s*</td>';
  REGEXP_BASENAME = '\bbasename\s*:\s*"(?P<BASENAME>.+?)"';
  REGEXP_SWFOBJECT = '\bswfobject\.embedSWF\s*\(\s*"(?P<BASEURL>https?://[^"]+?/)Player/player\.swf"';

{ TDownloader_DeutscheBahn }

class function TDownloader_DeutscheBahn.Provider: string;
begin
  Result := 'DeutscheBahn.com';
end;

class function TDownloader_DeutscheBahn.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_DeutscheBahn.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  BaseNameRegExp := RegExCreate(REGEXP_BASENAME, [rcoIgnoreCase, rcoSingleLine]);
  SwfObjectRegExp := RegExCreate(REGEXP_SWFOBJECT, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_DeutscheBahn.Destroy;
begin
  MovieTitleRegExp := nil;
  BaseNameRegExp := nil;
  SwfObjectRegExp := nil;
  inherited;
end;

function TDownloader_DeutscheBahn.GetMovieInfoUrl: string;
begin
  Result := 'http://bewegtbild.deutschebahn.com/btvo/site/index.php?s=5600&ids=' + MovieID;
end;

function TDownloader_DeutscheBahn.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var BaseName, BaseUrl: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(BaseNameRegExp, Page, 'BASENAME', BaseName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['basename']))
  else if not GetRegExpVar(SwfObjectRegExp, Page, 'BASEURL', BaseUrl) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['swfobject']))
  else
    begin
    MovieUrl := BaseUrl + BaseName + '_700k.mp4';
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DeutscheBahn);

end.
