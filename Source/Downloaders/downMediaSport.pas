unit downMediaSport;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, SynaUtil,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MediaSport = class(THttpDownloader)
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
  uDownloadClassifier,
  uMessages;

// http://www.mediasport.cz/rally-cz/video/09_luzicke_cerny_rz1.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*mediasport\.cz/';
  URLREGEXP_ID =        '[^/]+/video/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_URL = '\bflashvars\.id\s*=\s*"[^"]*\|HQ\|(?P<URL>https?://.+?)[|"]';

{ TDownloader_MediaSport }

class function TDownloader_MediaSport.Provider: string;
begin
  Result := 'MediaSport.cz';
end;

class function TDownloader_MediaSport.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MediaSport.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_MediaSport.Destroy;
begin
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_MediaSport.GetMovieInfoUrl: string;
begin
  Result := 'http://www.mediasport.cz/' + MovieID;
end;

function TDownloader_MediaSport.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Protocol, User, Password, Host, Port, Path, Params: string;
    i: integer;
begin
  Result := inherited AfterPrepareFromPage(Page, Http);
  if Result then
    begin
    ParseURL(MovieURL, Protocol, User, Password, Host, Port, Path, Params);
    i := Length(Path);
    while (i > 0) and (Path[i] <> '/') do
      Dec(i);
    if i > 0 then
      Path := Copy(Path, Succ(i), MaxInt);
    SetName(ChangeFileExt(Path, ''));
    end;
end;

initialization
  RegisterDownloader(TDownloader_MediaSport);

end.
