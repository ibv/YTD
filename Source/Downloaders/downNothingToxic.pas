unit downNothingToxic;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NothingToxic = class(THttpDownloader)
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

// http://www.nothingtoxic.com/media/1279921781/Bad_Doggie_Eats_An_Entire_Swiffer_Mop
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*nothingtoxic\.com/media/';
  URLREGEXP_ID =        '[0-9]+/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '\bvar\s+flv_media_url\s*=\s*"(?P<URL>https?://.+?)"';

{ TDownloader_NothingToxic }

class function TDownloader_NothingToxic.Provider: string;
begin
  Result := 'NothingToxic.sk';
end;

class function TDownloader_NothingToxic.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_NothingToxic.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_NothingToxic.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_NothingToxic.GetMovieInfoUrl: string;
begin
  Result := 'http://www.nothingtoxic.com/media/' + MovieID;
end;

function TDownloader_NothingToxic.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := False;
  if inherited AfterPrepareFromPage(Page, Http) then
    if not DownloadPage(Http, MovieUrl, hmHEAD) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := LastUrl;
      Result := True;
      end;
end;

initialization
  RegisterDownloader(TDownloader_NothingToxic);

end.
