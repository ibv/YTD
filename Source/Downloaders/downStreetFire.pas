unit downStreetFire;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_StreetFire = class(THttpDownloader)
    private
    protected
      VideoIDRegExp: IRegEx;
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

// http://www.streetfire.net/video/porsche-911-gt2_2007066.htm
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*streetfire\.net/video/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>\s*(?P<TITLE>.*?)\s*-\s+Car\s+Videos\s+on\s+StreetFire\s*</title>';
  REGEXP_EXTRACT_VIDEOID = '&lt;param\s+name=''FlashVars''\s+value=''&amp;video=(?P<VIDEOID>[0-9a-f-]+)';

{ TDownloader_StreetFire }

class function TDownloader_StreetFire.Provider: string;
begin
  Result := 'StreetFire.net';
end;

class function TDownloader_StreetFire.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_StreetFire.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoIDRegExp := RegExCreate(REGEXP_EXTRACT_VIDEOID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_StreetFire.Destroy;
begin
  MovieTitleRegExp := nil;
  VideoIDRegExp := nil;
  inherited;
end;

function TDownloader_StreetFire.GetMovieInfoUrl: string;
begin
  Result := 'http://www.streetfire.net/video/' + MovieID;
end;

function TDownloader_StreetFire.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var VideoID: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoIDRegExp, Page, 'VIDEOID', VideoID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieURL := 'http://px5.streetfire.net/flv/' + Copy(VideoID, 1, 3) + '/' + VideoID + '-.flv';
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_StreetFire);

end.
