unit downDailyMotion;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DailyMotion = class(THttpDownloader)
    private
    protected
      MovieParamsRegExp: TRegExp;
      JSONVarsRegExp: TRegExp;
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

// http://www.dailymotion.com/video/x8w3pf_condoms-are-bady_fun#hp-v-v2
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*dailymotion\.com/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_PARAMS = '\.addVariable\s*\(\s*"sequence"\s*,\s*"(?P<PARAMS>.+?)"';
  REGEXP_JSON_VARS = '"(?P<VARNAME>[a-z_][a-z0-9_]*)"\s*:\s*"(?P<VARVALUE>.*?)"';

{ TDownloader_DailyMotion }

class function TDownloader_DailyMotion.Provider: string;
begin
  Result := 'DailyMotion.com';
end;

class function TDownloader_DailyMotion.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_DailyMotion.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  JSONVarsRegExp := RegExCreate(REGEXP_JSON_VARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_DailyMotion.Destroy;
begin
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(JSONVarsRegExp);
  inherited;
end;

function TDownloader_DailyMotion.GetMovieInfoUrl: string;
begin
  Result := 'http://www.dailymotion.com/video/' + MovieID;
end;

function TDownloader_DailyMotion.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var
  Params, Title, Url, HDUrl, HQUrl, SDUrl: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieParamsRegExp, Page, 'PARAMS', Params) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else
    begin
    Params := UrlDecode(Params);
    GetRegExpVarPairs(JSONVarsRegExp, Params, ['videoTitle', 'sdURL', 'hqURL', 'hdURL'], [@Title, @SDUrl, @HQUrl, @HDUrl]);
    if HDUrl <> '' then
      Url := HDUrl
    else if HQUrl <> '' then
      Url := HQUrl
    else
      Url := SDUrl;
    if Title = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else if Url = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      SetName(Title);
      MovieURL := StripSlashes(Url);
      Result := True;
      SetPrepared(True);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DailyMotion);

end.
