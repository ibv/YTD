unit downDailyMotion;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DailyMotion = class(THttpDownloader)
    private
    protected
      MovieParamsRegExp: IRegEx;
      JSONVarsRegExp: IRegEx;
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
  MovieParamsRegExp := nil;
  JSONVarsRegExp := nil;
  inherited;
end;

function TDownloader_DailyMotion.GetMovieInfoUrl: string;
begin
  Result := 'http://www.dailymotion.com/video/' + MovieID;
end;

function TDownloader_DailyMotion.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const
  QUALITY_SD = 1;
  QUALITY_HQ = 2;
  QUALITY_HD = 3;
var
  VarMatches: IMatchCollection;
  Params, VarName, VarValue, Title, Url: string;
  i, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieParamsRegExp, Page, 'PARAMS', Params) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    Params := UrlDecode(Params);
    Title := '';
    Url := '';
    Quality := 0;
    VarMatches := JSONVarsRegExp.Matches(Params);
    try
      for i := 0 to Pred(VarMatches.Count) do
        begin
        VarName := VarMatches[i].Groups.ItemsByName['VARNAME'].Value;
        VarValue := VarMatches[i].Groups.ItemsByName['VARVALUE'].Value;
        if VarName = 'videoTitle' then
          Title := VarValue
        else if VarName = 'sdURL' then
          begin
          if Quality < QUALITY_SD then
            begin
            Url := VarValue;
            Quality := QUALITY_SD;
            end;
          end
        else if VarName = 'hqURL' then
          begin
          if Quality < QUALITY_HQ then
            begin
            Url := VarValue;
            Quality := QUALITY_HQ;
            end;
          end
        else if VarName = 'hdURL' then
          begin
          if Quality < QUALITY_HD then
            begin
            Url := VarValue;
            Quality := QUALITY_HD;
            end;
          end;
        end;
      if Title = '' then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
      else if Url = '' then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        SetName(Title);
        MovieURL := StripSlashes(Url);
        Result := True;
        SetPrepared(True);
        end;
    finally
      VarMatches := nil;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DailyMotion);

end.
