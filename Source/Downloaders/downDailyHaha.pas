unit downDailyHaha;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DailyHaha = class(THttpDownloader)
    private
    protected
      FlashObjectRegExp: IRegEx;
      FlashVarsRegExp: IRegEx;
      FlashVarSrcRegExp: IRegEx;
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

// http://www.dailyhaha.com/_vids/dog-chasing-shadow.htm
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*dailyhaha\.com/_vids/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_FLASHOBJECT = '\bAC_AX_RunContent\s*\((?P<OBJECT>.*?)\);';
  REGEXP_FLASHVARS = '\s*''(?P<VARNAME>.*?)''\s*,\s*''(?P<VARVALUE>.*?)''\s*,?';
  REGEXP_FLASHVARSRC = '(?:^|&)Vid=(?P<SRC>[^&]+)';

{ TDownloader_DailyHaha }

class function TDownloader_DailyHaha.Provider: string;
begin
  Result := 'DailyHaha.com';
end;

class function TDownloader_DailyHaha.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_DailyHaha.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashObjectRegExp := RegExCreate(REGEXP_FLASHOBJECT, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarSrcRegExp := RegExCreate(REGEXP_FLASHVARSRC, [rcoIgnoreCase]);
end;

destructor TDownloader_DailyHaha.Destroy;
begin
  MovieTitleRegExp := nil;
  FlashObjectRegExp := nil;
  FlashVarsRegExp := nil;
  FlashVarSrcRegExp := nil;
  inherited;
end;

function TDownloader_DailyHaha.GetMovieInfoUrl: string;
begin
  Result := 'http://www.dailyhaha.com/_vids/' + MovieID;
end;

function TDownloader_DailyHaha.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashObject, VarName, VarValue, UrlBase, FileName: string;
    FlashVars: IMatchCollection;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FlashObjectRegExp, Page, 'OBJECT', FlashObject) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else
    begin
    FlashVars := FlashVarsRegExp.Matches(FlashObject);
    try
      UrlBase := '';
      FileName := '';
      for i := 0 to Pred(FlashVars.Count) do
        begin
        VarName := FlashVars[i].Groups.ItemsByName['VARNAME'].Value;
        VarValue := FlashVars[i].Groups.ItemsByName['VARVALUE'].Value;
        if VarName = 'base' then
          UrlBase := VarValue
        else if VarName = 'FlashVars' then
          GetRegExpVar(FlashVarSrcRegExp, VarValue, 'SRC', FileName);
        end;
      if (UrlBase = '') or (FileName = '') then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        MovieURL := UrlBase + FileName;
        SetPrepared(True);
        Result := True;
        end;
    finally
      FlashVars := nil;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DailyHaha);

end.
