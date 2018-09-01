unit xxxXTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XTube = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: IRegEx;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xtube\.com/.*[?&]v=';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';
  REGEXP_FLASHVARS = '\.addVariable\s*\(\s*"(?P<VARNAME>[^"]+)"\s*,\s*"(?P<VARVALUE>[^"]+)"';

{ TDownloader_XTube }

class function TDownloader_XTube.Provider: string;
begin
  Result := 'XTube.com';
end;

class function TDownloader_XTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_XTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_XTube.Destroy;
begin
  MovieTitleRegExp := nil;
  FlashVarsRegExp := nil;
  inherited;
end;

function TDownloader_XTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xtube.com/play_re.php?v=' + MovieID;
end;

function TDownloader_XTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const
  PREFIX_FILENAME = '&filename=';
  PREFIX_FILENAME_LENGTH = Length(PREFIX_FILENAME);
var
  FlashVars: IMatchCollection;
  SwfUrl, UserID, VideoID, ClipID: string;
  VarName, VarValue: string;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  FlashVars := FlashVarsRegExp.Matches(Page);
  try
    SwfUrl := '';
    UserID := '';
    VideoID := '';
    ClipID := '';
    for i := 0 to Pred(FlashVars.Count) do
      begin
      VarName := FlashVars[i].Groups.ItemsByName['VARNAME'].Value;
      VarValue := FlashVars[i].Groups.ItemsByName['VARVALUE'].Value;
      if VarName = 'swfURL' then
        SwfUrl := VarValue
      else if VarName = 'user_id' then
        UserID := VarValue
      else if VarName = 'video_id' then
        VideoID := VarValue
      else if VarName = 'clip_id' then
        ClipID := VarValue;
      end;
    if SwfUrl = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['swfURL']))
    else if UserID = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['user_id']))
    else if VideoID = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['video_id']))
    else if ClipID = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['clip_id']))
    else if not DownloadPage(Http, 'http://video2.xtube.com/find_video.php?user_id=' + UserID + '&clip_id=' + ClipID + '&video_id=' + VideoID, Page) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else if Copy(Page, 1, PREFIX_FILENAME_LENGTH) <> PREFIX_FILENAME then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieUrl := SwfUrl + Copy(Page, Succ(PREFIX_FILENAME_LENGTH), MaxInt);
      SetPrepared(True);
      Result := True;
      end;
  finally
    FlashVars := nil;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XTube);
  {$ENDIF}

end.
