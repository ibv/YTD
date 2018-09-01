unit xxxPornoTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornoTube = class(THttpDownloader)
    private
    protected
      FlashIdRegExp: IRegEx;
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornotube\.com/channels\.php\?';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="contentheader">\s*<span>\s*Viewing Video:\s*(?:</span><span[^>]*>)*(?P<TITLE>.*?)\s*</span>\s*</div>';
  REGEXP_FLASHID = '<param\s+name="movie"\s+value="[^"]*[?&]v=(?P<ID>[^"&]+)"';
  REGEXP_FLASHVARS = '[?&](?P<VARNAME>[a-z_][a-z0-9_.]*)=(?P<VARVALUE>[^&]+)';

{ TDownloader_PornoTube }

class function TDownloader_PornoTube.Provider: string;
begin
  Result := 'PornoTube.com';
end;

class function TDownloader_PornoTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornoTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashIdRegExp := RegExCreate(REGEXP_FLASHID, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PornoTube.Destroy;
begin
  MovieTitleRegExp := nil;
  FlashVarsRegExp := nil;
  FlashIdRegExp := nil;
  inherited;
end;

function TDownloader_PornoTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornotube.com/channels.php?' + MovieID;
end;

function TDownloader_PornoTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashID, FlashVars, VarName, VarValue, MediaID, UserID, MediaDomain: string;
    VarList: IMatchCollection;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FlashIdRegExp, Page, 'ID', FlashID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not DownloadPage(Http, 'http://www.pornotube.com/player/player.php?' + FlashID, FlashVars) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    begin
    VarList := FlashVarsRegExp.Matches(FlashVars);
    try
      MediaID := '';
      UserID := '';
      MediaDomain := '';
      for i := 0 to Pred(VarList.Count) do
        begin
        VarName := VarList[i].Groups.ItemsByName['VARNAME'].Value;
        VarValue := VarList[i].Groups.ItemsByName['VARVALUE'].Value;
        if VarName = 'mediaId' then
          MediaID := VarValue
        else if VarName = 'userId' then
          UserID := VarValue
        else if VarName = 'mediaDomain' then
          MediaDomain := VarValue;
        end;
      if MediaID = '' then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['mediaId']))
      else if UserID = '' then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['userId']))
      else if MediaDomain = '' then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['mediaDomain']))
      else
        begin
        MovieUrl := MediaDomain + '.pornotube.com/' + UserId + '/' + MediaID + '.flv';
        SetPrepared(True);
        Result := True;
        end;
    finally
      VarList := nil;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornoTube);
  {$ENDIF}

end.
