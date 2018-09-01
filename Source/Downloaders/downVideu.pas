unit downVideu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Videu = class(THttpDownloader)
    private
    protected
      InfoUrlRegExp: IRegEx;
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

// http://www.videu.de/video/4etN3FlaaY
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videu\.de/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<H1\s+ID="detail_hl">(?P<TITLE>.*?)</H1>';
  REGEXP_INFO_URL = '&it=(?P<TYPE>[^&]+)&u=(?P<ID>[0-9]+)&host=(?P<HOST>[^&]+)';

{ TDownloader_Videu }

class function TDownloader_Videu.Provider: string;
begin
  Result := 'Videu.de';
end;

class function TDownloader_Videu.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Videu.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_Videu.Destroy;
begin
  MovieTitleRegExp := nil;
  InfoUrlRegExp := nil;
  inherited;
end;

function TDownloader_Videu.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videu.de/video/' + MovieID;
end;

function TDownloader_Videu.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Info, VidType, VidID, VidHost: string;
    Match: IMatch;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not DownloadPage(Http, 'http://www.videu.de/zgst372zst4u3.php?iid=' + MovieID, Info) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Match := InfoUrlRegExp.Match(Info);
    try
      if not Match.Matched then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        VidType := Match.Groups.ItemsByName['TYPE'].Value;
        VidID := Match.Groups.ItemsByName['ID'].Value;
        VidHost := Match.Groups.ItemsByName['HOST'].Value;
        MovieUrl := 'http://' + VidHost + '/userfiles/items/' + VidType + '/' + VidID + '/' + UpperCase(MovieID) + '_VIDEO1.flv';
        SetPrepared(True);
        Result := True;
        end;
    finally
      Match := nil;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Videu);

end.
