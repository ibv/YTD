unit downFreeVideoRu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeVideoRu = class(THttpDownloader)
    private
    protected
      VideoContextRegExp: TRegExp;
      HQVidUrlRegExp: TRegExp;
      VidUrlRegExp: TRegExp;
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

// http://freevideo.ru/video/view/?id=v14445361101&highquality=1
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*freevideo\.ru/video/view/?\?id=';
  URLREGEXP_ID =        'v[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div class="Title">(?P<TITLE>.*?)</div>';
  REGEXP_VIDEO_CONTEXT = '\.addVariable\s*\(\s*''context''\s*,\s*"(?P<CONTEXT>.*?)"';
  REGEXP_VIDEO_URL_HQ = '[{,]\s*"_vidURL_hq"\s*:\s*"(?P<URL>https?:.*?)"';
  REGEXP_VIDEO_URL = '[{,]\s*"_vidURL"\s*:\s*"(?P<URL>https?:.*?)"';

{ TDownloader_FreeVideoRu }

class function TDownloader_FreeVideoRu.Provider: string;
begin
  Result := 'FreeVideo.ru';
end;

class function TDownloader_FreeVideoRu.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FreeVideoRu.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoContextRegExp := RegExCreate(REGEXP_VIDEO_CONTEXT, [rcoIgnoreCase, rcoSingleLine]);
  HQVidUrlRegExp := RegExCreate(REGEXP_VIDEO_URL_HQ, [rcoIgnoreCase, rcoSingleLine]);
  VidUrlRegExp := RegExCreate(REGEXP_VIDEO_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_FreeVideoRu.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoContextRegExp);
  RegExFreeAndNil(HQVidUrlRegExp);
  RegExFreeAndNil(VidUrlRegExp);
  inherited;
end;

function TDownloader_FreeVideoRu.GetMovieInfoUrl: string;
begin
  Result := 'http://freevideo.ru/video/view/?id=' + MovieID + '&highquality=1';
end;

function TDownloader_FreeVideoRu.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Context, Request, Info, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoContextRegExp, Page, 'CONTEXT', Context) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else
    begin
    Request := 'video_url=1&context=' + UrlEncode(UrlDecode(Context)) + '&p_id[1]=4&devid=LoadupFlashPlayer&begun=1&p_id[0]=2&ticket=' + MovieID;
    if not DownloadPage(Http, 'http://freevideo.ru/video/view/url/bot/?' + Request, Info) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else if not (GetRegExpVar(HQVidUrlRegExp, Info, 'URL', Url) or GetRegExpVar(VidUrlRegExp, Info, 'URL', Url)) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := StripSlashes(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_FreeVideoRu);

end.
