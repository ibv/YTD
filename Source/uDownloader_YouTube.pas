unit uDownloader_YouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_YouTube = class(THttpDownloader)
    private
      fCookies: TStringList;
    protected
      YouTubeConfigRegExp: IRegEx;
      FormatListRegExp: IRegEx;
      ExtractFormatListRegExp: IRegEx;
      ExtractTimestampRegExp: IRegEx;
      ExtractVideoIdRegExp: IRegEx;
      Extension: string;
      function GetMovieInfoUrl: string; override;
      function GetInfoPageEncoding: TPageEncoding; override;
      function GetFileNameExt: string; override;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function BeforeDownload(Http: THttpSend): boolean; override;
      function GetBestVideoFormat(const FormatList: string): string; virtual;
      property Cookies: TStringList read fCookies;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  SynaCode;

const
  EXTRACT_CONFIG_REGEXP = '<param\s+name=((?:\\?["''])?)flashvars\1\s+value=\1(?P<FLASHVARS>.*?)\1';
  MOVIE_TITLE_REGEXP = '<title>(?:\s*YouTube\s*-)?\s*(?P<TITLE>.*?)\s*</title>';
  FLASHVARS_FORMATLIST_REGEXP = '&fmt_list=(?P<FORMATLIST>.*?)(?:&|$)';
  FLASHVARS_TIMESTAMP_REGEXP = '&t=(?P<TIMESTAMP>.*?)(?:&|$)';
  FLASHVARS_VIDEOID_REGEXP = '&video_id=(?P<VIDEOID>.*?)(?:&|$)';
  FORMAT_LIST_REGEXP = '(?P<FORMAT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)';

{ TDownloader_YouTube }

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  YouTubeConfigRegExp := RegExCreate(EXTRACT_CONFIG_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  ExtractFormatListRegExp := RegExCreate(FLASHVARS_FORMATLIST_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  ExtractTimestampRegExp := RegExCreate(FLASHVARS_TIMESTAMP_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  ExtractVideoIdRegExp := RegExCreate(FLASHVARS_VIDEOID_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  FormatListRegExp := RegExCreate(FORMAT_LIST_REGEXP, [rcoIgnoreCase]);
  fCookies := TStringList.Create;
end;

destructor TDownloader_YouTube.Destroy;
begin
  FreeAndNil(fCookies);
  YouTubeConfigRegExp := nil;
  MovieTitleRegExp := nil;
  ExtractFormatListRegExp := nil;
  ExtractTimestampRegExp := nil;
  ExtractVideoIdRegExp := nil;
  FormatListRegExp := nil;
  inherited;
end;

function TDownloader_YouTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/watch?v=' + MovieID;
end;

function TDownloader_YouTube.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_YouTube.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Cookies.Assign(Http.Cookies);
  Extension := '.flv';
end;

function TDownloader_YouTube.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := inherited BeforeDownload(Http);
  Http.Cookies.Assign(Cookies);
end;

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashVars, FormatList, TimeStamp, VideoId, VideoFormat: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg('Failed to locate flashvars.')
  else if not GetRegExpVar(ExtractFormatListRegExp, FlashVars, 'FORMATLIST', FormatList) then
    SetLastErrorMsg('Failed to locate format list.')
  else if not GetRegExpVar(ExtractTimestampRegExp, FlashVars, 'TIMESTAMP', Timestamp) then
    SetLastErrorMsg('Failed to locate timestamp.')
  else
    begin
    if not GetRegExpVar(ExtractVideoIdRegExp, FlashVars, 'VIDEOID', VideoId) then
      VideoId := MovieId;
    VideoFormat := GetBestVideoFormat(DecodeUrl(Trim(FormatList)));
    if VideoFormat = '' then
      VideoFormat := '22';
    if (VideoFormat = '34') or (VideoFormat = '35') then
      Extension := '.flv'
    else
      Extension := '.mp4';
    MovieURL := 'http://www.youtube.com/get_video.php?fmt=' + VideoFormat + '&video_id=' + MovieID + '&t=' + TimeStamp;
    Result := True;
    SetPrepared(True);
    end;
end;

function TDownloader_YouTube.GetBestVideoFormat(const FormatList: string): string;
var Matches: IMatchCollection;
    MaxVideoQuality, MaxAudioQuality: integer;
    VideoQuality, AudioQuality: integer;
    i: integer;
begin
  Result := '';
  MaxVideoQuality := 0;
  MaxAudioQuality := 0;
  Matches := FormatListRegExp.Matches(FormatList);
  try
    for i := 0 to Pred(Matches.Count) do
      with Matches[i].Groups do
        begin
        VideoQuality := StrToIntDef(GetItemByName('VIDEOQUALITY').Value, 0);
        AudioQuality := StrToIntDef(GetItemByName('AUDIOQUALITY').Value, 0);
        if (VideoQuality > MaxVideoQuality) or ((VideoQuality = MaxVideoQuality) and (AudioQuality > MaxAudioQuality)) then
          begin
          Result := GetItemByName('FORMAT').Value;
          MaxVideoQuality := VideoQuality;
          MaxAudioQuality := AudioQuality;
          end;
        end;
  finally
    Matches := nil;
    end;
end;

class function TDownloader_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TDownloader_YouTube.MovieIDParamName: string;
begin
  Result := 'YOUTUBE';
end;

class function TDownloader_YouTube.UrlRegExp: string;
begin
  // http://www.youtube.com/v/HANqEpKDHyk
  // http://www.youtube.com/watch/v/HANqEpKDHyk
  // http://www.youtube.com/watch?v=eYSbVcjyVyw
  Result := '^https?://(?:www\.)?youtube\.com/(?:v/|watch/v/|watch\?v=)(?P<' + MovieIDParamName + '>[^&?]+)';
end;

function TDownloader_YouTube.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

initialization
  RegisterDownloader(TDownloader_YouTube);

end.
