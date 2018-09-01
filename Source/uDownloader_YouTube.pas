unit uDownloader_YouTube;
{$INCLUDE 'ytd.inc'}
{$DEFINE FLASHVARS_PARSER}
  // Use a common parser for flashvars rather than a specific regexp for each variable.
  // Needed for GET_VIDEO_INFO.
{$DEFINE GET_VIDEO_INFO}
  // Use http://www.youtube.com/get_video_info script. Is is much easier to parse
  // and allows bypassing of age verification.
  // Otherwise use HTML video page.

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
      {$IFNDEF GET_VIDEO_INFO}
      YouTubeConfigRegExp: IRegEx;
      {$ENDIF}
      FormatListRegExp: IRegEx;
      {$IFDEF FLASHVARS_PARSER}
      FlashVarsParserRegExp: IRegEx;
      {$ELSE}
      ExtractFormatListRegExp: IRegEx;
      ExtractTokenRegExp: IRegEx;
      ExtractVideoIdRegExp: IRegEx;
      {$ENDIF}
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

{$IFDEF GET_VIDEO_INFO}
{$IFNDEF FLASHVARS_PARSER}
  FLASHVARS_PARSER is required for GET_VIDEO_INFO
{$ENDIF}
{$ENDIF}

uses
  uDownloadClassifier,
  SynaCode;

const
  {$IFNDEF GET_VIDEO_INFO}
  EXTRACT_CONFIG_REGEXP = '<param\s+name=((?:\\?["''])?)flashvars\1\s+value=\1(?P<FLASHVARS>.*?)\1';
  MOVIE_TITLE_REGEXP = '<title>(?:\s*YouTube\s*-)?\s*(?P<TITLE>.*?)\s*</title>';
  {$ENDIF}
  {$IFDEF FLASHVARS_PARSER}
  FLASHVARS_PARSER_REGEXP = '(?:^|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  {$ELSE}
  FLASHVARS_FORMATLIST_REGEXP = '&fmt_list=(?P<FORMATLIST>.*?)(?:&|$)';
  FLASHVARS_TOKEN_REGEXP = '&t=(?P<TOKEN>.*?)(?:&|$)';
  FLASHVARS_VIDEOID_REGEXP = '&video_id=(?P<VIDEOID>.*?)(?:&|$)';
  {$ENDIF}
  FORMAT_LIST_REGEXP = '(?P<FORMAT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)';

{ TDownloader_YouTube }

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  {$IFNDEF GET_VIDEO_INFO}
  YouTubeConfigRegExp := RegExCreate(EXTRACT_CONFIG_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
  {$IFDEF FLASHVARS_PARSER}
  FlashVarsParserRegExp := RegExCreate(FLASHVARS_PARSER_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  {$ELSE}
  ExtractFormatListRegExp := RegExCreate(FLASHVARS_FORMATLIST_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  ExtractTokenRegExp := RegExCreate(FLASHVARS_TOKEN_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  ExtractVideoIdRegExp := RegExCreate(FLASHVARS_VIDEOID_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
  FormatListRegExp := RegExCreate(FORMAT_LIST_REGEXP, [rcoIgnoreCase]);
  fCookies := TStringList.Create;
end;

destructor TDownloader_YouTube.Destroy;
begin
  FreeAndNil(fCookies);
  {$IFNDEF GET_VIDEO_INFO}
  YouTubeConfigRegExp := nil;
  MovieTitleRegExp := nil;
  {$ENDIF}
  {$IFDEF FLASHVARS_PARSER}
  FlashVarsParserRegExp := nil;
  {$ELSE}
  ExtractFormatListRegExp := nil;
  ExtractTokenRegExp := nil;
  ExtractVideoIdRegExp := nil;
  {$ENDIF}
  FormatListRegExp := nil;
  inherited;
end;

function TDownloader_YouTube.GetMovieInfoUrl: string;
begin
  {$IFDEF GET_VIDEO_INFO}
  Result := 'http://www.youtube.com/get_video_info?video_id=' + MovieID;
  {$ELSE}
  Result := 'http://www.youtube.com/watch?v=' + MovieID;
  {$ENDIF}
end;

function TDownloader_YouTube.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_YouTube.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Cookies.Assign(Http.Cookies);
end;

function TDownloader_YouTube.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := inherited BeforeDownload(Http);
  Http.Cookies.Assign(Cookies);
end;

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashVars, FormatList, Token, VideoId, VideoFormat: string;
    {$IFDEF FLASHVARS_PARSER}
    VarList: IMatchCollection;
    VarName, VarValue: string;
    i: integer;
    {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  {$IFDEF GET_VIDEO_INFO}
  FlashVars := Page;
  if FlashVars = '' then
  {$ELSE}
  if not GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars) then
  {$ENDIF}
    SetLastErrorMsg('Failed to locate flashvars.')
  {$IFDEF FLASHVARS_PARSER}
  else
    begin
    VarList := FlashVarsParserRegExp.Matches(FlashVars);
    FormatList := '';
    Token := '';
    VideoId := MovieID;
    for i := 0 to Pred(VarList.Count) do
      begin
      VarName := VarList[i].Groups.ItemsByName['VARNAME'].Value;
      VarValue := VarList[i].Groups.ItemsByName['VARVALUE'].Value;
      if VarName = 'fmt_list' then
        FormatList := VarValue
      {$IFDEF GET_VIDEO_INFO}
      else if VarName = 'title' then
        SetName(DecodeUrl(StringReplace(VarValue, '+', ' ', [rfReplaceAll])))
      {$ENDIF}
      else if VarName = {$IFDEF GET_VIDEO_INFO} 'token' {$ELSE} 't' {$ENDIF} then
        Token := VarValue
      else if VarName = 'video_id' then
        VideoID := VarValue;
      end;
    if FormatList = '' then
      SetLastErrorMsg('Failed to locate format list.')
    else if Token = '' then
      SetLastErrorMsg('Failed to locate token.')
  {$ELSE}
  else if not GetRegExpVar(ExtractFormatListRegExp, FlashVars, 'FORMATLIST', FormatList) then
    SetLastErrorMsg('Failed to locate format list.')
  else if not GetRegExpVar(ExtractTokenRegExp, FlashVars, 'TOKEN', Token) then
    SetLastErrorMsg('Failed to locate token.')
  {$ENDIF}
  else
    begin
    {$IFNDEF FLASHVARS_PARSER}
    if not GetRegExpVar(ExtractVideoIdRegExp, FlashVars, 'VIDEOID', VideoId) then
      VideoId := MovieId;
    {$ENDIF}
    VideoFormat := GetBestVideoFormat(DecodeUrl(Trim(FormatList)));
    if VideoFormat = '' then
      VideoFormat := '22';
    if (VideoFormat = '34') or (VideoFormat = '35') then
      Extension := '.flv'
    else
      Extension := '.mp4';
    MovieURL := 'http://www.youtube.com/get_video.php?fmt=' + VideoFormat + '&video_id=' + MovieID + '&t=' + Token;
    Result := True;
    SetPrepared(True);
    end;
  {$IFDEF FLASHVARS_PARSER}
    end;
  {$ENDIF}
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
