unit downYouTube;
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
    protected
      {$IFDEF GET_VIDEO_INFO}
      GetVideoInfoFailed: Boolean;
      {$ENDIF}
      YouTubeConfigRegExp: IRegEx;
      FormatListRegExp: IRegEx;
      {$IFDEF FLASHVARS_PARSER}
      FlashVarsParserRegExp: IRegEx;
      {$ELSE}
      ExtractFormatListRegExp: IRegEx;
      ExtractTokenRegExp: IRegEx;
      ExtractVideoIdRegExp: IRegEx;
      {$ENDIF}
      Extension: string;
      function GetBestVideoFormat(const FormatList: string): string; virtual;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
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
  uStringUtils,
  uDownloadClassifier,
  uMessages;

// http://www.youtube.com/v/HANqEpKDHyk
// http://www.youtube.com/watch/v/HANqEpKDHyk
// http://www.youtube.com/watch?v=eYSbVcjyVyw
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/(?:v/|watch/v/|watch\?v=)';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_CONFIG = '<param\s+name=((?:\\?["''])?)flashvars\1\s+value=\1(?P<FLASHVARS>.*?)\1';
  REGEXP_MOVIE_TITLE = '<title>(?:\s*YouTube\s*-)?\s*(?P<TITLE>.*?)\s*</title>';
  {$IFDEF FLASHVARS_PARSER}
  REGEXP_FLASHVARS_PARSER = '(?:^|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  {$ELSE}
  REGEXP_FLASHVARS_FORMATLIST = '&fmt_list=(?P<FORMATLIST>.*?)(?:&|$)';
  REGEXP_FLASHVARS_TOKEN = '&t=(?P<TOKEN>.*?)(?:&|$)';
  REGEXP_FLASHVARS_VIDEOID = '&video_id=(?P<VIDEOID>.*?)(?:&|$)';
  {$ENDIF}
  //REGEXP_FORMAT_LIST = '(?P<FORMAT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)';
  REGEXP_FORMAT_LIST = '(?P<FORMAT>[0-9]+)/(?P<WIDTH>[0-9]+)x(?P<HEIGHT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)'; //'34/640x360/9/0/115,5/0/7/0/0'

{ TDownloader_YouTube }

class function TDownloader_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TDownloader_YouTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  YouTubeConfigRegExp := RegExCreate(REGEXP_EXTRACT_CONFIG, [rcoIgnoreCase, rcoSingleLine]);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  {$IFDEF GET_VIDEO_INFO}
  GetVideoInfoFailed := False;
  {$ENDIF}
  {$IFDEF FLASHVARS_PARSER}
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER, [rcoIgnoreCase, rcoSingleLine]);
  {$ELSE}
  ExtractFormatListRegExp := RegExCreate(REGEXP_FLASHVARS_FORMATLIST, [rcoIgnoreCase, rcoSingleLine]);
  ExtractTokenRegExp := RegExCreate(REGEXP_FLASHVARS_TOKEN, [rcoIgnoreCase, rcoSingleLine]);
  ExtractVideoIdRegExp := RegExCreate(REGEXP_FLASHVARS_VIDEOID, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
  FormatListRegExp := RegExCreate(REGEXP_FORMAT_LIST, [rcoIgnoreCase]);
end;

destructor TDownloader_YouTube.Destroy;
begin
  YouTubeConfigRegExp := nil;
  MovieTitleRegExp := nil;
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

function TDownloader_YouTube.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_YouTube.GetMovieInfoUrl: string;
begin
  {$IFDEF GET_VIDEO_INFO}
  if not GetVideoInfoFailed then
    Result := 'http://www.youtube.com/get_video_info?video_id=' + MovieID
  else
  {$ENDIF}
  Result := 'http://www.youtube.com/watch?v=' + MovieID;
end;

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
{$IFDEF GET_VIDEO_INFO}
const GetVideoInfoDoesNotExist = 'status=fail&errorcode=150&';
      GetVideoInfoDoesNotExistLength = Length(GetVideoInfoDoesNotExist);
{$ENDIF}
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
  if not GetVideoInfoFailed then
    FlashVars := Page
  else
  {$ENDIF}
  GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars);
  if FlashVars = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  {$IFDEF GET_VIDEO_INFO}
  else if (not GetVideoInfoFailed) and (AnsiCompareText(Copy(FlashVars, 1, GetVideoInfoDoesNotExistLength), GetVideoInfoDoesNotExist) = 0) then
    begin
    // Some videos may fail with the new (age-verification-bypassing) implementation. If that happens,
    // try again using the old method.
    GetVideoInfoFailed := True;
    try
      Result := Prepare;
      Exit;
    finally
      GetVideoInfoFailed := False;
      end;
    end
  {$ENDIF}
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
      else if VarName = 'title' then
        SetName(WideToAnsi(Utf8ToWide(UrlDecode(VarValue))))
      {$IFDEF GET_VIDEO_INFO}
      else if (not GetVideoInfoFailed) and (VarName = 'token') then
        Token := VarValue
      {$ENDIF}
      else if {$IFDEF GET_VIDEO_INFO} GetVideoInfoFailed and {$ENDIF} (VarName = 't') then
        Token := VarValue
      else if VarName = 'video_id' then
        VideoID := VarValue;
      end;
    if FormatList = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format List']))
    else if Token = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Token']))
  {$ELSE}
  else if not GetRegExpVar(ExtractFormatListRegExp, FlashVars, 'FORMATLIST', FormatList) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format List']))
  else if not GetRegExpVar(ExtractTokenRegExp, FlashVars, 'TOKEN', Token) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Token']))
  {$ENDIF}
  else
    begin
    {$IFNDEF FLASHVARS_PARSER}
    if not GetRegExpVar(ExtractVideoIdRegExp, FlashVars, 'VIDEOID', VideoId) then
      VideoId := MovieId;
    {$ENDIF}
    VideoFormat := GetBestVideoFormat(UrlDecode(Trim(FormatList)));
    if VideoFormat = '' then
      VideoFormat := '22';
    if (VideoFormat = '34') or (VideoFormat = '35') then
      Extension := '.flv'
    else
      Extension := '.mp4';
    MovieURL := 'http://www.youtube.com/get_video?fmt=' + VideoFormat + '&video_id=' + VideoID + '&t=' + Token;
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

initialization
  RegisterDownloader(TDownloader_YouTube);

end.
