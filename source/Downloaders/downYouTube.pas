(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit downYouTube;
{$INCLUDE 'ytd.inc'}
{$DEFINE FLASHVARS_PARSER}
  // Use a common parser for flashvars rather than a specific regexp for each variable.
  // Needed for GET_VIDEO_INFO.
{$DEFINE GET_VIDEO_INFO}
  // Use http://www.youtube.com/get_video_info script. Is is much easier to parse
  // and allows bypassing of age verification.
  // Otherwise use HTML video page.
{$DEFINE FMT_URL_MAP}
  // Use fmt_url_map object rather than timestamps
{$DEFINE CONVERTSUBTITLES}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, uCompatibility, HttpSend, SynaCode,
  uOptions,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_YouTube = class(THttpDownloader)
    private
    protected
      {$IFDEF GET_VIDEO_INFO}
      GetVideoInfoFailed: Boolean;
      {$ENDIF}
      YouTubeConfigRegExp: TRegExp;
      FormatListRegExp: TRegExp;
      {$IFDEF FLASHVARS_PARSER}
      FlashVarsParserRegExp: TRegExp;
      {$ELSE}
      ExtractFormatListRegExp: TRegExp;
      ExtractTokenRegExp: TRegExp;
      ExtractVideoIdRegExp: TRegExp;
      {$ENDIF}
      Extension: string;
      MaxWidth, MaxHeight: integer;
      {$IFDEF FMT_URL_MAP}
      FmtUrlMapRegExp: TRegExp;
      {$ENDIF}
      {$IFDEF SUBTITLES}
        PreferredLanguages: string;
        {$IFDEF CONVERTSUBTITLES}
        ConvertSubtitles: boolean;
        {$ENDIF}
      {$ENDIF}
      function GetBestVideoFormat(const FormatList: string): string; virtual;
      {$IFDEF FMT_URL_MAP}
      function FindUrlForFormat(const VideoFormat, FormatUrlMap: string; out Url: string): boolean; virtual;
      {$ENDIF}
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$IFDEF SUBTITLES}
    public
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

{$IFDEF GET_VIDEO_INFO}
{$IFNDEF FLASHVARS_PARSER}
  'FLASHVARS_PARSER is required for GET_VIDEO_INFO'
{$ENDIF}
{$ENDIF}

uses
  uStringUtils,
  uDownloadClassifier,
  uMessages;

// http://www.youtube.com/v/b5AWQ5aBjgE
// http://www.youtube.com/watch/v/b5AWQ5aBjgE
// http://www.youtube.com/watch?v=b5AWQ5aBjgE
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/(?:v/|watch/v/|watch\?(?:.*&)*v=|embedded/)';
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
  {$IFDEF FMT_URL_MAP}
  REGEXP_FORMAT_URL_MAP = '(?P<FORMAT>[0-9]+)\|(?P<URL>https?://.+?)(?:,|$)';
  {$ENDIF}

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
  InfoPageEncoding := peUTF8;
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
  {$IFDEF FMT_URL_MAP}
  FmtUrlMapRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP, [rcoIgnoreCase]);
  {$ENDIF}
  {$IFDEF SUBTITLES}
    PreferredLanguages := 'en';
    {$IFDEF CONVERTSUBTITLES}
    ConvertSubtitles := True;
    {$ENDIF}
  {$ENDIF}
end;

destructor TDownloader_YouTube.Destroy;
begin
  RegExFreeAndNil(YouTubeConfigRegExp);
  RegExFreeAndNil(MovieTitleRegExp);
  {$IFDEF FLASHVARS_PARSER}
  RegExFreeAndNil(FlashVarsParserRegExp);
  {$ELSE}
  RegExFreeAndNil(ExtractFormatListRegExp);
  RegExFreeAndNil(ExtractTokenRegExp);
  RegExFreeAndNil(ExtractVideoIdRegExp);
  {$ENDIF}
  RegExFreeAndNil(FormatListRegExp);
  {$IFDEF FMT_URL_MAP}
  RegExFreeAndNil(FmtUrlMapRegExp);
  {$ENDIF}
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

{$IFDEF SUBTITLES}
function TDownloader_YouTube.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const SECONDS_PER_DAY = 60 * 60 * 24;
var Xml: TXmlDoc;
    CurLanguage, WantedLanguages, BestLanguage, BestLanguageName: string;
    CurLanguagePos, BestLanguagePos: integer;
    Url: string;
    s: AnsiString;
    i: integer;
    {$IFDEF CONVERTSUBTITLES}
    SrtXml: TXmlDoc;
    Srt: string;
    n, Code: integer;
    StartStr, DurationStr, Content: string;
    Start, Duration: double;
    {$ENDIF}
begin
  Result := False;
  if DownloadXml(Http, 'http://video.google.com/timedtext?v=' + MovieID + '&type=list', Xml) then
    try
      WantedLanguages := ',' + PreferredLanguages + ',';
      BestLanguage := '';
      BestLanguageName := '';
      BestLanguagePos := MaxInt;
      for i := 0 to Pred(Xml.Root.NodeCount) do
        if Xml.Root.Nodes[i].Name = 'track' then
          if GetXmlAttr(Xml.Root.Nodes[i], '', 'lang_code', CurLanguage) then
            begin
            if CurLanguage = '' then
              CurLanguagePos := Pred(MaxInt)
            else
              begin
              CurLanguagePos := Pos(',' + CurLanguage + ',', WantedLanguages);
              if CurLanguagePos <= 0 then
                if BestLanguagePos = MaxInt then
                  CurLanguagePos := Pred(MaxInt)
                else
                  CurLanguagePos := MaxInt;
              end;
            if CurLanguagePos < BestLanguagePos then
              begin
              BestLanguagePos := CurLanguagePos;
              BestLanguage := CurLanguage;
              GetXmlAttr(Xml.Root.Nodes[i], '', 'name', BestLanguageName);
              end;
            end;
//      if BestLanguagePos < MaxInt then
        begin
        Url := 'http://video.google.com/timedtext?type=track';
        if BestLanguageName <> '' then
          Url := Url + '&name=' + string(EncodeUrl(AnsiString(StringToUtf8(BestLanguageName))));
        if BestLanguage <> '' then
          Url := Url + '&lang=' + string(EncodeUrl(AnsiString(StringToUtf8(BestLanguage))));
        Url := Url + '&v=' + MovieID;
        if DownloadBinary(Http, Url, s) then
          if s <> '' then
            begin
            fSubtitles := s;
            fSubtitlesExt := '.xml';
            Result := True;
            {$IFDEF CONVERTSUBTITLES}
            if ConvertSubtitles then
              try
                SrtXml := TXmlDoc.Create;
                try
                  SrtXml.LoadFromStream(Http.Document);
                  Http.Document.Seek(0, 0);
                  n := 0;
                  Srt := '';
                  for i := 0 to Pred(SrtXml.Root.NodeCount) do
                    if SrtXml.Root.Nodes[i].Name = 'text' then
                      if GetXmlAttr(SrtXml.Root.Nodes[i], '', 'start', StartStr) then
                        if GetXmlAttr(SrtXml.Root.Nodes[i], '', 'dur', DurationStr) then
                          if GetXmlVar(SrtXml.Root.Nodes[i], '', Content) then
                            begin
                            Inc(n);
                            Val(StringReplace(StartStr, ',', '.', []), Start, Code);
                            if Code <> 0 then
                              Abort;
                            Val(StringReplace(DurationStr, ',', '.', []), Duration, Code);
                            if Code <> 0 then
                              Abort;
                            Srt := Srt + Format('%d'#13#10'%s --> %s'#13#10'%s'#13#10#13#10, [n, FormatDateTime('hh":"nn":"ss"."zzz', Start/SECONDS_PER_DAY), FormatDateTime('hh":"nn":"ss"."zzz', (Start + Duration)/SECONDS_PER_DAY), Content]);
                            end;
                  if Srt <> '' then
                    begin
                    fSubtitles := AnsiString(StringToUtf8(Srt, True));
                    fSubtitlesExt := '.srt';
                    end;
                finally
                  SrtXml.Free;
                  end;
              except
                on EAbort do
                  ;
                end;
            {$ENDIF}
            end;
        end;
    finally
      Xml.Free;
      end;
end;
{$ENDIF}

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
{$IFDEF GET_VIDEO_INFO}
const GetVideoInfoDoesNotExist = 'status=fail&errorcode=150&';
      GetVideoInfoDoesNotExistLength = Length(GetVideoInfoDoesNotExist);
{$ENDIF}
var FlashVars, FormatList, Title, VideoId, VideoFormat: string;
    {$IFDEF FMT_URL_MAP}
    Url, FmtUrlMap: string;
    {$ELSE}
    Token, Token1, Token2: string;
    {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  {$IFDEF GET_VIDEO_INFO}
  if not GetVideoInfoFailed then
    FlashVars := Page
  else
  {$ENDIF}
  GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars);
  if FlashVars = '' then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
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
    GetRegExpVarPairs(FlashVarsParserRegExp, FlashVars,
      ['fmt_list',  'title', 'video_id', {$IFDEF FMT_URL_MAP} 'fmt_url_map' {$ELSE} 'token', 't'     {$ENDIF} ],
      [@FormatList, @Title,  @VideoID,   {$IFDEF FMT_URL_MAP} @FmtUrlMap    {$ELSE} @Token1, @Token2 {$ENDIF} ]
      );
    {$IFDEF FMT_URL_MAP}
    FmtUrlMap := UrlDecode(FmtUrlMap);
    {$ELSE}
      {$IFDEF GET_VIDEO_INFO}
      if not GetVideoInfoFailed then
        Token := Token1
      else
        Token := Token2;
      {$ELSE}
      Token := Token2;
      {$ENDIF}
    {$ENDIF}
    if Title <> '' then
      SetName(Utf8ToString(Utf8String(UrlDecode(Title))));
    if FormatList = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Format List']))
    {$IFDEF FMT_URL_MAP}
    else if FmtUrlMap = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Format-URL Map']))
    {$ELSE}
    else if Token = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Token']))
    {$ENDIF}
  {$ELSE}
  else if not GetRegExpVar(ExtractFormatListRegExp, FlashVars, 'FORMATLIST', FormatList) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Format List']))
  else if not GetRegExpVar(ExtractTokenRegExp, FlashVars, 'TOKEN', Token) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Token']))
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
    {$IFDEF FMT_URL_MAP}
    if not FindUrlForFormat(VideoFormat, FmtUrlMap, Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieURL := Url;
      Result := True;
      SetPrepared(True);
      end;
    {$ELSE}
    MovieURL := 'http://www.youtube.com/get_video?video_id=' + VideoID + '&t=' + UrlDecode(Token) + '&eurl=&el=embedded&ps=default&fmt=' + VideoFormat + '&asv=2&noflv=1';
    Result := True;
    SetPrepared(True);
    {$ENDIF}
    end;
  {$IFDEF FLASHVARS_PARSER}
    end;
  {$ENDIF}
end;

{$IFDEF FMT_URL_MAP}
function TDownloader_YouTube.FindUrlForFormat(const VideoFormat, FormatUrlMap: string; out Url: string): boolean;
var Found: string;
begin
  Result := False;
  Url := '';
  if FmtUrlMapRegExp.Match(FormatUrlMap) then
    repeat
      Found := FmtUrlMapRegExp.SubexpressionByName('FORMAT');
      if VideoFormat = Found then
        begin
        Url := UrlDecode(FmtUrlMapRegExp.SubexpressionByName('URL'));
        Result := True;
        Break;
        end;
    until not FmtUrlMapRegExp.MatchAgain;
end;
{$ENDIF}

function TDownloader_YouTube.GetBestVideoFormat(const FormatList: string): string;
var MaxVideoQuality, MaxAudioQuality, Width, Height: integer;
    VideoQuality, AudioQuality: integer;
begin
  Result := '';
  MaxVideoQuality := 0;
  MaxAudioQuality := 0;
  if FormatListRegExp.Match(FormatList) then
    repeat
      VideoQuality := StrToIntDef(FormatListRegExp.SubexpressionByName('VIDEOQUALITY'), 0);
      AudioQuality := StrToIntDef(FormatListRegExp.SubexpressionByName('AUDIOQUALITY'), 0);
      Width := StrToIntDef(FormatListRegExp.SubexpressionByName('WIDTH'), 0);
      Height := StrToIntDef(FormatListRegExp.SubexpressionByName('HEIGHT'), 0);
      if (VideoQuality > MaxVideoQuality) or ((VideoQuality = MaxVideoQuality) and (AudioQuality > MaxAudioQuality)) then
        if (Width <= MaxWidth) or (MaxWidth <= 0) then
          if (Height <= MaxHeight) or (MaxHeight <= 0) then
            begin
            Result := FormatListRegExp.SubexpressionByName('FORMAT');
            MaxVideoQuality := VideoQuality;
            MaxAudioQuality := AudioQuality;
            end;
    until not FormatListRegExp.MatchAgain;
end;

procedure TDownloader_YouTube.SetOptions(const Value: TYTDOptions);
var s: string;
begin
  inherited;
  if Value.ReadProviderOption(Provider, 'max_video_width', s) then
    MaxWidth := StrToIntDef(s, -1);
  if Value.ReadProviderOption(Provider, 'max_video_height', s) then
    MaxHeight := StrToIntDef(s, -1);
  {$IFDEF SUBTITLES}
    if Value.ReadProviderOption(Provider, 'preferred_languages', s) then
      PreferredLanguages := s;
    {$IFDEF CONVERTSUBTITLES}
    if Value.ReadProviderOption(Provider, 'convert_subtitles', s) then
      ConvertSubtitles := StrToIntDef(s, 0) <> 0;
    {$ENDIF}
  {$ENDIF}
end;

initialization
  RegisterDownloader(TDownloader_YouTube);

end.
