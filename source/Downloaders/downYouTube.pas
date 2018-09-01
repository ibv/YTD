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
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format

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
      YouTubeConfigRegExp: TRegExp;
      FormatListRegExp: TRegExp;
      FlashVarsParserRegExp: TRegExp;
      FmtUrlMapRegExp: TRegExp;
      Extension: string;
      MaxWidth, MaxHeight: integer;
      {$IFDEF SUBTITLES}
        PreferredLanguages: string;
        {$IFDEF CONVERTSUBTITLES}
        ConvertSubtitles: boolean;
        {$ENDIF}
      {$ENDIF}
      function GetBestVideoFormat(const FormatList: string): string; virtual;
      function FindUrlForFormat(const VideoFormat, FormatUrlMap: string; out Url: string): boolean; virtual;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function ProcessFlashVars(const FlashVars: string; out Title, Url: string): boolean;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF SUBTITLES}
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uStringConsts,
  uStringUtils,
  uDownloadClassifier,
  {$IFDEF SUBTITLES}
  uSubtitles,
  {$ENDIF}
  uMessages;

// http://www.youtube.com/v/b5AWQ5aBjgE
// http://www.youtube.com/watch/v/b5AWQ5aBjgE
// http://www.youtube.com/watch?v=b5AWQ5aBjgE
// http://www.youtube.com/embed/b5AWQ5aBjgE
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/(?:v/|watch/v/|watch\?(?:.*&)*v=|embed/|embedded/)';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_CONFIG = '<embed\b[^>]*\sflashvars="(?P<FLASHVARS>[^"]+)"';
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_FLASHVARS_PARSER = '(?:^|&amp;|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  REGEXP_FORMAT_LIST = '(?P<FORMAT>[0-9]+)/(?P<WIDTH>[0-9]+)x(?P<HEIGHT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)'; //'34/640x360/9/0/115,5/0/7/0/0'
  REGEXP_FORMAT_URL_MAP = '(?P<FORMAT>[0-9]+)\|(?P<URL>https?://.+?)(?:,|$)';

{ TDownloader_YouTube }

class function TDownloader_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TDownloader_YouTube.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  YouTubeConfigRegExp := RegExCreate(REGEXP_EXTRACT_CONFIG);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER);
  FormatListRegExp := RegExCreate(REGEXP_FORMAT_LIST);
  FmtUrlMapRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP);
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
  RegExFreeAndNil(FlashVarsParserRegExp);
  RegExFreeAndNil(FormatListRegExp);
  RegExFreeAndNil(FmtUrlMapRegExp);
  inherited;
end;

function TDownloader_YouTube.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_YouTube.GetMovieInfoUrl: string;
begin
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
                            Val(StringReplace(StartStr, ',', '.', []), Start, Code);
                            if Code <> 0 then
                              Abort;
                            Val(StringReplace(DurationStr, ',', '.', []), Duration, Code);
                            if Code <> 0 then
                              Abort;
                            Srt := Srt + SubtitlesToSrt(n, Start/SECONDS_PER_DAY, (Start + Duration)/SECONDS_PER_DAY, Content);
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

function TDownloader_YouTube.ProcessFlashVars(const FlashVars: string; out Title, Url: string): boolean;
var Status, Reason, FmtList, FmtUrlMap, VideoFormat: string;
begin
  Result := False;
  Title := '';
  Url := '';
  if GetRegExpVarPairs(FlashVarsParserRegExp, FlashVars, ['status', 'reason', 'fmt_list', 'title', 'fmt_url_map'], [@Status, @Reason, @FmtList, @Title, @FmtUrlMap]) then
    if Status = 'fail' then
      SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Utf8ToString(Utf8String(UrlDecode(Reason)))]))
    else if FmtList = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format List']))
    else if FmtUrlMap = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format-URL Map']))
    else
      begin
      VideoFormat := GetBestVideoFormat(UrlDecode(Trim(FmtList)));
      if VideoFormat = '' then
        VideoFormat := '22';
      if (VideoFormat = '5') or (VideoFormat = '34') or (VideoFormat = '35') then
        Extension := '.flv'
      else if (VideoFormat = '43') or (VideoFormat = '45') then
        Extension := '.webm'
      else if (VideoFormat = '17') then
        Extension := '.3gp'
      else
        Extension := '.mp4';
      if not FindUrlForFormat(VideoFormat, UrlDecode(FmtUrlMap), Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        Title := Utf8ToString(Utf8String(UrlDecode(Title)));
        Result := True;
        end;
      end;
end;

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

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, Title, Url: string;
    InfoFound: boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  InfoFound := False;
  if DownloadPage(Http, 'http://www.youtube.com/get_video_info?video_id=' + MovieID, FlashVars) then
    InfoFound := ProcessFlashVars(FlashVars, Title, Url);
  if not InfoFound then
    if not GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
    else
      InfoFound := ProcessFlashVars(FlashVars, Title, Url);
  if InfoFound then
    begin
    if Title <> '' then
      SetName(Title);
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
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
