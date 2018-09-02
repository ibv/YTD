(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
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
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_YouTube,
    {$ELSE}
      guiOptionsVCL_YouTube,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_YouTube = class(TNestedDownloader)
    private
    protected
      YouTubeConfigRegExp: TRegExp;
      YouTubeConfigJSRegExp: TRegExp;
      FormatListRegExp: TRegExp;
      FlashVarsParserRegExp: TRegExp;
      HttpFmtUrlMapRegExp: TRegExp;
      RtmpFmtUrlMapRegExp: TRegExp;
      Extension: string;
      MaxWidth, MaxHeight: integer;
      AvoidWebM: boolean;
      {$IFDEF SUBTITLES}
        PreferredLanguages: string;
      {$ENDIF}
    protected
      function GetBestVideoFormat(const FormatList, FormatUrlMap: string): string;
      function GetVideoFormatExt(const VideoFormat: string): string;
      function GetDownloader(Http: THttpSend; const VideoFormat, FormatUrlMap: string; out Url: string; out Downloader: TDownloader): boolean;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function ProcessFlashVars(Http: THttpSend; const FlashVars: string; out Title, Url: string): boolean;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF SUBTITLES}
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    end;

const
  OPTION_YOUTUBE_MAXVIDEOWIDTH {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'max_video_width';
  OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT = 0;
  OPTION_YOUTUBE_MAXVIDEOHEIGHT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'max_video_height';
  OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT = 0;
  OPTION_YOUTUBE_AVOIDWEBM {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'avoid_webm';
  OPTION_YOUTUBE_AVOIDWEBM_DEFAULT = False;
  {$IFDEF SUBTITLES}
    OPTION_YOUTUBE_PREFERREDLANGUAGES {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'preferred_languages';
    OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'en';
  {$ENDIF}

implementation

uses
  uStringConsts,
  uStringUtils,
  uDownloadClassifier,
  uHttpDirectDownloader, uRtmpDirectDownloader, rtmpdump_dll,
  {$IFDEF SUBTITLES}
  uSubtitles,
  {$ENDIF}
  uMessages;

// http://www.youtube.com/v/b5AWQ5aBjgE
// http://www.youtube.com/watch/v/b5AWQ5aBjgE
// http://www.youtube.com/watch?v=b5AWQ5aBjgE
// http://www.youtube.com/embed/b5AWQ5aBjgE
// http://www.youtube.com/watch_popup?v=b5AWQ5aBjgE
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/(?:v/|watch/v/|watch(?:_popup)?\?(?:.*&)*v=|embed/|embedded/)';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_CONFIG = '<embed\b[^>]*\sflashvars="(?P<FLASHVARS>[^"]+)"';
  REGEXP_EXTRACT_CONFIG_JS = '\bflashvars\s*=(?P<QUOTE>\\?["''])(?P<FLASHVARS>.+?)(?P=QUOTE)';
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_FLASHVARS_PARSER = '(?:^|&amp;|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  REGEXP_FORMAT_LIST = '(?P<FORMAT>[0-9]+)/(?P<WIDTH>[0-9]+)x(?P<HEIGHT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)'; //'34/640x360/9/0/115,5/0/7/0/0'
  REGEXP_FORMAT_URL_MAP_HTTP = '(?:^|[,&])url=(?P<URL>https?(?::|%3A)(?:/|%2F){2}[^&,]+)[^,]*&itag=(?P<FORMAT>[0-9]+)';
  REGEXP_FORMAT_URL_MAP_RTMP = '(?:^|[,&])conn=(?P<URL>rtmpt?e?(?::|%3A)(?:/|%2F){2}[^&,]+)[^,]*&stream=(?P<STREAM>[^&,]+)[^,]*&itag=(?P<FORMAT>[0-9]+)';

const
  EXTENSION_FLV {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '.flv';
  EXTENSION_WEBM {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '.webm';
  EXTENSION_MP4 {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '.mp4';
  EXTENSION_3GP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '.3gp';

type
  TDownloader_YouTube_HTTP = class(THttpDirectDownloader);
  TDownloader_YouTube_RTMP = class(TRtmpDirectDownloader)
    public
      function Prepare: boolean; override;
    end;

{ TDownloader_YouTube }

class function TDownloader_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TDownloader_YouTube.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [
    {$IFDEF SUBTITLES} dfSubtitles {$IFDEF CONVERTSUBTITLES} , dfSubtitlesConvert {$ENDIF} {$ENDIF}
    , dfRtmpLiveStream
    ];
end;

class function TDownloader_YouTube.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

{$IFDEF GUI}
class function TDownloader_YouTube.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_YouTube;
end;
{$ENDIF}

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  YouTubeConfigRegExp := RegExCreate(REGEXP_EXTRACT_CONFIG);
  YouTubeConfigJSRegExp := RegExCreate(REGEXP_EXTRACT_CONFIG_JS);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER);
  FormatListRegExp := RegExCreate(REGEXP_FORMAT_LIST);
  HttpFmtUrlMapRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP_HTTP);
  RtmpFmtUrlMapRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP_RTMP);
  MaxWidth := OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT;
  MaxHeight := OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT;
  {$IFDEF SUBTITLES}
    PreferredLanguages := OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT;
  {$ENDIF}
end;

destructor TDownloader_YouTube.Destroy;
begin
  RegExFreeAndNil(YouTubeConfigRegExp);
  RegExFreeAndNil(YouTubeConfigJSRegExp);
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsParserRegExp);
  RegExFreeAndNil(FormatListRegExp);
  RegExFreeAndNil(HttpFmtUrlMapRegExp);
  RegExFreeAndNil(RtmpFmtUrlMapRegExp);
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

function TDownloader_YouTube.GetVideoFormatExt(const VideoFormat: string): string;
begin
  case StrToIntDef(VideoFormat, 0) of
    5, 6, 34, 35:
      Result := EXTENSION_FLV;
    43..46, 100..102:
      Result := EXTENSION_WEBM;
    13, 17, 18:
      Result := EXTENSION_3GP;
    else
      Result := EXTENSION_MP4;
    end;
end;

function TDownloader_YouTube.ProcessFlashVars(Http: THttpSend; const FlashVars: string; out Title, Url: string): boolean;
var
  Status, Reason, FmtList, FmtUrlMap, VideoFormat: string;
  D: TDownloader;
begin
  Result := False;
  Title := '';
  Url := '';
  if GetRegExpVarPairs(FlashVarsParserRegExp, FlashVars, ['status', 'reason', 'fmt_list', 'title', 'url_encoded_fmt_stream_map'], [@Status, @Reason, @FmtList, @Title, @FmtUrlMap]) then
    if Status = 'fail' then
      SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Utf8ToString(Utf8String(UrlDecode(Reason)))]))
    else if FmtList = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format List']))
    else if FmtUrlMap = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format-URL Map']))
    else
      begin
      FmtUrlMap := UrlDecode(FmtUrlMap);
      VideoFormat := GetBestVideoFormat(UrlDecode(Trim(FmtList)), FmtUrlMap);
      if VideoFormat = '' then
        VideoFormat := '22';
      Extension := GetVideoFormatExt(VideoFormat);
      if not GetDownloader(Http, VideoFormat, FmtUrlMap, Url, D) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else if CreateNestedDownloaderFromDownloader(D) then
        begin
        Title := Utf8ToString(Utf8String(UrlDecode(Title)));
        Result := True;
        end;
      end;
end;

function TDownloader_YouTube.GetDownloader(Http: THttpSend; const VideoFormat, FormatUrlMap: string; out Url: string; out Downloader: TDownloader): boolean;
var
  FoundFormat, Stream: string;
  HTTPDownloader: TDownloader_YouTube_HTTP;
  RTMPDownloader: TDownloader_YouTube_RTMP;
begin
  Result := False;
  Url := '';
  Downloader := nil;
  if HttpFmtUrlMapRegExp.Match(FormatUrlMap) then
    repeat
      FoundFormat := HttpFmtUrlMapRegExp.SubexpressionByName('FORMAT');
      if VideoFormat = FoundFormat then
        begin
        Url := UrlDecode(HttpFmtUrlMapRegExp.SubexpressionByName('URL'));
        HTTPDownloader := TDownloader_YouTube_HTTP.Create(Url);
        HTTPDownloader.Cookies.Assign(Http.Cookies);
        Downloader := HTTPDownloader;
        Result := True;
        Break;
        end;
    until not HttpFmtUrlMapRegExp.MatchAgain;
  if not Result then
    if RtmpFmtUrlMapRegExp.Match(FormatUrlMap) then
      repeat
        FoundFormat := RtmpFmtUrlMapRegExp.SubexpressionByName('FORMAT');
        if VideoFormat = FoundFormat then
          begin
          Url := UrlDecode(RtmpFmtUrlMapRegExp.SubexpressionByName('URL'));
          Stream := UrlDecode(RtmpFmtUrlMapRegExp.SubexpressionByName('STREAM'));
          RTMPDownloader := TDownloader_YouTube_RTMP.Create(Url);
          RTMPDownloader.Playpath := Stream;
          RTMPDownloader.SwfVfy := 'http://s.ytimg.com/yt/swfbin/watch_as3-vflk8NbNX.swf';
          RTMPDownloader.PageUrl := GetMovieInfoUrl;
          Downloader := RTMPDownloader;
          Result := True;
          Break;
          end;
      until not RtmpFmtUrlMapRegExp.MatchAgain;
end;

function TDownloader_YouTube.GetBestVideoFormat(const FormatList, FormatUrlMap: string): string;
var QualityIndex, MaxVideoQuality, MaxAudioQuality, Width, Height: integer;
    VideoQuality, AudioQuality: integer;
    VideoFormat, Ext: string;
    IsHTTP: boolean;
begin
  Result := '';
  MaxVideoQuality := 0;
  MaxAudioQuality := 0;
  IsHTTP := HttpFmtUrlMapRegExp.Match(FormatUrlMap);
  if FormatListRegExp.Match(FormatList) then
    repeat
      //VideoQuality := StrToIntDef(FormatListRegExp.SubexpressionByName('VIDEOQUALITY'), 0);
      AudioQuality := StrToIntDef(FormatListRegExp.SubexpressionByName('AUDIOQUALITY'), 0);
      Width := StrToIntDef(FormatListRegExp.SubexpressionByName('WIDTH'), 0);
      Height := StrToIntDef(FormatListRegExp.SubexpressionByName('HEIGHT'), 0);
      VideoFormat := FormatListRegExp.SubexpressionByName('FORMAT');
      // Now use these values to calculate quality
      Ext := GetVideoFormatExt(VideoFormat);
      if Ext = EXTENSION_MP4 then
        QualityIndex := 100
      else if Ext = EXTENSION_WEBM then
        if not IsHTTP then
          QualityIndex := 0 // not available over RTMP
        else if AvoidWebM then
          QualityIndex := 1
        else
          QualityIndex := 80
      else if Ext = EXTENSION_FLV then
        QualityIndex := 60
      else if Ext = EXTENSION_3GP then
        QualityIndex := 40
      else
        QualityIndex := 20;
      VideoQuality := Width * Height * QualityIndex;
      AudioQuality := AudioQuality * QualityIndex;
      if (VideoQuality > MaxVideoQuality) or ((VideoQuality = MaxVideoQuality) and (AudioQuality > MaxAudioQuality)) then
        if (Width <= MaxWidth) or (MaxWidth <= 0) then
          if (Height <= MaxHeight) or (MaxHeight <= 0) then
            begin
                    MaxVideoQuality := VideoQuality;
            MaxAudioQuality := AudioQuality;
            Result := VideoFormat;
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
    InfoFound := ProcessFlashVars(Http, FlashVars, Title, Url);
  if not InfoFound then
    if GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars) then
      InfoFound := ProcessFlashVars(Http, FlashVars, Title, Url);
  if not InfoFound then
    if GetRegExpVar(YouTubeConfigJSRegExp, Page, 'FLASHVARS', FlashVars) then
      InfoFound := ProcessFlashVars(Http, JSDecode(FlashVars), Title, Url);
  if not InfoFound then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    if Title <> '' then
      SetName(Title);
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

procedure TDownloader_YouTube.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  MaxWidth := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT);
  MaxHeight := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT);
  AvoidWebM := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_AVOIDWEBM, OPTION_YOUTUBE_AVOIDWEBM_DEFAULT);
  {$IFDEF SUBTITLES}
    PreferredLanguages := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT);
  {$ENDIF}
end;

{ TDownloader_YouTube_RTMP }

function TDownloader_YouTube_RTMP.Prepare: boolean;
var fPlaypath, fSwfVfy, fPageUrl: string;
begin
  fPlaypath := Playpath;
  fSwfVfy := SwfVfy;
  fPageUrl := PageUrl;
  Result := inherited Prepare;
  Playpath := fPlaypath;
  SwfVfy := fSwfVfy;
  PageUrl := fPageUrl;
end;

initialization
  RegisterDownloader(TDownloader_YouTube);

end.
