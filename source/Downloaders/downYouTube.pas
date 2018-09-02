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
  TTextDecoderFunction = function(const Text: string): string of object;

  TDownloader_YouTube = class(TNestedDownloader)
    private
    protected
      YouTubeConfigRegExp: TRegExp;
      YouTubeConfigJSRegExp: TRegExp;
      YouTubeConfigJSONRegExp: TRegExp;
      FormatListRegExp: TRegExp;
      FlashVarsParserRegExp: TRegExp;
      JSONParserRegExp: TRegExp;
      FormatUrlMapRegExp: TRegExp;
      FormatUrlMapVarsRegExp: TRegExp;
      Extension: string;
      MaxWidth, MaxHeight: integer;
      AvoidWebM: boolean;
      {$IFDEF SUBTITLES}
        PreferredLanguages: string;
      {$ENDIF}
    protected
      function GetBestVideoFormat(const FormatList, FormatUrlMap: string): string;
      function GetVideoFormatExt(const VideoFormat: string): string;
      function GetDownloader(Http: THttpSend; const VideoFormat, FormatUrlMap: string; Live, Vevo: boolean; out Url: string; out Downloader: TDownloader): boolean;
      function ProcessFlashVars(Http: THttpSend; Parser: TRegExp; TextDecoder: TTextDecoderFunction; const FlashVars: string; out Title, Url: string): boolean;
      function FlashVarsDecode(const Text: string): string;
      function JSONVarsDecode(const Text: string): string;
      function UpdateVevoSignature(const Signature: string): string;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
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
  uStrings,
  uDownloadClassifier,
  uHttpDirectDownloader, uRtmpDirectDownloader, 
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/(?:v/|watch/v/|watch(?:_popup)?\?(?:.*&)*v=|embed/(?!videoseries)|embedded/)';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_CONFIG = '<embed\b[^>]*\sflashvars="(?P<FLASHVARS>[^"]+)"';
  REGEXP_EXTRACT_CONFIG_JS = '\bflashvars\s*=(?P<QUOTE>\\?["''])(?P<FLASHVARS>.+?)(?P=QUOTE)';
  REGEXP_EXTRACT_CONFIG_JSON = '(?:\.playerConfig|\bytplayer\.config)\s*=\s*\{(?P<JSON>.+?)\}\s*;';
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_FLASHVARS_PARSER = '(?:^|&amp;|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  REGEXP_JSON_PARSER = REGEXP_PARSER_FLASHVARS_JS;
  REGEXP_FORMAT_LIST = '(?P<FORMAT>[0-9]+)/(?P<WIDTH>[0-9]+)x(?P<HEIGHT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)'; //'34/640x360/9/0/115,5/0/7/0/0'
  REGEXP_FORMAT_URL_MAP_ITEM = '(?:^|,)(?P<ITEM>.*?)(?:$|(?=,))';
  REGEXP_FORMAT_URL_MAP_VARS = '(?:^|&)(?P<VARNAME>[^=]+)=(?P<VARVALUE>[^&]*)';

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
    , dfRtmpLiveStream, dfRtmpRealtime
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
  YouTubeConfigJSONRegExp := RegExCreate(REGEXP_EXTRACT_CONFIG_JSON);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER);
  JSONParserRegExp := RegExCreate(REGEXP_JSON_PARSER);
  FormatListRegExp := RegExCreate(REGEXP_FORMAT_LIST);
  FormatUrlMapRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP_ITEM);
  FormatUrlMapVarsRegExp := RegExCreate(REGEXP_FORMAT_URL_MAP_VARS);
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
  RegExFreeAndNil(YouTubeConfigJSONRegExp);
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsParserRegExp);
  RegExFreeAndNil(JSONParserRegExp);
  RegExFreeAndNil(FormatListRegExp);
  RegExFreeAndNil(FormatUrlMapRegExp);
  RegExFreeAndNil(FormatUrlMapVarsRegExp);
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

 function MatchLanguageCode(const Language, PreferredLanguages: string; out Position: integer; out ExactMatch: boolean): boolean;
   var
     Preferred: string;
     ix: integer;
   begin
     Result := False;
     if Language <> '' then
       begin
       Preferred := ',' + PreferredLanguages + ',';
       Position := Pos(',' + Language + ',', Preferred);
       if Position > 0 then
         ExactMatch := True
       else
         begin
         ix := Pos('-', Language);
         if ix > 0 then
           begin
           Position := Pos(',' + Copy(Language, 1, Pred(ix)) + ',', Preferred);
           if Position > 0 then
             ExactMatch := False;
           end;
         end;
       Result := Position > 0;
       end;
   end; 

const SECONDS_PER_DAY = 60 * 60 * 24;
var Xml: TXmlDoc;
    CurLanguage, BestLanguage {, BestLanguageName}: string;
    CurLanguagePos, BestLanguagePos: integer;
    ExactLanguageMatch: boolean;
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
      BestLanguage := '';
      //BestLanguageName := '';
      BestLanguagePos := MaxInt;
      for i := 0 to Pred(Xml.Root.NodeCount) do
        if Xml.Root.Nodes[i].Name = 'track' then
          if GetXmlAttr(Xml.Root.Nodes[i], '', 'lang_code', CurLanguage) then
            begin
            if not MatchLanguageCode(CurLanguage, PreferredLanguages, CurLanguagePos, ExactLanguageMatch) then
              CurLanguagePos := Pred(MaxInt)
            else if not ExactLanguageMatch then
              CurLanguagePos := CurLanguagePos + 2*Length(PreferredLanguages);
            if CurLanguagePos < BestLanguagePos then
              begin
              BestLanguagePos := CurLanguagePos;
              BestLanguage := CurLanguage;
              //GetXmlAttr(Xml.Root.Nodes[i], '', 'lang_original', BestLanguageName);
              end;
            end;
      //if BestLanguagePos < MaxInt then
        begin
        Url := 'http://video.google.com/timedtext?type=track';
        //if BestLanguageName <> '' then
        //  Url := Url + '&name=' + string(EncodeUrl(AnsiString(StringToUtf8(BestLanguageName))));
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

function TDownloader_YouTube.FlashVarsDecode(const Text: string): string;
begin
  Result := {$IFDEF UNICODE} string {$ENDIF} (UrlDecode(Text));
end;

function TDownloader_YouTube.JSONVarsDecode(const Text: string): string;
begin
  Result := {$IFDEF UNICODE} string {$ENDIF} (JSDecode(Text));
end;

function TDownloader_YouTube.ProcessFlashVars(Http: THttpSend; Parser: TRegExp; TextDecoder: TTextDecoderFunction; const FlashVars: string; out Title, Url: string): boolean;
var
  Status, Reason, FmtList, FmtUrlMap, VideoFormat, PS, PTK: string;
  D: TDownloader;
begin
  Result := False;
  Title := '';
  Url := '';
  if GetRegExpVarPairs(Parser, FlashVars, ['status', 'reason', 'fmt_list', 'title', 'url_encoded_fmt_stream_map', 'ps', 'ptk'], [@Status, @Reason, @FmtList, @Title, @FmtUrlMap, @PS, @PTK]) then
    if Status = 'fail' then
      SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Utf8ToString(Utf8String(UrlDecode(Reason)))]))
    else if FmtList = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format List']))
    else if FmtUrlMap = '' then
      SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Format-URL Map']))
    else
      begin
      FmtUrlMap := TextDecoder(FmtUrlMap);
      VideoFormat := GetBestVideoFormat(TextDecoder(Trim(FmtList)), FmtUrlMap);
      if VideoFormat = '' then
        VideoFormat := '22';
      Extension := GetVideoFormatExt(VideoFormat);
      if not GetDownloader(Http, VideoFormat, FmtUrlMap, PS = 'live', PTK = 'vevo', Url, D) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else if CreateNestedDownloaderFromDownloader(D) then
        begin
        Title := UrlDecode(Title, peUtf8);
        Result := True;
        end;
      end;
end;

function TDownloader_YouTube.GetDownloader(Http: THttpSend; const VideoFormat, FormatUrlMap: string; Live, Vevo: boolean; out Url: string; out Downloader: TDownloader): boolean;
var
  FoundFormat, Server, Stream, Signature, Signature2: string;
  Formats: TStringArray;
  i: integer;
  HTTPDownloader: TDownloader_YouTube_HTTP;
  RTMPDownloader: TDownloader_YouTube_RTMP;
begin
  Result := False;
  Url := '';
  Downloader := nil;
  if GetRegExpAllVar(FormatUrlMapRegExp, FormatUrlMap, 'ITEM', Formats) then
    for i := 0 to Pred(Length(Formats)) do
      if GetRegExpVarPairs(FormatUrlMapVarsRegExp, Formats[i], ['itag', 'url', 'conn', 'stream', 'sig', 's'], [@FoundFormat, @Url, @Server, @Stream, @Signature, @Signature2]) then
        if FoundFormat = VideoFormat then
          begin
          if Url <> '' then
            begin
            Url := UrlDecode(Url);
            if Signature = '' then
              Signature := Signature2;
            if Vevo then
              Signature := UpdateVevoSignature(Signature);
            if Signature <> '' then
              Url := Url + '&signature=' + UrlDecode(Signature);
            HTTPDownloader := TDownloader_YouTube_HTTP.Create(Url);
            HTTPDownloader.Cookies.Assign(Http.Cookies);
            Downloader := HTTPDownloader;
            Result := True;
            end
          else if (Server <> '') and (Stream <> '') then
            begin
            Url := UrlDecode(Server);
            Stream := UrlDecode(Stream);
            RTMPDownloader := TDownloader_YouTube_RTMP.Create(Url);
            RTMPDownloader.Playpath := Stream;
            RTMPDownloader.SwfVfy := 'http://s.ytimg.com/yt/swfbin/watch_as3-vflk8NbNX.swf';
            RTMPDownloader.PageUrl := GetMovieInfoUrl;
            RTMPDownloader.Live := Live;
            Downloader := RTMPDownloader;
            Result := True;
            end;
          Break;
          end;
end;

function TDownloader_YouTube.GetBestVideoFormat(const FormatList, FormatUrlMap: string): string;
var
  QualityIndex, MaxVideoQuality, MaxAudioQuality, Width, Height: integer;
  VideoQuality, AudioQuality: integer;
  sAudioQuality, sWidth, sHeight, VideoFormat, Ext: string;
begin
  Result := '';
  MaxVideoQuality := 0;
  MaxAudioQuality := 0;
  if GetRegExpVars(FormatListRegExp, FormatList, ['AUDIOQUALITY', 'WIDTH', 'HEIGHT', 'FORMAT'], [@sAudioQuality, @sWidth, @sHeight, @VideoFormat]) then
    repeat
      AudioQuality := StrToIntDef(sAudioQuality, 0);
      Width := StrToIntDef(sWidth, 0);
      Height := StrToIntDef(sHeight, 0);
      // Now use these values to calculate quality
      Ext := GetVideoFormatExt(VideoFormat);
      if Ext = EXTENSION_MP4 then
        QualityIndex := 100
      else if Ext = EXTENSION_WEBM then
        if AvoidWebM then
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
    until not GetRegExpVarsAgain(FormatListRegExp, ['AUDIOQUALITY', 'WIDTH', 'HEIGHT', 'FORMAT'], [@sAudioQuality, @sWidth, @sHeight, @VideoFormat]);
end;

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, Title, Url: string;
    InfoFound: boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  InfoFound := False;
  if DownloadPage(Http, 'http://www.youtube.com/get_video_info?video_id=' + MovieID, FlashVars) then
    InfoFound := ProcessFlashVars(Http, FlashVarsParserRegExp, FlashVarsDecode, FlashVars, Title, Url);
  if not InfoFound then
    if GetRegExpVar(YouTubeConfigRegExp, Page, 'FLASHVARS', FlashVars) then
      InfoFound := ProcessFlashVars(Http, FlashVarsParserRegExp, FlashVarsDecode, FlashVars, Title, Url);
  if not InfoFound then
    if GetRegExpVar(YouTubeConfigJSRegExp, Page, 'FLASHVARS', FlashVars) then
      InfoFound := ProcessFlashVars(Http, FlashVarsParserRegExp, FlashVarsDecode, JSDecode(FlashVars), Title, Url);
  if not InfoFound then
    if GetRegExpVar(YouTubeConfigJSONRegExp, Page, 'JSON', FlashVars) then
      InfoFound := ProcessFlashVars(Http, JSONParserRegExp, JSONVarsDecode, JSDecode(FlashVars), Title, Url);
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
begin
  inherited;
  MaxWidth := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT);
  MaxHeight := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT);
  AvoidWebM := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_AVOIDWEBM, OPTION_YOUTUBE_AVOIDWEBM_DEFAULT);
  {$IFDEF SUBTITLES}
    PreferredLanguages := Value.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT);
  {$ENDIF}
end;

function TDownloader_YouTube.UpdateVevoSignature(const Signature: string): string;

  procedure YoutubeSwap(var Str: string; Pos: integer);
    var
      C: Char;
    begin
      c := Str[1];
      Str[1] := Str[1 + (Pos mod Length(Str))];
      Str[1 + Pos] := c;
    end;

  procedure Reverse(var Str: string);
    var
      i, n: integer;
      c: Char;
    begin
      n := Length(Str);
      for i := 1 to (n div 2) do
        begin
        c := Str[i];
        Str[i] := Str[n-i+1];
        Str[n-i+1] := c;
        end;
    end;

  procedure Slice(var Str: string; Pos: integer);
    begin
      System.Delete(Str, 1, Pos);
    end;

begin
  // Thanks to mandel99
  // Da se to najit v html5player-vflwMrwdI.js, kdyz dam hledat Reverse. Potrebuju
  // to trochu projit, jestli by se to nedalo vyziskat nejak automatizovane, abych
  // to nemusel porad prepisovat. URL na skript se najde v konfiguracnim stringu
  // v "assets"\s*:\s*\{\s*"js"\s*:\s*"(?P<URL>\\/\\/s.ytimg.com\\/[^"]+?)"
  // (pozor, neobsahuje protokol, jen server a cestu).
  Result := Signature;
  YoutubeSwap(Result, 3);
  Reverse(Result);
  YoutubeSwap(Result, 39);
  Reverse(Result);
  YoutubeSwap(Result, 51);
  Slice(Result, 1);
  YoutubeSwap(Result, 36);
  YoutubeSwap(Result, 14);
end;

{ TDownloader_YouTube_RTMP }

function TDownloader_YouTube_RTMP.Prepare: boolean;
var
  fPlaypath, fSwfVfy, fPageUrl: string;
  fLive: boolean;
begin
  fPlaypath := Playpath;
  fSwfVfy := SwfVfy;
  fPageUrl := PageUrl;
  fLive := Live;
  Result := inherited Prepare;
  if fPlayPath <> '' then
    Playpath := fPlaypath;
  if fSwfVfy <> '' then
    SwfVfy := fSwfVfy;
  PageUrl := fPageUrl;
  Live := fLive;
end;

initialization
  RegisterDownloader(TDownloader_YouTube);

end.
