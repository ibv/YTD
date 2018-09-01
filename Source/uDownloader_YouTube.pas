unit uDownloader_YouTube;

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
      SimplifyJSONRegExp: IRegEx;
      FormatListRegExp: IRegEx;
      YouTubeTimestamp: string;
      HDAvailable: boolean;
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
  ulkJSON, SynaCode;

const
  EXTRACT_CONFIG_REGEXP = '\.setConfig\s*\(\s*(?P<JSON>\{.*?\})\s*\)\s*;';
  SIMPLIFY_JSON_REGEXP = '''\s*:\s*\(.*?\),';
  FORMAT_LIST_REGEXP = '(?P<FORMAT>[0-9]+)/(?P<VIDEOQUALITY>[0-9]+)/(?P<AUDIOQUALITY>[0-9]+)/(?P<LENGTH>[0-9]+)';

{ TDownloader_YouTube }

constructor TDownloader_YouTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  YouTubeConfigRegExp := RegExCreate(EXTRACT_CONFIG_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  SimplifyJSONRegExp := RegExCreate(SIMPLIFY_JSON_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  FormatListRegExp := RegExCreate(FORMAT_LIST_REGEXP, [rcoIgnoreCase]);
  fCookies := TStringList.Create;
end;

destructor TDownloader_YouTube.Destroy;
begin
  FreeAndNil(fCookies);
  YouTubeConfigRegExp := nil;
  SimplifyJSONRegExp := nil;
  FormatListRegExp := nil;
  inherited;
end;

function TDownloader_YouTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/watch?v=' + MovieID;
end;

function TDownloader_YouTube.GetFileNameExt: string;
begin
  if HDAvailable then
    Result := '.mp4'
  else
    Result := '.flv';
end;

function TDownloader_YouTube.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Cookies.Assign(Http.Cookies);
  YouTubeTimestamp := '';
  HDAvailable := False;
end;

function TDownloader_YouTube.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := inherited BeforeDownload(Http);
  Http.Cookies.Assign(Cookies);
end;

function TDownloader_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var JSONtext, VideoFormat: string;
    Matches: IMatchCollection;
    JSON, JSONobj: TlkJSONObject;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Matches := YouTubeConfigRegExp.Matches(Page);
  for i := 0 to Pred(Matches.Count) do
    begin
    JSONtext := Matches[i].Groups.ItemsByName['JSON'].Value;
    if JSONtext <> '' then
      begin
      JSONtext := SimplifyJSONRegExp.Replace(JSONtext, ''': null,');
      JSON := TlkJSON.ParseText(JSONtext) as TlkJSONobject;
      if JSON <> nil then
        try
          if JSON.IndexOfName('VIDEO_TITLE') >= 0 then
            begin
            SetName(Trim(JSON.getString('VIDEO_TITLE')));
            HDAvailable := JSON.getBoolean('IS_HD_AVAILABLE');
            JSONobj := JSON.Field['SWF_ARGS'] as TlkJSONobject;
            if JSONobj <> nil then
              try
                YouTubeTimeStamp := DecodeUrl(Trim(JSONobj.getString('t')));
                if YouTubeTimeStamp <> '' then
                  begin
                  //if HDAvailable then
                  //  begin
                    VideoFormat := GetBestVideoFormat(DecodeUrl(Trim(JSONobj.getString('fmt_list'))));
                    if VideoFormat = '' then
                      VideoFormat := '22';
                  //  end
                  //else
                  //  VideoFormat := '18';
                  MovieURL := 'http://www.youtube.com/get_video.php?fmt=' + VideoFormat + '&video_id=' + MovieID + '&t=' + YouTubeTimeStamp;
                  Result := True;
                  SetPrepared(True);
                  Break;
                  end;
              finally
                // Note: DO NOT DO THIS!
                // JSONobj.Free;
                end;
            Break;
            end;
        finally
          JSON.Free;
          end;
      end;
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
