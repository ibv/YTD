unit uYouTubeDownloader;

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader;

type
  TYouTubeDownloader = class(TCommonDownloader)
    private
      fCookies: TStringList;
    protected
      YouTubeConfigRegExp: IRegEx;
      SimplifyJSONRegExp: IRegEx;
      YouTubeTimestamp: string;
      HDAvailable: boolean;
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function AfterPrepareFromPage(var Page: string): boolean; override;
      function BeforeDownload(Http: THttpSend): boolean; override;
      property Cookies: TStringList read fCookies;
    public
      constructor Create(const AMovieID: string); reintroduce; virtual;
      destructor Destroy; override;
    end;

implementation

uses
  ulkJSON, SynaCode, uStringUtils;

const
  EXTRACT_CONFIG_REGEXP = '\.setConfig\s*\(\s*(?P<JSON>\{.*?\})\s*\)\s*;';
  SIMPLIFY_JSON_REGEXP = '''\s*:\s*\(.*?\),';

{ TYouTubeDownloader }

constructor TYouTubeDownloader.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  YouTubeConfigRegExp := RegExCreate(EXTRACT_CONFIG_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  SimplifyJSONRegExp := RegExCreate(SIMPLIFY_JSON_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  fCookies := TStringList.Create;
end;

destructor TYouTubeDownloader.Destroy;
begin
  FreeAndNil(fCookies);
  YouTubeConfigRegExp := nil;
  SimplifyJSONRegExp := nil;
  inherited;
end;

function TYouTubeDownloader.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/watch?v=' + MovieID;
end;

function TYouTubeDownloader.GetFileNameExt: string;
begin
  if HDAvailable then
    Result := '.mp4'
  else
    Result := '.flv';
end;

function TYouTubeDownloader.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Page := WideToAnsi(Utf8ToWide(Page));
  Cookies.Assign(Http.Cookies);
  YouTubeTimestamp := '';
  HDAvailable := False;
end;

function TYouTubeDownloader.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := inherited BeforeDownload(Http);
  Http.Cookies.Assign(Cookies);
end;

function TYouTubeDownloader.AfterPrepareFromPage(var Page: string): boolean;
var JSONtext, VideoFormat: string;
    Matches: IMatchCollection;
    JSON, JSONobj: TlkJSONObject;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page);
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
                  if HDAvailable then
                    VideoFormat := '22'
                  else
                    VideoFormat := '18';
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

end.
