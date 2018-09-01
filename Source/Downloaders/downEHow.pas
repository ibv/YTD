unit downEHow;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EHow = class(THttpDownloader)
    private
    protected
      SwitchVideoRegExp: IRegEx;
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

// http://www.ehow.com/video_4871930_clean-computer-monitor-glass.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ehow\.com/video_';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_SWITCH_VIDEO_FUNCTION = '\sfunction\s+switchVideoTo(?P<VERSION>.*?)\s*\([^)]*\)\s\{.*?\sshowPlayer\s*\(\s*\{[^}]*\bid\s*:\s*''(?P<URL>https?://.*?)''';

{ TDownloader_EHow }

class function TDownloader_EHow.Provider: string;
begin
  Result := 'EHow.com';
end;

class function TDownloader_EHow.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_EHow.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  SwitchVideoRegExp := RegExCreate(REGEXP_SWITCH_VIDEO_FUNCTION, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_EHow.Destroy;
begin
  MovieTitleRegExp := nil;
  SwitchVideoRegExp := nil;
  inherited;
end;

function TDownloader_EHow.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ehow.com/video_' + MovieID;
end;

function TDownloader_EHow.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Urls: IMatchCollection;
    Url, s: string;
    UrlQuality, Quality: integer;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Urls := SwitchVideoRegExp.Matches(Page);
  try
    Url := '';
    UrlQuality := 0;
    for i := 0 to Pred(Urls.Count) do
      begin
      s := Urls[i].Groups.ItemsByName['VERSION'].Value;
      if AnsiCompareText(s, 'SD') = 0 then
        Quality := 1
      else if AnsiCompareText(s, 'HD') = 0 then
        Quality := 2
      else
        Quality := 0;
      if Quality > UrlQuality then
        begin
        UrlQuality := Quality;
        Url := Urls[i].Groups.ItemsByName['URL'].Value;
        end;
      end;
    if Url <> '' then
      begin
      MovieUrl := Url;
      SetPrepared(True);
      Result := True;
      end;
  finally
    Urls := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_EHow);

end.
