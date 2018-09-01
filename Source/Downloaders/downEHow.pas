unit downEHow;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EHow = class(THttpDownloader)
    private
    protected
      SwitchVideoRegExp: TRegExp;
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
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(SwitchVideoRegExp);
  inherited;
end;

function TDownloader_EHow.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ehow.com/video_' + MovieID;
end;

function TDownloader_EHow.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, s: string;
    UrlQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Url := '';
  UrlQuality := 0;
  if SwitchVideoRegExp.Match(Page) then
    repeat
      s := SwitchVideoRegExp.SubexpressionByName('VERSION');
      if AnsiCompareText(s, 'SD') = 0 then
        Quality := 1
      else if AnsiCompareText(s, 'HD') = 0 then
        Quality := 2
      else
        Quality := 0;
      if Quality > UrlQuality then
        begin
        UrlQuality := Quality;
        Url := SwitchVideoRegExp.SubexpressionByName('URL');
        end;
    until not SwitchVideoRegExp.MatchAgain;
  if Url <> '' then
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_EHow);

end.
