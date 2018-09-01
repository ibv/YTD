unit downVideaCesky;
{$INCLUDE 'ytd.inc'}
{$DEFINE SUBTITLES}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downYouTube;

type
  TDownloader_VideaCesky = class(TNestedDownloader)
    private
    protected
      {$IFDEF SUBTITLES}
      SubtitlesRegExp: IRegEx;
      Subtitles: string;
      SubtitlesName: string;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      procedure CreateNestedDownloader(const MovieID: string); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.videacesky.cz/serialy/upoutavka-na-treti-radu-the-guild
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videacesky\.cz/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>[^<]*?)\s*-\s*Videa\s*Èesky';
  REGEXP_EXTRACT_YOUTUBE_ID = '\sflashvars="[^"]*&amp;file=(?P<URL>https?://(?:[a-z0-9-]+\.)*youtube\.com/watch\?v=(?P<ID>[^"]+?))&amp;';
  {$IFDEF SUBTITLES}
  REGEXP_EXTRACT_SUBTITLES = '\sflashvars="[^"]*&amp;captions\.file=(?P<SUBTITLES>https?://[^&"]+)';
  {$ENDIF}

{ TDownloader_VideaCesky }

class function TDownloader_VideaCesky.Provider: string;
begin
  Result := 'VideaCesky.cz';
end;

class function TDownloader_VideaCesky.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_VideaCesky.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedIDRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE_ID, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE_ID, [rcoIgnoreCase, rcoSingleLine]);
  {$IFDEF SUBTITLES}
  SubtitlesRegExp := RegExCreate(REGEXP_EXTRACT_SUBTITLES, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
end;

destructor TDownloader_VideaCesky.Destroy;
begin
  MovieTitleRegExp := nil;
  NestedIDRegExp := nil;
  NestedUrlRegExp := nil;
  {$IFDEF SUBTITLES}
  SubtitlesRegExp := nil;
  {$ENDIF}
  inherited;
end;

function TDownloader_VideaCesky.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videacesky.cz/dummy/' + MovieID;
end;

procedure TDownloader_VideaCesky.CreateNestedDownloader(const MovieID: string);
begin
  inherited;
  NestedDownloader := TDownloader_YouTube.Create(MovieID);
end;

function TDownloader_VideaCesky.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url: string;
begin
  Result := inherited AfterPrepareFromPage(Page, Http);
  {$IFDEF SUBTITLES}
  Subtitles := '';
  SubtitlesName := '';
  if Result then
    if GetRegExpVar(SubtitlesRegExp, Page, 'SUBTITLES', Url) then
      if not DownloadPage(Http, Url, Subtitles, peUTF8) then
        Subtitles := ''
      else
        SubtitlesName := ChangeFileExt(GetThisFileName, ExtractFileExt(Url));
  {$ENDIF}
end;

function TDownloader_VideaCesky.Download: boolean;
{$IFDEF SUBTITLES}
var Overwrite: boolean;
{$ENDIF}
begin
  Result := inherited Download;
  {$IFDEF SUBTITLES}
  if (Subtitles <> '') and (SubtitlesName <> '') then
    begin
    Overwrite := True;
    if FileExists(SubtitlesName) then
      if Assigned(OnFileNameValidate) then
        OnFileNameValidate(Self, SubtitlesName, Overwrite);
    if Overwrite then
      with TFileStream.Create(SubtitlesName, fmCreate) do
        try
          WriteBuffer(Subtitles[1], Length(Subtitles));
        finally
          Free;
          end;
    end;
  {$ENDIF}
end;

initialization
  RegisterDownloader(TDownloader_VideaCesky);

end.
