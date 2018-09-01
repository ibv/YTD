unit listYouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_YouTube = class(TPlaylistDownloader)
    private
    protected
      NextPageRegExp: TRegExp;
      function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
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
  uDownloadClassifier;

// http://www.youtube.com/view_play_list?p=90D6E7C4DE68E49E
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=';
  URLREGEXP_ID =        '[^/?&"]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PLAYLIST_ITEM = '<a[^>]*\sid="video-long-title-(?P<ID>[^"]+)[^>]*>(?P<NAME>[^<]+)</a>';
  REGEXP_NEXT_PAGE = '<a\s+href="(?P<URL>https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=[^"&]+&sort_field=[^&"]*&page=[0-9]+)"\s+class="yt-uix-pager-link"\s+data-page="(?P<PAGE>[0-9]+)"';

{ TPlaylist_YouTube }

class function TPlaylist_YouTube.Provider: string;
begin
  Result := 'YouTube.com';
end;

class function TPlaylist_YouTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_YouTube.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM, [rcoIgnoreCase, rcoSingleLine]);
  NextPageRegExp := RegExCreate(REGEXP_NEXT_PAGE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TPlaylist_YouTube.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  RegExFreeAndNil(NextPageRegExp);
  inherited;
end;

function TPlaylist_YouTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/view_play_list?p=' + MovieID;
end;

function TPlaylist_YouTube.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Trim(Match.SubexpressionByName('NAME'));
end;

function TPlaylist_YouTube.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := 'http://www.youtube.com/watch?v=' + Match.SubexpressionByName('ID');
end;

function TPlaylist_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Again: boolean;
    Url: string;
    PageNumber, FoundPageNumber: integer;
begin
  PageNumber := 1;
  repeat
    Again := False;
    Result := inherited AfterPrepareFromPage(Page, Http);
    if NextPageRegExp.Match(Page) then
      repeat
        FoundPageNumber := StrToIntDef(NextPageRegExp.SubexpressionByName('PAGE'), 0);
        if FoundPageNumber > PageNumber then
          begin
          PageNumber := FoundPageNumber;
          Url := NextPageRegExp.SubexpressionByName('URL');
          Again := DownloadPage(Http, Url, Page);
          Break;
          end;
      until not NextPageRegExp.MatchAgain;
  until not Again;
end;

initialization
  RegisterDownloader(TPlaylist_YouTube);

end.
