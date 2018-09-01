unit listYouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_YouTube = class(TPlaylistDownloader)
    private
    protected
      NextPageRegExp: IRegEx;
      function GetPlayListItemName(Match: IMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: IMatch; Index: integer): string; override;
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
  REGEXP_PLAYLIST_ITEM = '<a\s+id="video-long-title-(?P<ID>[^"]+)[^>]*>(?P<NAME>[^<]+)</a>';
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
  PlayListItemRegExp := nil;
  NextPageRegExp := nil;
  inherited;
end;

function TPlaylist_YouTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/view_play_list?p=' + MovieID;
end;

function TPlaylist_YouTube.GetPlayListItemName(Match: IMatch; Index: integer): string;
begin
  Result := Trim(Match.Groups.ItemsByName['NAME'].Value);
end;

function TPlaylist_YouTube.GetPlayListItemURL(Match: IMatch; Index: integer): string;
begin
  Result := 'http://www.youtube.com/watch?v=' + Match.Groups.ItemsByName['ID'].Value;
end;

function TPlaylist_YouTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Again: boolean;
    Url: string;
    i, PageNumber, FoundPageNumber: integer;
    OtherPageUrls: IMatchCollection;
begin
  PageNumber := 1;
  repeat
    Result := inherited AfterPrepareFromPage(Page, Http);
    OtherPageUrls := NextPageRegExp.Matches(Page);
    try
      Again := False;
      for i := 0 to Pred(OtherPageUrls.Count) do
        begin
        FoundPageNumber := StrToIntDef(OtherPageUrls[i].Groups.ItemsByName['PAGE'].Value, 0);
        if FoundPageNumber > PageNumber then
          begin
          PageNumber := FoundPageNumber;
          Url := OtherPageUrls[i].Groups.ItemsByName['URL'].Value;
          Again := DownloadPage(Http, Url, Page);
          Break;
          end;
        end;
    finally
      OtherPageUrls := nil;
      end;
  until not Again;
end;

initialization
  RegisterDownloader(TPlaylist_YouTube);

end.
