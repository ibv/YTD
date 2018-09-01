unit uDownloader_Playlist_YouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TDownloader_YouTube_Playlist = class(TPlaylistDownloader)
    private
    protected
      NextPageRegExp: IRegEx;
      function GetPlayListItemName(Match: IMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: IMatch; Index: integer): string; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function MovieIDParamName: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

const PLAYLIST_URL_REGEXP = '';

const PLAYLIST_ITEM_REGEXP = '<a\s+id="video-long-title-(?P<ID>[^"]+)[^>]*>(?P<NAME>[^<]+)</a>';
const NEXT_PAGE_REGEXP = '<a\s+href="(?P<URL>https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=[^"&]+&sort_field=[^&"]*&page=[0-9]+)"\s+class="yt-uix-pager-link"\s+data-page="(?P<PAGE>[0-9]+)"';

{ TDownloader_YouTube_Playlist }

constructor TDownloader_YouTube_Playlist.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(PLAYLIST_ITEM_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  NextPageRegExp := RegExCreate(NEXT_PAGE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_YouTube_Playlist.Destroy;
begin
  PlayListItemRegExp := nil;
  NextPageRegExp := nil;
  inherited;
end;

class function TDownloader_YouTube_Playlist.Provider: string;
begin
  Result := 'YouTube';
end;

class function TDownloader_YouTube_Playlist.MovieIDParamName: string;
begin
  Result := 'YOUTUBEPLAYLIST';
end;

class function TDownloader_YouTube_Playlist.UrlRegExp: string;
begin
  // http://www.youtube.com/view_play_list?p=90D6E7C4DE68E49E
  //Result := '^https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=(?P<' + MovieIDParamName + '>[^"&]+)$';
  Result := '^https?://(?:[a-z0-9-]+\.)*youtube\.com/view_play_list\?p=(?P<' + MovieIDParamName + '>[^"&]+)';
end;

function TDownloader_YouTube_Playlist.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youtube.com/view_play_list?p=' + MovieID;
end;

function TDownloader_YouTube_Playlist.GetPlayListItemName(Match: IMatch; Index: integer): string;
begin
  Result := Trim(Match.Groups.ItemsByName['NAME'].Value);
end;

function TDownloader_YouTube_Playlist.GetPlayListItemURL(Match: IMatch; Index: integer): string;
begin
  Result := 'http://www.youtube.com/watch?v=' + Match.Groups.ItemsByName['ID'].Value;
end;

function TDownloader_YouTube_Playlist.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Again: boolean;
    Url: string;
    i, PageNumber: integer;
    OtherPageUrls: IMatchCollection;
begin
  PageNumber := 1;
  repeat
    Result := inherited AfterPrepareFromPage(Page, Http);
    OtherPageUrls := NextPageRegExp.Matches(Page);
    try
      Again := False;
      Inc(PageNumber);
      for i := 0 to Pred(OtherPageUrls.Count) do
        if StrToIntDef(OtherPageUrls[i].Groups.ItemsByName['PAGE'].Value, 0) = PageNumber then
          begin
          Url := OtherPageUrls[i].Groups.ItemsByName['URL'].Value;
          Again := DownloadPage(Http, Url, Page);
          Break;
          end;
    finally
      OtherPageUrls := nil;
      end;
  until not Again;
end;

initialization
  {$IFDEF MULTIDOWNLOADS}
  RegisterDownloader(TDownloader_YouTube_Playlist);
  {$ENDIF}

end.
