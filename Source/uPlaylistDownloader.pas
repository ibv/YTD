unit uPlaylistDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TPlaylistDownloader = class(THttpDownloader)
    private
    protected
      PlayListItemRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
      function GetPlayListItemName(Match: IMatch; Index: integer): string; virtual;
      function GetPlayListItemURL(Match: IMatch; Index: integer): string; virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

{ TPlaylistDownloader }

constructor TPlaylistDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TPlaylistDownloader.Destroy;
begin
  PlayListItemRegExp := nil;
  inherited;
end;

class function TPlaylistDownloader.Provider: string;
begin
  Result := '-playlist-';
end;

class function TPlaylistDownloader.MovieIDParamName: string;
begin
  Result := 'PLAYLIST';
end;

class function TPlaylistDownloader.UrlRegExp: string;
begin
  Result := '^(?P<' + MovieIDParamName + '>https?://.+)$'
end;

function TPlaylistDownloader.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TPlaylistDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Urls: IMatchCollection;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  {$IFDEF MULTIDOWNLOADS}
  SetLastErrorMsg('No URLs specified.');
  if PlayListItemRegExp <> nil then
    begin
    Urls := PlayListItemRegExp.Matches(Page);
    if Urls.Count <= 0 then
      SetLastErrorMsg('No playlist URLs found.')
    else
      for i := 0 to Pred(Urls.Count) do
        begin
        UrlList.Add(GetPlayListItemURL(Urls[i], i));
        NameList.Add(GetPlayListItemName(Urls[i], i));
        end;
    end;
  {$ELSE}
  SetLastErrorMsg('Playlist downloader needs MULTIDOWNLOADS');
  {$ENDIF}
  Result := UrlList.Count > 0;
  if Result then
    SetPrepared(True);
end;

function TPlaylistDownloader.GetPlayListItemName(Match: IMatch; Index: integer): string;
begin
  Result := Match.Groups.ItemsByName['URL'].Value;
end;

function TPlaylistDownloader.GetPlayListItemURL(Match: IMatch; Index: integer): string;
begin
  Result := 'Playlist Item ' + IntToStr(Index);
end;

function TPlaylistDownloader.Download: boolean;
begin
  AddUrlToDownloadList(MovieURL);
  Result := True;
end;

end.
