unit uPlaylistDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader;

type
  TPlaylistDownloader = class(TCommonDownloader)
    private
      fUrlList: TStringList;
      fNameList: TStringList;
    protected
      PlayListItemRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function GetPlayListItemName(Match: IMatch; Index: integer): string; virtual;
      function GetPlayListItemURL(Match: IMatch; Index: integer): string; virtual;
      function GetItemCount: integer; virtual;
      function GetItemUrl(Index: integer): string; virtual;
      function GetItemName(Index: integer): string; virtual;
      property UrlList: TStringList read fUrlList;
      property NameList: TStringList read fNameList;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      property Count: integer read GetItemCount;
      property Urls[Index: integer]: string read GetItemUrl; default;
      property Names[Index: integer]: string read GetItemName;
    end;

implementation

{ TPlaylistDownloader }

constructor TPlaylistDownloader.Create(const AMovieID: string);
begin
  inherited;
  fUrlList := TStringList.Create;
  fNameList := TStringList.Create;
end;

destructor TPlaylistDownloader.Destroy;
begin
  FreeAndNil(fUrlList);
  FreeAndNil(fNameList);
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

function TPlaylistDownloader.GetItemCount: integer;
begin
  Result := UrlList.Count;
end;

function TPlaylistDownloader.GetItemUrl(Index: integer): string;
begin
  Result := UrlList[Index];
end;

function TPlaylistDownloader.GetItemName(Index: integer): string;
begin
  Result := NameList[Index];
end;

function TPlaylistDownloader.GetPlayListItemName(Match: IMatch; Index: integer): string;
begin
  Result := 'Playlist Item ' + IntToStr(Index);
end;

function TPlaylistDownloader.GetPlayListItemURL(Match: IMatch; Index: integer): string;
begin
  Result := Match.Groups.ItemsByName['URL'].Value;
end;

function TPlayListDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Urls: IMatchCollection;
    Url: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  SetLastErrorMsg('No URLs specified.');
  if PlayListItemRegExp <> nil then
    begin
    Urls := PlayListItemRegExp.Matches(Page);
    if Urls.Count <= 0 then
      SetLastErrorMsg('No playlist URLs found.')
    else
      for i := 0 to Pred(Urls.Count) do
        begin
        Url := GetPlayListItemURL(Urls[i], i);
        if (Url <> '') and (UrlList.IndexOf(Url) < 0) then
          begin
          UrlList.Add(Url);
          NameList.Add(GetPlayListItemName(Urls[i], i));
          end;
        end;
    end;
  Result := UrlList.Count > 0;
  if Result then
    SetPrepared(True);
end;

end.
