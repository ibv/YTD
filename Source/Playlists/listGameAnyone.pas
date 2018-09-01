unit listGameAnyone;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader;

type
  TPlaylist_GameAnyone = class(TPlaylistDownloader)
    private
    protected
      function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; override;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

// http://www.gameanyone.com/game/PC/936.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*gameanyone\.com/game/[^/]+/';
  URLREGEXP_ID =        '[0-9]+\.html?';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PLAYLIST_ITEM = '<a\s+href="https?://(?:[a-z0-9-]+\.)*gameanyone\.com/video/(?P<ID>[0-9]+)">(?P<NAME>[^<]+)</a>\s+\([0-9]+:[0-9]{2}\)';

{ TPlaylist_GameAnyone }

class function TPlaylist_GameAnyone.Provider: string;
begin
  Result := 'GameAnyone.com';
end;

class function TPlaylist_GameAnyone.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_GameAnyone.Create(const AMovieID: string);
begin
  inherited;
  PlayListItemRegExp := RegExCreate(REGEXP_PLAYLIST_ITEM, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TPlaylist_GameAnyone.Destroy;
begin
  RegExFreeAndNil(PlayListItemRegExp);
  inherited;
end;

function TPlaylist_GameAnyone.GetMovieInfoUrl: string;
begin
  Result := 'http://www.gameanyone.com/game/dummy/' + MovieID;
end;

function TPlaylist_GameAnyone.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Trim(Match.SubexpressionByName('NAME'));
end;

function TPlaylist_GameAnyone.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := 'http://www.gameanyone.com/video/' + Match.SubexpressionByName('ID');
end;

initialization
  RegisterDownloader(TPlaylist_GameAnyone);

end.
