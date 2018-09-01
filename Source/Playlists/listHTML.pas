unit listHTML;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader,
  uDownloadClassifier;

type
  TPlaylist_HTML = class(TPlaylistDownloader)
    private
      fClassifier: TDownloadClassifier;
    protected
      function GetUrlRegExp: string; virtual;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
      property Classifier: TDownloadClassifier read fClassifier;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

const
  REGEXP_URL = '(?:href|src)=(["''])(?P<URL>https?://.*?)\1';

{ TPlaylist_HTML }

constructor TPlaylist_HTML.Create(const AMovieID: string);
begin
  inherited;
  PlaylistItemRegExp := RegExCreate(GetUrlRegExp);
  fClassifier := TDownloadClassifier.Create;
end;

destructor TPlaylist_HTML.Destroy;
begin
  RegExFreeAndNil(PlaylistItemRegExp);
  FreeAndNil(fClassifier);
  inherited;
end;

function TPlaylist_HTML.GetUrlRegExp: string;
begin
  Result := REGEXP_URL;
end;

function TPlaylist_HTML.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := inherited GetPlayListItemURL(Match, Index);
  Result := HtmlDecode(Result);
  Classifier.Url := Result;
  if Classifier.Downloader = nil then
    Result := '';
end;

end.
