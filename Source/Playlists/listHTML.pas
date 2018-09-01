unit listHTML;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader,
  uDownloadClassifier;

type
  TPlaylist_HTML = class(TPlaylistDownloader)
    private
      fClassifier: TDownloadClassifier;
    protected
      function GetPlayListItemURL(Match: IMatch; Index: integer): string; override;
      property Classifier: TDownloadClassifier read fClassifier;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

//const URL_REGEXP = '(?:href|src)=(["'']?)(?P<URL>https?://[a-z0-9./?&%+-]+)\1';
const URL_REGEXP = '(?:href|src)=(["''])(?P<URL>https?://.*?)\1';

{ TPlaylist_HTML }

constructor TPlaylist_HTML.Create(const AMovieID: string);
begin
  inherited;
  PlaylistItemRegExp := RegExCreate(URL_REGEXP);
  fClassifier := TDownloadClassifier.Create;
end;

destructor TPlaylist_HTML.Destroy;
begin
  PlaylistItemRegExp := nil;
  FreeAndNil(fClassifier);
  inherited;
end;

function TPlaylist_HTML.GetPlayListItemURL(Match: IMatch; Index: integer): string;
begin
  Result := inherited GetPlayListItemURL(Match, Index);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  Classifier.Url := Result;
  if Classifier.Downloader = nil then
    Result := '';
end;

end.
