unit uDownloadClassifier;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE,
  uDownloader;

type
  TDownloadClassifier = class
    private
      fUrlClassifier: IRegEx;
      fUrl: string;
      fDownloader: TDownloader;
      fOwnsDownloader: boolean;
    protected
      function GetProviderCount: integer; virtual;
      function GetProviders(Index: integer): TDownloaderClass; virtual;
      procedure SetUrl(const Value: string); virtual;
      property UrlClassifier: IRegEx read fUrlClassifier write fUrlClassifier;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear; virtual;
      property Url: string read fUrl write SetUrl;
      property Downloader: TDownloader read fDownloader;
      property OwnsDownloader: boolean read fOwnsDownloader write fOwnsDownloader;
      property ProviderCount: integer read GetProviderCount;
      property Providers[Index: integer]: TDownloaderClass read GetProviders;
    end;

procedure RegisterDownloader(Downloader: TDownloaderClass);

implementation

var RegisteredDownloaders: TList;

procedure RegisterDownloader(Downloader: TDownloaderClass);
begin
  RegisteredDownloaders.Add(Downloader);
end;

{ TDownloadClassifier }

constructor TDownloadClassifier.Create;
var RE, s: string;
    i: integer;
begin
  inherited Create;
  RE := '';
  for i := 0 to Pred(RegisteredDownloaders.Count) do
    begin
    s := TDownloaderClass(RegisteredDownloaders[i]).UrlRegExp;
    if s <> '' then
      begin
      s := '(?:' + s + ')';
      if RE = '' then
        RE := s
      else
        RE := RE + '|' + s;
      end;
    end;
  fUrlClassifier := RegExCreate(RE, [rcoIgnoreCase]);
  Clear;
end;

destructor TDownloadClassifier.Destroy;
begin
  Clear;
  fUrlClassifier := nil;
  inherited;
end;

procedure TDownloadClassifier.Clear;
begin
  fUrl := '';
  if OwnsDownloader then
    FreeAndNil(fDownloader)
  else
    fDownloader := nil;
end;

procedure TDownloadClassifier.SetUrl(const Value: string);
var Match: IMatch;
    DC: TDownloaderClass;
    i: integer;
begin
  Clear;
  fUrl := Value;
  Match := UrlClassifier.Match(Value);
  try
    if Match.Matched then
      for i := 0 to Pred(ProviderCount) do
        begin
        DC := Providers[i];
        with Match.Groups.ItemsByName[DC.MovieIDParamName] do
          if Value <> '' then
            begin
            fDownloader := DC.Create(Value);
            Break;
            end;
        end;
  finally
    Match := nil;
    end;
end;

function TDownloadClassifier.GetProviderCount: integer;
begin
  Result := RegisteredDownloaders.Count;
end;

function TDownloadClassifier.GetProviders(Index: integer): TDownloaderClass;
begin
  Result := TDownloaderClass(RegisteredDownloaders[Index]);
end;

initialization
  RegisteredDownloaders := TList.Create;

finalization
  RegisteredDownloaders.Free;
  RegisteredDownloaders := nil;

end.
