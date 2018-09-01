unit uDownloadClassifier;

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
      procedure SetUrl(const Value: string); virtual;
      property UrlClassifier: IRegEx read fUrlClassifier write fUrlClassifier;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear; virtual;
      property Url: string read fUrl write SetUrl;
      property Downloader: TDownloader read fDownloader;
      property OwnsDownloader: boolean read fOwnsDownloader write fOwnsDownloader;
    end;

implementation

uses
  uCommonDownloader,
  uYouTubeDownloader,  // YouTube.com
  uNJoyDownloader,     // N-joy.cz
  uBlipTvDownloader,   // Blip.tv
  uBlipTvDownloaderV2, // Blip.tv
  uStreamDownloader,   // Stream.cz
  uYTDregexp;

{ TDownloadClassifier }

constructor TDownloadClassifier.Create;
begin
  inherited Create;
  fUrlClassifier := RegExCreate(SUPPORTED_URLS_REGEXP, [rcoIgnoreCase]);
  Clear;
end;

destructor TDownloadClassifier.Destroy;
begin
  Clear;
  fUrlClassifier := nil; // No idea how to actually free it
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
begin
  Clear;
  fUrl := Value;
  Match := UrlClassifier.Match(Value);
  try
    if Match.Matched then
      repeat
        with Match.Groups.ItemsByName['YOUTUBE'] do
          if Value <> '' then
            begin
            fDownloader := TYouTubeDownloader.Create(Value);
            Break;
            end;
        with Match.Groups.ItemsByName['NJOY'] do
          if Value <> '' then
            begin
            fDownloader := TNJoyDownloader.Create(Value);
            Break;
            end;
        with Match.Groups.ItemsByName['BLIPTV'] do
          if Value <> '' then
            begin
            fDownloader := TBlipTvDownloader.Create(Value);
            Break;
            end;
        with Match.Groups.ItemsByName['BLIPTVV2'] do
          if Value <> '' then
            begin
            fDownloader := TBlipTvDownloaderV2.Create(Value);
            Break;
            end;
        with Match.Groups.ItemsByName['STREAM'] do
          if Value <> '' then
            begin
            fDownloader := TStreamDownloader.Create(Value);
            Break;
            end;
      until True;
  finally
    Match := nil;
    end;
end;

end.
