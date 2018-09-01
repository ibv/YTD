unit uExternalDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader;

type
  EExternalDownloaderError = class(EDownloaderError);

  TExternalDownloader = class(TCommonDownloader)
    private
      fAborted: boolean;
      fDownloadedBytes: int64;
      fTotalBytes: int64;
    protected
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      procedure SetPrepared(Value: boolean); override;
      function GetTempDir: string; virtual;
      property DownloadedBytes: int64 read fDownloadedBytes write fDownloadedBytes;
      property TotalBytes: int64 read fTotalBytes write fTotalBytes;
      property Aborted: boolean read fAborted write fAborted;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      procedure AbortTransfer; override;
    end;

implementation

constructor TExternalDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TExternalDownloader.Destroy;
begin
  inherited;
end;

procedure TExternalDownloader.AbortTransfer;
begin
  inherited;
  Aborted := True;
end;

function TExternalDownloader.GetDownloadedSize: int64;
begin
  Result := DownloadedBytes;
end;

function TExternalDownloader.GetTotalSize: int64;
begin
  Result := TotalBytes;
end;

procedure TExternalDownloader.SetPrepared(Value: boolean);
begin
  inherited;
  DownloadedBytes := 0;
  TotalBytes := -1;
end;

function TExternalDownloader.GetTempDir: string;
const MAX_TEMP_PATH = MAX_PATH + 16;
begin
  SetLength(Result, MAX_TEMP_PATH);
  SetLength(Result, GetTempPath(MAX_TEMP_PATH, @(Result[1])));
  if Result <> '' then
    Result := IncludeTrailingBackslash(Result);
end;

end.
