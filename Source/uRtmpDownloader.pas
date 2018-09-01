unit uRtmpDownloader;

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader,
  RtmpDump_DLL;

type
  ERtmpDownloaderError = class(EDownloaderError);
  
  TRtmpDownloader = class(TCommonDownloader)
    private
      fRtmpUrl: string;
      fRtmpPlayPath: string;
      fAborted: boolean;
      fDownloadedBytes: int64;
      fTotalBytes: int64;
    protected
      procedure OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer); virtual;
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      procedure SetPrepared(Value: boolean); override;
      property RtmpUrl: string read fRtmpUrl write fRtmpUrl;
      property RtmpPlayPath: string read fRtmpPlayPath write fRtmpPlayPath;
      property DownloadedBytes: int64 read fDownloadedBytes write fDownloadedBytes;
      property TotalBytes: int64 read fTotalBytes write fTotalBytes;
      property Aborted: boolean read fAborted write fAborted;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
      //function Prepare: boolean; override;
      procedure AbortTransfer; override;
    end;

implementation

procedure RtmpDumpDownloadProgressCallback(Tag: integer; DownloadedSize: integer; PercentDone: double; var DoAbort: integer); cdecl;
begin
  TRtmpDownloader(Tag).OnRtmpDownloadProgress(DownloadedSize, PercentDone, DoAbort);
end;

{ TRtmpDownloader }

constructor TRtmpDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TRtmpDownloader.Destroy;
begin
  inherited;
end;

{function TRtmpDownloader.DownloadStream(const Url, PlayPath, FileName: string): boolean;
begin
end;}

procedure TRtmpDownloader.OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer);
begin
  DownloadedBytes := DownloadedSize;
  if PercentDone >= 99.9 then
    TotalBytes := DownloadedSize
  else if PercentDone > 0 then
    TotalBytes := Trunc(int64(DownloadedSize) * (100 / PercentDone))
  else
    TotalBytes := -1;
  DoProgress;
  if fAborted then
    DoAbort := 1
  else
    DoAbort := 0;
end;

function TRtmpDownloader.Download: boolean;
var TempDir, LogFileName, VideoFileName, Url, PlayPath: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  if (RtmpUrl <> '') and (RtmpPlayPath <> '') then
    begin
    SetLength(TempDir, 1024);
    SetLength(TempDir, GetTempPath(1024, @(TempDir[1])));
    if TempDir <> '' then
      TempDir := IncludeTrailingBackslash(TempDir);
    LogFileName := TempDir + ExtractFileName(FileName) + '.log';
    if FileExists(LogFileName) then
      DeleteFile(PChar(LogFileName));
    SetLastErrorMsg('See error log in "' + LogFileName + '"');
    VideoFileName := FileName;
    Url := RtmpUrl;
    PlayPath := RtmpPlayPath;
    if not RtmpDump_Init then
      Raise ERtmpDownloaderError.Create('Failed to load rtmpdump_dll.dll or one of its prerequisites (ssleay32-0.9.8.dll, cryptoeay32-0.9.8.dll)');
    RetCode := RtmpDump_Download(Integer(Self), RtmpDumpDownloadProgressCallback, PChar(LogFileName), PChar(VideoFileName), PChar(Url), PChar(PlayPath));
    case RetCode of
      0: // Download complete
           Result := True;
      2: // Incomplete download
           Result := (100*DownloadedBytes div TotalBytes) > 96; // May report incomplete even though it is not
      end;
    end;
end;

procedure TRtmpDownloader.AbortTransfer;
begin
  inherited;
  Aborted := True;
end;

function TRtmpDownloader.GetDownloadedSize: int64;
begin
  Result := DownloadedBytes;
end;

function TRtmpDownloader.GetTotalSize: int64;
begin
  Result := TotalBytes;
end;

procedure TRtmpDownloader.SetPrepared(Value: boolean);
begin
  inherited;
  DownloadedBytes := 0;
  TotalBytes := -1;
end;

end.
