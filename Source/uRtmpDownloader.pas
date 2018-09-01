unit uRtmpDownloader;

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader, uExternalDownloader,
  RtmpDump_DLL;

type
  ERtmpDownloaderError = class(EExternalDownloaderError);

  TRtmpDownloader = class(TExternalDownloader)
    private
      fRtmpUrl: string;
      fRtmpPlayPath: string;
    protected
      procedure OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer); virtual;
      property RtmpUrl: string read fRtmpUrl write fRtmpUrl;
      property RtmpPlayPath: string read fRtmpPlayPath write fRtmpPlayPath;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
      //function Prepare: boolean; override;
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
  if Aborted then
    DoAbort := 1
  else
    DoAbort := 0;
end;

function TRtmpDownloader.Download: boolean;
var LogFileName, VideoFileName, Url, PlayPath: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  if (RtmpUrl <> '') and (RtmpPlayPath <> '') then
    begin
    LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
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

end.
