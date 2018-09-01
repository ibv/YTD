unit uRtmpDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader, uExternalDownloader,
  RtmpDump_DLL;

type
  ERtmpDownloaderError = class(EExternalDownloaderError);

  TRtmpDownloader = class(TExternalDownloader)
    private
      fRtmpDumpOptions: TRtmpDumpOptions;
    protected
      procedure ClearRtmpDumpOptions; virtual;
      procedure AddRtmpDumpOption(ShortOption: char; const Argument: string = ''); virtual;
      procedure OnRtmpDownloadProgress(DownloadedSize: integer; PercentDone: double; var DoAbort: integer); virtual;
      property RtmpDumpOptions: TRtmpDumpOptions read fRtmpDumpOptions;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
    end;

implementation

uses
  uMessages;
  
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

procedure TRtmpDownloader.ClearRtmpDumpOptions;
begin
  SetLength(fRtmpDumpOptions, 0);
end;

procedure TRtmpDownloader.AddRtmpDumpOption(ShortOption: char; const Argument: string);
var n: integer;
begin
  n := Length(fRtmpDumpOptions);
  SetLength(fRtmpDumpOptions, Succ(n));
  fRtmpDumpOptions[n].ShortOption := ShortOption;
  fRtmpDumpOptions[n].Argument := Argument;
end;

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

function TRtmpDownloader.Prepare: boolean;
begin
  ClearRtmpDumpOptions;
  Result := inherited Prepare;
end;

function TRtmpDownloader.Download: boolean;
var LogFileName: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  if Options.ProxyHost <> '' then
    AddRtmpDumpOption('S', Options.ProxyHost + ':' + Options.ProxyPort);
  AddRtmpDumpOption('o', FileName);
  LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
  if FileExists(LogFileName) then
    DeleteFile(PChar(LogFileName));
  SetLastErrorMsg(Format(ERR_SEE_LOGFILE, [LogFileName]));
  if not RtmpDump_Init then
    Raise ERTMPDownloaderError.CreateFmt(ERR_FAILED_TO_LOAD_DLL, ['rtmpdump_dll.dll']);
  RetCode := RtmpDump_Download(Integer(Self), RtmpDumpDownloadProgressCallback, PChar(LogFileName), RtmpDumpOptions);
  case RetCode of
    0: // Download complete
         Result := True;
    2: // Incomplete download
         Result := (100*DownloadedBytes div TotalBytes) > 96; // May report incomplete even though it is not
    end;
end;

end.
