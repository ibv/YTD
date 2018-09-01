unit uDownloadThread;

interface

uses
  SysUtils, Classes,
  uDownloader;

type
  TDownloadThread = class;

  EDownloadThreadError = class(Exception);

  TDownloadThreadState = (dtsWaiting, dtsPreparing, dtsDownloading, dtsFinished, dtsFailed, dtsAborted);

  TDTStateChangeEvent = procedure(Sender: TDownloadThread; State: TDownloadThreadState) of object;
  TDTDownloadProgressEvent = procedure(Sender: TDownloadThread; TotalSize, DownloadedSize: int64) of object;
  TDTExceptionEvent = procedure(Sender: TDownloadThread; Error: Exception) of object;

  TDownloadThread = class(TThread)
    private
      fDownloader: TDownloader;
      fState: TDownloadThreadState;
      fOnStateChange: TDTStateChangeEvent;
      fOnDownloadProgress: TDTDownloadProgressEvent;
      fOnException: TDTExceptionEvent;
    protected
      procedure SetState(Value: TDownloadThreadState); virtual;
      procedure ReportState; virtual;
    protected
      CallersDownloaderProgressEvent: TDownloaderProgressEvent;
      ReportSender: TObject;
      ReportTotalSize: int64;
      ReportDownloadedSize: int64;
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
      procedure ReportProgress; virtual;
    protected
      ReportException: Exception;
      procedure ReportError; virtual;
    protected
      procedure Execute; override;
      property Downloader: TDownloader read fDownloader;
      property State: TDownloadThreadState read fState write SetState;
    public
      constructor Create(ADownloader: TDownloader; CreateSuspended: Boolean); virtual;
      destructor Destroy; override;
    published
      property OnStateChange: TDTStateChangeEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TDTDownloadProgressEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnException: TDTExceptionEvent read fOnException write fOnException;
    end;

implementation

{ TDownloadThread }

constructor TDownloadThread.Create(ADownloader: TDownloader; CreateSuspended: Boolean);
begin
  fDownloader := ADownloader;
  fState := dtsWaiting;
  inherited Create(CreateSuspended);
end;

destructor TDownloadThread.Destroy;
begin
  inherited;
end;

procedure TDownloadThread.Execute;
begin
  try
    CallersDownloaderProgressEvent := Downloader.OnProgress;
    try
      Downloader.OnProgress := DownloaderProgress;
      repeat
        if Terminated then
          Break;
        State := dtsPreparing;
        if not Downloader.Prepare then
          Raise EDownloadThreadError.Create('Failed to prepare a download.');
        if Terminated then
          Break;
        State := dtsDownloading;
        if not Downloader.Download then
          if Terminated then
            Break
          else
            Raise EDownloadThreadError.Create('Download failed.');
        if Terminated then
          Break;
        State := dtsFinished;
      until True;
      if Terminated then
        begin
        State := dtsAborted;
        Downloader.AbortTransfer;
        end;
    finally
      Downloader.OnProgress := CallersDownloaderProgressEvent;
      end;
  except
    on E: Exception do
      try
        ReportException := E;
        if Assigned(OnException) then
          Synchronize(ReportError);
      finally
        State := dtsFailed;
        end;
    end;
end;

procedure TDownloadThread.SetState(Value: TDownloadThreadState);
begin
  fState := Value;
  if Assigned(OnStateChange) then
    Synchronize(ReportState);
end;

procedure TDownloadThread.ReportState;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self, Self.State);
end;

procedure TDownloadThread.DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean);
begin
  if Assigned(CallersDownloaderProgressEvent) or Assigned(OnDownloadProgress) then
    begin
    ReportSender := Sender;
    ReportTotalSize := TotalSize;
    ReportDownloadedSize := DownloadedSize;
    Synchronize(ReportProgress);
    end;
  if Terminated then
    DoAbort := True;
end;

procedure TDownloadThread.ReportProgress;
var DoAbort: boolean;
begin
  DoAbort := False;
  if Assigned(CallersDownloaderProgressEvent) then
    CallersDownloaderProgressEvent(ReportSender, ReportTotalSize, ReportDownloadedSize, DoAbort);
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self, ReportTotalSize, ReportDownloadedSize);
  if DoAbort then
    Terminate;
end;

procedure TDownloadThread.ReportError;
begin
  if Assigned(OnException) then
    OnException(Self, ReportException);
end;

end.
