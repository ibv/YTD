unit uDownloadListItem;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uDownloader, uDownloadThread;

type
  TDownloadListItem = class
    private
      fDownloaderOwned: boolean;
      fDownloader: TDownloader;
      fThread: TDownloadThread;
      fState: TDownloadThreadState;
      fTotalSize, fDownloadedSize: int64;
      fErrorClass: string;
      fErrorMessage: string;
      fOnDownloadProgress: TNotifyEvent;
      fOnError: TNotifyEvent;
      fOnStateChange: TNotifyEvent;
      fOnThreadFinished: TNotifyEvent;
      fTag: integer;
    protected
      procedure SetState(Value: TDownloadThreadState); virtual;
      procedure SetTotalSize(Value: int64); virtual;
      procedure SetDownloadedSize(Value: int64); virtual;
      procedure SetErrorClass(const Value: string); virtual;
      procedure SetErrorMessage(const Value: string); virtual;
      function GetWaiting: boolean; virtual;
      function GetDownloading: boolean; virtual;
      function GetFinished: boolean; virtual;
      function GetPaused: boolean; virtual;
      procedure CreateThread; virtual;
      procedure ClearThread; virtual;
      procedure InitStatus; virtual;
      procedure ThreadStateChange(Sender: TDownloadThread; State: TDownloadThreadState); virtual;
      procedure ThreadDownloadProgress(Sender: TDownloadThread; TotalSize, DownloadedSize: int64); virtual;
      procedure ThreadException(Sender: TDownloadThread; Error: Exception); virtual;
      procedure ThreadTerminate(Sender: TObject); virtual;
      property Thread: TDownloadThread read fThread;
    public
      constructor Create(ADownloader: TDownloader; ADownloaderOwned: boolean = True); virtual;
      destructor Destroy; override;
      procedure Start; virtual;
      procedure Stop; virtual;
      procedure Pause; virtual;
      function Download: boolean; virtual;
      property Downloader: TDownloader read fDownloader;
      property State: TDownloadThreadState read fState;
      property TotalSize: int64 read fTotalSize;
      property DownloadedSize: int64 read fDownloadedSize;
      property ErrorClass: string read fErrorClass;
      property ErrorMessage: string read fErrorMessage;
      property Waiting: boolean read GetWaiting;
      property Downloading: boolean read GetDownloading;
      property Finished: boolean read GetFinished;
      property Paused: boolean read GetPaused;
    published
      property Tag: integer read fTag write fTag;
      property DownloaderOwned: boolean read fDownloaderOwned write fDownloaderOwned;
      property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TNotifyEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnError: TNotifyEvent read fOnError write fOnError;
      property OnThreadFinished: TNotifyEvent read fOnThreadFinished write fOnThreadFinished;
    end;

implementation

{ TDownloadListItem }

constructor TDownloadListItem.Create(ADownloader: TDownloader; ADownloaderOwned: boolean);
begin
  inherited Create;
  fDownloader := ADownloader;
  fDownloaderOwned := ADownloaderOwned;
  fThread := nil;
  InitStatus;
end;

destructor TDownloadListItem.Destroy;
begin
  Stop;
  if DownloaderOwned then
    FreeAndNil(fDownloader);
  inherited;
end;

procedure TDownloadListItem.InitStatus;
begin
  SetState(dtsWaiting);
  SetTotalSize(-1);
  SetDownloadedSize(0);
  SetErrorClass('');
  SetErrorMessage('');
end;

procedure TDownloadListItem.SetState(Value: TDownloadThreadState);
begin
  fState := Value;
end;

procedure TDownloadListItem.SetTotalSize(Value: int64);
begin
  fTotalSize := Value;
end;

procedure TDownloadListItem.SetDownloadedSize(Value: int64);
begin
  fDownloadedSize := Value;
end;

procedure TDownloadListItem.SetErrorClass(const Value: string);
begin
  fErrorClass := Value;
end;

procedure TDownloadListItem.SetErrorMessage(const Value: string);
begin
  fErrorMessage := Value;
end;

function TDownloadListItem.GetWaiting: boolean;
begin
  Result := State in [dtsWaiting];
end;

function TDownloadListItem.GetDownloading: boolean;
begin
  Result := State in [dtsPreparing, dtsDownloading];
end;

function TDownloadListItem.GetFinished: boolean;
begin
  Result := State in [dtsFinished, dtsFailed, dtsAborted];
end;

function TDownloadListItem.GetPaused: boolean;
begin
  Result := (Thread <> nil) and (Thread.Suspended);
end;

procedure TDownloadListItem.CreateThread;
begin
  fThread := TDownloadThread.Create(Downloader, True);
  Thread.OnStateChange := ThreadStateChange;
  Thread.OnDownloadProgress := ThreadDownloadProgress;
  Thread.OnException := ThreadException;
  Thread.OnTerminate := ThreadTerminate;
  Thread.FreeOnTerminate := False;
  InitStatus;
end;

procedure TDownloadListItem.ClearThread;
begin
  FreeAndNil(fThread);
end;

procedure TDownloadListItem.Start;
begin
  if Thread = nil then
    CreateThread;
  if Thread.Suspended then
    Thread.Resume;
end;

procedure TDownloadListItem.Stop;
begin
  if Thread <> nil then
    begin
    if Paused then
      Thread.Resume;
    Thread.Terminate;
    Thread.WaitFor;
    end;
end;

procedure TDownloadListItem.Pause;
begin
  if Thread <> nil then
    Thread.Suspend;
end;

function TDownloadListItem.Download: boolean;
begin
  Start;
  Thread.WaitFor;
  Result := (State = dtsFinished);
end;

procedure TDownloadListItem.ThreadDownloadProgress(Sender: TDownloadThread; TotalSize, DownloadedSize: int64);
begin
  SetTotalSize(TotalSize);
  SetDownloadedSize(DownloadedSize);
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self);
end;

procedure TDownloadListItem.ThreadException(Sender: TDownloadThread; Error: Exception);
begin
  SetErrorClass(Error.ClassName);
  SetErrorMessage(Error.Message);
  if Assigned(OnError) then
    OnError(Self);
end;

procedure TDownloadListItem.ThreadStateChange(Sender: TDownloadThread; State: TDownloadThreadState);
begin
  SetState(State);
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TDownloadListItem.ThreadTerminate(Sender: TObject);
begin
  ClearThread;
  if Assigned(OnThreadFinished) then
    OnThreadFinished(Self);
end;

end.
