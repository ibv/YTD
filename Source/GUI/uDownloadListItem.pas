unit uDownloadListItem;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, ShellApi, 
  uDownloader, uDownloadThread,
  {$IFDEF CONVERTERS}
  uCreateProcessAsync,
  {$ENDIF}
  uOptions;

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
      fOnFileNameValidate: TDownloaderFileNameValidateEvent;
      fOnError: TNotifyEvent;
      fOnStateChange: TNotifyEvent;
      fOnThreadFinished: TNotifyEvent;
      fTag: integer;
      fOptions: TYTDOptions;
      {$IFDEF CONVERTERS}
      fConvertState: TConvertThreadState;
      fConvertThread: TThread;
      fConvertHandle: THandle;
      fOnConvertThreadFinished: TNotifyEvent;
      {$ENDIF}
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
      procedure ThreadFileNameValidate(Sender: TDownloadThread; var FileName: string; var Valid: boolean); virtual;
      procedure ThreadException(Sender: TDownloadThread; Error: Exception); virtual;
      procedure ThreadTerminate(Sender: TObject); virtual;
      property Thread: TDownloadThread read fThread;
      {$IFDEF CONVERTERS}
      procedure SetConvertState(Value: TConvertThreadState); virtual;
      procedure ConvertFinishedEvent(Sender: TThread; hProcess, hThread: THandle; ResultCode: DWORD); virtual;
      property ConvertThread: TThread read fConvertThread write fConvertThread;
      property ConvertHandle: THandle read fConvertHandle write fConvertHandle;
      {$ENDIF}
    public
      constructor Create(ADownloader: TDownloader; ADownloaderOwned: boolean = True); virtual;
      destructor Destroy; override;
      procedure Start; virtual;
      procedure Stop; virtual;
      procedure Pause; virtual;
      procedure PlayMedia; virtual;
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
      {$IFDEF CONVERTERS}
      function Convert(Force: boolean = False; const ForceConverter: string = ''): boolean; virtual;
      property ConvertState: TConvertThreadState read fConvertState;
      {$ENDIF}
    published
      property Tag: integer read fTag write fTag;
      property Options: TYTDOptions read fOptions write fOptions;
      property DownloaderOwned: boolean read fDownloaderOwned write fDownloaderOwned;
      property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TNotifyEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnFileNameValidate: TDownloaderFileNameValidateEvent read fOnFileNameValidate write fOnFileNameValidate;
      property OnError: TNotifyEvent read fOnError write fOnError;
      property OnThreadFinished: TNotifyEvent read fOnThreadFinished write fOnThreadFinished;
      {$IFDEF CONVERTERS}
      property OnConvertThreadFinished: TNotifyEvent read fOnConvertThreadFinished write fOnConvertThreadFinished;
      {$ENDIF}
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
  {$IFDEF CONVERTERS}
  if ConvertThread <> nil then
    begin
    ConvertThread.Terminate;
    ConvertThread := nil;
    end;
  if ConvertHandle <> 0 then
    begin
    UnregisterWait(ConvertHandle);
    ConvertHandle := 0;
    end;
  {$ENDIF}
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
  {$IFDEF CONVERTERS}
  SetConvertState(ctsWaiting);
  {$ENDIF}
end;

procedure TDownloadListItem.SetState(Value: TDownloadThreadState);
begin
  fState := Value;
end;

{$IFDEF CONVERTERS}
procedure TDownloadListItem.SetConvertState(Value: TConvertThreadState);
begin
  fConvertState := Value;
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TDownloadListItem.ConvertFinishedEvent(Sender: TThread; hProcess, hThread: THandle; ResultCode: DWORD);
begin
  ConvertThread := nil;
  ConvertHandle := 0;
  if ResultCode = 0 then
    SetConvertState(ctsFinished)
  else
    SetConvertState(ctsFailed);
  if Assigned(OnConvertThreadFinished) then
    OnConvertThreadFinished(Self);
end;

function TDownloadListItem.Convert(Force: boolean; const ForceConverter: string): boolean;
var Converter: TConverter;
    ID, CommandLine, MediaFile: string;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    WaitThread: TThread;
    WaitHandle: THandle;
begin
  Result := False;
  if Force or (State = dtsFinished) then
    if (ConvertState = ctsWaiting) or Force then
      begin
      if Force and (ForceConverter <> '') then
        ID := ForceConverter
      else
        ID := Options.SelectedConverterID;
      if Options.ReadConverter(ID, Converter) then
        begin
        MediaFile := ExpandFileName(Downloader.Options.DestinationPath + Downloader.FileName);
        CommandLine := '"' + Converter.ExePath + '" ' + Converter.CommandLine;
        CommandLine := StringReplace(CommandLine, '{$FULLPATH}', MediaFile, [rfReplaceAll, rfIgnoreCase]);
        CommandLine := StringReplace(CommandLine, '{$FILENAME}', ExtractFileName(MediaFile), [rfReplaceAll, rfIgnoreCase]);
        CommandLine := StringReplace(CommandLine, '{$FILEPATH}', ExtractFilePath(MediaFile), [rfReplaceAll, rfIgnoreCase]);
        CommandLine := StringReplace(CommandLine, '{$FILEEXT}', ExtractFileExt(MediaFile), [rfReplaceAll, rfIgnoreCase]);
        CommandLine := StringReplace(CommandLine, '{$FILENOEXT}', ChangeFileExt(ExtractFileName(MediaFile), ''), [rfReplaceAll, rfIgnoreCase]);
        CommandLine := StringReplace(CommandLine, '{$TITLE}', Downloader.Name, [rfReplaceAll, rfIgnoreCase]);
        CommandLine := Trim(CommandLine);
        FillChar(StartupInfo, Sizeof(StartupInfo), 0);
        StartupInfo.cb := Sizeof(StartupInfo);
        StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
        case Converter.Visibility of
          cvVisible:
            StartupInfo.wShowWindow := SW_SHOWNORMAL;
          cvMinimized:
            StartupInfo.wShowWindow := SW_SHOWMINNOACTIVE;
          cvHidden:
            StartupInfo.wShowWindow := SW_HIDE;
          end;
        if CreateProcessAsync(
          nil, // Application name
          PChar(CommandLine), // Command line
          nil, // Process attributes
          nil, // Thread attributes
          False, // Inherit handles
          CREATE_NEW_CONSOLE {$IFDEF UNICODE} or CREATE_UNICODE_ENVIRONMENT {$ENDIF} , // Creation flags
          nil, // Environment
          nil, // Current directory
          StartupInfo, ProcessInfo, WaitThread, WaitHandle, ConvertFinishedEvent)
        then
          begin
          ConvertThread := WaitThread;
          ConvertHandle := WaitHandle;
          SetConvertState(ctsConverting);
          Result := True;
          end
        else
          SetConvertState(ctsFailed);
        end;
      end;
end;
{$ENDIF}

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
  Downloader.Options := Options;
  fThread := TDownloadThread.Create(Downloader, True);
  Thread.OnStateChange := ThreadStateChange;
  Thread.OnDownloadProgress := ThreadDownloadProgress;
  Thread.OnFileNameValidate := ThreadFileNameValidate;
  Thread.OnException := ThreadException;
  Thread.OnTerminate := ThreadTerminate;
  Thread.FreeOnTerminate := True;
  InitStatus;
end;

procedure TDownloadListItem.ClearThread;
begin
  fThread := nil;
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

procedure TDownloadListItem.PlayMedia;
var FN: string;
begin
  if (State = dtsFinished) then
    begin
    FN := Downloader.Options.DestinationPath + Downloader.FileName;
    if FileExists(FN) then
      ShellExecute(0, 'open', PChar(FN), nil, nil, SW_SHOWNORMAL);
    end;
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

procedure TDownloadListItem.ThreadFileNameValidate(Sender: TDownloadThread; var FileName: string; var Valid: boolean);
begin
  if Assigned(OnFileNameValidate) then
    OnFileNameValidate(Self, FileName, Valid);
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
