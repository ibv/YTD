unit uDirectoryMonitor;

interface

uses
  SysUtils, Classes, Windows, Messages;

const
  WMUSER_DIRECTORYCHANGED = WM_USER + 1;

type
  TActionToWatch = (awChangeFileName, awChangeDirName, awChangeAttributes, awChangeSize, awChangeLastWrite, awChangeLastAccess, awChangeCreation, awChangeSecurity);
  TActionsToWatch = set of TActionToWatch;
  TDirectoryMonitorAction = (daUnknown, daFileAdded, daFileRemoved, daFileModified, daFileRenamedOldName, daFileRenamedNewName);
  TDirectoryChangeEvent = procedure(Sender: TObject; Action: TDirectoryMonitorAction; const FileName: string) of object;

const
  AllActions = [Low(TActionToWatch)..High(TActionToWatch)];

type
  TDirectoryMonitor = class;
  TDirectoryMonitorWorkerThread = class;

  TDirectoryMonitor = class
  private
    FHandle: THandle;
    FDirectory: string;
    FActions: TActionsToWatch;
    FSubdirectories: Boolean;
    FOnChange: TDirectoryChangeEvent;
    FWorkerThread: TDirectoryMonitorWorkerThread;
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetActions(const Value: TActionsToWatch);
    procedure SetDirectory(const Value: string);
    procedure SetSubdirectories(const Value: Boolean);
  protected
    procedure WndProc(var Msg: TMessage); virtual;
    procedure DoChange(Action: TDirectoryMonitorAction; const FileName: string); virtual;
    property Handle: THandle read FHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Directory: string read FDirectory write SetDirectory;
    property Actions: TActionsToWatch read FActions write SetActions default AllActions;
    property Subdirectories: Boolean read FSubdirectories write SetSubdirectories default True;
    property OnChange: TDirectoryChangeEvent read FOnChange write FOnChange;
    property Active: boolean read GetActive write SetActive;
  end;

  TDirectoryMonitorWorkerThread = class(TThread)
  private
    FDirectory: string;
    FActions: TActionsToWatch;
    FSubdirectories: Boolean;
    FOwnerHandle: THandle;
    FDirHandle: THandle;
    FChangeHandle: THandle;
    FShutdownHandle: THandle;
    function GetNotifyMask: DWORD;
    function GetNotifyAction(SystemAction: DWORD): TDirectoryMonitorAction;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(Owner: TDirectoryMonitor);
    destructor Destroy; override;
  end;

implementation

constructor TDirectoryMonitorWorkerThread.Create(Owner: TDirectoryMonitor);
begin
  inherited Create(False);
  FDirectory := IncludeTrailingPathDelimiter(Owner.FDirectory);
  FSubdirectories := Owner.Subdirectories;
  FActions := Owner.FActions;
  FOwnerHandle := Owner.Handle;
  FDirHandle := CreateFile(
                  PChar(FDirectory),
                  FILE_LIST_DIRECTORY,
                  FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                  nil,
                  OPEN_EXISTING,
                  FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
                  0
                );
  FChangeHandle := CreateEvent(nil, FALSE, FALSE, nil);
  FShutdownHandle := CreateEvent(nil, FALSE, FALSE, nil);
end;

destructor TDirectoryMonitorWorkerThread.Destroy;
begin
  Terminate;
  if FDirHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FDirHandle);
  if FChangeHandle <> 0 then
    CloseHandle(FChangeHandle);
  if FShutdownHandle <> 0 then
    CloseHandle(FShutdownHandle);
  inherited Destroy;
end;

function TDirectoryMonitorWorkerThread.GetNotifyAction(SystemAction: DWORD): TDirectoryMonitorAction;
begin
  case SystemAction of
    FILE_ACTION_ADDED:
      Result := daFileAdded;
    FILE_ACTION_REMOVED:
      Result := daFileRemoved;
    FILE_ACTION_MODIFIED:
      Result := daFileModified;
    FILE_ACTION_RENAMED_OLD_NAME:
      Result := daFileRenamedOldName;
    FILE_ACTION_RENAMED_NEW_NAME:
      Result := daFileRenamedNewName;
    else
      Result := daUnknown;
  end;
end;

procedure TDirectoryMonitorWorkerThread.Execute;
type
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..MAX_PATH] of WCHAR;
  end;
  PFileNotifyInformation = ^TFileNotifyInformation;
const
  BUFFER_SIZE = 65536;
var
  WaitResult, BytesRead: DWORD;
  Info: PFileNotifyInformation;
  Buffer: array[0..BUFFER_SIZE-1] of byte;
  Events: array[0..1] of THandle;
  Overlap: TOverlapped;
  Action: TDirectoryMonitorAction;
  FileName: string;
  FileNamePtr: PChar;
begin
  if FDirHandle <> INVALID_HANDLE_VALUE then begin
    FillChar(Overlap, SizeOf(Overlap), 0);
    Overlap.hEvent := FChangeHandle;
    Events[0] := FChangeHandle;
    Events[1] := FShutdownHandle;
    while not Terminated do begin
      if ReadDirectoryChangesW(FDirHandle, @Buffer[0], BUFFER_SIZE, FSubdirectories, GetNotifyMask, @BytesRead, @Overlap, nil) then begin
        WaitResult := WaitForMultipleObjects(2, @events[0], FALSE, INFINITE);
        if WaitResult = WAIT_OBJECT_0 then
        begin
           Info := @Buffer[0];
          repeat
            Action := GetNotifyAction(Info^.Action);
            FileName := WideCharLenToString(@Info^.FileName[0], Info^.FileNameLength);
            FileName := FDirectory + string(PChar(FileName));
            FileNamePtr := StrNew(PChar(FileName));
            if not PostMessage(FOwnerHandle, WMUSER_DIRECTORYCHANGED, WPARAM(Action), LPARAM(FileNamePtr)) then
              StrDispose(FileNamePtr);
            if Info.NextEntryOffset = 0 then
              Break
            else
              Inc(PByte(Info), Info.NextEntryOffset);
          until Terminated;
        end;
      end;
    end;
  end;
end;

function TDirectoryMonitorWorkerThread.GetNotifyMask: DWORD;
begin
  Result := 0;
  if awChangeFileName in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if awChangeDirName in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if awChangeSize in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if awChangeAttributes in FActions then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if awChangeLastWrite in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if awChangeSecurity in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  if awChangeLastAccess in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if awChangeCreation in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
end;

procedure TDirectoryMonitorWorkerThread.TerminatedSet;
begin
  if FShutdownHandle <> 0 then
    SetEvent(FShutdownHandle);
  inherited;
end;

constructor TDirectoryMonitor.Create;
begin
  inherited Create;
  FHandle := AllocateHwnd(WndProc);
  FDirectory := '';
  FActions := [Low(TActionToWatch)..High(TActionToWatch)];
  FSubdirectories := True;
  FWorkerThread := nil;
end;

destructor TDirectoryMonitor.Destroy;
begin
  FOnChange := nil;
  Stop;
  DeallocateHwnd(FHandle);
  inherited Destroy;
end;

procedure TDirectoryMonitor.DoChange(Action: TDirectoryMonitorAction; const FileName: string);
begin
  if Assigned(OnChange) then
    OnChange(Self, Action, FileName);
end;

function TDirectoryMonitor.GetActive: boolean;
begin
  Result := FWorkerThread <> nil;
end;

procedure TDirectoryMonitor.SetActions(const Value: TActionsToWatch);
begin
  if FActions <> Value then begin
    FActions := Value;
    if Active then
      Start;
  end;
end;

procedure TDirectoryMonitor.SetActive(const Value: boolean);
begin
  FreeAndNil(FWorkerThread);
  if Value then
    FWorkerThread := TDirectoryMonitorWorkerThread.Create(Self);
end;

procedure TDirectoryMonitor.SetDirectory(const Value: string);
begin
  if FDirectory <> Value then begin
    FDirectory := Value;
    if Active then
      Start;
  end;
end;

procedure TDirectoryMonitor.SetSubdirectories(const Value: Boolean);
begin
  if FSubdirectories <> Value then begin
    FSubdirectories := Value;
    if Active then
      Start;
  end;
end;

procedure TDirectoryMonitor.Start;
begin
  Active := True;
end;

procedure TDirectoryMonitor.Stop;
begin
  Active := False;
end;

procedure TDirectoryMonitor.WndProc(var Msg: TMessage);
var
  Action: TDirectoryMonitorAction;
  FileName: string;
  FileNamePtr: PChar;
begin
  if Msg.Msg = WMUSER_DIRECTORYCHANGED then begin
    Action := TDirectoryMonitorAction(Msg.WParam);
    FileNamePtr := PChar(Msg.LParam);
    FileName := string(FileNamePtr);
    StrDispose(FileNamePtr);
    DoChange(Action, FileName);
  end;
end;

end.