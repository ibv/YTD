unit uFAR;
{$INCLUDE 'ufar.inc'}
{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, Windows, Classes, {$IFDEF FPC} Contnrs, {$ELSE} Generics.Collections, {$ENDIF}
  {$IFDEF FAR3} Plugin3, {$ELSE} PluginW, {$ENDIF} FarKeysW, FarColor,
  uSystem, uCompatibility, uLog;

//===== Plugin's API ===========================================================

var
  FarApi: TPluginStartupInfo;
  FSF: TFarStandardFunctions;

//===== Classes ================================================================

{$IFNDEF FAR3_UP}
type
  TPluginPanelItemFlags = DWORD;
  TFarDialogItemFlags = DWORD;
  TFarDialogFlags = DWORD;
  TFarMessageFlags = DWORD;
  TIntPtr = NativeInt;
  TUIntPtr = NativeUInt;
  TEditorFlags = DWORD;
{$ENDIF}

type
  TFarCharArray = array of TFarChar;
  PFarCharArray = array of PFarChar;

type
  TFarPanelItem = class;
  TFarPanelInfo = class;

  TFarPanelItem = class
  private
    fPanel: TFarPanelInfo;
    fItemIndex: integer;
    fCreationTime: FILETIME;
    fLastAccessTime: FILETIME;
    fLastWriteTime: FILETIME;
    {$IFDEF FAR3_UP}
    fChangeTime: FILETIME;
    {$ENDIF}
    fFileSize: Int64;
    fAllocationSize: Int64;
    fFileName: string;
    fAlternateFileName: string;
    fDescription: string;
    fOwner: string;
    fFlags: TPluginPanelItemFlags;
    fFileAttributes: DWORD;
    fNumberOfLinks: DWORD;
    fCRC32: DWORD;
    function GetFlag(const Index: Integer): boolean;
    procedure SetFlag(const Index: Integer; const Value: boolean);
  protected
    procedure Load(const AIndex: integer);
  public
    constructor Create(APanel: TFarPanelInfo; const AIndex: integer);
    destructor Destroy; override;
    property Panel: TFarPanelInfo read fPanel;
    property ItemIndex: integer read fItemIndex;
    property CreationTime: FILETIME read fCreationTime;
    property LastAccessTime: FILETIME read fLastAccessTime;
    property LastWriteTime: FILETIME read fLastWriteTime;
    {$IFDEF FAR3_UP}
    property ChangeTime: FILETIME read fChangeTime;
    {$ENDIF}
    property FileSize: Int64 read fFileSize;
    property AllocationSize: Int64 read fAllocationSize;
    property FileName: string read fFileName;
    property AlternateFileName: string read fAlternateFileName;
    property Description: string read fDescription;
    property Owner: string read fOwner;
    property Flags: TPluginPanelItemFlags read fFlags;
    property FileAttributes: DWORD read fFileAttributes;
    property NumberOfLinks: DWORD read fNumberOfLinks;
    property CRC32: DWORD read fCRC32;
    //
    property Selected: boolean index PPIF_SELECTED read GetFlag write SetFlag;
  end;

  TFarPanelItems = class(TObjectList<TFarPanelItem>);

  TFarPanelInfo = class
  private
    fHandle: THandle;
    {$IFDEF FAR3_UP}
    fPluginHandle: THandle;
    fOwnerGUID: TGUID;
    fFlags: TPANELINFOFLAGS;
    {$ELSE}
    fPluginHandle: integer;
    fFlags: DWORD;
    {$ENDIF}
    fPanelRect: TRect;
    fItems: TFarPanelItems;
    fSelectedItemsCount: integer;
    fCurrentItemIndex: integer;
    fTopPanelItemIndex: integer;
    fViewMode: DWORD;
    fSortMode: DWORD;
    fPanelType: DWORD;
    fDirectory: string;
    {$IFDEF FAR3_UP}
    fDirectoryParam: string;
    fDirectoryPlugin: TGUID;
    fDirectoryFile: string;
    {$ENDIF}
    function GetFlag(const Index: Integer): boolean;
    function GetHasSelectedItems: boolean;
    function GetIsPluginPanel: boolean;
  protected
    procedure Load(APanelHandle: THandle; ALoadItems: boolean = True);
  public
    constructor Create(APanelHandle: THandle; ALoadItems: boolean = True); overload;
    constructor Create(AActivePanel: boolean; ALoadItems: boolean = True); overload;
    destructor Destroy; override;
    procedure ApplySelection;
    procedure Refresh;
    {$IFDEF FAR3_UP}
    property PluginHandle: THandle read fPluginHandle;
    property OwnerGUID: TGUID read fOwnerGUID;
    property Flags: TPANELINFOFLAGS read fFlags;
    {$ELSE}
    property PluginHandle: integer read fPluginHandle;
    property Flags: DWORD read fFlags;
    {$ENDIF}
    property PanelRect: TRect read fPanelRect;
    property Items: TFarPanelItems read fItems;
    property SelectedItemsCount: integer read fSelectedItemsCount;
    property CurrentItemIndex: integer read fCurrentItemIndex;
    property TopPanelItemIndex: integer read fTopPanelItemIndex;
    property ViewMode: DWORD read fViewMode;
    property SortMode: DWORD read fSortMode;
    property PanelType: DWORD read fPanelType;
    property Directory: string read fDirectory;
    {$IFDEF FAR3_UP}
    property DirectoryParam: string read fDirectoryParam;
    property DirectoryPlugin: TGUID read fDirectoryPlugin;
    property DirectoryFile: string read fDirectoryFile;
    {$ENDIF}
    //
    property Handle: THandle read fHandle;
    property Left: boolean index PFLAGS_PANELLEFT read GetFlag;
    property HasSelectedItems: boolean read GetHasSelectedItems;
    property IsPluginPanel: boolean read GetIsPluginPanel;
  end;

type
  TFarConfig = class
  private
    {$IFDEF FAR3_UP}
    fPluginGUID: TGUID;
    fSettingsHandle: THandle;
    fCurrentKey: integer;
    {$ELSE}
    fCurrentKey: HKEY;
    fRootPath: string;
    {$ENDIF}
    fCurrentPath: string;
  protected
    function IsOpen: boolean; overload;
    function IsOpen(const Path: string): boolean; overload;
    function Open(Path: string = ''): boolean;
    procedure Close;
    function ReadStringEx(const Name: string; out Value: string): boolean;
    function ReadString(const Name: string; const Default: string = ''): string;
    function ReadIntegerEx(const Name: string; out Value: int64): boolean;
    function ReadInteger(const Name: string; const Default: int64 = 0): int64;
    function ReadBooleanEx(const Name: string; out Value: boolean): boolean;
    function ReadBoolean(const Name: string; const Default: boolean = False): boolean;
    function WriteString(const Name, Value: string): boolean;
    function WriteInteger(const Name: string; const Value: int64): boolean;
    function WriteBoolean(const Name: string; const Value: boolean): boolean;
    function DeleteValue(const Name: string): boolean;
    function DeleteKey(const Name: string): boolean;
    function ReadValueNames(List: TStrings): boolean;
    function ReadKeyNames(List: TStrings): boolean;
    property CurrentPath: string read fCurrentPath;
  public
    constructor Create( {$IFDEF FAR3_UP} APluginGUID: TGUID {$ELSE} const APath: string {$ENDIF} ; ADoOpen: boolean);
    destructor Destroy; override;
  end;

type
  TFarDialog = class
  private
    {$IFDEF FAR3_UP}
    fPluginGUID: TGUID;
    fDialogGUID: TGUID;
    {$ELSE}
    fPluginHandle: THandle;
    {$ENDIF}
    fItems: PFarDialogItemArray;
    fCapacity: integer;
    fCount: integer;
    fDialogHandle: THandle;
    fLastX: integer;
    fLastY: integer;
    function GetItem(Index: integer): PFarDialogItem;
    function AllocItem: PFarDialogItem;
    function AddItem(ItemType: DWORD; X1, Y1, X2, Y2: integer; Param, Data, UserData: Pointer; History, Mask: PFarChar; Flags: TFarDialogItemFlags; MaxLength: size_t): integer;
    procedure FreeDialogHandle;
    function GetItemEnabled(Index: integer): boolean;
    function GetItemChecked(Index: integer): boolean;
    procedure SetItemEnabled(Index: integer; const Value: boolean);
    procedure SetItemChecked(Index: integer; const Value: boolean);
    function GetItemText(Index: integer): string;
    procedure SetItemText(Index: integer; const Value: string);
  protected
    function AddDoubleBox(X1, Y1, X2, Y2: integer; Caption: PFarChar; Flags:  TFarDialogItemFlags): integer;
    function AddLabel(X1, Y1: integer; Caption: PFarChar; Flags:  TFarDialogItemFlags): integer; overload;
    function AddLabel(X1, Y1, X2: integer; Caption: PFarChar; Flags:  TFarDialogItemFlags): integer; overload;
    function AddCheckbox(X1, Y1: integer; Caption: PFarChar; Checked: boolean; Flags: TFarDialogItemFlags): integer;
    function AddRadio(X1, Y1: integer; Caption: PFarChar; Checked: boolean; Flags: TFarDialogItemFlags): integer;
    function AddEdit(X1, Y1, Width: integer; ValueBuffer, Mask, History: PFarChar; MaxLength: integer; Flags: TFarDialogItemFlags): integer; overload;
    function AddEdit(X1, Y1, Width: integer; const Value: string; var ValueBuffer: TFarCharArray; Mask, History: PFarChar; MaxLength: integer; Flags: TFarDialogItemFlags): integer; overload;
      // Pokud je X1 zaporne, vypocita se automaticky podle delky prvku, ktery je vuci tomuto Editu na indexu X1
    function AddButton(X1, Y1: integer; Caption: PFarChar; Flags:  TFarDialogItemFlags): integer;
    function Build(X1, Y1, X2, Y2: integer; Flags: TFarDialogFlags; HelpTopic: PFarChar): boolean;
    function Execute(out Button: integer): boolean;
  protected
    function DialogProc(Msg, Param1, Param2: TIntPtr; var DlgProcResult: TIntPtr): boolean; virtual;
    function SendDlgMessage(Msg, Param1: TIntPtr; Param2: Pointer): TIntPtr;
    property Count: integer read fCount;
    property Items[Index: integer]: PFarDialogItem read GetItem; default;
    property DialogHandle: THandle read fDialogHandle;
    property ItemChecked[Index: integer]: boolean read GetItemChecked write SetItemChecked;
    property ItemEnabled[Index: integer]: boolean read GetItemEnabled write SetItemEnabled;
    property ItemText[Index: integer]: string read GetItemText write SetItemText;
    property LastX: integer read fLastX;
    property LastY: integer read fLastY;
  public
    class function GetDialogInstance(const Handle: THandle): TFarDialog;
    constructor Create( {$IFDEF FAR3_UP} APluginGUID, ADialogGUID: TGUID {$ELSE} APluginHandle: THandle {$ENDIF} );
    destructor Destroy; override;
    procedure ClearDialog; virtual;
  end;

type
  TGetBooleanValueEvent = procedure(Sender: TObject; var Value: boolean) of object;

  TFarProgress = class
  private
    {$IFDEF FAR3_UP}
    fPluginGUID: TGUID;
    fDialogGUID: TGUID;
    {$ELSE}
    fPluginHandle: THandle;
    {$ENDIF}
    fSavedScreen: THandle;
    fConsoleRect: TSmallRect;
    fConsoleTitle: string;
    fIsConsoleTitle: boolean;
    fStdInput: THandle;
    fWidth: int64;
    fTitle: string;
    fFirstDisplay: boolean;
    fLastProgressTicks: DWORD;
    fGranularityMSec: DWORD;
    fCheckForEscape: boolean;
    fOnEscapePressed: TGetBooleanValueEvent;
  protected
    procedure PrepareConsole;
    procedure CleanupConsole;
  public
    constructor Create( {$IFDEF FAR3_UP} APluginGUID, ADialogGUID: TGUID {$ELSE} APluginHandle: THandle {$ENDIF} );
    destructor Destroy; override;
    procedure Show(const Msg: array of PFarChar); overload;
    procedure Show(TitleMsg, DescriptionMsg: integer; const FileName: string; Progress, Max: int64); overload;
    procedure Show(TitleMsg, DescriptionMsg, BetweenFileNamesMsg: integer; const FileName1, Filename2: string; Progress, Max: int64); overload;
    function NeedsRefresh: boolean;
    function BuildProgressBar(const Position, Max: int64): string;
    function BuildProgressFilename(const FileName: string): string;
    procedure Hide;
    property GranularityMSec: DWORD read fGranularityMSec write fGranularityMSec;
    property CheckForEscape: boolean read fCheckForEscape write fCheckForEscape;
    property OnEscapePressed: TGetBooleanValueEvent read fOnEscapePressed write fOnEscapePressed;
  end;

type
  TFarEditor = class
  public
    class function GetInfo(EditorID: TIntPtr; out Info: TEditorInfo): boolean;
    class function GetFileName(EditorID: TIntPtr; out FileName: string): boolean;
    class function GetString(EditorID, RowNumber: TIntPtr; out Str: string): boolean; overload;
    class function GetString(EditorID, RowNumber: TIntPtr; out Str: string; out SelStart, SelLength: TIntPtr): boolean; overload;
    class function SetString(EditorID, RowNumber: TIntPtr; const Str: string): boolean;
    class function SetCursorPosition(EditorID, RowNumber: TIntPtr): boolean; overload;
    class function SetCursorPosition(EditorID, RowNumber, ColNumber: TIntPtr): boolean; overload;
    class function SetCursorPosition(EditorID, RowNumber, ColNumber, TabPos, TopScreenLine, LeftPos, Overtype: TIntPtr): boolean; overload;
    class function Refresh(EditorID: TIntPtr): boolean;
  end;

  TFarEditorManaged = class
  private
    fEditorID: TIntPtr;
    fEditorTitle: string;
    fEditorFlags: TEditorFlags;
    fEditorCodePage: TUIntPtr;
    fFileName: string;
  protected
    procedure BeforeClose(var CanClose: boolean; var Result: TIntPtr); virtual;
    procedure AfterShowEditor; virtual;
  public
    class function FindByEditorID(EditorID: TIntPtr; out Editor: TFarEditorManaged): boolean;
    class procedure ProcessEditorEvent(Event, EditorID: TIntPtr; var Result: TIntPtr);
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function ShowEditor(Title: PFarChar; X1, Y1, X2, Y2: TIntPtr; Flags: TEditorFlags; StartLine, StartChar: TIntPtr; CodePage: TUIntPtr): boolean;
    property EditorID: TIntPtr read fEditorID;
    property FileName: string read fFileName;
  end;

type
  TFarUtils = class
  public
    class function CopyString(const s: PFarChar): string; overload;
    class function CopyString(const s: TFarCharArray): string; overload;
    class function CopyLString(const s: PFarChar; Length: integer): string;
    class function TruncateString(const s: string; Width: integer; FillerChar: Char): string;
    class function ShowMessage(const Msg: array of PFarChar; Flags: TFarMessageFlags; ButtonCount: integer): integer; overload;
    class function ShowMessage(const Msg: array of string; Flags: TFarMessageFlags; ButtonCount: integer): integer; overload;
    class function FullFileName(const s: string; const NTSchematics: boolean): string;
    class function GetConsoleRect: TSmallRect;
    class function GetConsoleWidth: integer;
    class function GetConsoleHeight: integer;
    class function GetMsg(MsgId: integer): PFarChar;
    class function TempFileName: string;
    class function Menu(const MenuID: TGUID; Title, HelpTopic: PFarChar; Items: TStrings; out ItemIndex: integer): boolean;
    class function SetProgressState(State: DWORD; Progress, Total: Int64): boolean; overload;
    class function SetProgressState(State: DWORD): boolean; overload;
  end;

{$IFDEF FAR3_UP}
var
  PluginID: TGUID;
{$ENDIF}

const
  GUID_NULL: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
  {$IFNDEF FAR3_UP}
  FMSG_NONE = 0;
  {$ENDIF}

implementation

const
  hPanel: array[boolean] of THandle = (PANEL_PASSIVE, PANEL_ACTIVE);

type
  TFarDialogs = TList<TFarDialog>;
  TFarEditorsManaged = TList<TFarEditorManaged>;

var
  fActiveDialogs: TFarDialogs = nil;
  fManagedEditors: TFarEditorsManaged = nil;

{ TFarUtils }

class function TFarUtils.CopyLString(const s: PFarChar; Length: integer): string;
begin
  if s = nil then
    Result := ''
  else
    SetString(Result, s, Length)
end;

class function TFarUtils.CopyString(const s: PFarChar): string;
begin
  if s = nil then
    Result := ''
  else
    Result := CopyLString(s, StrLen(s));
end;

class function TFarUtils.CopyString(const s: TFarCharArray): string;
var
  n: integer;
begin
  n := Length(s);
  if n <= 0 then
    Result := ''
  else
    Result := CopyString(@s[0]);
end;

class function TFarUtils.FullFileName(const s: string; const NTSchematics: boolean): string;
const
  ConvertPathFlags: array[boolean] of DWORD = (CPM_REAL, CPM_NATIVE);
var
  n: DWORD;
begin
  Result := '';
  if s <> '' then begin
    n := FarApi.FSF^.ConvertPath(CPM_NATIVE, PFarChar(s), nil, 0);
    if n > 0 then begin
      SetLength(Result, n);
      n := FarApi.FSF^.ConvertPath(CPM_NATIVE, PFarChar(s), @Result[1], n);
      if n > 0 then
        SetLength(Result, n-1) // remove trailing #0
      else
        Result := '';
    end;
  end;
end;

class function TFarUtils.GetConsoleHeight: integer;
var
  Rect: TSmallRect;
begin
  Rect := GetConsoleRect;
  Result := Rect.Bottom - Rect.Top + 1;
end;

class function TFarUtils.GetConsoleRect: TSmallRect;
begin
  if FarApi.AdvControl( {$IFDEF FAR3_UP} PluginID {$ELSE} FarApi.ModuleNumber {$ENDIF} , ACTL_GETFARRECT, {$IFDEF FAR3_UP} 0, {$ENDIF} @Result) = 0 then begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 79;
    Result.Bottom := 24;
  end;
end;

class function TFarUtils.GetConsoleWidth: integer;
var
  Rect: TSmallRect;
begin
  Rect := GetConsoleRect;
  Result := Rect.Right - Rect.Left + 1;
end;

class function TFarUtils.GetMsg(MsgId: integer): PFarChar;
const
  NO_MESSAGE = '';
begin
  if MsgId < 0 then
    Result := NO_MESSAGE
  else
    Result := FarApi.GetMsg( {$IFDEF FAR3_UP} PluginID {$ELSE} FarApi.ModuleNumber {$ENDIF} , MsgId);
end;

class function TFarUtils.Menu(const MenuID: TGUID; Title, HelpTopic: PFarChar; Items: TStrings; out ItemIndex: integer): boolean;
var
  Res: TIntPtr;
  MenuTexts: array of string;
  MenuItems: packed array of TFarMenuItem;
  i, Count: Integer;
begin
  Result := False;
  Count := Items.Count;
  if Count > 0 then begin
    SetLength(MenuItems, Count);
    FillChar(MenuItems[0], Count * Sizeof(MenuItems[0]), 0);
    SetLength(MenuTexts, Count);
    for i := 0 to Pred(Count) do begin
      MenuTexts[i] := Items[i];
      {$IFDEF FAR3_UP}
      MenuItems[i].Flags := MIF_NONE;
      {$ENDIF}
      MenuItems[i].TextPtr := PFarChar(MenuTexts[i]);
    end;
    Res := FarApi.Menu(
             {$IFDEF FAR3_UP}
             PluginID,
             MenuID,
             {$ELSE}
             FarApi.ModuleNumber,
             {$ENDIF}
             -1, -1,
             0,
             FMENU_AUTOHIGHLIGHT or FMENU_WRAPMODE,
             Title,
             nil,
             HelpTopic,
             nil,
             nil,
             @MenuItems[0],
             Count
           );
    if Res >= 0 then begin
      ItemIndex := Res;
      Result := True;
    end;
  end;
end;

class function TFarUtils.SetProgressState(State: DWORD; Progress, Total: Int64): boolean;
var
  Value: TProgressValue;
begin
  Result := FarApi.AdvControl( {$IFDEF FAR3_UP} PluginID {$ELSE} FarApi.ModuleNumber {$ENDIF} , ACTL_SETPROGRESSSTATE, {$IFDEF FAR3_UP} State, nil {$ELSE} Pointer(State) {$ENDIF} ) <> 0;
  if (State <> {$IFDEF FAR3_UP} TBPS_NOPROGRESS {$ELSE} PS_NOPROGRESS {$ENDIF} ) then
    if (Progress >= 0) and (Total > 0) then begin
      FillChar(Value, Sizeof(Value), 0);
      {$IFDEF FAR3_UP}
      Value.StructSize := Sizeof(Value);
      {$ENDIF}
      Value.Completed := Progress;
      Value.Total := Total;
      if FarApi.AdvControl( {$IFDEF FAR3_UP} PluginID {$ELSE} FarApi.ModuleNumber {$ENDIF} , ACTL_SETPROGRESSVALUE, {$IFDEF FAR3_UP} 0, {$ENDIF} @Value) = 0 then
        Result := False;
    end;
end;

class function TFarUtils.SetProgressState(State: DWORD): boolean;
begin
  Result := SetProgressState(State, -1, -1);
end;

class function TFarUtils.ShowMessage(const Msg: array of string; Flags: TFarMessageFlags; ButtonCount: integer): integer;
var
  i, n: integer;
  Msg2: array of PFarChar;
begin
  n := Length(Msg);
  SetLength(Msg2, n);
  for i := 0 to Pred(n) do
    Msg2[i] := PFarChar(Msg[i]);
  Result := ShowMessage(Msg2, Flags, ButtonCount);
end;

class function TFarUtils.ShowMessage(const Msg: array of PFarChar; Flags: TFarMessageFlags; ButtonCount: integer): integer;
var
  MsgArray: PPCharArray;
  i, n: integer;
begin
  n := Length(Msg);
  GetMem(MsgArray, n * Sizeof(PFarChar));
  for i := 0 to Pred(n) do
    MsgArray^[i] := Msg[i];
  Result := FarApi.Message( {$IFDEF FAR3_UP} PluginID, GUID_NULL {$ELSE} FarApi.ModuleNumber {$ENDIF} , Flags, nil, MsgArray, n, ButtonCount);
end;

class function TFarUtils.TempFileName: string;
var
  n: integer;
begin
  SetLength(Result, MAX_PATH);
  n := FarApi.FSF.MkTemp(PChar(Result), Length(Result), nil);
  if n = 0 then
    Result := SystemTempFile(SystemTempDir, '')
  else
    SetLength(Result, n);
end;

class function TFarUtils.TruncateString(const s: string; Width: integer; FillerChar: Char): string;
var
  n: integer;
begin
  Result := s;
  n := Length(s);
  if n > Width then begin
    // Pozor, musim zajistit, aby Result byl jedinecny!!!
    UniqueString(Result);
    FarApi.FSF.TruncPathStr(PChar(Result), Width);
    SetLength(Result, Width);
  end
  else if n < Width then
    Result := Result + StringOfChar(FillerChar, Width-n);
end;

{ TFarPanelInfo }

procedure TFarPanelInfo.ApplySelection;
const
  SelectedFlag: array[boolean] of integer = (0, 1);
var
  i: integer;
begin
  //Log('TFarPanelInfo.ApplySelection pro panel %d (%d polozek)', [Integer(Active), Items.Count]);
  FarApi.Control(Handle, FCTL_BEGINSELECTION, 0, nil);
  try
    for i := 0 to Pred(Items.Count) do begin
      //Log(' - polozka %d "%s", index v panelu %d, selected %d', [i, Items[i].FileName, Items[i].ItemIndex, Integer(Items[i].Selected)]);
      FarApi.Control(Handle, FCTL_SETSELECTION, Items[i].ItemIndex, Pointer(SelectedFlag[Items[i].Selected]));
    end;
  finally
    FarApi.Control(Handle, FCTL_ENDSELECTION, 0, nil);
    FarApi.Control(Handle, FCTL_REDRAWPANEL, 0, nil);
  end;
end;

constructor TFarPanelInfo.Create(AActivePanel, ALoadItems: boolean);
begin
  inherited Create;
  fItems := TFarPanelItems.Create(True);
  Load(hPanel[AActivePanel], ALoadItems);
end;

constructor TFarPanelInfo.Create(APanelHandle: THandle; ALoadItems: boolean);
begin
  inherited Create;
  fItems := TFarPanelItems.Create(True);
  Load(APanelHandle, ALoadItems);
end;

destructor TFarPanelInfo.Destroy;
begin
  FreeAndNil(fItems);
  inherited;
end;

function TFarPanelInfo.GetFlag(const Index: Integer): boolean;
begin
  Result := (fFlags and Index) <> 0;
end;

function TFarPanelInfo.GetHasSelectedItems: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Pred(Items.Count) do
    if Items[i].Selected then begin
      Result := True;
      Break;
    end;
end;

function TFarPanelInfo.GetIsPluginPanel: boolean;
begin
  Result := (PluginHandle <> 0) {and (PluginHandle <> INVALID_HANDLE_VALUE)};
end;

procedure TFarPanelInfo.Load(APanelHandle: THandle; ALoadItems: boolean);
var
  Info: TPanelInfo;
  i: Integer;
  nSize: integer;
  {$IFDEF FAR3_UP}
  Buffer: array of Byte;
  pDir: PFarPanelDirectory;
  {$ELSE}
  Buffer: TFarCharArray;
  {$ENDIF}
begin
  FillChar(Info, Sizeof(Info), 0);
  {$IFDEF FAR3_UP}
  Info.StructSize := Sizeof(Info);
  {$ENDIF}
  if FarApi.Control(APanelHandle, FCTL_GETPANELINFO, 0, @Info) <> 0 then begin
    fHandle := APanelHandle;
    // Read panel info
    {$IFDEF FAR3_UP}
    fPluginHandle := Info.PluginHandle;
    fOwnerGUID := Info.OwnerGuid;
    fFlags := Info.Flags;
    {$ELSE}
    fPluginHandle := Info.Plugin;
    fFlags := Info.Flags;
    {$ENDIF}
    fPanelRect := Info.PanelRect;
    fCurrentItemIndex := Info.CurrentItem;
    fTopPanelItemIndex := Info.TopPanelItem;
    fSelectedItemsCount := Info.SelectedItemsNumber;
    fViewMode := Info.ViewMode;
    fSortMode := Info.SortMode;
    fPanelType := Info.PanelType;
    // Read panel items
    fItems.Clear;
    if ALoadItems then
      for i := 0 to Pred(Info.ItemsNumber) do
        fItems.Add(TFarPanelItem.Create(Self, i));
    // Read directory info
    fDirectory := '';
    {$IFDEF FAR3_UP}
    fDirectoryParam := '';
    fDirectoryPlugin := GUID_NULL;
    fDirectoryFile := '';
    {$ENDIF}
    nSize := FarApi.Control(APanelHandle, {$IFDEF FAR3_UP} FCTL_GETPANELDIRECTORY {$ELSE} FCTL_GETPANELDIR {$ENDIF} , 0, nil);
    if nSize > 0 then begin
      SetLength(Buffer, nSize);
      FillChar(Buffer[0], nSize, 0);
      {$IFDEF FAR3_UP}
      pDir := @Buffer[0];
      pDir^.StructSize := Sizeof(pDir^);
      FarApi.Control(APanelHandle, FCTL_GETPANELDIRECTORY, nSize, pDir);
      fDirectory := {TFarUtils.FullFileName}(TFarUtils.CopyString(pDir^.Name));
      fDirectoryParam := TFarUtils.CopyString(pDir^.Param);
      fDirectoryPlugin := pDir^.PluginId;
      fDirectoryFile := TFarUtils.CopyString(pDir^.fFile);
      {$ELSE}
      FarApi.Control(APanelHandle, FCTL_GETPANELDIR, nSize, Buffer);
      fDirectory := TFarUtils.CopyString(@Buffer[0]);
      {$ENDIF}
    end;
  end;
end;

procedure TFarPanelInfo.Refresh;
begin
  FarApi.Control(Handle, FCTL_UPDATEPANEL, 1, nil);
end;

{ TFarPanelItem }

constructor TFarPanelItem.Create(APanel: TFarPanelInfo; const AIndex: integer);
begin
  inherited Create;
  fPanel := APanel;
  Load(AIndex);
end;

destructor TFarPanelItem.Destroy;
begin
  inherited;
end;

function TFarPanelItem.GetFlag(const Index: Integer): boolean;
begin
  Result := (fFlags and Index) <> 0;
end;

procedure TFarPanelItem.Load(const AIndex: integer);
var
  nSize: size_t;
  Buffer: array of Byte;
  PPI: PPluginPanelItem;
  {$IFDEF FAR3_UP}
  Rec: TFarGetPluginPanelItem;
  {$ENDIF}
begin
  fItemIndex := AIndex;
  nSize := FarApi.Control(Panel.Handle, FCTL_GETPANELITEM, AIndex, nil);
  if nSize > 0 then begin
    SetLength(Buffer, nSize);
    FillChar(Buffer[0], nSize, 0);
    PPI := @Buffer[0];
    {$IFDEF FAR3_UP}
    Rec.StructSize := Sizeof(Rec);
    Rec.Size := nSize;
    Rec.Item := PPI;
    {$ENDIF}
    FarApi.Control(Panel.Handle, FCTL_GETPANELITEM, AIndex, {$IFDEF FAr3_UP} @Rec {$ELSE} PPI {$ENDIF} );
    fCreationTime := {$IFDEF FAR3_UP} PPI^.CreationTime {$ELSE} PPI^.FindData.ftCreationTime {$ENDIF} ;
    fLastAccessTime := {$IFDEF FAR3_UP} PPI^.LastAccessTime {$ELSE} PPI^.FindData.ftLastAccessTime {$ENDIF} ;
    fLastWriteTime := {$IFDEF FAR3_UP} PPI^.LastWriteTime {$ELSE} PPI^.FindData.ftLastWriteTime {$ENDIF} ;
    {$IFDEF FAR3_UP}
    fChangeTime := PPI^.ChangeTime;
    {$ENDIF}
    fFileSize := {$IFDEF FAR3_UP} PPI^.FileSize {$ELSE} PPI^.FindData.nFileSize {$ENDIF} ;
    fAllocationSize := {$IFDEF FAR3_UP} PPI^.AllocationSize {$ELSE} PPI^.FindData.nPackSize {$ENDIF} ;
    fFileName := TFarUtils.CopyString( {$IFDEF FAR3_UP} PPI^.FileName {$ELSE} PPI^.FindData.cFileName {$ENDIF} );
    fAlternateFileName := TFarUtils.CopyString( {$IFDEF FAR3_UP} PPI^.AlternateFileName {$ELSE} PPI^.FindData.cAlternateFileName {$ENDIF} );
    fDescription := TFarUtils.CopyString(PPI^.Description);
    fOwner := TFarUtils.CopyString(PPI^.Owner);
    fFlags := PPI^.Flags;
    fFileAttributes := {$IFDEF FAR3_UP} PPI^.FileAttributes {$ELSE} PPI^.FindData.dwFileAttributes {$ENDIF} ;
    fNumberOfLinks := PPI^.NumberOfLinks;
    fCRC32 := PPI^.CRC32;
  end;
end;

procedure TFarPanelItem.SetFlag(const Index: Integer; const Value: boolean);
begin
  if Value then
    fFlags := (fFlags or DWORD(Index))
  else
    fFLags := (fFlags and (not Index));
end;

{ TFarDialog }

function FarDialogProc(Handle: THandle; Msg, Param1, Param2: TIntPtr): TIntPtr; stdcall;
var
  Dlg: TFarDialog;
begin
  Result := 0;
  Dlg := TFarDialog.GetDialogInstance(Handle);
  if Dlg <> nil then
    if not Dlg.DialogProc(Msg, Param1, Param2, Result) then
      Result := FarApi.DefDlgProc(Handle, Msg, Param1, Param2);
end;

function TFarDialog.AddButton(X1, Y1: integer; Caption: PFarChar; Flags: TFarDialogItemFlags): integer;
begin
  Result := AddItem(DI_BUTTON, X1, Y1, 0, 0, nil, Caption, nil, nil, nil, Flags, 0);
end;

function TFarDialog.AddCheckbox(X1, Y1: integer; Caption: PFarChar; Checked: boolean; Flags: TFarDialogItemFlags): integer;
const
  CheckedVal: array[boolean] of integer = (0, 1);
begin
  Result := AddItem(DI_CHECKBOX, X1, Y1, 0, 0, Pointer(CheckedVal[Checked]), Caption, nil, nil, nil, Flags, 0);
end;

function TFarDialog.AddDoubleBox(X1, Y1, X2, Y2: integer; Caption: PFarChar; Flags: TFarDialogItemFlags): integer;
begin
  Result := AddItem(DI_DOUBLEBOX, X1, Y1, X2, Y2, nil, Caption, nil, nil, nil, Flags, 0);
end;

function TFarDialog.AddEdit(X1, Y1, Width: integer; const Value: string; var ValueBuffer: TFarCharArray; Mask, History: PFarChar; MaxLength: integer; Flags: TFarDialogItemFlags): integer;
var
  n: integer;
begin
  n := Length(Value);
  if n < MaxLength then
    n := MaxLength;
  SetLength(ValueBuffer, Succ(n));
  StrPCopy(@ValueBuffer[0], Value);
  Result := AddEdit(X1, Y1, Width, @ValueBuffer[0], Mask, History, n, Flags);
end;

function TFarDialog.AddEdit(X1, Y1, Width: integer; ValueBuffer, Mask, History: PFarChar; MaxLength: integer; Flags: TFarDialogItemFlags): integer;
const
  EditType: array[boolean] of DWORD = (DI_EDIT, DI_FIXEDIT);
var
  Item, LastItem: PFarDialogItem;
begin
  Result := AddItem(EditType[Mask<>nil], X1, Y1, X1+Width, 0, nil, ValueBuffer, nil, History, Mask, Flags, MaxLength);
  if (X1 < 0) and (fCount > (-X1)) then begin
    Item := Items[Result];
    LastItem := Items[Result + X1];
    if (Item <> nil) and (LastItem <> nil) then begin
      Item^.X1 := LastItem^.X1 + integer(StrLen(LastItem^. {$IFDEF FAR3_UP} Data {$ELSE} PtrData {$ENDIF} )) + 5;
      if StrPos(LastItem^. {$IFDEF FAR3_UP} Data {$ELSE} PtrData {$ENDIF} , '&') <> nil then
        Dec(Item^.X1);
      Item^.X2 := Item^.X2 + Item^.X1;
    end;
  end;
end;

function TFarDialog.AddItem(ItemType: DWORD; X1, Y1, X2, Y2: integer;
  Param, Data, UserData: Pointer; History, Mask: PFarChar; Flags: TFarDialogItemFlags;
  MaxLength: size_t): integer;
var
  Item: PFarDialogItem;
begin
  Result := Count;
  Item := AllocItem;
  FillChar(Item^, Sizeof(Item^), 0);
  Item^.ItemType := ItemType;
  Item^.X1 := X1;
  Item^.Y1 := Y1;
  Item^.X2 := X2;
  Item^.Y2 := Y2;
  {$IFDEF FAR3_UP}
  Item^.Param.VBuf := Param;
  Item^.History := History;
  Item^.Mask := Mask;
  {$ELSE}
  if (Flags and DIF_MASKEDIT) <> 0 then
    Item^.Param.Mask := Mask
  else if (Flags and DIF_HISTORY) <> 0 then
    Item^.Param.History := History
  else
    Item^.Param.VBuf := Param;
  {$ENDIF}
  Item^.Flags := Flags;
  Item^. {$IFDEF FAR3_UP} Data {$ELSE} PtrData {$ENDIF} := Data;
  Item^. {$IFDEF FAR3_UP} MaxLength {$ELSE} MaxLen {$ENDIF} := MaxLength;
  {$IFDEF FAR3_UP}
  Item^.UserData := TIntPtr(UserData);
  {$ENDIF}
  fLastX := X1;
  fLastY := Y1;
end;

function TFarDialog.AddLabel(X1, Y1, X2: integer; Caption: PFarChar; Flags: TFarDialogItemFlags): integer;
begin
  Result := AddItem(DI_TEXT, X1, Y1, X2, 0, nil, Caption, nil, nil, nil, Flags, 0);
end;

function TFarDialog.AddLabel(X1, Y1: integer; Caption: PFarChar; Flags: TFarDialogItemFlags): integer;
begin
  Result := AddLabel(X1, Y1, 0, Caption, Flags);
end;

function TFarDialog.AddRadio(X1, Y1: integer; Caption: PFarChar; Checked: boolean; Flags: TFarDialogItemFlags): integer;
const
  CheckedVal: array[boolean] of integer = (0, 1);
begin
  Result := AddItem(DI_RADIOBUTTON, X1, Y1, 0, 0, Pointer(CheckedVal[Checked]), Caption, nil, nil, nil, Flags, 0);
end;

function TFarDialog.AllocItem: PFarDialogItem;
const
  GrowthValue = 64;
begin
  if fItems = nil then begin
    GetMem(fItems, GrowthValue * Sizeof(Result^));
    FillChar(fItems^, GrowthValue * Sizeof(Result^), 0);
    fCapacity := GrowthValue;
    fCount := 1;
    Result := Items[0];
  end
  else if fCount < fCapacity then begin
    Result := Items[fCount];
    Inc(fCount);
  end
  else begin
    ReallocMem(fItems, fCapacity + GrowthValue);
    FillChar(fItems^[fCapacity], GrowthValue * Sizeof(Result^), 0);
    Inc(fCapacity, GrowthValue);
    Result := Items[fCount];
    Inc(fCount);
  end;
end;

function TFarDialog.Build(X1, Y1, X2, Y2: integer; Flags: TFarDialogFlags; HelpTopic: PFarChar): boolean;
begin
  FreeDialogHandle;
  fDialogHandle := FarApi.DialogInit( {$IFDEF FAR3_UP} fPluginGUID, fDialogGUID, {$ELSE} fPluginHandle, {$ENDIF} X1, Y1, X2, Y2, HelpTopic, fItems, fCount, 0, Flags, @FarDialogProc, NativeInt(Self));
  Result := fDialogHandle <> INVALID_HANDLE_VALUE;
end;

procedure TFarDialog.ClearDialog;
begin
  FreeDialogHandle;
  FreeMem(fItems);
  fItems := nil;
  fCapacity := 0;
  fCount := 0;
end;

constructor TFarDialog.Create( {$IFDEF FAR3_UP} APluginGUID, ADialogGUID: TGUID {$ELSE} APluginHandle: THandle {$ENDIF} );
begin
  inherited Create;
  fActiveDialogs.Add(Self);
  fDialogHandle := INVALID_HANDLE_VALUE;
  {$IFDEF FAR3_UP}
  fPluginGUID := APluginGUID;
  fDialogGUID := ADialogGUID;
  {$ELSE}
  fPluginHandle := APluginHandle;
  {$ENDIF}
  fItems := nil;
  ClearDialog;
end;

destructor TFarDialog.Destroy;
begin
  FreeDialogHandle;
  ClearDialog;
  inherited;
end;

function TFarDialog.Execute(out Button: integer): boolean;
var
  Index: integer;
  Screen: THandle;
begin
  Result := False;
  Button := -1;
  if fDialogHandle <> INVALID_HANDLE_VALUE then begin
    Screen := FarApi.SaveScreen(0, 0, -1, -1);
    try
      Index := FarApi.DialogRun(fDialogHandle);
      if (Index >= 0) and (Index < Count) then begin
        Button := Index;
        Result := True;
      end;
    finally
      if Screen <> INVALID_HANDLE_VALUE then
        FarApi.RestoreScreen(Screen);
    end;
  end;
end;

function TFarDialog.DialogProc(Msg, Param1, Param2: TIntPtr; var DlgProcResult: TIntPtr): boolean;
begin
  DlgProcResult := 0;
  Result := False;
end;

procedure TFarDialog.FreeDialogHandle;
var
  i: integer;
begin
  if fDialogHandle <> INVALID_HANDLE_VALUE then begin
    FarApi.DialogFree(fDialogHandle);
    for i := Pred(fActiveDialogs.Count) downto 0 do
      if fActiveDialogs[i].DialogHandle = DialogHandle then begin
        fActiveDialogs.Delete(i);
        Break;
      end;
    fDialogHandle := INVALID_HANDLE_VALUE;
  end;
end;

class function TFarDialog.GetDialogInstance(const Handle: THandle): TFarDialog;
var
  i: integer;
begin
  Result := nil;
  if Handle <> INVALID_HANDLE_VALUE then
    for i := 0 to Pred(fActiveDialogs.Count) do
      if fActiveDialogs[i].DialogHandle = Handle then begin
        Result := fActiveDialogs[i];
        Break;
      end;
end;

function TFarDialog.GetItem(Index: integer): PFarDialogItem;
begin
  if (Index >= 0) and (Index < fCapacity) then
    Result := @(fItems[Index])
  else
    Result := nil;
end;

function TFarDialog.GetItemChecked(Index: integer): boolean;
begin
  Result := False;
  if DialogHandle <> INVALID_HANDLE_VALUE then
    Result := SendDlgMessage(DM_GETCHECK, Index, nil) <> 0;
end;

function TFarDialog.GetItemEnabled(Index: integer): boolean;
begin
  Result := False;
  if DialogHandle <> INVALID_HANDLE_VALUE then
    Result := SendDlgMessage(DM_ENABLE, Index, Pointer(-1)) <> 0;
end;

function TFarDialog.GetItemText(Index: integer): string;
var
  Len: integer;
  Rec: TFarDialogItemData;
begin
  Result := '';
  if DialogHandle <> INVALID_HANDLE_VALUE then begin
    Len := SendDlgMessage( {$IFDEF FAR3_UP} DM_GETTEXT {$ELSE} DM_GETTEXTLENGTH {$ENDIF} , Index, nil);
    if Len > 0 then begin
      SetLength(Result, Len);
      FillChar(Rec, Sizeof(Rec), 0);
      {$IFDEF FAR3_UP}
      Rec.StructSize := Sizeof(Rec);
      {$ENDIF}
      Rec.PtrLength := Len;
      Rec.PtrData := PFarChar(Result);
      SendDlgMessage(DM_GETTEXT, Index, @Rec);
    end;
  end;
end;

function TFarDialog.SendDlgMessage(Msg, Param1: TIntPtr; Param2: Pointer): TIntPtr;
begin
  Result := FarApi.SendDlgMessage(DialogHandle, Msg, Param1, {$IFNDEF FAR3_UP} NativeInt {$ENDIF} (Param2));
end;

procedure TFarDialog.SetItemChecked(Index: integer; const Value: boolean);
const
  CheckState: array[boolean] of integer = (BSTATE_UNCHECKED, BSTATE_CHECKED);
begin
  if DialogHandle <> INVALID_HANDLE_VALUE then
    SendDlgMessage(DM_SETCHECK, Index, Pointer(CheckState[Value]));
end;

procedure TFarDialog.SetItemEnabled(Index: integer; const Value: boolean);
const
  EnabledState: array[boolean] of integer = (0, 1);
begin
  if DialogHandle <> INVALID_HANDLE_VALUE then
    SendDlgMessage(DM_ENABLE, Index, Pointer(EnabledState[Value]));
end;

procedure TFarDialog.SetItemText(Index: integer; const Value: string);
var
  Rec: TFarDialogItemData;
begin
  if DialogHandle <> INVALID_HANDLE_VALUE then begin
    FillChar(Rec, Sizeof(Rec), 0);
    {$IFDEF FAR3_UP}
    Rec.StructSize := Sizeof(Rec);
    {$ENDIF}
    Rec.PtrLength := Length(Value);
    Rec.PtrData := PFarChar(Value);
    SendDlgMessage(DM_SETTEXT, Index, @Rec);
  end;
end;

{ TFarConfig }

procedure TFarConfig.Close;
begin
  //if fSettingsHandle <> 0 then
  //  Log('Close');
  {$IFDEF FAR3_UP}
  fCurrentKey := 0;
  if fSettingsHandle <> 0 then begin
    FarApi.SettingsControl(fSettingsHandle, SCTL_FREE, 0, nil);
    fSettingsHandle := 0;
  end;
  {$ELSE}
  if fCurrentKey <> 0 then begin
    RegCloseKey(fCurrentKey);
    fCurrentKey := 0;
  end;
  {$ENDIF}
  fCurrentPath := '';
end;

constructor TFarConfig.Create( {$IFDEF FAR3_UP} APluginGUID: TGUID {$ELSE} const APath: string {$ENDIF} ; ADoOpen: boolean);
begin
  inherited Create;
  {$IFDEF FAR3_UP}
  fPluginGUID := APluginGUID;
  fCurrentKey := 0;
  {$ELSE}
  fCurrentKey := 0;
  fRootPath := IncludeTrailingPathDelimiter(FarApi.RootKey) + APath;
  {$ENDIF}
  if ADoOpen then
    Open;
end;

function TFarConfig.DeleteKey(const Name: string): boolean;
var
  OldPath: string;
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsValue;
  {$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  OldPath := CurrentPath;
  try
    if Open(OldPath + '\' + Name) then begin
      {$IFDEF FAR3_UP}
      FillChar(Rec, SizeOf(Rec), 0);
      Rec.StructSize := SizeOf(Rec);
      Rec.Root := fCurrentKey;
      Rec.Value := nil;
      Result := FarApi.SettingsControl(fSettingsHandle, SCTL_DELETE, 0, @Rec) <> 0;
      {$ELSE}
      Result := RegDeleteKey(fCurrentKey, PChar(Name)) = ERROR_SUCCESS;
      {$ENDIF}
    end;
  finally
    Open(OldPath);
  end;
end;

function TFarConfig.DeleteValue(const Name: string): boolean;
{$IFDEF FAR3_UP}
var
  Rec: TFarSettingsValue;
{$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  Rec.Value := PFarChar(Name);
  Result := FarApi.SettingsControl(fSettingsHandle, SCTL_DELETE, 0, @Rec) <> 0;
  {$ELSE}
  Result := RegDeleteValue(fCurrentKey, PChar(Name)) = ERROR_SUCCESS;
  {$ENDIF}
end;

destructor TFarConfig.Destroy;
begin
  Close;
  inherited;
end;

function TFarConfig.IsOpen: boolean;
begin
  Result := {$IFDEF FAR3_UP} fSettingsHandle <> 0 {$ELSE} fCurrentKey <> 0 {$ENDIF} ;
end;

function TFarConfig.IsOpen(const Path: string): boolean;
begin
  Result := IsOpen and (fCurrentPath = Path);
end;

function TFarConfig.Open(Path: string): boolean;
{$IFDEF FAR3_UP}
var
  Rec: TFarSettingsCreate;
  RecVal: TFarSettingsValue;
  Dir: string;
  ix: integer;
{$ENDIF}
begin
  //Log('Open "%s"', [Path]);
  while (Path <> '') and (Path[1] = '\') do
    Delete(Path, 1, 1);
  if IsOpen(Path) then
    Result := True
  else begin
    Result := False;
    Close;
    {$IFDEF FAR3_UP}
    Rec.StructSize := SizeOf(Rec);
    Rec.Guid := fPluginGUID;
    Rec.Handle := 0;
    if FarApi.SettingsControl(INVALID_HANDLE_VALUE, SCTL_CREATE, 0, @Rec) <> 0 then begin
      Result := True;
      fSettingsHandle := Rec.Handle;
      fCurrentKey := 0;
      fCurrentPath := '';
      while Path <> '' do begin
        ix := Pos('\', Path);
        if ix <= 0 then begin
          Dir := Path;
          Path := '';
        end
        else begin
          Dir := Copy(Path, 1, Pred(ix));
          Path := Copy(Path, Succ(ix), MaxInt);
        end;
        if Dir <> '' then begin
          RecVal.StructSize := SizeOf(RecVal);
          RecVal.Root := fCurrentKey;
          RecVal.Value := PFarChar(Dir);
          fCurrentKey := THandle(FARAPI.SettingsControl(fSettingsHandle, SCTL_CREATESUBKEY, 0, @RecVal));
          if fCurrentKey <> 0 then begin
            if fCurrentPath = ''  then
              fCurrentPath := Dir
            else
              fCurrentPath := fCurrentPath + '\' + Dir;
          end
          else begin
            Result := False;
            Close;
          end;
        end;
      end;
    end;
    {$ELSE}
    if RegCreateKey(HKEY_CURRENT_USER, PChar(fRootPath + Path), fCurrentKey) <> ERROR_SUCCESS then
      fCurrentKey := 0
    else
      Result := True;
    {$ENDIF}
    end;
end;

function TFarConfig.ReadBoolean(const Name: string; const Default: boolean): boolean;
begin
  if not ReadBooleanEx(Name, Result) then
    Result := Default;
end;

function TFarConfig.ReadBooleanEx(const Name: string; out Value: boolean): boolean;
var
  iValue: int64;
begin
  Result := ReadIntegerEx(Name, iValue);
  Value := iValue <> 0;
end;

function TFarConfig.ReadInteger(const Name: string; const Default: int64): int64;
begin
  if not ReadIntegerEx(Name, Result) then
    Result := Default;
end;

function TFarConfig.ReadIntegerEx(const Name: string; out Value: int64): boolean;
var
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsItem;
  {$ELSE}
  DataType, DataSize, dwValue: DWORD;
  {$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  Rec.Name := PFarChar(Name);
  Rec.FType := FST_QWORD;
  Result := FarApi.SettingsControl(fSettingsHandle, SCTL_GET, 0, @Rec) <> 0;
  if Result then
    Value := Rec.Value.Number;
  {$ELSE}
  if RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS then
    case DataType of
      REG_QWORD:
        begin
          DataSize := Sizeof(Value);
          Result := RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, @Value, @DataSize) = ERROR_SUCCESS;
        end;
      REG_DWORD:
        begin
          DataSize := Sizeof(dwValue);
          Result := RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, @dwValue, @DataSize) = ERROR_SUCCESS;
          if Result then
            Value := dwValue;
        end;
      end;
  {$ENDIF}
end;

function TFarConfig.ReadKeyNames(List: TStrings): boolean;
var
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsEnum;
  Item: PFarSettingsName;
  i: Integer;
  {$ELSE}
  Buffer: array[0..16382] of Char;
  i, BufferSize: DWORD;
  s: string;
  {$ENDIF}
begin
  Result := False;
  List.Clear;
  if not IsOpen then
    Exit;
  //Log('TFarConfig.ReadKeyNames');
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  if FarApi.SettingsControl(fSettingsHandle, SCTL_ENUM, 0, @Rec) <> 0 then begin
    Item := Rec.Value.Items;
    for i := 0 to Pred(Rec.Count) do begin
      if Item^.FType = FST_SUBKEY then begin
        //Log('  %s', [TFarUtils.CopyString(Item^.Name)]);
        List.Add(TFarUtils.CopyString(Item^.Name));
      end;
      Inc(Item);
    end;
  end;
  {$ELSE}
  i := 0;
  BufferSize := Length(Buffer) * Sizeof(Buffer[1]);
  while RegEnumKeyEx(fCurrentKey, i, Buffer, BufferSize, nil, nil, nil, nil) = ERROR_SUCCESS do begin
    SetString(s, Buffer, BufferSize);
    //Log(' %s', [s]);
    List.Add(s);
    BufferSize := Length(Buffer) * Sizeof(Buffer[1]);
    Inc(i);
  end;
  {$ENDIF}
end;

function TFarConfig.ReadString(const Name, Default: string): string;
begin
  if not ReadStringEx(Name, Result) then
    Result := Default;
end;

function TFarConfig.ReadStringEx(const Name: string; out Value: string): boolean;
var
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsItem;
  {$ELSE}
  DataType, DataSize, n: DWORD;
  {$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  Rec.Name := PFarChar(Name);
  Rec.FType := FST_STRING;
  Result := FarApi.SettingsControl(fSettingsHandle, SCTL_GET, 0, @Rec) <> 0;
  if Result then
    Value := Rec.Value.Str;
  {$ELSE}
  if RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS then
    if DataType in [REG_SZ, REG_MULTI_SZ, REG_EXPAND_SZ] then
      if RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then begin
        n := DataSize div Sizeof(Value[1]);
        SetLength(Value, n);
        if RegQueryValueEx(fCurrentKey, PChar(Name), nil, @DataType, @Value[1], @DataSize) = ERROR_SUCCESS then begin
          Result := True;
          if (n > 0) and (DataType in [REG_SZ, REG_EXPAND_SZ]) and (Value[n] = #0) then begin
            Dec(n);
            SetLength(Value, n);
          end;
        end;
      end;
  {$ENDIF}
end;

function TFarConfig.ReadValueNames(List: TStrings): boolean;
var
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsEnum;
  Item: PFarSettingsName;
  i: Integer;
  {$ELSE}
  Buffer: array[0..16382] of Char;
  i, BufferSize: DWORD;
  s: string;
  {$ENDIF}
begin
  Result := False;
  List.Clear;
  if not IsOpen then
    Exit;
  //Log('TFarConfig.ReadValueNames');
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  if FarApi.SettingsControl(fSettingsHandle, SCTL_ENUM, 0, @Rec) <> 0 then begin
    Item := Rec.Value.Items;
    for i := 0 to Pred(Rec.Count) do begin
      if Item^.FType <> FST_SUBKEY then begin
        //Log('  %s', [TFarUtils.CopyString(Item^.Name)]);
        List.Add(TFarUtils.CopyString(Item^.Name));
      end;
      Inc(Item);
    end;
  end;
  {$ELSE}
  i := 0;
  BufferSize := Length(Buffer) * Sizeof(Buffer[1]);
  while RegEnumValue(fCurrentKey, i, Buffer, BufferSize, nil, nil, nil, nil) = ERROR_SUCCESS do begin
    SetString(s, Buffer, BufferSize);
    //Log(' %s', [s]);
    List.Add(s);
    BufferSize := Length(Buffer) * Sizeof(Buffer[1]);
    Inc(i);
  end;
  {$ENDIF}
end;

function TFarConfig.WriteBoolean(const Name: string; const Value: boolean): boolean;
const
  Bool2Int: array[boolean] of int64 = (0, 1);
begin
  Result := WriteInteger(Name, Bool2Int[Value]);
end;

function TFarConfig.WriteInteger(const Name: string; const Value: int64): boolean;
var
  {$IFDEF FAR3_UP}
  Rec: TFarSettingsItem;
  {$ELSE}
  dwValue: DWORD;
  {$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  Rec.Name := PFarChar(Name);
  Rec.FType := FST_QWORD;
  Rec.Value.Number := Value;
  Result := FarApi.SettingsControl(fSettingsHandle, SCTL_SET, 0, @Rec) <> 0;
  {$ELSE}
  Result := RegSetValueEx(fCurrentKey, PChar(Name), 0, REG_QWORD, @Value, SizeOf(Value)) = ERROR_SUCCESS;
  if (not Result) and (Value >= 0) and (Value < $100000000) then begin
    dwValue := Value;
    Result := RegSetValueEx(fCurrentKey, PChar(Name), 0, REG_DWORD, @dwValue, SizeOf(dwValue)) = ERROR_SUCCESS;
  end;
  {$ENDIF}
end;

function TFarConfig.WriteString(const Name, Value: string): boolean;
{$IFDEF FAR3_UP}
var
  Rec: TFarSettingsItem;
{$ENDIF}
begin
  Result := False;
  if not IsOpen then
    Exit;
  {$IFDEF FAR3_UP}
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.StructSize := SizeOf(Rec);
  Rec.Root := fCurrentKey;
  Rec.Name := PFarChar(Name);
  Rec.FType := FST_STRING;
  Rec.Value.Str := PFarChar(Value);
  Result := FarApi.SettingsControl(fSettingsHandle, SCTL_SET, 0, @Rec) <> 0;
  {$ELSE}
  Result := RegSetValueEx(fCurrentKey, PChar(Name), 0, REG_SZ, PChar(Value), Succ(Length(Value)*Sizeof(Value[1]))) = ERROR_SUCCESS;
  {$ENDIF}
end;

{ TFarProgress }

function TFarProgress.BuildProgressBar(const Position, Max: int64): string;
var
  ProgressWidth: integer;
begin
  if (Max <= 0) or (Position <= 0) then
    ProgressWidth := 0
  else if Position < Max then
    ProgressWidth := fWidth * Position div Max
  else
    ProgressWidth := fWidth;
  Result := StringOfChar(#$2588, ProgressWidth) + StringOfChar(#$2591, fWidth - ProgressWidth);
end;

function TFarProgress.BuildProgressFilename(const FileName: string): string;
begin
  Result := TFarUtils.TruncateString(FileName, fWidth, ' ');
end;

procedure TFarProgress.CleanupConsole;
begin
  if fIsConsoleTitle then begin
    SetConsoleTitle(PChar(fConsoleTitle));
    SetLength(fConsoleTitle, 0);
    fIsConsoleTitle := False;
  end;
  if fSavedScreen <> INVALID_HANDLE_VALUE then begin
    FarApi.RestoreScreen(fSavedScreen);
    fSavedScreen := INVALID_HANDLE_VALUE;
  end;
  fStdInput := 0;
  fLastProgressTicks := 0;
end;

constructor TFarProgress.Create( {$IFDEF FAR3_UP} APluginGUID, ADialogGUID: TGUID {$ELSE} APluginHandle: THandle {$ENDIF} );
begin
  inherited Create;
  {$IFDEF FAR3_UP}
  fPluginGUID := APluginGUID;
  fDialogGUID := ADialogGUID;
  {$ELSE}
  fPluginHandle := APluginHandle;
  {$ENDIF}
  fFirstDisplay := True;
  fIsConsoleTitle := False;
  fConsoleTitle := '';
  fSavedScreen := INVALID_HANDLE_VALUE;
  fStdInput := 0;
  fGranularityMSec := 500;
  fCheckForEscape := False;
  fConsoleRect := TFarUtils.GetConsoleRect;
  fWidth := (fConsoleRect.Right - fConsoleRect.Left - 14);
end;

destructor TFarProgress.Destroy;
begin
  Hide;
  inherited;
end;

procedure TFarProgress.Hide;
begin
  CleanupConsole;
  fFirstDisplay := True;
end;

function TFarProgress.NeedsRefresh: boolean;
var
  Ticks, NextTicks: DWORD;
begin
  if fFirstDisplay then
    Result := True
  else begin
    NextTicks := fLastProgressTicks + fGranularityMSec;
    Ticks := GetTickCount;
    Result := Ticks >= NextTicks;
    if Result and (NextTicks < fLastProgressTicks) then
      Result := Ticks < fLastProgressTicks;
  end;
end;

procedure TFarProgress.PrepareConsole;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  fSavedScreen := FarApi.SaveScreen(0, 0, -1, -1);
  fStdInput := GetStdHandle(STD_INPUT_HANDLE);
  fIsConsoleTitle := GetConsoleTitle(Buffer, Length(Buffer)) <> 0;
  if fIsConsoleTitle then begin
    fConsoleTitle := string(Buffer);
    if fTitle <> '' then
      SetConsoleTitle(PChar(fTitle));
  end;
end;

procedure TFarProgress.Show(TitleMsg, DescriptionMsg: integer; const FileName: string; Progress, Max: int64);
var
  Description, FN, ProgressBar: string;
begin
  Description := BuildProgressFileName(TFarUtils.CopyString(TFarUtils.GetMsg(DescriptionMsg)));
  FN := BuildProgressFilename(FileName);
  ProgressBar := BuildProgressBar(Progress, Max);
  Show([TFarUtils.GetMsg(TitleMsg), PFarChar(Description), PFarChar(FN), PFarChar(ProgressBar)]);
end;

procedure TFarProgress.Show(TitleMsg, DescriptionMsg, BetweenFileNamesMsg: integer; const FileName1, Filename2: string; Progress, Max: int64);
var
  Description, BetweenFileNames, FN1, FN2, ProgressBar: string;
begin
  Description := BuildProgressFileName(TFarUtils.CopyString(TFarUtils.GetMsg(DescriptionMsg)));
  BetweenFileNames := BuildProgressFileName(TFarUtils.CopyString(TFarUtils.GetMsg(BetweenFileNamesMsg)));
  FN1 := BuildProgressFilename(FileName1);
  FN2 := BuildProgressFilename(FileName2);
  ProgressBar := BuildProgressBar(Progress, Max);
  Show([TFarUtils.GetMsg(TitleMsg), PFarChar(Description), PFarChar(FN1), PFarChar(BetweenFileNames), PFarChar(FN2), PFarChar(ProgressBar)]);
end;

procedure TFarProgress.Show(const Msg: array of PFarChar);
var
  Flags: TFarMessageFlags;
  ReadCount: DWORD;
  InputRec: TInputRecord;
  EscPressed: boolean;
begin
  if not NeedsRefresh then
    Exit;
  if fFirstDisplay then begin
    PrepareConsole;
    fFirstDisplay := False;
    Flags := FMSG_LEFTALIGN;
  end
  else begin
    Flags := FMSG_LEFTALIGN or FMSG_KEEPBACKGROUND;
  end;
  fLastProgressTicks := GetTickCount;
  TFarUtils.ShowMessage(Msg, Flags, 0);
  // Check for ESCAPE
  if CheckForEscape then
    while PeekConsoleInput(fStdInput, InputRec, 1, ReadCount) and (ReadCount > 0) do begin
      ReadConsoleInput(fStdInput, InputRec, 1, ReadCount);
      if (InputRec.EventType = KEY_EVENT) and (InputRec.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) and InputRec.Event.KeyEvent.bKeyDown then begin
        EscPressed := True;
        if Assigned(OnEscapePressed) then
          OnEscapePressed(Self, EscPressed);
        if EscPressed then
          Abort;
      end;
    end;
end;

{ TFarEditor }

class function TFarEditor.GetFileName(EditorID: TIntPtr; out FileName: string): boolean;
var
  n: Integer;
  Buffer: TFarCharArray;
begin
  Result := False;
  n := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_GETFILENAME, {$IFDEF FAR3_UP} 0, {$ENDIF} nil);
  if n > 0 then begin
    SetLength(Buffer, n);
    n := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_GETFILENAME, {$IFDEF FAR3_UP} n, {$ENDIF} @Buffer[0]);
    if n > 0 then begin
      FileName := TFarUtils.CopyLString(@Buffer[0], Pred(n));
      Result := True;
    end;
  end;
end;

class function TFarEditor.GetInfo(EditorID: TIntPtr; out Info: TEditorInfo): boolean;
begin
  FillChar(Info, Sizeof(Info), 0);
  {$IFDEF FAR3_UP}
  Info.StructSize := Sizeof(Info);
  {$ENDIF}
  Result := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_GETINFO, {$IFDEF FAR3_UP} 0, {$ENDIF} @Info) <> 0;
end;

class function TFarEditor.GetString(EditorID, RowNumber: TIntPtr; out Str: string): boolean;
var
  SelStart, SelLength: TIntPtr;
begin
  Result := GetString(EditorID, RowNumber, Str, SelStart, SelLength);
end;

class function TFarEditor.GetString(EditorID, RowNumber: TIntPtr; out Str: string; out SelStart, SelLength: TIntPtr): boolean;
var
  Info: TEditorGetString;
begin
  Result := False;
  FillChar(Info, Sizeof(Info), 0);
  {$IFDEF FAR3_UP}
  Info.StructSize := Sizeof(Info);
  {$ENDIF}
  Info.StringNumber := RowNumber;
  if FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_GETSTRING, {$IFDEF FAR3_UP} 0, {$ENDIF} @Info) <> 0 then begin
    Str := TFarUtils.CopyLString(Info.StringText, Info.StringLength);
    if Info.SelStart = -1 then begin
      SelStart := 0;
      SelLength := 0;
    end
    else
      SelStart := Succ(Info.SelStart);
    if Info.SelEnd = -1 then
      SelLength := Length(Str)
    else
      SelLength := Info.SelEnd - Info.SelStart;
    Result := True;
  end;
end;

class function TFarEditor.Refresh(EditorID: TIntPtr): boolean;
begin
  Result := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_REDRAW, {$IFDEF FAR3_UP} 0, {$ENDIF} nil) <> 0;
end;

class function TFarEditor.SetCursorPosition(EditorID, RowNumber: TIntPtr): boolean;
begin
  Result := SetCursorPosition(EditorID, RowNumber, -1);
end;

class function TFarEditor.SetCursorPosition(EditorID, RowNumber, ColNumber: TIntPtr): boolean;
const
  LINES_FROM_TOP = 5;
var
  TopScreenLine: TIntPtr;
begin
  if RowNumber >= LINES_FROM_TOP then
    TopScreenLine := RowNumber - LINES_FROM_TOP
  else
    TopScreenLine := -1;
  Result := SetCursorPosition(EditorID, RowNumber, ColNumber, -1, TopScreenLine, -1, -1);
end;

class function TFarEditor.SetCursorPosition(EditorID, RowNumber, ColNumber, TabPos, TopScreenLine, LeftPos, Overtype: TIntPtr): boolean;
var
  Info: TEditorSetPosition;
begin
  FillChar(Info, Sizeof(Info), 0);
  {$IFDEF FAR3_UP}
  Info.StructSize := Sizeof(Info);
  {$ENDIF}
  Info.CurLine := RowNumber;
  Info.CurPos := ColNumber;
  Info.CurTabPos := TabPos;
  Info.TopScreenLine := TopScreenLine;
  Info.LeftPos := LeftPos;
  Info.Overtype := Overtype;
  Result := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_SETPOSITION, {$IFDEF FAR3_UP} 0, {$ENDIF} @Info) <> 0;
end;

class function TFarEditor.SetString(EditorID, RowNumber: TIntPtr; const Str: string): boolean;
var
  Info: TEditorSetString;
begin
  FillChar(Info, Sizeof(Info), 0);
  {$IFDEF FAR3_UP}
  Info.StructSize := Sizeof(Info);
  {$ENDIF}
  Info.StringNumber := RowNumber;
  Info.StringLength := Length(Str);
  Info.StringText := PFarChar(Str);
  Info.StringEOL := nil;
  Result := FarApi.EditorControl( {$IFDEF FAR3_UP} EditorID, {$ENDIF} ECTL_SETSTRING, {$IFDEF FAR3_UP} 0, {$ENDIF} @Info) <> 0;
end;

{ TFarEditorManaged }

procedure TFarEditorManaged.AfterShowEditor;
begin
end;

procedure TFarEditorManaged.BeforeClose(var CanClose: boolean; var Result: TIntPtr);
begin
end;

constructor TFarEditorManaged.Create(const AFileName: string);
begin
  inherited Create;
  fManagedEditors.Add(Self);
  fEditorID := 0;
  fEditorFlags := 0;
  fFileName := AFileName;
end;

destructor TFarEditorManaged.Destroy;
var
  ix: integer;
begin
  ix := fManagedEditors.IndexOf(Self);
  if ix >= 0 then begin
    fManagedEditors.Delete(ix);
  end;
  inherited;
end;

function TFarEditorManaged.ShowEditor(Title: PFarChar; X1, Y1, X2, Y2: TIntPtr; Flags: TEditorFlags; StartLine, StartChar: TIntPtr; CodePage: TUIntPtr): boolean;
var
  EditorInfo: TEditorInfo;
begin
  Result := False;
  Self.fEditorTitle := TFarUtils.CopyString(Title);
  Self.fEditorFlags := Flags;
  Self.fEditorCodePage := CodePage;
  if FarApi.Editor(PFarChar(Self.fFileName), Title, X1, Y1, X2, Y2, Self.fEditorFlags or EF_IMMEDIATERETURN, StartLine, StartChar, CodePage) = EEC_MODIFIED then begin
    if TFarEditor.GetInfo(-1, EditorInfo) then begin
      Self.fEditorID := EditorInfo.EditorID;
      Self.AfterShowEditor;
      Result := True;
    end;
  end;
end;

class function TFarEditorManaged.FindByEditorID(EditorID: TIntPtr; out Editor: TFarEditorManaged): boolean;
var
  EditorInfo: TEditorInfo;
  i: integer;
begin
  Result := False;
  Editor := nil;
  if EditorID = -1 then
    if TFarEditor.GetInfo(EditorID, EditorInfo) then
      EditorID := EditorInfo.EditorID;
  if EditorID >= 0 then
    for i := 0 to Pred(fManagedEditors.Count) do
      if fManagedEditors[i].EditorID = EditorID then begin
        Editor := fManagedEditors[i];
        Result := True;
        Break;
      end;
end;

class procedure TFarEditorManaged.ProcessEditorEvent(Event, EditorID: TIntPtr; var Result: TIntPtr);
var
  Editor: TFarEditorManaged;
  CanClose: boolean;
  NewFileName: string;
  EditorInfo: TEditorInfo;
begin
  if Event = EE_CLOSE then begin
    if FindByEditorID(EditorID, Editor) then begin
      CanClose := True;
      Editor.BeforeClose(CanClose, Result);
      if CanClose then begin
        FreeAndNil(Editor);
      end
      else begin
        // Note: As of 2014-05-16, FAR does not provide any functionality to
        // abort editor's closing. What I can do is to open a new editor with
        // the same settings and let the older one close.
        if (Editor.fEditorFlags and EF_DELETEONLYFILEONCLOSE) <> 0 then begin
          // If the edited file gets deleted, I need to create a new one. Note
          // that this only makes sense for temporary files!
          NewFileName := TFarUtils.TempFileName;
          if RenameFile(Editor.fFileName, NewFileName) then
            Editor.fFileName := NewFileName;
        end;
        if TFarEditor.GetInfo(Editor.EditorID, EditorInfo) then begin
          if Editor.ShowEditor(PFarChar(Editor.fEditorTitle), -1, -1, -1, -1, Editor.fEditorFlags, EditorInfo.CurLine, EditorInfo.CurPos, EditorInfo.CodePage) then begin
            TFarEditor.SetCursorPosition(Editor.EditorID, EditorInfo.CurLine, EditorInfo.CurPos, EditorInfo.CurTabPos, EditorInfo.TopScreenLine, EditorInfo.LeftPos, EditorInfo.Overtype);
            {$MESSAGE HINT 'Possibly setup blocks and bookmarks'}
          end;
        end
        else begin
          Editor.ShowEditor(PFarChar(Editor.fEditorTitle), 0, 0, -1, -1, Editor.fEditorFlags, 0, 1, Editor.fEditorCodePage);
        end;
      end;
    end;
  end;
end;

initialization
  fActiveDialogs := TFarDialogs.Create;
  fManagedEditors := TFarEditorsManaged.Create;

finalization
  FreeAndNil(fActiveDialogs);
  FreeAndNil(fManagedEditors);

end.
