(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit guiMainWINAPI;
{$INCLUDE 'ytd.inc'}
{$DEFINE PREPARETRANSLATIONS}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  HttpSend, SynaCode,
  uLanguages, uFunctions, uMessages, uOptions, uStrings, uCompatibility,
  guiOptions, guiFunctions, uDialogs, uUpgrade,
  uDownloadList, uDownloadListItem, uDownloadThread;

{$IFDEF SYSTRAY}
const
  WM_NOTIFYICON  = WM_USER + 1;
{$ENDIF}

type
  TFormMain = class(TApiForm)
    protected
      function DialogProc(var Msg: TMessage): boolean; override;
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      function DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: LRESULT): boolean; override;
      function DoSize(ResizeType, NewWidth, NewHeight: integer): boolean; override;
      function CanClose: boolean; override;
    private
      // Moje rucne vytvarene objekty
      Bitmap_DownloadStates: THandle;
      ImageList_DownloadStates: THandle;
      Bitmap_MainToolbar: THandle;
      ImageList_MainToolbar: THandle;
      MainToolbar: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    private
      // Moje soukroma data
      fLoading: boolean;
      fBugReportDisabled: boolean;
    protected
      // Moje properties a pomocne funkce
      Options: TYTDOptionsGUI;
      {$IFDEF THREADEDVERSION}
      Upgrade: TYTDUpgrade;
      {$ENDIF}
      DownloadList: TDownloadList;
      DownloadListHandle: THandle;
      NextProgressUpdate: DWORD;
      NextClipboardViewer: THandle;
      LastClipboardText: string;
      {$IFDEF CONVERTERS}
      LastConverterID: string;
      {$ENDIF}
      procedure StartClipboardMonitor;
      procedure StopClipboardMonitor;
      function ClipboardChanged: boolean;
      function ClipboardChainChange(Removing, NewNext: THandle): boolean;
      {$IFDEF SYSTRAY}
      function NotifyIconClick(Buttons: LPARAM): boolean;
      {$ENDIF}
      {$IFDEF SINGLEINSTANCE}
      function CopyData(SenderHandle: THandle; Info: PCopyDataStruct): boolean; 
      {$ENDIF}
      function ActionAddNewUrl: boolean;
      function ActionDeleteUrl: boolean;
      function ActionStart: boolean;
      function ActionStop: boolean;
      function ActionAddFromClipboard: boolean;
      function ActionCopyToClipboard: boolean;
      function ActionSelectAll: boolean;
      function ActionRefresh: boolean;
      function ActionAddFromWebPage: boolean;
      function ActionAddFromHtml: boolean;
      function ActionAddFromFile: boolean;
      function ActionSaveToFile: boolean;
      function ActionAbout: boolean;
      function ActionConvert: boolean;
      function ActionBugreport: boolean;
      function ActionDonate: boolean;
      function ActionEditConfig: boolean;
      function ActionOptions: boolean;
      function ActionPlay: boolean;
      function ActionExploreFolder: boolean;
      function ActionMenu: boolean;
      {$IFDEF THREADEDVERSION}
      procedure NewYTDEvent(Sender: TYTDUpgrade);
      procedure NewDefsEvent(Sender: TYTDUpgrade);
      {$ENDIF}
      procedure LoadSettings;
      procedure SaveSettings;
      function AddFromClipboard(IgnoreUnchangedText: boolean = False): integer; virtual;
      function AddTask(const Url: string): boolean; virtual;
      procedure AddTaskFromHTML(const Source: string); virtual;
      procedure DeleteTask(Index: integer); virtual;
      procedure StartPauseResumeTask(Index: integer); virtual;
      procedure StopTask(Index: integer); virtual;
      {$IFDEF CONVERTERS}
      procedure ConvertTask(Index: integer; const ConverterID: string); virtual;
      {$ENDIF}
      procedure RefreshItem(Index: integer); virtual;
      procedure RefreshAllItems; virtual;
      procedure DownloadListChange(Sender: TObject); virtual;
      procedure DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem); virtual;
      procedure DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem); virtual;
      function DownloadListGetDisplayInfo(DispInfo: PLVDispInfo): boolean; virtual;
      procedure GetProgress(out Progress, Total: int64);
      {$IFDEF SYSTRAY}
    protected
      fNotifyIconData: TNotifyIconData;
      {$ENDIF}
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
    end;

implementation

{$RESOURCE *.res}

uses
  guiConsts, guiAboutWINAPI, {$IFDEF CONVERTERS} guiConverterWINAPI, {$ENDIF} guiOptionsWINAPI,
  uScriptedDownloader;


// from resource.h
const
  IDC_LIST_DOWNLOADS = 1000;

const
  ACTION_ADDNEWURL = 40000;
  ACTION_DELETEURL = 40001;
  ACTION_START = 40002;
  ACTION_STOP = 40003;
  ACTION_ADDFROMCLIPBOARD1 = 40004;
  ACTION_ADDFROMCLIPBOARD2 = 40005;
  ACTION_COPYTOCLIPBOARD1 = 40006;
  ACTION_COPYTOCLIPBOARD2 = 40007;
  ACTION_SELECTALL = 40008;
  ACTION_REFRESH = 40009;
  ACTION_ADDFROMWEBPAGE = 40013;
  ACTION_ADDFROMHTML = 40014;
  ACTION_ADDFROMFILE = 40015;
  ACTION_SAVETOFILE = 40016;
  ACTION_ABOUT = 40017;
  ACTION_CONVERT = 40018;
  ACTION_BUGREPORT = 40020;
  ACTION_DONATE = 40021;
  ACTION_EDITCONFIG = 40022;
  ACTION_OPTIONS = 40023;
  ACTION_EXPLOREFOLDER = 40024;
  ACTION_PLAY = 40025;
  ACTION_MENU = 40026;


//
const
  LISTVIEW_SUBITEM_URL = 0;
  LISTVIEW_SUBITEM_PROVIDER = 1;
  LISTVIEW_SUBITEM_STATUS = 2;
  LISTVIEW_SUBITEM_TITLE = 3;
  LISTVIEW_SUBITEM_SIZE = 4;
  LISTVIEW_SUBITEM_PROGRESS = 5;

{$IFDEF PREPARETRANSLATIONS}
var
  TranslatedThreadStates: array[TDownloadThreadState] of string;
  TranslatedConvertThreadStates: array[TConvertThreadState] of string;
  TranslatedThreadStatePaused: string;

procedure PrepareTranslations;
var i: TDownloadThreadState;
    j: TConvertThreadState;
begin
  for i := Low(TDownloadThreadState) to High(TDownloadThreadState) do
    TranslatedThreadStates[i] := _(ThreadStates[i]);
  for j := Low(TConvertThreadState) to High(TConvertThreadState) do
    TranslatedConvertThreadStates[j] := _(ConvertThreadStates[j]);
  TranslatedThreadStatePaused := _(THREADSTATE_PAUSED);
end;
{$ENDIF}

{ TFormMain }

constructor TFormMain.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
  fLoading := True;
  try
    Options := TYTDOptionsGUI.Create;
    TScriptedDownloader.InitMainScriptEngine(Options.ScriptFileName);
    UseLanguage(Options.Language);
    DownloadList := TDownloadList.Create;
    DownloadList.OnListChange := DownloadListChange;
    DownloadList.OnStateChange := DownloadListItemChange;
    DownloadList.OnDownloadProgress := DownloadListProgress;
    DownloadList.OnError := DownloadListItemChange;
    DownloadList.OnFinished := DownloadListItemChange;
    {$IFDEF CONVERTERS}
    DownloadList.OnConverted := DownloadListItemChange;
    {$ENDIF}
    DownloadList.Options := Options;
    LoadSettings;
  finally
    fLoading := False;
    end;
  {$IFDEF THREADEDVERSION}
  Upgrade := TYTDUpgrade.Create(Options);
  Upgrade.OnNewYTDFound := NewYTDEvent;
  Upgrade.OnNewDefsFound := NewDefsEvent;
  if Options.CheckForNewVersionOnStartup then
    Upgrade.TestUpgrades(True);
  {$ENDIF}
end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(DownloadList);
  {$IFDEF THREADEDVERSION}
  FreeAndNil(Upgrade);
  {$ENDIF}
  FreeAndNil(Options);
  inherited;
end;

procedure TFormMain.CreateObjects;
const ToolbarButtonHints: array[0..16] of string
  // Translation is not needed, because these strings are copied from the VCL version's resources
        = (
            'Add new URL',
            'Add URLs from file',
            'Add URLs from HTML file',
            'Add URLs from HTML page',
            'Add URLs from Clipboard',
            'Copy URLs to Clipboard',
            'Save URL list',
            'Start/Pause/Resume',
            'Stop',
            'Convert',
            'Delete URL',
            'Options',
            'Edit config file',
            'Refresh',
            'Report a bug',
            'Donate',
            'About'
          );
const ToolbarButtons: array[0..22] of TTBButton
        = (
            (iBitmap:  0; idCommand: ACTION_ADDNEWURL;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  0),
            (iBitmap:  1; idCommand: ACTION_ADDFROMFILE;          fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  1),
            (iBitmap:  2; idCommand: ACTION_ADDFROMHTML;          fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  2),
            (iBitmap:  3; idCommand: ACTION_ADDFROMWEBPAGE;       fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  3),
            (iBitmap:  4; idCommand: ACTION_ADDFROMCLIPBOARD1;    fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  4),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap:  5; idCommand: ACTION_COPYTOCLIPBOARD1;     fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  5),
            (iBitmap:  6; idCommand: ACTION_SAVETOFILE;           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  6),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap:  7; idCommand: ACTION_START;                fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  7),
            (iBitmap:  8; idCommand: ACTION_STOP;                 fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  8),
            (iBitmap:  9; idCommand: ACTION_CONVERT;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString:  9),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap: 10; idCommand: ACTION_DELETEURL;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 10),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap: 11; idCommand: ACTION_OPTIONS;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 11),
            (iBitmap: 12; idCommand: ACTION_EDITCONFIG;           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 12),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap: 13; idCommand: ACTION_REFRESH;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 13),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: -1),
            (iBitmap: 14; idCommand: ACTION_BUGREPORT;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 14),
            (iBitmap: 15; idCommand: ACTION_DONATE;               fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 15),
            (iBitmap: 16; idCommand: ACTION_ABOUT;                fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; bReserved: (0, 0 {$IFDEF WIN64} , 0, 0, 0, 0 {$ENDIF} ); dwData: 0; iString: 16)
          );
var i: integer;
    Hints: string;
begin
  Bitmap_DownloadStates := LoadBitmap(hInstance, 'IMAGELIST_DOWNLOADSTATES');
  ImageList_DownloadStates := ImageList_Create(16, 16, ILC_COLOR8, 8, 8);
  ImageList_AddMasked(ImageList_DownloadStates, Bitmap_DownloadStates, clPurple);
  Bitmap_MainToolbar := LoadBitmap(hInstance, 'MAIN_TOOLBAR');
  ImageList_MainToolbar := ImageList_Create(16, 16, ILC_COLOR8, 8, 8);
  ImageList_AddMasked(ImageList_MainToolbar, Bitmap_MainToolbar, clPurple);
  MainToolbar := CreateWindowEx(0, TOOLBARCLASSNAME, nil, WS_CHILD or TBSTYLE_WRAPABLE or TBSTYLE_TOOLTIPS, 0, 0, 0, 0, Self.Handle, 0, hInstance, nil);
  SendMessage(MainToolbar, TB_SETIMAGELIST, 0, ImageList_MainToolbar);
  SendMessage(MainToolbar, TB_SETMAXTEXTROWS, 0, 0); // Needed to that the texts appear as hints rather than captions
  Hints := '';
  for i := 0 to Pred(Length(ToolbarButtonHints)) do
    Hints := Hints + _(ToolbarButtonHints[i]) + #0;
  SendMessage(MainToolbar, TB_ADDSTRING, 0, LPARAM(PChar(Hints)));
  SendMessage(MainToolbar, TB_BUTTONSTRUCTSIZE, Sizeof(ToolbarButtons[0]), 0);
  SendMessage(MainToolbar, TB_ADDBUTTONS, Length(ToolbarButtons), LPARAM(@ToolbarButtons[0]));
  PopupMenu := LoadMenu(hInstance, 'MAIN_POPUPMENU');
end;

procedure TFormMain.DestroyObjects;
begin
  DestroyMenu(PopupMenu); PopupMenu := 0;
  ImageList_Destroy(ImageList_DownloadStates); ImageList_DownloadStates := 0;
  FreeGDIObject(Bitmap_DownloadStates);
  DestroyWindow(MainToolbar); MainToolbar := 0;
  ImageList_Destroy(ImageList_MainToolbar); ImageList_MainToolbar := 0;
  FreeGDIObject(Bitmap_MainToolbar);
end;

function TFormMain.DialogProc(var Msg: TMessage): boolean;
begin
  Result := inherited DialogProc(Msg);
  if not Result then
    case Msg.Msg of
      {$IFDEF SYSTRAY}
      WM_NOTIFYICON:
        Result := NotifyIconClick(Msg.lParam);
      {$ENDIF}
      WM_DRAWCLIPBOARD:
        Result := ClipboardChanged;
      WM_CHANGECBCHAIN:
        Result := ClipboardChainChange(Msg.wParam, Msg.lParam);
      {$IFDEF SINGLEINSTANCE}
      WM_COPYDATA:
        Result := CopyData(Msg.wParam, PCopyDataStruct(Msg.lParam));
      {$ENDIF}
      end;
end;

function TFormMain.DoInitDialog: boolean;
const DownloadListStyle = LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_DOUBLEBUFFER or LVS_EX_LABELTIP;
var Current: TRect;
    DesiredLeft, DesiredTop, DesiredWidth, DesiredHeight: integer;
    {$IFDEF SINGLEINSTANCE}
    Param: string;
    {$ENDIF}
    i: integer;
begin
  Result := inherited DoInitDialog;
  {$IFDEF SINGLEINSTANCE}
  RegisterMainInstance(Self.Handle);
  {$ENDIF}
  CreateObjects;
  Self.Translate;
  // Application
  SetClassLong(Self.Handle, GCL_HICON, Icon);
  // Caption
  SetWindowText(Self.Handle, PChar(APPLICATION_CAPTION {$IFDEF UNICODE} + ' (Unicode)' {$ELSE} + ' (ANSI)' {$ENDIF} ));
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'MAIN_ACTIONS');
  // Toolbar
  SendMessage(MainToolbar, TB_AUTOSIZE, 0, 0);
  ShowWindow(MainToolbar, 1);
  // Download list
  DownloadListHandle := GetDlgItem(Self.Handle, IDC_LIST_DOWNLOADS);
  SendMessage(DownloadListHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, DownloadListStyle, DownloadListStyle);
  SendMessage(DownloadListHandle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageList_DownloadStates);
  ListViewInsertColumn(DownloadListHandle, 0, LISTVIEW_SUBITEM_URL,      alLeft,   160, _('URL'));
  ListViewInsertColumn(DownloadListHandle, 1, LISTVIEW_SUBITEM_PROVIDER, alLeft,    80, _('Provider'));
  ListViewInsertColumn(DownloadListHandle, 2, LISTVIEW_SUBITEM_STATUS,   alCenter,  96, _('Status'));
  ListViewInsertColumn(DownloadListHandle, 3, LISTVIEW_SUBITEM_TITLE,    alLeft,   200, _('Title'));
  ListViewInsertColumn(DownloadListHandle, 4, LISTVIEW_SUBITEM_SIZE,     alRight,   64, _('Size'));
  ListViewInsertColumn(DownloadListHandle, 5, LISTVIEW_SUBITEM_PROGRESS, alCenter, 120, _('Progress'));
  // Clipboard monitor
  NextClipboardViewer := 0;
  StartClipboardMonitor;
  // Tray icon
  {$IFDEF SYSTRAY}
    Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
    fNotifyIconData.cbSize := Sizeof(fNotifyIconData);
    fNotifyIconData.Wnd := Self.Handle;
    fNotifyIconData.uID := Integer(Self);
    fNotifyIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    fNotifyIconData.uCallbackMessage := WM_NOTIFYICON;
    fNotifyIconData.hIcon := Icon;
    StrPCopy(fNotifyIconData.szTip, Copy(APPLICATION_CAPTION, 1, Pred(Length(fNotifyIconData.szTip))));
    Shell_NotifyIcon(NIM_ADD, @fNotifyIconData);
  {$ENDIF}
  // Facilitate resizing
  SetControlAnchors(DownloadListHandle, [akTop, akLeft, akRight, akBottom]);
  // Resize the main form
  DesiredLeft := Options.MainFormLeft;
  DesiredTop := Options.MainFormTop;
  DesiredWidth := Options.MainFormWidth;
  DesiredHeight := Options.MainFormHeight;
  if GetWindowRect(Handle, Current) then
    begin
    if (DesiredLeft <= -32768) or (DesiredLeft >= (GetSystemMetrics(SM_CXSCREEN) + 20)) then
      DesiredLeft := Current.Left;
    if (DesiredTop <= -32768) or (DesiredTop >= (GetSystemMetrics(SM_CYSCREEN) + 20)) then
      DesiredTop := Current.Top;
    if DesiredWidth <= 0 then
      DesiredWidth := Current.Right - Current.Left + 1;
    if DesiredHeight <= 0 then
      DesiredHeight := Current.Bottom - Current.Top + 1;
    MoveWindow(Handle, DesiredLeft, DesiredTop, DesiredWidth, DesiredHeight, True);
    end;
  // Resize downloadlist columns
  for i := 0 to 5 do
    ListViewSetColumnWidth(DownloadListHandle, i, Options.DownloadListColumnWidth[i]);
  {$IFDEF SINGLEINSTANCE}
  // Load URLs from command line
  for i := 1 to ParamCount do
    begin
    Param := ParamStr(i);
    if Param <> '' then
      if Param[1] <> '-' then
        AddTask(Param);
    end;
  {$ENDIF}
  // Redraw screen
  ActionRefresh;
end;

function TFormMain.DoClose: boolean;
var Current: TRect;
    i: integer;
begin
  {$IFDEF SINGLEINSTANCE}
  UnregisterMainInstance(Self.Handle);
  {$ENDIF}
  Result := inherited DoClose;
  if Result then
    begin
    DownloadList.StopAll;
    if GetWindowRect(Handle, Current) then
      begin
      Options.MainFormLeft := Current.Left;
      Options.MainFormTop := Current.Top;
      Options.MainFormWidth := Current.Right - Current.Left + 1;
      Options.MainFormHeight := Current.Bottom - Current.Top + 1;
      end;
    for i := 0 to 5 do
      Options.DownloadListColumnWidth[i] := ListViewGetColumnWidth(DownloadListHandle, i);
    SaveSettings;
    StopClipboardMonitor;
    {$IFDEF SYSTRAY}
    Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
    {$ENDIF}
    Accelerators := 0;
    DestroyObjects;
    end;
end;

function TFormMain.CanClose: boolean;
begin
  if DownloadList.DownloadingCount <= 0 then
    Result := True
  else
    Result := (MessageBox(0, PChar(_(MAINFORM_CAN_CLOSE)), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes);
end;

function TFormMain.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    0, // Menu
    1: // Accelerators
      case Identifier of
        ACTION_ADDNEWURL:
          Result := ActionAddNewUrl;
        ACTION_DELETEURL:
          Result := ActionDeleteUrl;
        ACTION_START:
          Result := ActionStart;
        ACTION_STOP:
          Result := ActionStop;
        ACTION_ADDFROMCLIPBOARD1,
        ACTION_ADDFROMCLIPBOARD2:
          Result := ActionAddFromClipboard;
        ACTION_COPYTOCLIPBOARD1,
        ACTION_COPYTOCLIPBOARD2:
          Result := ActionCopyToClipboard;
        ACTION_SELECTALL:
          Result := ActionSelectAll;
        ACTION_REFRESH:
          Result := ActionRefresh;
        ACTION_ADDFROMWEBPAGE:
          Result := ActionAddFromWebPage;
        ACTION_ADDFROMHTML:
          Result := ActionAddFromHtml;
        ACTION_ADDFROMFILE:
          Result := ActionAddFromFile;
        ACTION_SAVETOFILE:
          Result := ActionSaveToFile;
        ACTION_ABOUT:
          Result := ActionAbout;
        ACTION_CONVERT:
          Result := ActionConvert;
        ACTION_BUGREPORT:
          Result := (not fBugReportDisabled) and ActionBugreport;
        ACTION_DONATE:
          Result := ActionDonate;
        ACTION_EDITCONFIG:
          Result := ActionEditConfig;
        ACTION_OPTIONS:
          Result := ActionOptions;
        ACTION_EXPLOREFOLDER:
          Result := ActionExploreFolder;
        ACTION_PLAY:
          Result := ActionPlay;
        ACTION_MENU:
          Result := ActionMenu;
        end;
    end;
end;

function TFormMain.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: LRESULT): boolean;
begin
  if (ControlID = IDC_LIST_DOWNLOADS) and (Code = LVN_GETDISPINFO) then
    Result := DownloadListGetDisplayInfo(PLVDispInfo(LParam))
  else if (ControlID = IDC_LIST_DOWNLOADS) and (Code = NM_DBLCLK) then
    Result := ActionPlay
  else
    Result := inherited DoNotify(Control, ControlID, Code, WParam, LParam, NotifyResult);
end;

function TFormMain.DownloadListGetDisplayInfo(DispInfo: PLVDispInfo): boolean;

  function GetStateImageIndex(Item: TDownloadListItem): integer;
    begin
      Result := ThreadStateImgs[Item.State];
      case Item.State of
        dtsPreparing:
          if Item.Paused then
            Result := 4;
        dtsDownloading:
          if Item.Paused then
            Result := 4;
        {$IFDEF CONVERTERS}
        dtsFinished:
            if (Item.ConvertState <> ctsWaiting) or (Options.SelectedConverterID <> '') then
              Result := ConvertThreadStateImgs[Item.ConvertState];
        {$ENDIF}
        end;
    end;

  function GetStateText(Item: TDownloadListItem): string;
    begin
      Result := {$IFDEF PREPARETRANSLATIONS} TranslatedThreadStates[Item.State] {$ELSE} _(ThreadStates[Item.State]) {$ENDIF} ;
      case Item.State of
        dtsPreparing,
        dtsDownloading:
          if Item.Paused then
            Result := {$IFDEF PREPARETRANSLATIONS} TranslatedThreadStatePaused {$ELSE} _(THREADSTATE_PAUSED) {$ENDIF} ; // Download thread state: Paused
        {$IFDEF CONVERTERS}
        dtsFinished:
            if (Item.ConvertState <> ctsWaiting) or (Options.SelectedConverterID <> '') then
              Result := {$IFDEF PREPARETRANSLATIONS} TranslatedConvertThreadStates[Item.ConvertState] {$ELSE} _(ConvertThreadStates[Item.ConvertState]) {$ENDIF} ;
        {$ENDIF}
        end;
    end;

  function GetProgress(Item: TDownloadListItem): string;
    begin
      Result := '';
      case Item.State of
        dtsDownloading:
          Result := GetProgressStr(Item.DownloadedSize, Item.TotalSize);
        dtsFailed:
          Result := Item.ErrorMessage + ' (' + Item.ErrorClass + ')';
        dtsAborted:
          Result := GetProgressStr(Item.DownloadedSize, Item.TotalSize);
        end;
    end;

var DlItem: TDownloadListItem;
begin
try
  Result := False;
  if DownloadList <> nil then
    begin
    DlItem := DownloadList[DispInfo^.item.iItem];
    case DispInfo^.item.iSubItem of
      LISTVIEW_SUBITEM_URL:
        begin
        Result := ListViewSetVirtualItemText(DispInfo, DownloadList.Urls[DispInfo^.item.iItem]);
        DispInfo^.item.iImage := GetStateImageIndex(DlItem);
        if DispInfo^.item.iImage >= 0 then
          DispInfo^.item.mask := DispInfo^.item.mask or LVIF_IMAGE;
        end;
      LISTVIEW_SUBITEM_PROVIDER:
        Result := ListViewSetVirtualItemText(DispInfo, DlItem.Downloader.Provider);
      LISTVIEW_SUBITEM_STATUS:
        Result := ListViewSetVirtualItemText(DispInfo, GetStateText(DlItem));
      LISTVIEW_SUBITEM_TITLE:
        if DlItem.Downloader.Prepared then
          Result := ListViewSetVirtualItemText(DispInfo, DlItem.Downloader.Name)
        else
          Result := ListViewSetVirtualItemText(DispInfo, '');
      LISTVIEW_SUBITEM_SIZE:
        if DlItem.Downloader.Prepared and (DlItem.TotalSize >= 0) then
          Result := ListViewSetVirtualItemText(DispInfo, PrettySize(DlItem.TotalSize))
        else
          Result := ListViewSetVirtualItemText(DispInfo, '');
      LISTVIEW_SUBITEM_PROGRESS:
        Result := ListViewSetVirtualItemText(DispInfo, GetProgress(DlItem));
      end;
    end;
except
  on Exception do
    Result := False;
    end;
end;

function TFormMain.DoSize(ResizeType, NewWidth, NewHeight: integer): boolean;
begin
  {$IFDEF SYSTRAY}
  if ResizeType = SIZE_MINIMIZED then
    ShowWindow(Self.Handle, SW_HIDE);
  {$ENDIF}
  Result := inherited DoSize(ResizeType, NewWidth, NewHeight);
end;

function TFormMain.ActionAbout: boolean;
begin
  Result := True;
  with TFormAbout.Create do
    try
      DownloadClassifier := Self.DownloadList.DownloadClassifier;
      Options := Self.Options;
      ShowModal;
    finally
      Free;
      end;
end;

function TFormMain.ActionAddFromClipboard: boolean;
begin
  Result := True;
  if AddFromClipboard = 0 then
    ErrorMessageBox(_(MAINFORM_NO_SUPPORTED_URL), APPLICATION_TITLE);
end;

function TFormMain.ActionAddFromFile: boolean;
var L: TStringList;
    FileName: string;
    i, n: integer;
begin
  Result := True;
  FileName := '';
  if OpenDialog(FileName, '', '', _('List files (*.lst)|*.lst|All files|*.*')) then
    begin
    L := TStringList.Create;
    try
      n := 0;
      L.LoadFromFile(FileName);
      for i := 0 to Pred(L.Count) do
        if L[i] <> '' then
          if AddTask(L[i]) then
            Inc(n);
      if n = 0 then
        ErrorMessageBox(_(MAINFORM_NO_SUPPORTED_URL), APPLICATION_TITLE);
    finally
      FreeAndNil(L);
      end;
    end;
end;

function TFormMain.ActionAddFromHtml: boolean;
var FileName: string;
begin
  Result := True;
  FileName := '';
  if OpenDialog(FileName, '', '', _('HTML files|*.htm;*.html|All files|*.*')) then
    AddTaskFromHTML(FileName);
end;

function TFormMain.ActionAddFromWebPage: boolean;
var Url: string;
begin
  Result := True;
  GetClipboardAsText(Self.Handle, Url);
  if InputQuery(APPLICATION_TITLE, _(MAINFORM_ENTER_PAGE_URL), Url) then
    AddTaskFromHTML(Url);
end;

function TFormMain.ActionAddNewUrl: boolean;
var Url: string;
begin
  Result := True;
  GetClipboardAsText(Self.Handle, Url);
  if InputQuery(APPLICATION_TITLE, _(MAINFORM_ENTER_VIDEO_URL), Url) then
    if not AddTask(Url) then
      ErrorMessageBox(_(MAINFORM_URL_NOT_SUPPORTED), APPLICATION_TITLE);
end;

function TFormMain.ActionBugreport: boolean;
var Index: integer;
begin
  Result := True;
  Index := ListViewGetSelectedItem(DownloadListHandle);
  if Index >= 0 then
    if DownloadList[Index].DownloadedSize > MAX_DOWNLOAD_SIZE_FOR_BUGREPORT then
      MessageBox(0, PChar(_(MAINFORM_NOBUGREPORTIFDOWNLOADSTARTED)), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or MB_TASKMODAL)
    else if MessageBox(0, PChar(_(MAINFORM_REPORT_BUG)), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
      ReportBug(DownloadList, Index);
end;

function TFormMain.ActionConvert: boolean;
{$IFDEF CONVERTERS}
var L: TList;
    i: integer;
    ConverterID: string;
{$ENDIF}
begin
  {$IFDEF CONVERTERS}
  Result := True;
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  if not Options.ConvertersActivated then
    begin
    ErrorMessageBox(_(CONVERTERS_INACTIVE_WARNING), APPLICATION_TITLE);
    Exit;
    end;
  {$ENDIF}
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      if LastConverterID = '' then
        ConverterID := Options.SelectedConverterID
      else
        ConverterID := LastConverterID;
      if SelectConverter(Options, ConverterID, Self, _(MAINFORM_CONVERT_WITH)) then
        begin
        LastConverterID := ConverterID;
        for i := 0 to Pred(L.Count) do
          ConvertTask(Integer(L[i]), ConverterID);
        end;
    finally
      FreeAndNil(L);
      end;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TFormMain.ActionCopyToClipboard: boolean;
var Urls: string;
    L: TList;
    i: integer;
begin
  Result := True;
  Urls := '';
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      for i := 0 to Pred(L.Count) do
        if Urls = '' then
          Urls := DownloadList.Urls[Integer(L[i])]
        else
          Urls := Urls + EOLN + DownloadList.Urls[Integer(L[i])];
    finally
      FreeAndNil(L);
      end;
  if Urls <> '' then
    begin
    StopClipboardMonitor;
    try
      SetClipboardAsText(Self.Handle, Urls);
    finally
      StartClipboardMonitor;
      end;
    end;
end;

function TFormMain.ActionDeleteUrl: boolean;
var L: TList;
    i: integer;
begin
  Result := True;
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      if MessageBox(0, PChar(_(MAINFORM_DELETE_TRANSFERS)), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
        for i := Pred(L.Count) downto 0 do
          DeleteTask(Integer(L[i]));
    finally
      FreeAndNil(L);
      end;
end;

function TFormMain.ActionDonate: boolean;
begin
  Result := True;
  Run(DONATE_URL, Handle);
end;

function TFormMain.ActionEditConfig: boolean;
begin
  Result := True;
  Options.Save;
  MessageBox(0, PChar(_(MAINFORM_EDIT_CONFIG)), PChar(APPLICATION_TITLE), MB_OK or MB_ICONWARNING or MB_TASKMODAL);
  if ShellExecute(Handle, 'edit', PChar(Options.FileName), nil, nil, SW_SHOWNORMAL) <= 32 then
    Run('notepad', '"' + Options.FileName + '"', Handle);
end;

function TFormMain.ActionExploreFolder: boolean;
var Index: integer;
begin
  Result := True;
  Index := ListViewGetSelectedItem(DownloadListHandle);
  if Index >= 0 then
    DownloadList.Items[Index].ExploreMedia;
end;

function TFormMain.ActionMenu: boolean;
var
  Current: TRect;
begin
  if GetWindowRect(Handle, Current) then
    Result := DoContextMenu(PopupMenu, Point(Current.Left, Current.Top))
  else
    Result := DoContextMenu(PopupMenu, Point(0, 0))
end;

function TFormMain.ActionOptions: boolean;
var F: TFormOptions;
begin
  Result := True;
  F := TFormOptions.Create;
  try
    F.Options := Options;
    if F.ShowModal = idOK then
      begin
      SaveSettings;
      if Options.AutoStartDownloads then
        DownloadList.StartAll;
      StopClipboardMonitor;
      StartClipboardMonitor;
      end;
  finally
    FreeAndNil(F);
    end;
end;

function TFormMain.ActionPlay: boolean;
var Index: integer;
begin
  Result := True;
  Index := ListViewGetSelectedItem(DownloadListHandle);
  if Index >= 0 then
    DownloadList.Items[Index].PlayMedia;
end;

function TFormMain.ActionRefresh: boolean;
begin
  Result := True;
  RefreshAllItems;
end;

function TFormMain.ActionSaveToFile: boolean;
var FileName: string;
    L: TStringList;
    i: integer;
begin
  Result := True;
  FileName := 'ytd.lst';
  if SaveDialog(FileName) then
    begin
    L := TStringList.Create;
    try
      for i := 0 to Pred(DownloadList.Count) do
        if DownloadList[i].State <> dtsFinished then
          L.Add(DownloadList.Urls[i]);
      L.SaveToFile(FileName);
    finally
      FreeAndNil(L);
      end;
    end;
end;

function TFormMain.ActionSelectAll: boolean;
//var i: integer;
begin
  Result := True;
  ListViewSelectItem(DownloadListHandle, -1, True);
//  for i := 0 to Pred(DownloadList.Count) do
//    ListViewSelectItem(DownloadListHandle, i, True);
end;

function TFormMain.ActionStart: boolean;
var L: TList;
    i: integer;
begin
  Result := True;
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      for i := 0 to Pred(L.Count) do
        StartPauseResumeTask(Integer(L[i]));
    finally
      FreeAndNil(L);
      end;
end;

function TFormMain.ActionStop: boolean;
var L: TList;
    i: integer;
begin
  Result := True;
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      if MessageBox(0, PChar(_(MAINFORM_STOP_TRANSFERS)), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
        for i := 0 to Pred(L.Count) do
          StopTask(Integer(L[i]));
    finally
      FreeAndNil(L);
      end;
end;

procedure TFormMain.StartClipboardMonitor;
var s: string;
begin
  if GetClipboardAsText(Self.Handle, s) then
    LastClipboardText := s
  else
    LastClipboardText := '';
  if Options.MonitorClipboard then
    NextClipboardViewer := SetClipboardViewer(Self.Handle)
  else
    NextClipboardViewer := 0;
end;

procedure TFormMain.StopClipboardMonitor;
begin
  ChangeClipboardChain(Self.Handle, NextClipboardViewer);
  NextClipboardViewer := 0;
end;

function TFormMain.ClipboardChanged: boolean;
begin
  if NextClipboardViewer <> 0 then
    SendMessage(NextClipboardViewer, WM_DRAWCLIPBOARD, 0, 0);
  try
    DownloadList.AutoTryHtmlParserTemporarilyDisabled := True;
    AddFromClipboard(True);
  finally
    DownloadList.AutoTryHtmlParserTemporarilyDisabled := False;
    end;
  Result := True;
end;

function TFormMain.ClipboardChainChange(Removing, NewNext: THandle): boolean;
begin
  if Removing = NextClipboardViewer then
    NextClipboardViewer := NewNext;
  Result := True;
end;

{$IFDEF THREADEDVERSION}
procedure TFormMain.NewYTDEvent(Sender: TYTDUpgrade);
begin
  if (Sender.OnlineYTDVersion <> '') and (Sender.OnlineYTDUrl <> '') then
    if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) < 0 then
      begin
      fBugReportDisabled := True;
      EnableMenuItem(PopupMenu, ACTION_BUGREPORT, MF_BYCOMMAND or MF_GRAYED);
      ToolbarButtonSetEnabled(MainToolbar, ACTION_BUGREPORT, False);
      if MessageBox(0, PChar(Format(_(MAINFORM_NEW_VERSION_AVAILABLE), [Sender.OnlineYTDVersion])), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONQUESTION or MB_TASKMODAL) = idYes then
        guiFunctions.UpgradeYTD(Sender, Handle);
      end;
end;

procedure TFormMain.NewDefsEvent(Sender: TYTDUpgrade);
begin
  if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) >= 0 then
    if TScriptedDownloader.MainScriptEngine <> nil then
      if (Sender.OnlineDefsVersion <> '') and (Sender.OnlineDefsUrl <> '') then
        if Sender.CompareVersions(TScriptedDownloader.MainScriptEngine.Version, Sender.OnlineDefsVersion) < 0 then
          begin
          fBugReportDisabled := True;
          if MessageBox(0, PChar(Format(_(MAINFORM_NEW_DEFS_VERSION_AVAILABLE), [Sender.OnlineDefsVersion])), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONQUESTION or MB_TASKMODAL) = idYes then
            if guiFunctions.UpgradeDefs(Sender, Handle) then
              fBugReportDisabled := False;
          EnableMenuItem(PopupMenu, ACTION_BUGREPORT, MF_BYCOMMAND or MF_GRAYED);
          ToolbarButtonSetEnabled(MainToolbar, ACTION_BUGREPORT, False);
          end;
end;
{$ENDIF}

{$IFDEF SYSTRAY}
function TFormMain.NotifyIconClick(Buttons: LPARAM): boolean;
begin
  Result := False;
  case Buttons of
    {WM_LBUTTONDBLCLK} WM_LBUTTONDOWN:
      begin
      ShowWindow(Self.Handle, SW_SHOWNORMAL);
      SetForegroundWindow(Self.Handle);
      Result := True;
      end;
    end;
end;
{$ENDIF}

{$IFDEF SINGLEINSTANCE}
function TFormMain.CopyData(SenderHandle: THandle; Info: PCopyDataStruct): boolean;
var
  UrlW: WideString;
begin
  Result := False;
  if Info <> nil then
    if Info^.dwData = COPYDATA_URL then
      if (Info^.cbData > 0) and (Info^.lpData <> nil) then
        begin
        SetLength(UrlW, Info^.cbData div Sizeof(WideChar));
        Move(Info^.lpData^, UrlW[1], Info^.cbData);
        Result := AddTask(UrlW);
        end; 
end;
{$ENDIF}

procedure TFormMain.LoadSettings;
begin
  DownloadList.LoadFromOptions;
end;

procedure TFormMain.SaveSettings;
begin
  if not fLoading then
    begin
    DownloadList.SaveToOptions;
    Options.Save;
    end;
end;

function TFormMain.AddFromClipboard(IgnoreUnchangedText: boolean): integer;
var L: TStringList;
    s: string;
    i: integer;
begin
  Result := 0;
  if GetClipboardAsText(Self.Handle, s) then
    if (not IgnoreUnchangedText) or (s <> LastClipboardText) then
      begin
      L := TStringList.Create;
      try
        L.Text := s;
        for i := 0 to Pred(L.Count) do
          if AddTask(L[i]) then
            Inc(Result);
      finally
        FreeAndNil(L);
        end;
      end;
end;

function TFormMain.AddTask(const Url: string): boolean;
begin
  Result := DownloadList.Add(Url) >= 0;
  if Result then
    SaveSettings;
end;

procedure TFormMain.AddTaskFromHTML(const Source: string);
begin
  DownloadList.AddFromHTML(Source);
  SaveSettings;
end;

procedure TFormMain.DeleteTask(Index: integer);
begin
  DownloadList.Delete(Index);
  SaveSettings;
end;

procedure TFormMain.StartPauseResumeTask(Index: integer);
var Item: TDownloadListItem;
begin
  Item := DownloadList.Items[Index];
  if Item.Downloading then
    if Item.Paused then
      Item.Start
    else
      Item.Pause
  else
    begin
    Item.RetryCount := Options.DownloadRetryCount;
    Item.Start;
    end;
end;

procedure TFormMain.StopTask(Index: integer);
begin
  DownloadList.Items[Index].Stop;
end;

{$IFDEF CONVERTERS}
procedure TFormMain.ConvertTask(Index: integer; const ConverterID: string);
begin
  DownloadList.Items[Index].Convert(True, ConverterID);
end;
{$ENDIF}

procedure TFormMain.DownloadListChange(Sender: TObject);
begin
  RefreshAllItems;
end;

procedure TFormMain.DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem);
var Idx: integer;
begin
  Idx := Sender.IndexOf(Item);
  if Self.Handle <> 0 then
    ShowApiError(SendMessage(DownloadListHandle, LVM_SETITEMCOUNT, Sender.Count, 0) = 0);
  if (DownloadList <> nil) and (DownloadList[Idx] <> nil) then
    if DownloadList[Idx].State = dtsFinished then
      SaveSettings;
  if Idx >= 0 then
    RefreshItem(Idx);
end;

procedure TFormMain.DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem);
var Ticks: DWORD;
    Idx: integer;
    Progress, Total: int64;
begin
  Ticks := GetTickCount;
  if (Ticks > NextProgressUpdate) or ((NextProgressUpdate > $f0000000) and (Ticks < $10000000)) then
    begin
    NextProgressUpdate := Ticks + 250; // 0.25 sec.
    Idx := Sender.IndexOf(Item);
    if Idx >= 0 then
      RefreshItem(Idx);
    GetProgress(Progress, Total);
    if Total > 0 then
      ShowTotalProgressBar(Self.Handle, pbsNormal, Progress, Total)
    else
      ShowTotalProgressBar(Self.Handle, pbsNoProgress);
    end;
end;

procedure TFormMain.GetProgress(out Progress, Total: int64);
var
  i: integer;
begin
  Progress := 0;
  Total := 0;
  if DownloadList <> nil then
    for i := 0 to Pred(DownloadList.Count) do
      if DownloadList[i].State in [dtsWaiting, dtsPreparing, dtsDownloading] then
        begin
        Inc(Progress, DownloadList[i].DownloadedSize);
        Inc(Total, DownloadList[i].TotalSize);
        end;
end;

procedure TFormMain.RefreshItem(Index: integer);
begin
  if Self.Handle <> 0 then
    SendMessage(DownloadListHandle, LVM_UPDATE, Index, 0);
end;

procedure TFormMain.RefreshAllItems;
var i: integer;
begin
  if Self.Handle <> 0 then
    begin
    ShowApiError(SendMessage(DownloadListHandle, LVM_SETITEMCOUNT, DownloadList.Count, 0) = 0);
    for i := 0 to Pred(DownloadList.Count) do
      RefreshItem(i);
    UpdateWindow(DownloadListHandle);
    end;
end;

initialization
  InitCommonControls;
  uApiForm.ApiFormTranslateFunction := @uLanguages._;
  {$IFDEF PREPARETRANSLATIONS}
  PrepareTranslations;
  {$ENDIF}

end.
