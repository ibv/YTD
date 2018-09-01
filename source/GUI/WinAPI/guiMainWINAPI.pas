(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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
  SynaCode,
  uLanguages, uFunctions, uMessages, uOptions, uStringUtils, uCompatibility,
  guiOptions, guiFunctions, uDialogs,
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
      function DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: integer): boolean; override;
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
      Options: TYTDOptions;
      DownloadList: TDownloadList;
      DownloadListHandle: THandle;
      NextProgressUpdate: DWORD;
      {$IFDEF CONVERTERS}
      LastConverterID: string;
      {$ENDIF}
      {$IFDEF SYSTRAY}
      function NotifyIconClick(Buttons: DWORD): boolean;
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
      {$IFDEF THREADEDVERSION}
      procedure NewVersionEvent(Sender: TObject; const Version, Url: string);
      {$ENDIF}
      procedure LoadSettings;
      procedure SaveSettings;
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
  guiConsts, guiAboutWINAPI, {$IFDEF CONVERTERS} guiConverterWINAPI, {$ENDIF} guiOptionsWINAPI;

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
  if Options.CheckForNewVersionOnStartup then
    Options.GetNewestVersionInBackground(NewVersionEvent);
  {$ENDIF}
end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(DownloadList);
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
            (iBitmap:  0; idCommand: ACTION_ADDNEWURL;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  0),
            (iBitmap:  1; idCommand: ACTION_ADDFROMFILE;          fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  1),
            (iBitmap:  2; idCommand: ACTION_ADDFROMHTML;          fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  2),
            (iBitmap:  3; idCommand: ACTION_ADDFROMWEBPAGE;       fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  3),
            (iBitmap:  4; idCommand: ACTION_ADDFROMCLIPBOARD1;    fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  4),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap:  5; idCommand: ACTION_COPYTOCLIPBOARD1;     fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  5),
            (iBitmap:  6; idCommand: ACTION_SAVETOFILE;           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  6),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap:  7; idCommand: ACTION_START;                fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  7),
            (iBitmap:  8; idCommand: ACTION_STOP;                 fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  8),
            (iBitmap:  9; idCommand: ACTION_CONVERT;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString:  9),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap: 10; idCommand: ACTION_DELETEURL;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 10),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap: 11; idCommand: ACTION_OPTIONS;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 11),
            (iBitmap: 12; idCommand: ACTION_EDITCONFIG;           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 12),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap: 13; idCommand: ACTION_REFRESH;              fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 13),
            (iBitmap: -1; idCommand: 0;                           fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_SEP;    dwData: 0; iString: -1),
            (iBitmap: 14; idCommand: ACTION_BUGREPORT;            fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 14),
            (iBitmap: 15; idCommand: ACTION_DONATE;               fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 15),
            (iBitmap: 16; idCommand: ACTION_ABOUT;                fsState: TBSTATE_ENABLED; fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 16)
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
  {$IFDEF SYSTRAY}
  if not Result then
    case Msg.Msg of
      WM_NOTIFYICON:
        Result := NotifyIconClick(Msg.lParam);
      end;
  {$ENDIF}
end;

function TFormMain.DoInitDialog: boolean;
const DownloadListStyle = LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_DOUBLEBUFFER or LVS_EX_LABELTIP;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  // Caption
  SetWindowText(Self.Handle, APPLICATION_CAPTION);
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
  // Redraw screen
  ActionRefresh;
end;

function TFormMain.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    DownloadList.StopAll;
    SaveSettings;
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
    Result := (MessageBox(0, PChar(_(MAINFORM_CAN_CLOSE)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes);
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
        end;
    end;
end;

function TFormMain.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: integer): boolean;
begin
  if (ControlID = IDC_LIST_DOWNLOADS) and (Code = LVN_GETDISPINFO) then
    Result := DownloadListGetDisplayInfo(PLVDispInfo(LParam))
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
  if ResizeType = SIZE_MINIMIZED then
    begin
    ShowWindow(Self.Handle, SW_HIDE);
    end;
  Result := inherited DoSize(ResizeType, NewWidth, NewHeight);
end;

function TFormMain.ActionAbout: boolean;
begin
  Result := True;
  with TFormAbout.Create(Self) do
    try
      DownloadClassifier := Self.DownloadList.DownloadClassifier;
      Options := Self.Options;
      ShowModal;
    finally
      Free;
      end;
end;

function TFormMain.ActionAddFromClipboard: boolean;
var L: TStringList;
    s: string;
    i, n: integer;
begin
  Result := True;
  if GetClipboardAsText(Self.Handle, s) then
    begin
    L := TStringList.Create;
    try
      n := 0;
      L.Text := s;
      for i := 0 to Pred(L.Count) do
        if AddTask(L[i]) then
          Inc(n);
      if n = 0 then
        ErrorMessageBox(_(MAINFORM_NO_SUPPORTED_URL), APPLICATION_TITLE);
    finally
      L.Free;
      end;
    end;
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
      L.Free;
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
    if MessageBox(0, PChar(_(MAINFORM_REPORT_BUG)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
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
      L.Free;
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
      L.Free;
      end;
  if Urls <> '' then
    SetClipboardAsText(Self.Handle, Urls);
end;

function TFormMain.ActionDeleteUrl: boolean;
var L: TList;
    i: integer;
begin
  Result := True;
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      if MessageBox(0, PChar(_(MAINFORM_DELETE_TRANSFERS)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
        for i := Pred(L.Count) downto 0 do
          DeleteTask(Integer(L[i]));
    finally
      L.Free;
      end;
end;

function TFormMain.ActionDonate: boolean;
begin
  Result := True;
  ShellExecute(0, 'open', DONATE_URL, nil, nil, SW_SHOWNORMAL);
end;

function TFormMain.ActionEditConfig: boolean;
begin
  Result := True;
  Options.Save;
  MessageBox(0, PChar(_(MAINFORM_EDIT_CONFIG)), APPLICATION_TITLE, MB_OK or MB_ICONWARNING or MB_TASKMODAL);
  ShellExecute(Handle, 'edit', PChar(Options.FileName), nil, nil, SW_SHOWNORMAL);
end;

function TFormMain.ActionOptions: boolean;
var F: TFormOptions;
begin
  Result := True;
  F := TFormOptions.Create(Self);
  try
    F.Options := Options;
    if F.ShowModal = idOK then
      begin
      SaveSettings;
      if Options.AutoStartDownloads then
        DownloadList.StartAll;
      end;
  finally
    F.Free;
    end;
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
      L.Free;
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
      L.Free;
      end;
end;

function TFormMain.ActionStop: boolean;
var L: TList;
    i: integer;
begin
  Result := True;
  if ListViewGetSelectedItems(DownloadListHandle, L) then
    try
      if MessageBox(0, PChar(_(MAINFORM_STOP_TRANSFERS)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_TASKMODAL) = idYes then
        for i := 0 to Pred(L.Count) do
          StopTask(Integer(L[i]));
    finally
      L.Free;
      end;
end;

{$IFDEF THREADEDVERSION}
procedure TFormMain.NewVersionEvent(Sender: TObject; const Version, Url: string);
begin
  if IsNewerVersion(Version) then
    begin
    fBugReportDisabled := True;
    EnableMenuItem(PopupMenu, ACTION_BUGREPORT, MF_BYCOMMAND or MF_GRAYED);
    ToolbarButtonSetEnabled(MainToolbar, ACTION_BUGREPORT, False);
    if MessageBox(0, PChar(Format(_(MAINFORM_NEW_VERSION_AVAILABLE), [Version])), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONQUESTION or MB_TASKMODAL) = idYes then
      ShellExecute(Handle, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
    end;
end;
{$ENDIF}

{$IFDEF SYSTRAY}
function TFormMain.NotifyIconClick(Buttons: DWORD): boolean;
begin
  Result := False;
  case Buttons of
    {WM_LBUTTONDBLCLK} WM_LBUTTONDOWN:
      begin
      if not ShowWindow(Self.Handle, SW_SHOW) then
        ShowWindow(Self.Handle, SW_RESTORE);
      Result := True;
      end;
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
    Item.Start;
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
begin
  Ticks := GetTickCount;
  if (Ticks > NextProgressUpdate) or ((NextProgressUpdate > $f0000000) and (Ticks < $10000000)) then
    begin
    NextProgressUpdate := Ticks + 250; // 0.25 sec.
    Idx := Sender.IndexOf(Item);
    if Idx >= 0 then
      RefreshItem(Idx);
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
