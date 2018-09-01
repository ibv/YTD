(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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


                              {$DEFINE TODO}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  SynaCode,
  uLanguages, uMessages, uOptions, uStringUtils, uCompatibility,
  guiOptions, guiConsts, guiFunctions, uDialogs,
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
      Icon_Main: THandle;
      Bitmap_DownloadStates: THandle;
      ImageList_DownloadStates: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    private
      // Moje soukroma data
      fLoading: boolean;
    protected
      // Moje properties a pomocne funkce
      Options: TYTDOptions;
      DownloadList: TDownloadList;
      DownloadListHandle: THandle;
      NextProgressUpdate: DWORD;
      function NotifyIconClick(Buttons: DWORD): boolean;
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
    protected
      class function DefaultResourceName: string; override;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
    end;

implementation

{$RESOURCE guiMainWINAPI.res}

uses
  guiAboutWINAPI;

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

  IDM_ADD_NEW_URL1 = 50000;
  IDM_ADD_URLS_FROM_CLIPBOARD1 = 50001;
  IDM_ADD_URLS_FROM_FILE1 = 50002;
  IDM_ADD_URLS_FROM_HTML_FILE1 = 50003;
  IDM_ADD_URLS_FROM_HTML_PAGE1 = 50004;
  IDM_SAVE_URL_LIST_TO_FILE1 = 50005;
  IDM_START_PAUSE_RESUME1 = 50007;
  IDM_STOP1 = 50008;
  IDM_CONVERT1 = 50009;
  IDM_DELETE_URL1 = 50010;
  IDM_COPY_URLS_TO_CLIPBOARD1 = 50011;
  IDM_SELECT_ALL1 = 50012;
  IDM_REFRESH_1 = 50015;
  IDM_OPTIONS1 = 50016;
  IDM_EDIT_CONFIG_FILE1 = 50017;
  IDM_REPORT_A_BUG1 = 50018;
  IDM_DONATE1 = 50019;
  IDM_ABOUT1 = 50020;

//
const
  LISTVIEW_SUBITEM_URL = 0;
  LISTVIEW_SUBITEM_PROVIDER = 1;
  LISTVIEW_SUBITEM_STATUS = 2;
  LISTVIEW_SUBITEM_TITLE = 3;
  LISTVIEW_SUBITEM_SIZE = 4;
  LISTVIEW_SUBITEM_PROGRESS = 5;

{ TFormMain }

class function TFormMain.DefaultResourceName: string;
begin
  Result := 'guiMainWINAPI';
end;

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
begin
  Icon_Main := LoadIcon(hInstance, 'MAINICON');
  Bitmap_DownloadStates := LoadBitmap(hInstance, 'IMAGELIST_DOWNLOADSTATES');
  ImageList_DownloadStates := ImageList_Create(16, 16, ILC_COLOR8, 8, 8);
  ImageList_AddMasked(ImageList_DownloadStates, Bitmap_DownloadStates, clPurple);
  PopupMenu := LoadMenu(hInstance, 'MAIN_POPUPMENU');
end;

procedure TFormMain.DestroyObjects;
begin
  DestroyMenu(PopupMenu); PopupMenu := 0;
  ImageList_Destroy(ImageList_DownloadStates); ImageList_DownloadStates := 0;
  FreeGDIObject(Bitmap_DownloadStates); Bitmap_DownloadStates := 0;
  FreeGDIObject(Icon_Main); Icon_Main := 0;
end;

function TFormMain.DialogProc(var Msg: TMessage): boolean;
begin
  Result := inherited DialogProc(Msg);
  if not Result then
    case Msg.Msg of
      WM_NOTIFYICON:
        Result := NotifyIconClick(Msg.lParam);
      end;
end;

function TFormMain.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  // Ikona
  SendMessage(Self.Handle, WM_SETICON, 0, Icon_Main);
  // Caption
  SetWindowText(Self.Handle, APPLICATION_CAPTION);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'MAIN_ACTIONS');
  // Download list
  DownloadListHandle := GetDlgItem(Self.Handle, IDC_LIST_DOWNLOADS);
  SendMessage(DownloadListHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, SendMessage(DownloadListHandle, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0) or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_DOUBLEBUFFER or LVS_EX_LABELTIP);
  ListViewInsertColumn(DownloadListHandle, 0, LISTVIEW_SUBITEM_URL,      alLeft,   160, _('URL'));
  ListViewInsertColumn(DownloadListHandle, 1, LISTVIEW_SUBITEM_PROVIDER, alLeft,    80, _('Provider'));
  ListViewInsertColumn(DownloadListHandle, 2, LISTVIEW_SUBITEM_STATUS,   alCenter,  96, _('Status'));
  ListViewInsertColumn(DownloadListHandle, 3, LISTVIEW_SUBITEM_TITLE,    alLeft,   200, _('Title'));
  ListViewInsertColumn(DownloadListHandle, 4, LISTVIEW_SUBITEM_SIZE,     alRight,   64, _('Size'));
  ListViewInsertColumn(DownloadListHandle, 5, LISTVIEW_SUBITEM_PROGRESS, alCenter, 120, _('Progress'));
  SendMessage(DownloadListHandle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageList_DownloadStates);
  // Tray icon
  {$IFDEF SYSTRAY}
    Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
    fNotifyIconData.cbSize := Sizeof(fNotifyIconData);
    fNotifyIconData.Wnd := Self.Handle;
    fNotifyIconData.uID := Integer(Self);
    fNotifyIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    fNotifyIconData.uCallbackMessage := WM_NOTIFYICON;
    fNotifyIconData.hIcon := Icon_Main;
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
    Result := (MessageBox(Self.Handle, PChar(_(MAINFORM_CAN_CLOSE)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_APPLMODAL) = idYes);
end;

function TFormMain.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    0: // Menu
      case Identifier of
        IDM_ADD_NEW_URL1:
          Result := ActionAddNewUrl;
        IDM_ADD_URLS_FROM_CLIPBOARD1:
          Result := ActionAddFromClipboard;
        IDM_ADD_URLS_FROM_FILE1:
          Result := ActionAddFromFile;
        IDM_ADD_URLS_FROM_HTML_FILE1:
          Result := ActionAddFromHtml;
        IDM_ADD_URLS_FROM_HTML_PAGE1:
          Result := ActionAddFromWebPage;
        IDM_SAVE_URL_LIST_TO_FILE1:
          Result := ActionSaveToFile;
        IDM_START_PAUSE_RESUME1:
          Result := ActionStart;
        IDM_STOP1:
          Result := ActionStop;
        IDM_CONVERT1:
          Result := ActionConvert;
        IDM_DELETE_URL1:
          Result := ActionDeleteUrl;
        IDM_COPY_URLS_TO_CLIPBOARD1:
          Result := ActionCopyToClipboard;
        IDM_SELECT_ALL1:
          Result := ActionSelectAll;
        IDM_REFRESH_1:
          Result := ActionRefresh;
        IDM_OPTIONS1:
          Result := ActionOptions;
        IDM_EDIT_CONFIG_FILE1:
          Result := ActionEditConfig;
        IDM_REPORT_A_BUG1:
          Result := ActionBugreport;
        IDM_DONATE1:
          Result := ActionDonate;
        IDM_ABOUT1:
          Result := ActionAbout;
      end;
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
          Result := ActionBugreport;
        ACTION_DONATE:
          Result := ActionDonate;
        ACTION_EDITCONFIG:
          Result := ActionEditConfig;
        ACTION_OPTIONS:
          Result := ActionOptions;
        end;
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
    if MessageBox(Self.Handle, PChar(_(MAINFORM_REPORT_BUG)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_APPLMODAL) = idYes then
      ReportBug(DownloadList, Index);
end;

function TFormMain.ActionConvert: boolean;
begin
  {$IFNDEF TODO}
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
      if MessageBox(Self.Handle, PChar(_(MAINFORM_DELETE_TRANSFERS)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_APPLMODAL) = idYes then
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
  MessageBox(Self.Handle, PChar(_(MAINFORM_EDIT_CONFIG)), APPLICATION_TITLE, MB_OK or MB_ICONWARNING or MB_APPLMODAL);
  ShellExecute(Handle, 'edit', PChar(Options.FileName), nil, nil, SW_SHOWNORMAL);
end;

function TFormMain.ActionOptions: boolean;
begin
  {$IFNDEF TODO}
  {$ENDIF}
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
      if MessageBox(Self.Handle, PChar(_(MAINFORM_STOP_TRANSFERS)), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONWARNING or MB_APPLMODAL) = idYes then
        for i := 0 to Pred(L.Count) do
          StartPauseResumeTask(Integer(L[i]));
    finally
      L.Free;
      end;
end;

{$IFDEF THREADEDVERSION}
procedure TFormMain.NewVersionEvent(Sender: TObject; const Version, Url: string);
begin
  if Version > {$INCLUDE 'YTD.version'} then
    begin
{$IFNDEF TODO}
    actReportBug.Enabled := False;
{$ENDIF}
    if MessageBox(Self.Handle, PChar(Format(_(MAINFORM_NEW_VERSION_AVAILABLE), [Version])), APPLICATION_TITLE, MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL) = idYes then
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

function TFormMain.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: integer): boolean;
begin
  if (ControlID = IDC_LIST_DOWNLOADS) and (Code = LVN_GETDISPINFO) then
    Result := DownloadListGetDisplayInfo(PLVDispInfo(LParam))
  else
    Result := inherited DoNotify(Control, ControlID, Code, WParam, LParam, NotifyResult);
end;

var StaticDisplayInfoText: string;

function TFormMain.DownloadListGetDisplayInfo(DispInfo: PLVDispInfo): boolean;

  function ShowStatic(const Text: string): boolean;
    begin
      StaticDisplayInfoText := Text;
      DispInfo^.item.pszText := PChar(StaticDisplayInfoText);
      DispInfo^.item.mask := DispInfo^.item.mask or LVIF_TEXT;
      Result := True;
    end;

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
      Result := _(ThreadStates[Item.State]);
      case Item.State of
        dtsPreparing:
          if Item.Paused then
            Result := _(THREADSTATE_PAUSED); // Download thread state: Paused
        dtsDownloading:
          if Item.Paused then
            Result := _(THREADSTATE_PAUSED); // Download thread state: Paused
        {$IFDEF CONVERTERS}
        dtsFinished:
            if (Item.ConvertState <> ctsWaiting) or (Options.SelectedConverterID <> '') then
              Result := _(ConvertThreadStates[Item.ConvertState]);
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
  Result := False;
  DlItem := DownloadList[DispInfo^.item.iItem];
  case DispInfo^.item.iSubItem of
    LISTVIEW_SUBITEM_URL:
      begin
      Result := ShowStatic(DownloadList.Urls[DispInfo^.item.iItem]);
      DispInfo^.item.iImage := GetStateImageIndex(DlItem);
      if DispInfo^.item.iImage >= 0 then
        DispInfo^.item.mask := DispInfo^.item.mask or LVIF_IMAGE;
      end;
    LISTVIEW_SUBITEM_PROVIDER:
      Result := ShowStatic(DlItem.Downloader.Provider);
    LISTVIEW_SUBITEM_STATUS:
      Result := ShowStatic(GetStateText(DlItem));
    LISTVIEW_SUBITEM_TITLE:
      if DlItem.Downloader.Prepared then
        Result := ShowStatic(DlItem.Downloader.Name)
      else
        Result := ShowStatic('');
    LISTVIEW_SUBITEM_SIZE:
      if DlItem.Downloader.Prepared and (DlItem.TotalSize >= 0) then
        Result := ShowStatic(PrettySize(DlItem.TotalSize))
      else
        Result := ShowStatic('');
    LISTVIEW_SUBITEM_PROGRESS:
      Result := ShowStatic(GetProgress(DlItem));
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

end.
