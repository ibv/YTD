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

unit guiMainLCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  windows,LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ClipBrd, Menus, ImgList, ActnList,
  ToolWin, ExtCtrls,  {CommDlg, CommCtrl,}
  {$IFNDEF DELPHI7_UP}
  FileCtrl,
  {$ENDIF}
  {$IFDEF DELPHIXE4_UP}
  UITypes,
  {$ENDIF}
  {$IFDEF DELPHIX_SEATTLE_UP}
  Types,
  {$ENDIF}
  SynaCode,
  uLanguages, uFunctions, uMessages, uOptions, uStrings, uCompatibility,
  guiOptions, guiFunctions, {uDialogs,} uUpgrade,
  uDownloadList, uDownloadListItem, uDownloadThread, Types;

{$IFDEF SYSTRAY}
const
  WM_NOTIFYICON  = WM_USER + 1;
{$ENDIF}

type

  { TFormYTD }

  TFormYTD = class(TForm)
    actQClick: TAction;
    actVQuality: TAction;
    Downloads: TListView;
    Item1: TMenuItem;
    Item2: TMenuItem;
    Item3: TMenuItem;
    Item6: TMenuItem;
    item4: TMenuItem;
    Item5: TMenuItem;
    VQualityPopUp: TPopupMenu;
    ToolVQuality: TToolButton;
    TrayIcon: TTrayIcon;
    YTDActions: TActionList;
    ActionImages: TImageList;
    actAddNewUrl: TAction;
    actDeleteURL: TAction;
    actAddUrlsFromClipboard: TAction;
    actAddUrlsFromClipboard2: TAction;
    StateImages: TImageList;
    actStart: TAction;
    actStop: TAction;
    actCopyUrlsToClipboard: TAction;
    actCopyUrlsToClipboard2: TAction;
    actRefresh: TAction;
    actSelectAll: TAction;
    DownloadsPopup: TPopupMenu;
    mnuAddNewUrl: TMenuItem;
    mnuAddUrlFromClipboard: TMenuItem;
    mnuDelete: TMenuItem;
    mnuCopyUrls: TMenuItem;
    mnuStart: TMenuItem;
    mnuStop: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuRefresh: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    ToolBar1: TToolBar;
    ToolAddNew: TToolButton;
    ToolAddClipboard: TToolButton;
    ToolStart: TToolButton;
    ToolStop: TToolButton;
    ToolButton5: TToolButton;
    ToolDelete: TToolButton;
    ToolCopy: TToolButton;
    ToolButton9: TToolButton;
    ToolRefresh: TToolButton;
    StatusBar1: TStatusBar;
    N3: TMenuItem;
    actAddUrlsFromHTML: TAction;
    OpenHtmlFile: TOpenDialog;
    actAddUrlsFromHTMLfile: TAction;
    OpenUrlList: TOpenDialog;
    actAddUrlsFromFile: TAction;
    ToolAddFromFile: TToolButton;
    ToolButton1: TToolButton;
    ToolAddFromHTML: TToolButton;
    ToolAddFromHTMLfile: TToolButton;
    mnuAddUrlFromFile: TMenuItem;
    mnuAddUrlFromHtmlPage: TMenuItem;
    mnuAddUrlFromHtmlFile: TMenuItem;
    N4: TMenuItem;
    actSaveUrlList: TAction;
    mnuSaveUrlList: TMenuItem;
    ToolSave: TToolButton;
    SaveUrlList: TSaveDialog;
    ToolButton2: TToolButton;
    actAbout: TAction;
    ToolButton3: TToolButton;
    ToolAbout: TToolButton;
    mnuAbout: TMenuItem;
    actConvert: TAction;
    ToolConvert: TToolButton;
    mnuConvert: TMenuItem;
    ToolOptions: TToolButton;
    mnuOptions: TMenuItem;
    actReportBug: TAction;
    ToolReportBug: TToolButton;
    actDonate: TAction;
    ToolDonate: TToolButton;
    mnuReportBug: TMenuItem;
    mnuDonate: TMenuItem;
    actEditConfigFile: TAction;
    ToolEditConfigFile: TToolButton;
    ToolButton4: TToolButton;
    Editconfigfile1: TMenuItem;
    actOptions: TAction;
    actPlay: TAction;
    actExploreFolder: TAction;
    N5: TMenuItem;
    mnuPlay: TMenuItem;
    mnuExplore: TMenuItem;
    actMenu: TAction;
    procedure DownloadsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DownloadsDblClick(Sender: TObject);
    procedure DownloadsData(Sender: TObject; Item: TListItem);
    procedure actAddNewUrlExecute(Sender: TObject);
    procedure actDeleteURLExecute(Sender: TObject);
    procedure actAddUrlsFromClipboardExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actCopyUrlsToClipboardExecute(Sender: TObject);
    procedure actAddUrlsFromClipboard2Execute(Sender: TObject);
    procedure actCopyUrlsToClipboard2Execute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAddUrlsFromHTMLExecute(Sender: TObject);
    procedure actAddUrlsFromHTMLfileExecute(Sender: TObject);
    procedure actAddUrlsFromFileExecute(Sender: TObject);
    procedure actSaveUrlListExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actConvertExecute(Sender: TObject);
    procedure actReportBugExecute(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure actEditConfigFileExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure actExploreFolderExecute(Sender: TObject);
    procedure actMenuExecute(Sender: TObject);
    procedure Item1Click(Sender: TObject);
    procedure Item2Click(Sender: TObject);
    procedure Item3Click(Sender: TObject);
    procedure item4Click(Sender: TObject);
    procedure Item5Click(Sender: TObject);
    procedure Item6Click(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  protected
    fLoading: boolean;
    fNextTotalRecalculation: TDateTime;
    fTotalProgress: int64;
    fTotalData: int64;
    fMenuID: integer;
    fProvID, fVQID: integer;
    {$IFDEF SYSTRAY}
    fNotifyIconData: TNotifyIconData;
    procedure WMClickIcon(var msg: TMessage); message WM_NOTIFYICON;
    procedure ApplicationMinimize(Sender: TObject);
    {$ENDIF}
    procedure StartClipboardMonitor;
    procedure StopClipboardMonitor;
    procedure WMDrawClipboard(var msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMChangeCbChain(var msg: TMessage); message WM_CHANGECBCHAIN;
    {$IFDEF SINGLEINSTANCE}
    procedure WMCopyData(var msg: TMessage); message WM_COPYDATA;
    {$ENDIF}
    {$IFDEF THREADEDVERSION}
    procedure NewYTDEvent(Sender: TYTDUpgrade);
    procedure NewDefsEvent(Sender: TYTDUpgrade);
    {$ENDIF}
    procedure GetProgress(out Progress, Total: int64);
  protected
    DownloadList: TDownloadList;
    NextProgressUpdate: DWORD;
    Options: TYTDOptionsGUI;
    Upgrade: TYTDUpgrade;
    NextClipboardViewer: WPARAM;
    LastClipboardText: string;
    {$IFDEF CONVERTERS}
    LastConverterID: string;
    {$ENDIF}
    procedure Refresh; virtual;
    procedure DownloadListChange(Sender: TObject); virtual;
    procedure DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    procedure DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    function AddFromClipboard(IgnoreUnchangedText: boolean = False): integer; virtual;
    function AddTask(const Url: string): boolean; virtual;
    procedure AddTaskFromHTML(const Source: string); virtual;
    procedure DeleteTask(Index: integer); virtual;
    procedure StartPauseResumeTask(Index: integer); virtual;
    procedure StopTask(Index: integer); virtual;
    {$IFDEF CONVERTERS}
    procedure ConvertTask(Index: integer; const ConverterID: string); virtual;
    {$ENDIF}
    procedure PlayMedia(Index: integer); virtual;
    procedure ExploreMedia(Index: integer); virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    procedure VQualityPopUpSetting;
  public
  end;

var FormYTD: TFormYTD;

implementation

{$R *.dfm}

uses
  guiConsts, guiAboutLCL, {$IFDEF CONVERTERS} guiConverterLCL, {$ENDIF} guiOptionsLCL,
  uScriptedDownloader;

const

  LVM_FIRST                    = $1000;
  LVM_GETHEADER                = LVM_FIRST + 31;
  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54;
  LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55;
  LVM_GETSUBITEMRECT           = LVM_FIRST + 56;
  LVM_SETHOVERTIME             = LVM_FIRST + 71;
  LVM_GETHOVERTIME             = LVM_FIRST + 72;


{ TFormYTD }

procedure TFormYTD.FormCreate(Sender: TObject);
var
  i: integer;
  arch: string;
  {$IFDEF SINGLEINSTANCE}
  Param: string;
  {$ENDIF}
begin
  fLoading := True;
  try
    {$IFDEF SINGLEINSTANCE}
    RegisterMainInstance(Self.Handle);
    {$ENDIF}
    arch:='';
    if SizeOf(Pointer) > 4 then arch:=' - x64';
    Caption := APPLICATION_CAPTION + arch + ' (fpc)';
    Options := TYTDOptionsGUI.Create;
    TScriptedDownloader.InitMainScriptEngine(Options.ScriptFileName);
    UseLanguage(Options.Language);
    {$IFDEF GETTEXT}
    TranslateProperties(self);
    {$ENDIF}
    DownloadList := TDownloadList.Create;
    DownloadList.OnListChange := DownloadListChange;
    DownloadList.OnStateChange := DownloadListItemChange;
    DownloadList.OnDownloadProgress := DownloadListProgress;
    DownloadList.OnError := DownloadListItemChange;
    DownloadList.OnFinished := DownloadListItemChange;
    {$IFDEF CONVERTERS}
    DownloadList.OnConverted := DownloadListItemChange;
    {$ELSE}
    actConvert.Visible := False;
    actConvert.Enabled := False;
    {$ENDIF}
    DownloadList.Options := Options;
    LoadSettings;
    NextClipboardViewer := 0;
    StartClipboardMonitor;
    {$IFDEF SYSTRAY}
    if Options.MinimizeToTray then
      begin
      { ///Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
      fNotifyIconData.cbSize := Sizeof(fNotifyIconData);
      fNotifyIconData.Wnd := Self.Handle;
      fNotifyIconData.uID := Integer(Self);
      fNotifyIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      fNotifyIconData.uCallbackMessage := WM_NOTIFYICON;
      fNotifyIconData.hIcon := Application.Icon.Handle;
      StrPCopy(fNotifyIconData.szTip, Copy(Caption, 1, Pred(Length(fNotifyIconData.szTip))));
      Shell_NotifyIcon(NIM_ADD, @fNotifyIconData);}
      //TrayIcon.ShowBalloonHint;
      Application.OnMinimize := ApplicationMinimize;
      end;
    TrayIcon.Hint:=Caption;
    TrayIcon.BalloonHint:=Caption;
    {$ENDIF}
    // Use double-buffered listview (removes flickering)
    SendMessage(Downloads.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, SendMessage(Downloads.Handle, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0) or LVS_EX_DOUBLEBUFFER);
    // Window size
    if (Options.MainFormLeft > -32768) and (Options.MainFormLeft < (Screen.Width + 20)) then
      Self.Left := Options.MainFormLeft;
    if (Options.MainFormTop > -32768) and (Options.MainFormTop < (Screen.Height + 20)) then
      Self.Top := Options.MainFormTop;
    if Options.MainFormWidth > 0 then
      Self.Width := Options.MainFormWidth;
    if Options.MainFormHeight > 0 then
      Self.Height := Options.MainFormHeight;
    // Column widths
    for i := 0 to Pred(Downloads.Columns.Count) do
      if Options.DownloadListColumnWidth[i] > 0 then
      Downloads.Columns[i].Width := Options.DownloadListColumnWidth[i];
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
    CheckForExternalLibraries(0, Options);
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

procedure TFormYTD.FormDestroy(Sender: TObject);
begin
  {$IFDEF SINGLEINSTANCE}
  UnregisterMainInstance(Self.Handle);
  {$ENDIF}
  DownloadList.StopAll;
  StopClipboardMonitor;
  {$IFDEF SYSTRAY}
  ///if Options.MinimizeToTray then
    ///Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
  {$ENDIF}
  try
    FreeAndNil(DownloadList);
  except
  end;
  {$IFDEF THREADEDVERSION}
  FreeAndNil(Upgrade);
  {$ENDIF}
  FreeAndNil(Options);
end;

procedure TFormYTD.GetProgress(out Progress, Total: int64);
var
  i: integer;
begin
  if Now >= fNextTotalRecalculation then
    begin
    fTotalProgress := 0;
    fTotalData := 0;
    if DownloadList <> nil then
      for i := 0 to Pred(DownloadList.Count) do
        if DownloadList[i].State in [dtsWaiting, dtsPreparing, dtsDownloading] then
          begin
          Inc(fTotalProgress, DownloadList[i].DownloadedSize);
          Inc(fTotalData, DownloadList[i].TotalSize);
          end;
    end;
  fNextTotalRecalculation := Now + 0.5/(24*60*60);
  Progress := fTotalProgress;
  Total := fTotalData;
end;

procedure TFormYTD.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DownloadList.DownloadingCount <= 0 then
    CanClose := True
  else
    CanClose := (MessageDlg(_(MAINFORM_CAN_CLOSE), mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes);
end;

procedure TFormYTD.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  DownloadList.StopAll;
  Options.MainFormLeft := Self.Left;
  Options.MainFormTop := Self.Top;
  Options.MainFormWidth := Self.Width;
  Options.MainFormHeight := Self.Height;
  for i := 0 to Pred(Downloads.Columns.Count) do
    Options.DownloadListColumnWidth[i] := Downloads.Columns[i].Width;
  SaveSettings;
end;

{$IFDEF SYSTRAY}
{$ifdef mswindows}

procedure TFormYTD.WMClickIcon(var msg: TMessage);
begin
  case Msg.lParam of
    {WM_LBUTTONDBLCLK} WM_LBUTTONDOWN:
      begin
        Show;
        BringToFront;
      end;
    end;
end;

{$endif}

procedure TFormYTD.ApplicationMinimize(Sender: TObject);
begin
  WindowState:=wsMinimized;
  Hide;
end;
{$ENDIF}

procedure TFormYTD.StartClipboardMonitor;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    LastClipboardText := Clipboard.AsText
  else
    LastClipboardText := '';
  if Options.MonitorClipboard then
    NextClipboardViewer := SetClipboardViewer(Self.Handle)
  else
    NextClipboardViewer := 0;
end;

procedure TFormYTD.StopClipboardMonitor;
begin
  ChangeClipboardChain(Self.Handle, NextClipboardViewer);
  NextClipboardViewer := 0;
end;

{$ifdef mswindows}

procedure TFormYTD.WMDrawClipboard(var msg: TMessage);
begin
  if NextClipboardViewer <> 0 then
    SendMessage(NextClipboardViewer, WM_DRAWCLIPBOARD, 0, 0);
  try
    DownloadList.AutoTryHtmlParserTemporarilyDisabled := True;
    AddFromClipboard(True);
  finally
    DownloadList.AutoTryHtmlParserTemporarilyDisabled := False;
    end;
  msg.Result := 0;
end;

procedure TFormYTD.WMChangeCbChain(var msg: TMessage);
begin
  if Msg.wParam = NextClipboardViewer then
    NextClipboardViewer := Msg.lParam;
  msg.Result := 0;
end;

{$endif}

{$IFDEF SINGLEINSTANCE}
procedure TFormYTD.WMCopyData(var msg: TMessage);
var
  Info: PCopyDataStruct;
  UrlW: WideString;
begin
  Info := PCopyDataStruct(msg.lParam);
  if Info <> nil then
    if Info^.dwData = COPYDATA_URL then
      if (Info^.cbData > 0) and (Info^.lpData <> nil) then
        begin
        SetLength(UrlW, Info^.cbData div Sizeof(WideChar));
        Move(Info^.lpData^, UrlW[1], Info^.cbData);
        AddTask(UrlW);
        end;
  Msg.Result := 0;
end;
{$ENDIF}

{$IFDEF THREADEDVERSION}
procedure TFormYTD.NewYTDEvent(Sender: TYTDUpgrade);
begin
  if (Sender.OnlineYTDVersion <> '') and (Sender.OnlineYTDUrl <> '') then
    if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) < 0 then
      begin
      actReportBug.Enabled := False;
      if MessageDlg(Format(_(MAINFORM_NEW_VERSION_AVAILABLE), [Sender.OnlineYTDVersion]), mtInformation, [mbYes, mbNo], 0) = mrYes then
        guiFunctions.UpgradeYTD(Sender, Handle);
      end;
end;

procedure TFormYTD.NewDefsEvent(Sender: TYTDUpgrade);
begin
  if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) >= 0 then
    if TScriptedDownloader.MainScriptEngine <> nil then
      if (Sender.OnlineDefsVersion <> '') and (Sender.OnlineDefsUrl <> '') then
        if Sender.CompareVersions(TScriptedDownloader.MainScriptEngine.Version, Sender.OnlineDefsVersion) < 0 then
          begin
          actReportBug.Enabled := False;
          if MessageDlg(Format(_(MAINFORM_NEW_DEFS_VERSION_AVAILABLE), [Sender.OnlineDefsVersion]), mtInformation, [mbYes, mbNo], 0) = mrYes then
            if guiFunctions.UpgradeDefs(Sender, Handle, True) then
              actReportBug.Enabled := True;
          end;
end;
{$ENDIF}

procedure TFormYTD.DownloadListChange(Sender: TObject);
begin
  Refresh;
end;

procedure TFormYTD.DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem);
var Idx: integer;
begin
  Idx := Sender.IndexOf(Item);
  Downloads.Items.Count := Sender.Count;
  if Idx >= 0 then
    begin
    ///Downloads.UpdateItems(Idx, Idx);
    Downloads.BeginUpdate;
    Downloads.Update;

    if (DownloadList <> nil) and (DownloadList[Idx] <> nil) then
      if DownloadList[Idx].State = dtsFinished then
        SaveSettings;
    end;
    Downloads.EndUpdate;
end;

procedure TFormYTD.DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem);
var Ticks: DWORD;
begin
  Ticks := GetTickCount64;
  if (Ticks > NextProgressUpdate) or ((NextProgressUpdate > $f0000000) and (Ticks < $10000000)) then
    begin
    NextProgressUpdate := Ticks + 250; // 0.25 sec.
    Downloads.Refresh;
    end;
end;

procedure TFormYTD.DownloadsData(Sender: TObject; Item: TListItem);
var DlItem: TDownloadListItem;
    sState, sTitle, sSize, sProgress: string;
    {$IFNDEF F-PC}
    iStateImage: integer;
    {$ENDIF}
    Progress, Total: int64;
begin
  if DownloadList <> nil then
    if Item.Index < DownloadList.Count then
      begin
      DlItem := DownloadList[Item.Index];
      Item.Caption := DownloadList.Urls[Item.Index];
      {$IFNDEF F-PC}
      Item.StateIndex := -1;///Integer(DlItem.State);
      {$ENDIF}
      Item.SubItems.Add(DlItem.Downloader.Provider);
      sState := _(ThreadStates[DlItem.State]);
      {$IFNDEF F-PC}
      iStateImage := ThreadStateImgs[DlItem.State];
      {$ENDIF}
      sTitle := '';
      sSize := '';
      sProgress := '';
      if DlItem.Downloader.Prepared then
        begin
        sTitle := DlItem.Downloader.Name;
        if DlItem.TotalSize >= 0 then
          sSize := PrettySize(DlItem.TotalSize);
        end;
      case DlItem.State of
        dtsWaiting:
          ;
        dtsPreparing:
          begin
          if DlItem.Paused then
            begin
            sState := _(THREADSTATE_PAUSED); // Download thread state: Paused
            {$IFNDEF F-PC}
            iStateImage := 4;
            {$ENDIF}
            end;
          end;
        dtsDownloading:
          begin
          sProgress := GetProgressStr(DlItem.DownloadedSize, DlItem.TotalSize);
          if DlItem.Paused then
            begin
            sState := _(THREADSTATE_PAUSED); // Download thread state: Paused
            {$IFNDEF F-PC}
            iStateImage := 4;
            {$ENDIF}
            end;
          end;
        dtsFinished:
          begin
          {$IFDEF CONVERTERS}
            if (DlItem.ConvertState <> ctsWaiting) or (Options.SelectedConverterID <> '') then
              begin
              sState := _(ConvertThreadStates[DlItem.ConvertState]);
              {$IFNDEF F-PC}
              iStateImage := ConvertThreadStateImgs[DlItem.ConvertState];
              {$ENDIF}
              end;
          {$ENDIF}
          end;
        dtsFailed:
          sProgress := DlItem.ErrorMessage + ' (' + DlItem.ErrorClass + ')';
        dtsAborted:
          sProgress := GetProgressStr(DlItem.DownloadedSize, DlItem.TotalSize);
        end;
      {$IFNDEF F-PC}
      Item.StateIndex := -1;///iStateImage;
      {$ENDIF}
      Item.SubItems.Add(sState);
      Item.SubItems.Add(sTitle);
      Item.SubItems.Add(sSize);
      Item.SubItems.Add(sProgress);
      end;
  GetProgress(Progress, Total);
  if Total > 0 then
    ShowTotalProgressBar(Self.Handle, pbsNormal, Progress, Total)
  else
    ShowTotalProgressBar(Self.Handle, pbsNoProgress);
end;

procedure TFormYTD.actAddNewUrlExecute(Sender: TObject);
var Url: string;
begin
  Url := '';
  if Clipboard.HasFormat(CF_TEXT) then
    Url := Clipboard.AsText;
  if InputQuery(APPLICATION_TITLE, _(MAINFORM_ENTER_VIDEO_URL), Url) then
    if not AddTask(Url) then
      MessageDlg(_(MAINFORM_URL_NOT_SUPPORTED), mtError, [mbOK], 0);
end;

procedure TFormYTD.actDeleteURLExecute(Sender: TObject);
var i: integer;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if MessageDlg(_(MAINFORM_DELETE_TRANSFERS), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    Exit;
  if Downloads.SelCount = 1 then
    DeleteTask(Downloads.Selected.Index)
  else
    for i := Pred(Downloads.Items.Count) downto 0 do
      if Downloads.Items[i].Selected then
        DeleteTask(i);
end;

procedure TFormYTD.actStartExecute(Sender: TObject);
var i: integer;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if Downloads.SelCount = 1 then
    StartPauseResumeTask(Downloads.Selected.Index)
  else
    for i := 0 to Pred(Downloads.Items.Count) do
      if Downloads.Items[i].Selected then
        StartPauseResumeTask(i);
end;

procedure TFormYTD.actStopExecute(Sender: TObject);
var i: integer;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if MessageDlg(_(MAINFORM_STOP_TRANSFERS), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then 
    Exit;
  if Downloads.SelCount = 1 then
    StopTask(Downloads.Selected.Index)
  else
    for i := 0 to Pred(Downloads.Items.Count) do
      if Downloads.Items[i].Selected then
        StopTask(i);
end;

procedure TFormYTD.actReportBugExecute(Sender: TObject);
begin
  if Downloads.SelCount < 1 then
    Exit;
  if not IsSSLAvailable then
    MessageDlg(_(MAINFORM_NOBUGREPORTIFDOWNLOADSTARTED), mtError, [mbOK], 0)
  else if DownloadList[Downloads.Selected.Index].DownloadedSize > MAX_DOWNLOAD_SIZE_FOR_BUGREPORT then
    MessageDlg(_(MAINFORM_NOBUGREPORTIFDOWNLOADSTARTED), mtError, [mbOK], 0)
  else if MessageDlg(_(MAINFORM_REPORT_BUG), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    Exit;
  ReportBug(DownloadList, Downloads.Selected.Index);  
end;

procedure TFormYTD.actDonateExecute(Sender: TObject);
begin
  ///Run(DONATE_URL, Handle);
end;

procedure TFormYTD.actConvertExecute(Sender: TObject);
{$IFDEF CONVERTERS}
var i: integer;
    ConverterID: string;
{$ENDIF}
begin
  {$IFDEF CONVERTERS}
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  if not Options.ConvertersActivated then
    begin
    MessageDlg(_(CONVERTERS_INACTIVE_WARNING), mtError, [mbOK], 0);
    Exit;
    end;
  {$ENDIF}
  if Downloads.SelCount < 1 then
    Exit;
  if LastConverterID = '' then
    ConverterID := Options.SelectedConverterID
  else
    ConverterID := LastConverterID;
  if SelectConverter(Options, ConverterID, Self, _(MAINFORM_CONVERT_WITH)) then
    begin
    LastConverterID := ConverterID;
    if Downloads.SelCount = 1 then
      ConvertTask(Downloads.Selected.Index, LastConverterID)
    else
      for i := 0 to Pred(Downloads.Items.Count) do
        if Downloads.Items[i].Selected then
          ConvertTask(i, LastConverterID);
    end;
  {$ENDIF}
end;

procedure TFormYTD.actAddUrlsFromClipboardExecute(Sender: TObject);
begin
  if AddFromClipboard = 0 then
    MessageDlg(_(MAINFORM_NO_SUPPORTED_URL), mtError, [mbOK], 0);
end;

procedure TFormYTD.actAddUrlsFromClipboard2Execute(Sender: TObject);
begin
  actAddUrlsFromClipboard.Execute;
end;

procedure TFormYTD.actCopyUrlsToClipboardExecute(Sender: TObject);
var i: integer;
    s: string;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if Downloads.SelCount = 1 then
    s := DownloadList.Urls[Downloads.Selected.Index]
  else
    begin
    s := '';
    for i := 0 to Pred(Downloads.Items.Count) do
      if Downloads.Items[i].Selected then
        if s = '' then
          s := DownloadList.Urls[i]
        else
          s := s + EOLN + DownloadList.Urls[i];
    end;
  if s <> '' then
    begin
    StopClipboardMonitor;
    try
      Clipboard.AsText := s;
    finally
      StartClipboardMonitor;
      end;
    end;
end;

procedure TFormYTD.actCopyUrlsToClipboard2Execute(Sender: TObject);
begin
  actCopyUrlsToClipboard.Execute;
end;

procedure TFormYTD.actRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TFormYTD.Refresh;
begin
  ///{$IFDEF FPC}
  {$IFNDEF FPC}
  if Downloads.Items.Count <> DownloadList.Count then
    begin
    if Downloads.Items.Count > DownloadList.Count then
      Downloads.Items.Clear;
    while Downloads.Items.Count < DownloadList.Count do
      Downloads.Items.Add;
    end;
  {$ELSE}
  Downloads.Items.Count := DownloadList.Count;
  {$ENDIF}
  Downloads.Invalidate;
end;

function TFormYTD.AddFromClipboard(IgnoreUnchangedText: boolean): integer;
var L: TStringList;
    i: integer;
    s: string;
begin
  Result := 0;
  if Clipboard.HasFormat(CF_TEXT) then
    begin
    s := Clipboard.AsText;
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
end;

function TFormYTD.AddTask(const Url: string): boolean;
begin
  Result := DownloadList.Add(Url) >= 0;
  if Result then
    SaveSettings;
end;

procedure TFormYTD.AddTaskFromHTML(const Source: string);
begin
  DownloadList.AddFromHTML(Source);
  SaveSettings;
end;

procedure TFormYTD.DeleteTask(Index: integer);
begin
  DownloadList.Delete(Index);
  SaveSettings;
end;

procedure TFormYTD.StartPauseResumeTask(Index: integer);
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

procedure TFormYTD.StopTask(Index: integer);
begin
  DownloadList.Items[Index].Stop;
end;

{$IFDEF CONVERTERS}
procedure TFormYTD.ConvertTask(Index: integer; const ConverterID: string);
begin
  DownloadList.Items[Index].Convert(True, ConverterID);
end;
{$ENDIF}

procedure TFormYTD.PlayMedia(Index: integer);
begin
  DownloadList.Items[Index].PlayMedia;
end;

procedure TFormYTD.ExploreMedia(Index: integer);
begin
  DownloadList.Items[Index].ExploreMedia;
end;

procedure TFormYTD.actSelectAllExecute(Sender: TObject);
var i: integer;
begin
  for i := 0 to Pred(Downloads.Items.Count) do
    Downloads.Items[i].Selected := True;
end;

procedure TFormYTD.LoadSettings;
begin
  DownloadList.LoadFromOptions;
end;

procedure TFormYTD.SaveSettings;
begin
  if not fLoading then
    begin
    DownloadList.SaveToOptions;
    Options.Save;
    end;
end;

procedure TFormYTD.actAddUrlsFromFileExecute(Sender: TObject);
var L: TStringList;
    i, n: integer;
begin
  if OpenUrlList.Execute then
    begin
    L := TStringList.Create;
    try
      n := 0;
      L.LoadFromFile(OpenUrlList.FileName);
      for i := 0 to Pred(L.Count) do
        if L[i] <> '' then
          if AddTask(L[i]) then
            Inc(n);
      if n = 0 then
        MessageDlg(_(MAINFORM_NO_SUPPORTED_URL), mtError, [mbOK], 0);
    finally
      FreeAndNil(L);
      end;
    end;
end;

procedure TFormYTD.actSaveUrlListExecute(Sender: TObject);
var L: TStringList;
    i: integer;
begin
  if SaveUrlList.Execute then
    begin
    L := TStringList.Create;
    try
      for i := 0 to Pred(DownloadList.Count) do
        if DownloadList[i].State <> dtsFinished then
          L.Add(DownloadList.Urls[i]);
      L.SaveToFile(SaveUrlList.FileName);
    finally
      FreeAndNil(L);
      end;
    end;
end;

procedure TFormYTD.actAddUrlsFromHTMLExecute(Sender: TObject);
var Url: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    Url := Clipboard.AsText;
  if InputQuery(APPLICATION_TITLE, _(MAINFORM_ENTER_PAGE_URL), Url) then
    AddTaskFromHTML(Url);
end;

procedure TFormYTD.actAddUrlsFromHTMLfileExecute(Sender: TObject);
begin
  if OpenHtmlFile.Execute then
    AddTaskFromHTML(OpenHtmlFile.FileName);
end;

procedure TFormYTD.actAboutExecute(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
    try
      DownloadClassifier := Self.DownloadList.DownloadClassifier;
      Options := Self.Options;
      ShowModal;
    finally
      Free;
      end;
end;

procedure TFormYTD.DownloadsDblClick(Sender: TObject);
//var CursorPos: TPoint;
//    HitTestInfo: THitTests;
begin
//  CursorPos := Downloads.ScreenToClient(Mouse.CursorPos);
//  HitTestInfo := Downloads.GetHitTestInfoAt(CursorPos.X, CursorPos.Y);
//  if HitTestInfo <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon] then
    if Downloads.Selected <> nil then
      PlayMedia(Downloads.Selected.Index);
end;

procedure TFormYTD.actEditConfigFileExecute(Sender: TObject);
begin
  Options.Save;
  MessageDlg(_(MAINFORM_EDIT_CONFIG), mtWarning, [mbOK], 0);
  {$ifdef mswindows}
  if ShellExecute(Handle, 'edit', PChar(Options.FileName), nil, nil, SW_SHOWNORMAL) <= 32 then
    Run('notepad', '"' + Options.FileName + '"', Handle);
  {$else}
    Run('leafpad',  Options.FileName  , Handle);
  {$endif}
end;

procedure TFormYTD.actOptionsExecute(Sender: TObject);
var F: TFormOptions;
begin
  F := TFormOptions.Create(Self);
  try
    F.Options := Options;
    if F.ShowModal = mrOK then
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



procedure TFormYTD.DownloadsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, ind: integer;
  Item : TLIstItem;

const
   Res : array[0..5] of integer = (0,1920,1280,1024,720,512);

begin
  if button  = mbMiddle then
  begin
    Item := Downloads.GetItemAt(X, Y);
    if Assigned(Item) then
    begin
      fMenuID:=-1;
      i := Item.index;
      fProvID := i;
      for ind:=0 to VQualityPopUp.Items.Count-1 do
      begin
        VQualityPopUp.Items[ind].Checked:=false;
        if Res[ind] = DownloadList[i].Downloader.MaxVResolution then
        begin
          VQualityPopUp.Items[ind].Checked:=true;
          fVQID:=ind;
        end;
      end;
    end;
    VQualityPopUp.PopUp;
  end;
end;

procedure TFormYTD.VQualityPopUpSetting;
var
  value: string;
const
   Res : array[0..5] of integer = (0,1920,1280,1024,720,512);

begin
  if (fVQID <> fMenuID) and (fMenuID >= 0) then
  begin
    VQualityPopUp.Items[fVQID].Checked:=false;
      VQualityPopUp.Items[fMenuID].Checked:=true;
      value := Inputbox(MSG_MAX_VIDEO_BITRATE, MSG_VIDEO_BITRATE_VALUE, '0');
    Options.WriteProviderOption(DownloadList[fProvID].Downloader.ProviderName, 'max_video_width', Res[fMenuID]);
    Options.WriteProviderOption(DownloadList[fProvID].Downloader.ProviderName, 'max_video_bitrate', value);
    DownloadList[fProvID].Downloader.MaxVResolution:=Res[fMenuID];
    DownloadList[fProvID].Downloader.MaxVBitRate := StrToIntDef(value,0);
  end;

end;

procedure TFormYTD.actPlayExecute(Sender: TObject);
begin
  if Downloads.Selected <> nil then
    PlayMedia(Downloads.Selected.Index);
end;

procedure TFormYTD.actExploreFolderExecute(Sender: TObject);
begin
  if Downloads.Selected <> nil then
    ExploreMedia(Downloads.Selected.Index);
end;

procedure TFormYTD.actMenuExecute(Sender: TObject);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, 0));
  DownloadsPopup.Popup(P.X, P.Y);
end;

procedure TFormYTD.Item1Click(Sender: TObject);
begin
  fMenuID:=0;
  VQualityPopUpSetting;
end;

procedure TFormYTD.Item2Click(Sender: TObject);
begin
  fMenuID:=1;
  VQualityPopUpSetting;

end;

procedure TFormYTD.Item3Click(Sender: TObject);
begin
  fMenuID:=2;
  VQualityPopUpSetting;
end;

procedure TFormYTD.item4Click(Sender: TObject);
begin
  fMenuID:=3;
  VQualityPopUpSetting;
end;

procedure TFormYTD.Item5Click(Sender: TObject);
begin
  fMenuID:=4;
  VQualityPopUpSetting;
end;

procedure TFormYTD.Item6Click(Sender: TObject);
begin
  fMenuID:=5;
  VQualityPopUpSetting;
end;



procedure TFormYTD.TrayIconClick(Sender: TObject);
begin
  if WindowState = wsMinimized then
    begin
      WindowState:=wsNormal;
      Show;
    end;
end;




end.
