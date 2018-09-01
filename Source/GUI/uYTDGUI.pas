unit uYTDGUI;
{$INCLUDE 'ytd.inc'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ClipBrd, FileCtrl, Menus, ImgList, ActnList,
  Registry, ToolWin, CommDlg,
  {$IFDEF SYSTRAY}
  ShellApi,
  {$ENDIF}
  uLanguages, uMessages, uOptions, FileCtrlGUI,
  uDownloadList, uDownloadListItem, uDownloadThread,
  uYTDAbout;

{$IFDEF SYSTRAY}
const
  WM_NOTIFYICON  = WM_USER + 1;
{$ENDIF}

type
  TFormYTD = class(TForm)
    Downloads: TListView;
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
    actAutoDownload: TAction;
    actSelectAll: TAction;
    DownloadsPopup: TPopupMenu;
    AddnewURL1: TMenuItem;
    AddURLsfromClipboard1: TMenuItem;
    DeleteURL1: TMenuItem;
    CopyURLstoClipboard1: TMenuItem;
    StartPauseResume1: TMenuItem;
    Stop1: TMenuItem;
    DeleteURL2: TMenuItem;
    Autodownload1: TMenuItem;
    Refresh1: TMenuItem;
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
    ToolAutodownload: TToolButton;
    ToolRefresh: TToolButton;
    StatusBar1: TStatusBar;
    actDownloadDirectory: TAction;
    N3: TMenuItem;
    Setdownloaddirectory1: TMenuItem;
    ToolDownloadDir: TToolButton;
    actAutoOverwrite: TAction;
    ToolAutooverwrite: TToolButton;
    AutoOverwrite1: TMenuItem;
    actAddUrlsFromHTML: TAction;
    OpenHtmlFile: TOpenDialog;
    actAddUrlsFromHTMLfile: TAction;
    OpenUrlList: TOpenDialog;
    actAddUrlsFromFile: TAction;
    ToolAddFromFile: TToolButton;
    ToolButton1: TToolButton;
    ToolAddFromHTML: TToolButton;
    ToolAddFromHTMLfile: TToolButton;
    AddURLsfromfile1: TMenuItem;
    AddURLsfromHTMLpage1: TMenuItem;
    AddURLsfromHTMLfile1: TMenuItem;
    N4: TMenuItem;
    actSaveUrlList: TAction;
    SaveURLlist1: TMenuItem;
    ToolSave: TToolButton;
    SaveUrlList: TSaveDialog;
    ToolButton2: TToolButton;
    actAbout: TAction;
    ToolButton3: TToolButton;
    ToolAbout: TToolButton;
    N5: TMenuItem;
    About1: TMenuItem;
    procedure DownloadsData(Sender: TObject; Item: TListItem);
    procedure actAddNewUrlExecute(Sender: TObject);
    procedure actDeleteURLExecute(Sender: TObject);
    procedure actAddUrlsFromClipboardExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actCopyUrlsToClipboardExecute(Sender: TObject);
    procedure actAddUrlsFromClipboard2Execute(Sender: TObject);
    procedure actCopyUrlsToClipboard2Execute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAutoDownloadExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actDownloadDirectoryExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAutoOverwriteExecute(Sender: TObject);
    procedure actAddUrlsFromHTMLExecute(Sender: TObject);
    procedure actAddUrlsFromHTMLfileExecute(Sender: TObject);
    procedure actAddUrlsFromFileExecute(Sender: TObject);
    procedure actSaveUrlListExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure DownloadsDblClick(Sender: TObject);
  private
    {$IFDEF SYSTRAY}
    fNotifyIconData: TNotifyIconData;
    procedure CMClickIcon(var msg: TMessage); message WM_NOTIFYICON;
    procedure ApplicationMinimize(Sender: TObject);
    {$ENDIF}
  protected
    DownloadList: TDownloadList;
    NextProgressUpdate: DWORD;
    Options: TYTDOptions;
    procedure Refresh; virtual;
    procedure DownloadListChange(Sender: TObject); virtual;
    procedure DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    procedure DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    function PrettySize(Size: int64): string; virtual;
    procedure AddTask(const Url: string); virtual;
    procedure AddTaskFromHTML(const Source: string); virtual;
    procedure DeleteTask(Index: integer); virtual;
    procedure StartPauseResumeTask(Index: integer); virtual;
    procedure StopTask(Index: integer); virtual;
    procedure PlayMedia(Index: integer); virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    procedure LoadDownloadList; virtual;
    procedure SaveDownloadList; virtual;
  public
  end;

var
  FormYTD: TFormYTD;

implementation

{$R *.DFM}

{gnugettext: scan-all}
const
  THREADSTATE_WAITING = 'Waiting'; // GUI: Download thread state: Waiting for its turn
  THREADSTATE_PREPARING = 'Preparing'; // GUI: Download thread state: Preparing download (getting title, URL...)
  THREADSTATE_DOWNLOADING = 'Downloading'; // GUI: Download thread state: Downloading
  THREADSTATE_FINISHED = 'Finished'; // GUI: Download thread state: Download finished successfully
  THREADSTATE_FAILED = 'Failed'; // GUI: Download thread state: Download failed
  THREADSTATE_ABORTED = 'Aborted'; // GUI: Download thread state: Download was aborted by user
{gnugettext: reset}

const
  States: array[TDownloadThreadState] of string
        = (THREADSTATE_WAITING, THREADSTATE_PREPARING, THREADSTATE_DOWNLOADING, THREADSTATE_FINISHED, THREADSTATE_FAILED, THREADSTATE_ABORTED);

  {$IFNDEF FPC}
  StateImgs: array[TDownloadThreadState] of integer
        = (-1, 3, 3, 2, 1, 0);
  {$ENDIF}

{ TFormYTD }

procedure TFormYTD.FormCreate(Sender: TObject);
begin
  Options := TYTDOptions.Create;
  UseLanguage(Options.Language);
  TranslateProperties(self);
  Caption := Application.Title + ' v' + {$INCLUDE 'ytd.version'} ;
  DownloadList := TDownloadList.Create;
  DownloadList.OnListChange := DownloadListChange;
  DownloadList.OnStateChange := DownloadListItemChange;
  DownloadList.OnDownloadProgress := DownloadListProgress;
  DownloadList.OnError := DownloadListItemChange;
  DownloadList.OnFinished := DownloadListItemChange;
  DownloadList.DestinationPath := Options.DestinationPath;
  DownloadList.AutoOverwrite := Options.OverwriteMode = omAlways;
  DownloadList.AutoStart := True;
  DownloadList.Options := Options;
  LoadSettings;
  actAutoDownload.Checked := DownloadList.AutoStart;
  actAutoOverwrite.Checked := DownloadList.AutoOverwrite;
  {$IFDEF SYSTRAY}
  Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
  fNotifyIconData.cbSize := Sizeof(fNotifyIconData);
  fNotifyIconData.Wnd := Self.Handle;
  fNotifyIconData.uID := Integer(Self);
  fNotifyIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  fNotifyIconData.uCallbackMessage := WM_NOTIFYICON;
  fNotifyIconData.hIcon := Application.Icon.Handle;
  StrPCopy(fNotifyIconData.szTip, Copy(Caption, 1, Pred(Length(fNotifyIconData.szTip))));
  Shell_NotifyIcon(NIM_ADD, @fNotifyIconData);
  Application.OnMinimize := ApplicationMinimize;
  {$ENDIF}
end;

procedure TFormYTD.FormDestroy(Sender: TObject);
begin
  DownloadList.StopAll;
  FreeAndNil(Options);
  FreeAndNil(DownloadList);
  {$IFDEF SYSTRAY}
  Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
  {$ENDIF}
end;

procedure TFormYTD.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DownloadList.DownloadingCount <= 0 then
    CanClose := True
  else
    CanClose := (MessageDlg(_('There are downloads in progress'#10'Do you really want to quit?'), mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes); // GUI: confirmation before quitting YTD
end;

procedure TFormYTD.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DownloadList.StopAll;
  SaveSettings;
end;

{$IFDEF SYSTRAY}
procedure TFormYTD.CMClickIcon(var msg: TMessage);
begin
  case Msg.lParam of
    {WM_LBUTTONDBLCLK} WM_LBUTTONDOWN:
      Show;
    end;
end;

procedure TFormYTD.ApplicationMinimize(Sender: TObject);
begin
  Hide;
end;
{$ENDIF}

procedure TFormYTD.DownloadListChange(Sender: TObject);
begin
  Refresh;
end;

procedure TFormYTD.DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem);
{$IFNDEF FPC}
var Idx: integer;
{$ENDIF}
begin
  {$IFDEF FPC}
  Refresh;
  {$ELSE}
  Idx := Sender.IndexOf(Item);
  Downloads.Items.Count := Sender.Count;
  if (DownloadList <> nil) and (DownloadList[Idx] <> nil) then
    if DownloadList[Idx].State = dtsFinished then
    SaveDownloadList;
  if Idx >= 0 then
    Downloads.UpdateItems(Idx, Idx);
  {$ENDIF}
end;

procedure TFormYTD.DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem);
var Ticks: DWORD;
begin
  Ticks := GetTickCount;
  if (Ticks > NextProgressUpdate) or ((NextProgressUpdate > $f0000000) and (Ticks < $10000000)) then
    begin
    NextProgressUpdate := Ticks + 250; // 0.25 sec.
    Downloads.Refresh;
    end;
end;

procedure TFormYTD.DownloadsData(Sender: TObject; Item: TListItem);

  function GetProgressStr(Item: TDownloadListItem): string;
    var n: int64;
    begin
      Result := PrettySize(Item.DownloadedSize);
      if Item.TotalSize > 0 then
        begin
        n := 1000*Item.DownloadedSize div Item.TotalSize;
        Result := Format('%s (%d.%d%%)', [Result, n div 10, n mod 10]);
        end
    end;

var DlItem: TDownloadListItem;
    sState, sTitle, sSize, sProgress: string;
    {$IFNDEF FPC}
    iStateImage: integer;
    {$ENDIF}
begin
  if Item.Index < DownloadList.Count then
    begin
    DlItem := DownloadList[Item.Index];
    Item.Caption := DownloadList.Urls[Item.Index];
    {$IFNDEF FPC}
    Item.StateIndex := Integer(DlItem.State);
    {$ENDIF}
    Item.SubItems.Add(DlItem.Downloader.Provider);
    sState := _(States[DlItem.State]);
    {$IFNDEF FPC}
    iStateImage := StateImgs[DlItem.State];
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
          sState := _('Paused'); // Download thread state: Paused
          {$IFNDEF FPC}
          iStateImage := 4;
          {$ENDIF}
          end;
        end;
      dtsDownloading:
        begin
        sProgress := GetProgressStr(DlItem);
        if DlItem.Paused then
          begin
          sState := _('Paused'); // Download thread state: Paused
          {$IFNDEF FPC}
          iStateImage := 4;
          {$ENDIF}
          end;
        end;
      dtsFinished:
        ;
      dtsFailed:
        sProgress := DlItem.ErrorClass + ': ' + DlItem.ErrorMessage;
      dtsAborted:
        sProgress := GetProgressStr(DlItem);
      end;
    {$IFNDEF FPC}
    Item.StateIndex := iStateImage;
    {$ENDIF}
    Item.SubItems.Add(sState);
    Item.SubItems.Add(sTitle);
    Item.SubItems.Add(sSize);
    Item.SubItems.Add(sProgress);
    end;
end;

function TFormYTD.PrettySize(Size: int64): string;
begin
  if Size <= 0 then
    Result := ''
  else if Size < 10*1e3 then
    Result := IntToStr(Size) + ' B'
  else if Size < 10*1e6 then
    Result := IntToStr(Size div (1 shl 10)) + ' KiB'
  else if Size < 10*1e9 then
    Result := IntToStr(Size div (1 shl 20)) + ' MiB'
  else if Size < 10*1e12 then
    Result := IntToStr(Size div (1 shl 30)) + ' GiB'
  else
    Result := IntToStr(Size div (1 shl 40)) + ' TiB';
end;

procedure TFormYTD.actAddNewUrlExecute(Sender: TObject);
var Url: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    Url := Clipboard.AsText;
  if InputQuery(APPLICATION_TITLE, _('Enter video URL:'), Url) then // GUI: User wants to add a new URL
    AddTask(Url);
end;

procedure TFormYTD.actDeleteURLExecute(Sender: TObject);
var i: integer;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if MessageDlg(_('Do you really want to delete selected transfer(s)?'), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then // GUI: User wants to delete URL(s)
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
  if MessageDlg(_('Do you really want to stop selected transfer(s)?'), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then // GUI: User wants to abort transfers
    Exit;
  if Downloads.SelCount = 1 then
    StopTask(Downloads.Selected.Index)
  else
    for i := 0 to Pred(Downloads.Items.Count) do
      if Downloads.Items[i].Selected then
        StopTask(i);
end;

procedure TFormYTD.actAddUrlsFromClipboardExecute(Sender: TObject);
var L: TStringList;
    i: integer;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    begin
    L := TStringList.Create;
    try
      L.Text := Clipboard.AsText;
      for i := 0 to Pred(L.Count) do
        AddTask(L[i]);
    finally
      L.Free;
      end;
    end;
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
  Clipboard.AsText := s;
end;

procedure TFormYTD.actCopyUrlsToClipboard2Execute(Sender: TObject);
begin
  actCopyUrlsToClipboard.Execute;
end;

procedure TFormYTD.actRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TFormYTD.actDownloadDirectoryExecute(Sender: TObject);
var Dir: string;
begin
  Dir := DownloadList.DestinationPath;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    begin
    DownloadList.DestinationPath := Dir;
    Options.DestinationPath := Dir;
    end;
end;

procedure TFormYTD.Refresh;
begin
  {$IFDEF FPC}
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

procedure TFormYTD.AddTask(const Url: string);
begin
  DownloadList.Add(Url);
  //SaveDownloadList;
end;

procedure TFormYTD.AddTaskFromHTML(const Source: string);
begin
  DownloadList.AddFromHTML(Source);
  //SaveDownloadList;
end;

procedure TFormYTD.DeleteTask(Index: integer);
begin
  DownloadList.Delete(Index);
  SaveDownloadList;
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
    Item.Start;
end;

procedure TFormYTD.StopTask(Index: integer);
begin
  DownloadList.Items[Index].Stop;
end;

procedure TFormYTD.PlayMedia(Index: integer);
var Item: TDownloadListItem;
begin
  Item := DownloadList.Items[Index];
  if Item.Finished then
    if FileExists(Item.Downloader.DestinationPath + Item.Downloader.FileName) then
      ShellExecute(Handle, 'open', PChar(Item.Downloader.FileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormYTD.actAutoDownloadExecute(Sender: TObject);
begin
  DownloadList.AutoStart := not DownloadList.AutoStart;
  actAutoDownload.Checked := DownloadList.AutoStart;
  if DownloadList.AutoStart then
    DownloadList.StartAll;
end;

procedure TFormYTD.actSelectAllExecute(Sender: TObject);
var i: integer;
begin
  for i := 0 to Pred(Downloads.Items.Count) do
    Downloads.Items[i].Selected := True;
end;

const REGISTRY_KEY = '\Software\Pepak\YouTube Downloader';
      REGISTRY_KEY_LIST = REGISTRY_KEY + '\Download list';
      REGISTRY_DOWNLOADDIR = 'Download directory';
      REGISTRY_AUTOSTART = 'Autostart downloads';
      REGISTRY_AUTOOVERWRITE = 'Automatically overwrite existing files';

procedure TFormYTD.LoadSettings;
var Reg: TRegistry;
begin
  if Options.DontUseRegistry then
    begin
    // Don't do anything.
    end
  else
    begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.Access := KEY_READ;
      if Reg.OpenKey(REGISTRY_KEY, False) then
        begin
        if Reg.ValueExists(REGISTRY_DOWNLOADDIR) then
          DownloadList.DestinationPath := Reg.ReadString(REGISTRY_DOWNLOADDIR);
        if Reg.ValueExists(REGISTRY_AUTOSTART) then
          DownloadList.AutoStart := Reg.ReadInteger(REGISTRY_AUTOSTART) <> 0;
        if Reg.ValueExists(REGISTRY_AUTOOVERWRITE) then
          DownloadList.AutoOverwrite := Reg.ReadInteger(REGISTRY_AUTOOVERWRITE) <> 0;
        end;
    finally
      Reg.Free;
      end;
    end;
  LoadDownloadList;
end;

procedure TFormYTD.SaveSettings;
var Reg: TRegistry;
begin
  SaveDownloadList;
  if Options.DontUseRegistry then
    begin
    try
      Options.Save;
    except
      on Exception do
        ;
      end;
    end
  else
    begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.Access := KEY_ALL_ACCESS;
      if Reg.OpenKey(REGISTRY_KEY, True) then
        begin
        Reg.WriteString(REGISTRY_DOWNLOADDIR, DownloadList.DestinationPath);
        Reg.WriteInteger(REGISTRY_AUTOSTART, Integer(DownloadList.AutoStart));
        Reg.WriteInteger(REGISTRY_AUTOOVERWRITE, Integer(DownloadList.AutoOverwrite));
        end;
    finally
      Reg.Free;
      end;
    end;
end;

procedure TFormYTD.LoadDownloadList;
var L: TStringList;
    i: integer;
    Reg: TRegistry;
begin
  L := TStringList.Create;
  try
    if Options.DontUseRegistry then
      begin
      Options.LoadUrls(L);
      end
    else
      begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_CURRENT_USER;
        Reg.Access := KEY_READ;
        if Reg.OpenKey(REGISTRY_KEY_LIST, False) then
          Reg.GetValueNames(L);
        for i := 0 to Pred(L.Count) do
          L[i] := Reg.ReadString(L[i]);
      finally
        Reg.Free;
        end;
      end;
    for i := 0 to Pred(L.Count) do
      if L[i] <> '' then
        AddTask(L[i]);
  finally
    L.Free;
    end;
end;

procedure TFormYTD.SaveDownloadList;
var L: TStringList;
    i: integer;
    Reg: TRegistry;
begin
  L := TStringList.Create;
  try
    for i := 0 to Pred(DownloadList.Count) do
      if DownloadList[i].State <> dtsFinished then
        L.Add(DownloadList.Urls[i]);
    //if L.Count > 0 then
      if Options.DontUseRegistry then
        begin
        try
          Options.SaveUrls(L);
        except
          on Exception do
            ;
          end;
        end
      else
        begin
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_CURRENT_USER;
          Reg.Access := KEY_ALL_ACCESS;
          if Reg.KeyExists(REGISTRY_KEY_LIST) then
            Reg.DeleteKey(REGISTRY_KEY_LIST);
          if Reg.OpenKey(REGISTRY_KEY_LIST, True) then
            for i := 0 to Pred(L.Count) do
              Reg.WriteString(IntToStr(i), L[i]);
        finally
          Reg.Free;
          end;
        end;
  finally
    L.Free;
    end;
end;

procedure TFormYTD.actAutoOverwriteExecute(Sender: TObject);
begin
  DownloadList.AutoOverwrite := not DownloadList.AutoOverwrite;
  actAutoOverwrite.Checked := DownloadList.AutoOverwrite;
  if DownloadList.AutoOverwrite then
    Options.OverwriteMode := omAlways
  else
    Options.OverwriteMode := omAsk;
end;

procedure TFormYTD.actAddUrlsFromFileExecute(Sender: TObject);
var L: TStringList;
    i: integer;
begin
  if OpenUrlList.Execute then
    begin
    L := TStringList.Create;
    try
      L.LoadFromFile(OpenUrlList.FileName);
      for i := 0 to Pred(L.Count) do
        if L[i] <> '' then
          AddTask(L[i]);
    finally
      L.Free;
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
      L.Free;
      end;
    end;
end;

procedure TFormYTD.actAddUrlsFromHTMLExecute(Sender: TObject);
var Url: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    Url := Clipboard.AsText;
  if InputQuery(APPLICATION_TITLE, _('Enter page URL:'), Url) then // GUI: User wants to add links from a HTML page
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

end.
