unit uYTDGUI;
{$INCLUDE 'ytd.inc'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ClipBrd, FileCtrl, Menus, ImgList,
  ActnList, Registry, ToolWin,
  uDownloadList, uDownloadListItem, uDownloadThread;

type
  TFormYTD = class(TForm)
    YTDMainMenu: TMainMenu;
    PanelMenu: TPanel;
    Downloads: TListView;
    YTDActions: TActionList;
    ActionImages: TImageList;
    actAddNewUrl: TAction;
    actDeleteURL: TAction;
    actAddUrlsFromClipboard: TAction;
    RedrawDelay: TTimer;
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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    StatusBar1: TStatusBar;
    actDownloadDirectory: TAction;
    N3: TMenuItem;
    Setdownloaddirectory1: TMenuItem;
    ToolButton8: TToolButton;
    procedure DownloadsData(Sender: TObject; Item: TListItem);
    procedure actAddNewUrlExecute(Sender: TObject);
    procedure actDeleteURLExecute(Sender: TObject);
    procedure actAddUrlsFromClipboardExecute(Sender: TObject);
    procedure RedrawDelayTimer(Sender: TObject);
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
  private
  protected
    DownloadList: TDownloadList;
    ProgressChanged: boolean;
    procedure Refresh; virtual;
    procedure DownloadListChange(Sender: TObject); virtual;
    procedure DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    procedure DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    function PrettySize(Size: int64): string; virtual;
    procedure AddTask(const Url: string); virtual;
    procedure DeleteTask(Index: integer); virtual;
    procedure StartPauseResumeTask(Index: integer); virtual;
    procedure StopTask(Index: integer); virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
  public
  end;

var
  FormYTD: TFormYTD;

implementation

{$R *.DFM}

const
  States: array[TDownloadThreadState] of string
        = ('Waiting', 'Preparing', 'Downloading', 'Finished', 'Failed', 'Aborted');

  StateImgs: array[TDownloadThreadState] of integer
        = (-1, 3, 3, 2, 1, 0);

{ TFormYTD }

procedure TFormYTD.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' v' + {$INCLUDE 'ytd.version'} ;
  DownloadList := TDownloadList.Create;
  DownloadList.OnListChange := DownloadListChange;
  DownloadList.OnStateChange := DownloadListItemChange;
  DownloadList.OnDownloadProgress := DownloadListProgress;
  DownloadList.OnError := DownloadListItemChange;
  DownloadList.OnFinished := DownloadListItemChange;
  DownloadList.DestinationPath := '';
  DownloadList.AutoStart := True;
  LoadSettings;
  actAutoDownload.Checked := DownloadList.AutoStart;
end;

procedure TFormYTD.FormDestroy(Sender: TObject);
begin
  RedrawDelay.Enabled := False;
  FreeAndNil(DownloadList);
end;

procedure TFormYTD.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DownloadList.DownloadingCount <= 0 then
    CanClose := True
  else
    CanClose := (MessageDlg('There are downloads in progress'#13'Do you really want to quit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes);
end;

procedure TFormYTD.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

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
    Downloads.UpdateItems(Idx, Idx);
end;

procedure TFormYTD.DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem);
begin
  ProgressChanged := True;
end;

procedure TFormYTD.RedrawDelayTimer(Sender: TObject);
begin
  if ProgressChanged then
    begin
    ProgressChanged := False;
    Refresh;
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
    iStateImage: integer;
begin
  if Item.Index < DownloadList.Count then
    begin
    DlItem := DownloadList[Item.Index];
    Item.Caption := DownloadList.Urls[Item.Index];
    Item.StateIndex := Integer(DlItem.State);
    Item.SubItems.Add(DlItem.Downloader.Provider);
    sState := States[DlItem.State];
    iStateImage := StateImgs[DlItem.State];
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
          sState := 'Paused';
          iStateImage := 4;
          end;
        end;
      dtsDownloading:
        begin
        sProgress := GetProgressStr(DlItem);
        if DlItem.Paused then
          begin
          sState := 'Paused';
          iStateImage := 4;
          end;
        end;
      dtsFinished:
        ;
      dtsFailed:
        sProgress := DlItem.ErrorClass + ': ' + DlItem.ErrorMessage;
      dtsAborted:
        sProgress := GetProgressStr(DlItem);
      end;
    Item.StateIndex := iStateImage;
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
    Result := IntToStr(Size div (1 shl 10)) + ' KB'
  else if Size < 10*1e9 then
    Result := IntToStr(Size div (1 shl 20)) + ' MB'
  else if Size < 10*1e12 then
    Result := IntToStr(Size div (1 shl 30)) + ' GB'
  else
    Result := IntToStr(Size div (1 shl 40)) + ' TB';
end;

procedure TFormYTD.actAddNewUrlExecute(Sender: TObject);
var Url: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    Url := Clipboard.AsText;
  if InputQuery('YouTube Downloader', 'Enter video URL:', Url) then
    AddTask(Url);
end;

procedure TFormYTD.actDeleteURLExecute(Sender: TObject);
var i: integer;
begin
  if Downloads.SelCount < 1 then
    Exit;
  if MessageDlg('Do you really want to delete selected transfer(s)?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
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
  if MessageDlg('Do you really want to stop selected transfer(s)?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
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
          s := s + #13 + DownloadList.Urls[i];
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
    DownloadList.DestinationPath := Dir;
end;

procedure TFormYTD.Refresh;
begin
  Downloads.Items.Count := DownloadList.Count;
  Downloads.Invalidate;
end;

procedure TFormYTD.AddTask(const Url: string);
begin
  DownloadList.Add(Url);
end;

procedure TFormYTD.DeleteTask(Index: integer);
begin
  DownloadList.Delete(Index);
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
      REGISTRY_DOWNLOADDIR = 'Download directory';
      REGISTRY_AUTOSTART = 'Autostart downloads';

procedure TFormYTD.LoadSettings;
var Reg: TRegistry;
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
      end;
  finally
    Reg.Free;
    end;
end;

procedure TFormYTD.SaveSettings;
var Reg: TRegistry;
    AutoSt: integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.Access := KEY_ALL_ACCESS;
    if Reg.OpenKey(REGISTRY_KEY, True) then
      begin
      Reg.WriteString(REGISTRY_DOWNLOADDIR, DownloadList.DestinationPath);
      if DownloadList.AutoStart then
        AutoSt := 1
      else
        AutoSt := 0;
      Reg.WriteInteger(REGISTRY_AUTOSTART, AutoSt);
      end;
  finally
    Reg.Free;
    end;
end;

end.
