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

unit guiMainVCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ClipBrd, FileCtrl, Menus, ImgList, ActnList,
  ToolWin, CommDlg, ShellApi,
  SynaCode,
  uLanguages, uMessages, uOptions, uStringUtils,
  guiOptions, FileCtrlGUI,
  uDownloadList, uDownloadListItem, uDownloadThread;

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
    mnuAddNewUrl: TMenuItem;
    mnuAddUrlFromClipboard: TMenuItem;
    mnuDelete: TMenuItem;
    mnuCopyUrls: TMenuItem;
    mnuStart: TMenuItem;
    mnuStop: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuAutoDownload: TMenuItem;
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
    ToolAutodownload: TToolButton;
    ToolRefresh: TToolButton;
    StatusBar1: TStatusBar;
    actDownloadDirectory: TAction;
    N3: TMenuItem;
    mnuSetDownloadDir: TMenuItem;
    ToolDownloadDir: TToolButton;
    actAutoOverwrite: TAction;
    ToolAutooverwrite: TToolButton;
    mnuAutoOverwrite: TMenuItem;
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
    N5: TMenuItem;
    mnuAbout: TMenuItem;
    actConvert: TAction;
    ToolConvert: TToolButton;
    mnuConvert: TMenuItem;
    actSelectConverter: TAction;
    ToolSelectConverter: TToolButton;
    mnuSelectConverter: TMenuItem;
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
    procedure actConvertExecute(Sender: TObject);
    procedure actSelectConverterExecute(Sender: TObject);
    procedure actReportBugExecute(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure actEditConfigFileExecute(Sender: TObject);
  protected
    fLoading: boolean;
    {$IFDEF SYSTRAY}
    fNotifyIconData: TNotifyIconData;
    procedure WMClickIcon(var msg: TMessage); message WM_NOTIFYICON;
    procedure ApplicationMinimize(Sender: TObject);
    {$ENDIF}
    {$IFDEF THREADEDVERSION}
    procedure NewVersionEvent(Sender: TObject; const Version, Url: string); virtual;
    {$ENDIF}
  protected
    DownloadList: TDownloadList;
    NextProgressUpdate: DWORD;
    Options: TYTDOptions;
    {$IFDEF CONVERTERS}
    LastConverterID: string;
    {$ENDIF}
    procedure Refresh; virtual;
    procedure DownloadListChange(Sender: TObject); virtual;
    procedure DownloadListItemChange(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    procedure DownloadListProgress(Sender: TDownloadList; Item: TDownloadListItem); virtual;
    function PrettySize(Size: int64): string; virtual;
    function AddTask(const Url: string): boolean; virtual;
    procedure AddTaskFromHTML(const Source: string); virtual;
    procedure DeleteTask(Index: integer); virtual;
    procedure StartPauseResumeTask(Index: integer); virtual;
    procedure StopTask(Index: integer); virtual;
    procedure ReportBug(Index: integer); virtual;
    {$IFDEF CONVERTERS}
    procedure ConvertTask(Index: integer; const ConverterID: string); virtual;
    {$ENDIF}
    procedure PlayMedia(Index: integer); virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
  public
  end;

var
  FormYTD: TFormYTD;

implementation

{$R *.DFM}

uses
  guiConsts, guiAboutVCL, guiConverterVCL;

{$IFNDEF FPC}
const
  ThreadStateImgs: array[TDownloadThreadState] of integer
                 = (-1, 3, 3, 2, 1, 0);

  {$IFDEF CONVERTERS}
  ConvertThreadStateImgs: array[TConvertThreadState] of integer
                 = (4, 6, 5, 1, 1);
  {$ENDIF}

{$ENDIF}

const
  DONATE_URL = 'https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=paypal.com@pepak.net&currency_code=USD';
  BUGREPORT_URL = 'http://ytd.pepak.net/bugreport.php?version=%s&url=%s&error=%s';

{ TFormYTD }

procedure TFormYTD.FormCreate(Sender: TObject);
begin
  fLoading := True;
  try
    Caption := Application.Title + ' v' + {$INCLUDE 'ytd.version'} ;
    Options := TYTDOptionsGUI.Create;
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
    actSelectConverter.Checked := Options.SelectedConverterID <> '';
    {$ELSE}
    actConvert.Visible := False;
    actConvert.Enabled := False;
    actSelectConverter.Visible := False;
    actSelectConverter.Enabled := False;
    {$ENDIF}
    DownloadList.Options := Options;
    LoadSettings;
    actAutoDownload.Checked := DownloadList.Options.AutoStartDownloads;
    actAutoOverwrite.Checked := DownloadList.Options.OverwriteMode = omAlways;
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
  finally
    fLoading := False;
    end;
  {$IFDEF THREADEDVERSION}
  if Options.CheckForNewVersionOnStartup then
    Options.GetNewestVersionInBackground(NewVersionEvent);
  {$ENDIF}
end;

procedure TFormYTD.FormDestroy(Sender: TObject);
begin
  DownloadList.StopAll;
  {$IFDEF SYSTRAY}
  Shell_NotifyIcon(NIM_DELETE, @fNotifyIconData);
  {$ENDIF}
  FreeAndNil(DownloadList);
  FreeAndNil(Options);
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
procedure TFormYTD.WMClickIcon(var msg: TMessage);
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

{$IFDEF THREADEDVERSION}
procedure TFormYTD.NewVersionEvent(Sender: TObject; const Version, Url: string);
begin
  if Version > {$INCLUDE 'YTD.version'} then
    begin
    actReportBug.Enabled := False;
    if MessageDlg(Format(_('A newer version (%s) is available.'#10'Do you want to download it?'), [Version]), mtInformation, [mbYes, mbNo], 0) = mrYes then
      ShellExecute(Handle, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
    end;
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
      SaveSettings;
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
    sState := _(ThreadStates[DlItem.State]);
    {$IFNDEF FPC}
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
        begin
        {$IFDEF CONVERTERS}
          if (DlItem.ConvertState <> ctsWaiting) or (Options.SelectedConverterID <> '') then
            begin
            sState := _(ConvertThreadStates[DlItem.ConvertState]);
            {$IFNDEF FPC}
            iStateImage := ConvertThreadStateImgs[DlItem.ConvertState];
            {$ENDIF}
            end;
        {$ENDIF}
        end;
      dtsFailed:
        sProgress := DlItem.ErrorMessage + ' (' + DlItem.ErrorClass + ')';
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
    if not AddTask(Url) then
      MessageDlg(_('This URL is not supported.'#10'See the documenation for supported URLs.'), mtError, [mbOK], 0);
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

procedure TFormYTD.actReportBugExecute(Sender: TObject);
begin
  if Downloads.SelCount < 1 then
    Exit;
  if MessageDlg(_('Do you really want to report a bug for this transfer?'), mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    Exit;
  ReportBug(Downloads.Selected.Index);  
end;

procedure TFormYTD.actDonateExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', DONATE_URL, nil, nil, SW_SHOWNORMAL);
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
  if SelectConverter(Options, ConverterID, Self, _('Convert selected files with')) then
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

procedure TFormYTD.actSelectConverterExecute(Sender: TObject);
{$IFDEF CONVERTERS}
var ConverterID: string;
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
  ConverterID := Options.SelectedConverterID;
  actSelectConverter.Checked := ConverterID = '';
  actSelectConverter.Checked := ConverterID <> '';
  if SelectConverter(Options, ConverterID, Self, _('Automatically convert with')) then
    begin
    LastConverterID := ConverterID;
    Options.SelectedConverterID := ConverterID;
    actSelectConverter.Checked := ConverterID = '';
    actSelectConverter.Checked := ConverterID <> '';
    end;
  {$ENDIF}
end;

procedure TFormYTD.actAddUrlsFromClipboardExecute(Sender: TObject);
var L: TStringList;
    i, n: integer;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    begin
    L := TStringList.Create;
    try
      n := 0;
      L.Text := Clipboard.AsText;
      for i := 0 to Pred(L.Count) do
        if AddTask(L[i]) then
          Inc(n);
      if n = 0 then
        MessageDlg(_('No supported URLs found.'#10'See the documenation for supported URLs.'), mtError, [mbOK], 0);
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
  Dir := DownloadList.Options.DestinationPath;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    begin
    DownloadList.Options.DestinationPath := Dir;
    SaveSettings;
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
    Item.Start;
end;

procedure TFormYTD.StopTask(Index: integer);
begin
  DownloadList.Items[Index].Stop;
end;

procedure TFormYTD.ReportBug(Index: integer);
var BugReportUrl: string;
begin
  BugReportUrl := Format(BUGREPORT_URL,
                       [ {$INCLUDE 'YTD.version'} ,
                         EncodeUrl(StringToUtf8(DownloadList.Urls[Index])),
                         EncodeUrl(StringToUtf8(DownloadList[Index].Downloader.LastErrorMsg))
                       ]);
  ShellExecute(0, 'open', PChar(BugReportUrl), nil, nil, SW_SHOWNORMAL);
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

procedure TFormYTD.actAutoDownloadExecute(Sender: TObject);
begin
  DownloadList.Options.AutoStartDownloads := not DownloadList.Options.AutoStartDownloads;
  actAutoDownload.Checked := DownloadList.Options.AutoStartDownloads;
  SaveSettings;
  if DownloadList.Options.AutoStartDownloads then
    DownloadList.StartAll;
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

procedure TFormYTD.actAutoOverwriteExecute(Sender: TObject);
begin
  if DownloadList.Options.OverwriteMode = omAlways then
    DownloadList.Options.OverwriteMode := omAsk
  else
    DownloadList.Options.OverwriteMode := omAlways;
  actAutoOverwrite.Checked := DownloadList.Options.OverwriteMode = omAlways;
  SaveSettings;
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
        MessageDlg(_('No supported URLs found.'#10'See the documenation for supported URLs.'), mtError, [mbOK], 0);
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

procedure TFormYTD.actEditConfigFileExecute(Sender: TObject);
begin
  Options.Save;
  ShellExecute(Handle, 'edit', PChar(Options.FileName), nil, nil, SW_SHOWNORMAL);
end;

end.
