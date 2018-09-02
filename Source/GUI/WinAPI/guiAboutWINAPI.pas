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

unit guiAboutWINAPI;
{$INCLUDE 'ytd.inc'}

{$DEFINE OVERRIDETITLEANDURL}
  // Title will always be APPLICATION_TITLE and homepage will always be APPLICATION_URL,
  // regardless what's written in the resource file.
{.DEFINE STATICPROVIDERLIST}
  // Provider list is static, not virtual. This is mostly for testing.

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uFunctions, uDownloadClassifier, uDownloader, uOptions,
  uUpgrade,
  guiFunctions;

type
  TFormAbout = class(TApiForm)
    protected
      function DialogProc(var Msg: TMessage): boolean; override;
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      {$IFNDEF STATICPROVIDERLIST}
      function DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: LRESULT): boolean; override;
      {$ENDIF}
    protected
      function DoCtlColorStatic(DeviceContext: HDC; Control: THandle; out Brush: THandle): boolean; virtual;
      function DoSetCursor(Control: THandle; HitTestCode, Identifier: Word): boolean; virtual;
    private
      Cursor_Hand: THandle;
      Brush_Form: THandle;
      Font_Default: THandle;
      Font_Title: THandle;
      Font_Info: THandle;
      Font_Link: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    private
      fDownloadClassifier: TDownloadClassifier;
      fOptions: TYTDOptions;
      fUpgrade: TYTDUpgrade;
      fNewestVersionColor: TColor;
      fNewestVersionUrl: string;
      fNewestDefsColor: TColor;
      fNewestDefsUrl: string;
    protected
      procedure NewYTDEvent(Sender: TYTDUpgrade); virtual;
      procedure NewDefsEvent(Sender: TYTDUpgrade); virtual;
      procedure LabelHomepageClick; virtual;
      procedure LabelNewestVersionClick; virtual;
      procedure LabelNewestDefsClick; virtual;
      {$IFNDEF STATICPROVIDERLIST}
      function ListProvidersGetDisplayInfo(DispInfo: PLVDispInfo): boolean; virtual;
      {$ENDIF}
      procedure LoadProviders; virtual;
      property Upgrade: TYTDUpgrade read fUpgrade;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions write fOptions;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier write fDownloadClassifier;
    end;

implementation

{$RESOURCE *.res}

uses
  uScriptedDownloader;

// from resource.h
const
  IDC_LABEL_VERSIONCAPTION = 1000;
  IDC_LABEL_NEWESTVERSIONCAPTION = 1001;
  IDC_LABEL_HOMEPAGECAPTION = 1002;
  IDC_LABEL_YOUTUBEDOWNLOADER = 1003;
  IDC_LABEL_VERSION = 1004;
  IDC_LABEL_NEWESTVERSION = 1007;
  IDC_LABEL_HOMEPAGE = 1008;
  IDC_LABEL_NEWESTDEFSVERSION = 1009;
  IDC_LABEL_DEFSVERSION = 1010;
  IDC_LIST_PROVIDERS = 1006;

  ACTION_CLOSE = 40000;

//
const
  LISTVIEW_SUBITEM_PROVIDER = 0;
  LISTVIEW_SUBITEM_COMPONENTS = 1;

{ TFormAbout }

constructor TFormAbout.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormAbout.Destroy;
begin
  FreeAndNil(fUpgrade);
  inherited;
end;

procedure TFormAbout.CreateObjects;
var hdc: THandle;
    ly: integer;
    FontBuf: TLogFont;
begin
  // Create hand cursor
  Cursor_Hand := LoadCursor(0, IDC_HAND);
  // Create brush for form background
  Brush_Form := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
  // Get pixels per logical unit
  hdc := GetDC(0);
  ly := GetDeviceCaps(hdc, LOGPIXELSY);
  ReleaseDC(0, hdc);
  // Load the default font, upon which other fonts will be based
  Font_Default := SendDlgItemMessage(Handle, IDC_LABEL_YOUTUBEDOWNLOADER, WM_GETFONT, 0, 0);
  // Create font for the title label
  Font_Title := 0;
  if GetObject(Font_Default, Sizeof(FontBuf), @FontBuf) <> 0 then
    begin
    FontBuf.lfHeight := -19 * ly div 72;
    FontBuf.lfWidth := 0;
    FontBuf.lfWeight := FW_BOLD;
    FontBuf.lfUnderline := 1;
    Font_Title := CreateFontIndirect(FontBuf);
    end;
  // Create font for info texts
  Font_Info := 0;
  if GetObject(Font_Default, Sizeof(FontBuf), @FontBuf) <> 0 then
    begin
    FontBuf.lfWeight := FW_BOLD;
    Font_Info := CreateFontIndirect(FontBuf);
    end;
  // Create font for links
  Font_Link := 0;
  if GetObject(Font_Info, Sizeof(FontBuf), @FontBuf) <> 0 then
    begin
    FontBuf.lfUnderline := 1;
    Font_Link := CreateFontIndirect(FontBuf);
    end;
end;

procedure TFormAbout.DestroyObjects;
begin
  FreeGDIObject(Cursor_Hand);
  FreeGDIObject(Brush_Form);
  FreeGDIObject(Font_Default);
  FreeGDIObject(Font_Title);
  FreeGDIObject(Font_Info);
  FreeGDIObject(Font_Link); 
end;

function TFormAbout.DialogProc(var Msg: TMessage): boolean;
var H: THandle;
begin
  Result := inherited DialogProc(Msg);
  if not Result then
    case Msg.Msg of
      WM_CTLCOLORSTATIC:
        begin
        H := Msg.Result;
        Result := DoCtlColorStatic(Msg.wParam, Msg.lParam, H);
        if Result then
          Msg.Result := H;
        end;
      WM_SETCURSOR:
        begin
        Result := DoSetCursor(Msg.wParam, Msg.lParamLo, Msg.lParamHi);
        if Result then
          Msg.Result := 1;
        end;
      end;
end;

function TFormAbout.DoInitDialog: boolean;
{$IFNDEF THREADEDVERSION}
var fVersion, fUrl: string;
{$ENDIF}
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  // Label "YTD"
  {$IFDEF OVERRIDETITLEANDURL}
  SetDlgItemText(Self.Handle, IDC_LABEL_YOUTUBEDOWNLOADER, PChar(APPLICATION_TITLE));
  {$ENDIF}
  SendDlgItemMessage(Handle, IDC_LABEL_YOUTUBEDOWNLOADER, WM_SETFONT, Font_Title, 1);
  // Label "Version:" and version number
  SetDlgItemText(Self.Handle, IDC_LABEL_VERSION, PChar(APPLICATION_VERSION));
  SendDlgItemMessage(Handle, IDC_LABEL_VERSION, WM_SETFONT, Font_Info, 1);
  if TScriptedDownloader.MainScriptEngine <> nil then
    begin
    SetDlgItemText(Self.Handle, IDC_LABEL_DEFSVERSION, PChar(TScriptedDownloader.MainScriptEngine.Version));
    SendDlgItemMessage(Handle, IDC_LABEL_DEFSVERSION, WM_SETFONT, Font_Info, 1);
    end;
  // Label "Newest version:"
  fNewestVersionColor := {$IFDEF THREADEDVERSION} clBlack {$ELSE} clRed {$ENDIF} ;
  fNewestVersionUrl := '';
  fNewestDefsColor := {$IFDEF THREADEDVERSION} clBlack {$ELSE} clRed {$ENDIF} ;
  fNewestDefsUrl := '';
  {$IFDEF THREADEDVERSION}
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar(_('checking...')));
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION, PChar(_('checking...')));
  {$ENDIF}
  SendDlgItemMessage(Handle, IDC_LABEL_NEWESTVERSION, WM_SETFONT, Font_Info, 1);
  SendDlgItemMessage(Handle, IDC_LABEL_NEWESTDEFSVERSION, WM_SETFONT, Font_Info, 1);
  // Label "Homepage:"
  {$IFDEF OVERRIDETITLEANDURL}
  SetDlgItemText(Self.Handle, IDC_LABEL_HOMEPAGE, PChar(APPLICATION_URL));
  {$ENDIF}
  SendDlgItemMessage(Handle, IDC_LABEL_HOMEPAGE, WM_SETFONT, Font_Link, 1);
  // Get the newest version info
  if Options <> nil then
    begin
    FreeAndNil(fUpgrade);
    fUpgrade := TYTDUpgrade.Create(Options);
    fUpgrade.OnNewYTDFound := NewYTDEvent;
    fUpgrade.OnNewDefsFound := NewDefsEvent;
    fUpgrade.TestUpgrades( {$IFDEF THREADEDVERSION} True {$ELSE} False {$ENDIF} );
    end;
  // ListView with provider info
  LoadProviders;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LABEL_YOUTUBEDOWNLOADER), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LIST_PROVIDERS), [akTop, akLeft, akRight, akBottom]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'ABOUT_ACTIONS');
end;

function TFormAbout.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    DestroyObjects;
end;

function TFormAbout.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1: // Accelerators
      case Identifier of
        ACTION_CLOSE:
          Close;
        end;
    STN_CLICKED: // Click on a label
      case Identifier of
        IDC_LABEL_HOMEPAGE:
          begin
          LabelHomepageClick;
          Result := True;
          end;
        IDC_LABEL_NEWESTVERSION:
          begin
          LabelNewestVersionClick;
          Result := True;
          end;
        IDC_LABEL_NEWESTDEFSVERSION:
          begin
          LabelNewestDefsClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

{$IFNDEF STATICPROVIDERLIST}
function TFormAbout.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; WParam: WPARAM; LParam: LPARAM; out NotifyResult: LRESULT): boolean;
begin
  if (ControlID = IDC_LIST_PROVIDERS) and (Code = LVN_GETDISPINFO) then
    Result := ListProvidersGetDisplayInfo(PLVDispInfo(LParam))
  else
    Result := inherited DoNotify(Control, ControlID, Code, WParam, LParam, NotifyResult);
end;
{$ENDIF}

function TFormAbout.DoCtlColorStatic(DeviceContext: HDC; Control: THandle; out Brush: THandle): boolean;
begin
  Result := False;
  Brush := 0;
  if Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTVERSION) then
    begin
    SetTextColor(DeviceContext, fNewestVersionColor);
    SetBkMode(DeviceContext, TRANSPARENT);
    Brush := Brush_Form;
    Result := True;
    end
  else if Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION) then
    begin
    SetTextColor(DeviceContext, fNewestDefsColor);
    SetBkMode(DeviceContext, TRANSPARENT);
    Brush := Brush_Form;
    Result := True;
    end
  else if Control = GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE) then
    begin
    SetTextColor(DeviceContext, clBlue);
    SetBkMode(DeviceContext, TRANSPARENT);
    Brush := Brush_Form;
    Result := True;
    end
end;

function TFormAbout.DoSetCursor(Control: THandle; HitTestCode, Identifier: Word): boolean;
begin
  Result := False;
  if False
     or (Control = GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE))
     or ((Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTVERSION)) and (fNewestVersionUrl <> ''))
     or ((Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION)) and (fNewestDefsUrl <> ''))
  then
    begin
    SetCursor(Cursor_Hand);
    Result := True;
    end;
end;

procedure TFormAbout.NewYTDEvent(Sender: TYTDUpgrade);
begin
  fNewestVersionUrl := Sender.OnlineYTDUrl;
  if Sender.OnlineYTDVersion = '' then
    begin
    fNewestVersionColor := clRed;
    SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar(_('not found')));
    end
  else
    begin
    fNewestVersionColor := clBlack;
    SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar(_( Sender.OnlineYTDVersion )));
    if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) < 0 then
      begin
      fNewestVersionColor := clBlue;
      SendDlgItemMessage(Handle, IDC_LABEL_NEWESTVERSION, WM_SETFONT, Font_Link, 1);
      ShowWindow(GetDlgItem(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION), SW_HIDE);
      end;
    end;
end;

procedure TFormAbout.NewDefsEvent(Sender: TYTDUpgrade);
begin
  fNewestDefsUrl := Sender.OnlineDefsUrl;
  if Sender.OnlineDefsVersion = '' then
    begin
    fNewestDefsColor := clRed;
    SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION, PChar(_('not found')));
    end
  else
    begin
    fNewestDefsColor := clBlack;
    SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTDEFSVERSION, PChar(_( Sender.OnlineDefsVersion )));
    if TScriptedDownloader.MainScriptEngine <> nil then
      if Sender.CompareVersions(TScriptedDownloader.MainScriptEngine.Version, Sender.OnlineDefsVersion) < 0 then
        begin
        fNewestDefsColor := clBlue;
        SendDlgItemMessage(Handle, IDC_LABEL_NEWESTDEFSVERSION, WM_SETFONT, Font_Link, 1);
        end;
    end;
end;

procedure TFormAbout.LabelHomepageClick;
begin
  Run(APPLICATION_URL, Handle);
end;

procedure TFormAbout.LabelNewestVersionClick;
begin
  if fUpgrade <> nil then
    guiFunctions.UpgradeYTD(fUpgrade, Handle);
end;

procedure TFormAbout.LabelNewestDefsClick;
begin
  if fUpgrade <> nil then
    if guiFunctions.UpgradeDefs(fUpgrade, Handle, True) then
      begin
      fNewestDefsColor := clBlack;
      fNewestDefsUrl := '';
      SetDlgItemText(Self.Handle, IDC_LABEL_DEFSVERSION, PChar(_( fUpgrade.OnlineDefsVersion )));
      SendDlgItemMessage(Handle, IDC_LABEL_DEFSVERSION, WM_SETFONT, Font_Info, 1);
      SendDlgItemMessage(Handle, IDC_LABEL_NEWESTDEFSVERSION, WM_SETFONT, Font_Info, 1);
      Update;
      end;
end;

{$IFNDEF STATICPROVIDERLIST}
function TFormAbout.ListProvidersGetDisplayInfo(DispInfo: PLVDispInfo): boolean;
begin
  Result := False;
  case DispInfo^.item.iSubItem of
    LISTVIEW_SUBITEM_PROVIDER:
      Result := ListViewSetVirtualItemText(DispInfo, DownloadClassifier.Names[DispInfo^.item.iItem]);
    LISTVIEW_SUBITEM_COMPONENTS:
      Result := ListViewSetVirtualItemText(DispInfo, DownloadClassifier.NameClasses[DispInfo^.item.iItem]);
    end;
end;
{$ENDIF}

procedure TFormAbout.LoadProviders;
var ListViewHandle: THandle;
{$IFDEF STATICPROVIDERLIST}
var i: integer;
    Item: LV_ITEM;
{$ENDIF}
begin
  ListViewHandle := GetDlgItem(Self.Handle, IDC_LIST_PROVIDERS);
  // Create columns
  ListViewInsertColumn(ListViewHandle, 0, LISTVIEW_SUBITEM_PROVIDER, alLeft, 100, _('Provider'));
  ListViewInsertColumn(ListViewHandle, 1, LISTVIEW_SUBITEM_COMPONENTS, alLeft, 264, _('Components'));
  // Create items
  SendMessage(ListViewHandle, LVM_DELETEALLITEMS, 0, 0);
  if DownloadClassifier <> nil then
    if DownloadClassifier.NameCount > 0 then
      begin
      ShowApiError(SendMessage(ListViewHandle, LVM_SETITEMCOUNT, DownloadClassifier.NameCount, 0) = 0);
      {$IFDEF STATICPROVIDERLIST}
      for i := 0 to Pred(DownloadClassifier.NameCount) do
        begin
        Item.mask := LVIF_TEXT;
        Item.iItem := i;
        Item.iSubItem := LISTVIEW_SUBITEM_PROVIDER;
        Item.pszText := PChar(DownloadClassifier.Names[i]);
        ShowApiError(SendMessage(ListViewHandle, LVM_INSERTITEM, 0, integer(@Item)) = 0);
        Item.mask := LVIF_TEXT;
        Item.iItem := i;
        Item.iSubItem := LISTVIEW_SUBITEM_COMPONENTS;
        Item.pszText := PChar(DownloadClassifier.NameClasses[i]);
        ShowApiError(SendMessage(ListViewHandle, LVM_SETITEM, 0, integer(@Item)) = 0);
        end;
      {$ENDIF}
      end;
end;

initialization
   InitCommonControls;

end.
