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

unit guiAboutWINAPI;
{$INCLUDE 'ytd.inc'}
{.DEFINE STATICPROVIDERLIST}

interface

{$RESOURCE guiAboutWINAPI.res guiAboutWINAPI.rc}

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiForm, uApiGraphics,
  uLanguages, uMessages, uDownloadClassifier, uDownloader, uOptions;

type
  TFormAbout = class(TApiForm)
    protected
      function DialogProc(var Msg: TMessage): boolean; override;
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      {$IFNDEF STATICPROVIDERLIST}
      function DoNotify(Control: THandle; ControlID: DWORD; Code, WParam, LParam: integer; out NotifyResult: integer): boolean; override;
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
      procedure CreateGdiObjects;
      procedure DestroyGdiObjects;
    private
      fDownloadClassifier: TDownloadClassifier;
      fOptions: TYTDOptions;
      fNewestVersionColor: TColor;
      fNewestVersionUrl: string;
    protected
      procedure NewVersionEvent(Sender: TObject; const Version, Url: string); virtual;
      procedure LabelHomepageClick; virtual;
      procedure LabelNewestVersionClick; virtual;
      {$IFNDEF STATICPROVIDERLIST}
      function ListProvidersGetDisplayInfo(DispInfo: PLVDispInfo): boolean; virtual;
      {$ENDIF}
      procedure LoadProviders; virtual;
    protected
      class function DefaultResourceName: string; override;
    public
      constructor Create(const ADialogResourceName: string); override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions write fOptions;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier write fDownloadClassifier;
    end;

implementation

// from resource.h
const
  IDC_LABEL_VERSIONCAPTION = 1000;
  IDC_LABEL_NEWESTVERSIONCAPTION = 1001;
  IDC_LABEL_HOMEPAGECAPTION = 1002;
  IDC_LABEL_YOUTUBEDOWNLOADER = 1003;
  IDC_LABEL_VERSION = 1004;
  IDC_LABEL_NEWESTVERSION = 1007;
  IDC_LABEL_HOMEPAGE = 1008;
  IDC_LIST_PROVIDERS = 1006;

//
const
  LISTVIEW_SUBITEM_PROVIDER = 0;
  LISTVIEW_SUBITEM_COMPONENTS = 1;

{ TFormAbout }

class function TFormAbout.DefaultResourceName: string;
begin
  Result := 'guiAboutWinAPI';
end;

constructor TFormAbout.Create(const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormAbout.Destroy;
begin
  inherited;
end;

procedure TFormAbout.CreateGdiObjects;
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

procedure TFormAbout.DestroyGdiObjects;
begin
  FreeGDIObject(Cursor_Hand); Cursor_Hand := 0;
  FreeGDIObject(Brush_Form); Brush_Form := 0;
  FreeGDIObject(Font_Default); Font_Default := 0;
  FreeGDIObject(Font_Title); Font_Title := 0;
  FreeGDIObject(Font_Info); Font_Info := 0;
  FreeGDIObject(Font_Link); Font_Link := 0;
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
begin
  Result := inherited DoInitDialog;
  CreateGDIObjects;
  // Caption
  SetWindowText(Self.Handle, PChar(_('About YouTube Downloader')));
  // Label "YouTube Downloader"
  SetDlgItemText(Self.Handle, IDC_LABEL_YOUTUBEDOWNLOADER, APPLICATION_TITLE);
  SendDlgItemMessage(Handle, IDC_LABEL_YOUTUBEDOWNLOADER, WM_SETFONT, Font_Title, 1);
  // Label "Version:" and version number
  SetDlgItemText(Self.Handle, IDC_LABEL_VERSIONCAPTION, PChar(_('Version:')));
  SetDlgItemText(Self.Handle, IDC_LABEL_VERSION, {$INCLUDE 'ytd.version'});
  SendDlgItemMessage(Handle, IDC_LABEL_VERSION, WM_SETFONT, Font_Info, 1);
  // Label "Newest version:"
  fNewestVersionColor := {$IFDEF THREADEDVERSION} clBlack {$ELSE} clRed {$ENDIF} ;
  fNewestVersionUrl := '';
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSIONCAPTION, PChar(_('Newest version:')));
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar( {$IFDEF THREADEDVERSION}_('checking...') {$ELSE} _('not found') {$ENDIF} ));
  SendDlgItemMessage(Handle, IDC_LABEL_NEWESTVERSION, WM_SETFONT, Font_Info, 1);
  // Label "Homepage:"
  SetDlgItemText(Self.Handle, IDC_LABEL_HOMEPAGECAPTION, PChar(_('Homepage:')));
  SetDlgItemText(Self.Handle, IDC_LABEL_HOMEPAGE, APPLICATION_URL);
  SendDlgItemMessage(Handle, IDC_LABEL_HOMEPAGE, WM_SETFONT, Font_Link, 1);
  // Get the newest version info
  if Options <> nil then
    {$IFDEF THREADEDVERSION}
    Options.GetNewestVersionInBackground(NewVersionEvent);
    {$ELSE}
    if Options.GetNewestVersion(Version, Url) then
      NewVersionEvent(Options, Version, Url);
    {$ENDIF}
  // ListView with provider info
  LoadProviders;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LABEL_YOUTUBEDOWNLOADER), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LIST_PROVIDERS), [akTop, akLeft, akRight, akBottom]);
end;

function TFormAbout.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    DestroyGdiObjects;
end;

function TFormAbout.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
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
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

{$IFNDEF STATICPROVIDERLIST}
function TFormAbout.DoNotify(Control: THandle; ControlID: DWORD; Code, WParam, LParam: integer; out NotifyResult: integer): boolean;
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
  if False then
    begin
    end
  else if Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTVERSION) then
    begin
    SetTextColor(DeviceContext, fNewestVersionColor);
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
  if (Control = GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE)) or ((Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTVERSION)) and (fNewestVersionUrl <> '')) then
    begin
    SetCursor(Cursor_Hand);
    Result := True;
    end;
end;

procedure TFormAbout.NewVersionEvent(Sender: TObject; const Version, Url: string);
begin
  fNewestVersionUrl := Url;
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar(_( Version )));
  if Version > {$INCLUDE 'YTD.version'} then
    begin
    fNewestVersionColor := clBlue;
    SendDlgItemMessage(Handle, IDC_LABEL_NEWESTVERSION, WM_SETFONT, Font_Link, 1);
    end;
end;

procedure TFormAbout.LabelHomepageClick;
begin
  ShellExecute(Handle, 'open', APPLICATION_URL, nil, nil, 0);
end;

procedure TFormAbout.LabelNewestVersionClick;
begin
  if fNewestVersionUrl <> '' then
    ShellExecute(Handle, 'open', PChar(fNewestVersionUrl), nil, nil, 0);
end;

{$IFNDEF STATICPROVIDERLIST}
var StaticDisplayInfoText: string;

function TFormAbout.ListProvidersGetDisplayInfo(DispInfo: PLVDispInfo): boolean;
begin
  Result := False;
  case DispInfo^.item.iSubItem of
    LISTVIEW_SUBITEM_PROVIDER:
      begin
      StaticDisplayInfoText := DownloadClassifier.Names[DispInfo^.item.iItem];
      DispInfo^.item.pszText := PChar(StaticDisplayInfoText);
      Result := True;
      end;
    LISTVIEW_SUBITEM_COMPONENTS:
      begin
      StaticDisplayInfoText := DownloadClassifier.NameClasses[DispInfo^.item.iItem];
      DispInfo^.item.pszText := PChar(StaticDisplayInfoText);
      Result := True;
      end;
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
