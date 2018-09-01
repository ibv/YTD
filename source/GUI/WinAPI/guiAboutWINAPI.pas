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
{$DEFINE OVERRIDETITLEANDURL}

interface

uses
  SysUtils, Classes, Windows, Messages, ShellApi,
  uApiForm, uApiGraphics,
  uLanguages, uDownloadClassifier, uDownloader, uOptions;

type
  TFormAbout = class(TApiForm)
    private
      fDownloadClassifier: TDownloadClassifier;
      fOptions: TYTDOptions;
      fNewestVersionColor: TColor;
      fNewestVersionUrl: string;
    protected
      function DialogProc(var Msg: TMessage): boolean; override;
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      function DoCtlColorStatic(DeviceContext: HDC; Control: THandle; out Brush: THandle): boolean; virtual;
      function DoSetCursor(Control: THandle; HitTestCode, Identifier: Word): boolean; virtual;
    private
      Cursor_Default: THandle;
      Cursor_Hand: THandle;
      Brush_Form: THandle;
      Font_Default: THandle;
      Font_Title: THandle;
      Font_Info: THandle;
      Font_Link: THandle;
      procedure PrepareGdiObjects;
    private
    protected
      procedure NewVersionEvent(Sender: TObject; const Version, Url: string); virtual;
      procedure LabelHomepageClick; virtual;
      procedure LabelNewestVersionClick; virtual;
    public
      constructor Create(const ADialogResourceName: string); override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions write fOptions;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier write fDownloadClassifier;
    end;

implementation

{$RESOURCE guiAboutWINAPI.res guiAboutWINAPI.rc}

// from resource.h
const
  IDC_LABEL_VERSIONCAPTION = 1000;
  IDC_LABEL_NEWESTVERSIONCAPTION = 1001;
  IDC_LABEL_HOMEPAGECAPTION = 1002;
  IDC_LABEL_YOUTUBEDOWNLOADER = 1003;
  IDC_LABEL_VERSION = 1004;
  IDC_LABEL_NEWESTVERSION = 1007;
  IDC_LABEL_HOMEPAGE = 1008;

// Built-in:
const
  YTD_TITLE = 'YouTube Downloader';
  YTD_HOMEPAGE = 'http://www.pepak.net/download/youtube-downloader/';

{ TFormAbout }

constructor TFormAbout.Create(const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormAbout.Destroy;
begin
  inherited;
end;

procedure TFormAbout.PrepareGdiObjects;
var hdc: THandle;
    ly: integer;
    FontBuf: TLogFont;
begin
  // Create default cursor
  Cursor_Default := LoadCursor(0, IDC_ARROW);
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
  PrepareGDIObjects;
  // Label "YouTube Downloader"
  {$IFDEF OVERRIDETITLEANDURL}
  SetDlgItemText(Self.Handle, IDC_LABEL_YOUTUBEDOWNLOADER, YTD_TITLE);
  {$ENDIF}
  SendDlgItemMessage(Handle, IDC_LABEL_YOUTUBEDOWNLOADER, WM_SETFONT, Font_Title, 1);
  // Label "Version:" and version number
  SetDlgItemText(Self.Handle, IDC_LABEL_VERSIONCAPTION, PChar(_('Version:')));
  SetDlgItemText(Self.Handle, IDC_LABEL_VERSION, {$INCLUDE 'ytd.version'});
  SendDlgItemMessage(Handle, IDC_LABEL_VERSION, WM_SETFONT, Font_Info, 1);
  // Label "Newest version:"
  fNewestVersionColor := {$IFDEF THREADEDVERSION} clBlack {$ELSE} clRed {$ENDIF} ;
  fNewestVersionUrl := '';
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSIONCAPTION, PChar(_('Newest version:')));
  SetDlgItemText(Self.Handle, IDC_LABEL_NEWESTVERSION, PChar(_( {$IFDEF THREADEDVERSION} 'checking...' {$ELSE} 'not found' {$ENDIF} )));
  SendDlgItemMessage(Handle, IDC_LABEL_NEWESTVERSION, WM_SETFONT, Font_Info, 1);
  // Label "Homepage:"
  SetDlgItemText(Self.Handle, IDC_LABEL_HOMEPAGECAPTION, PChar(_('Homepage:')));
  {$IFDEF OVERRIDETITLEANDURL}
  SetDlgItemText(Self.Handle, IDC_LABEL_HOMEPAGE, YTD_HOMEPAGE);
  {$ENDIF}
  SendDlgItemMessage(Handle, IDC_LABEL_HOMEPAGE, WM_SETFONT, Font_Link, 1);
  // Get the newest version info
  if Options <> nil then
    {$IFDEF THREADEDVERSION}
    Options.GetNewestVersionInBackground(NewVersionEvent);
    {$ELSE}
    if Options.GetNewestVersion(Version, Url) then
      NewVersionEvent(Options, Version, Url);
    {$ENDIF}
end;

function TFormAbout.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    FreeGDIObject(Font_Title);
    FreeGDIObject(Font_Info);
    FreeGDIObject(Font_Link);
    FreeGDIObject(Brush_Form);
    end;
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
var Cur: THandle;
begin
  Result := True;
  if (Control = GetDlgItem(Self.Handle, IDC_LABEL_HOMEPAGE)) or ((Control = GetDlgItem(Self.Handle, IDC_LABEL_NEWESTVERSION)) and (fNewestVersionUrl <> '')) then
    Cur := Cursor_Hand
  else
    Cur := Cursor_Default;
  SetCursor(Cur);
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
  ShellExecute(Handle, 'open', YTD_HOMEPAGE, nil, nil, 0);
end;

procedure TFormAbout.LabelNewestVersionClick;
begin
  if fNewestVersionUrl <> '' then
    ShellExecute(Handle, 'open', PChar(fNewestVersionUrl), nil, nil, 0);
end;

end.
