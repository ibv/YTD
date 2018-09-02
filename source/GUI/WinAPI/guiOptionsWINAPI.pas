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

unit guiOptionsWINAPI;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uLanguages, uMessages, uOptions, guiConsts, guiFunctions, uDialogs;

type
  TFrameOptions = class;

  TFormOptions = class(TApiForm)
    protected
    private
      fOptions: TYTDOptions;
    private
      PageOptions: TApiTabControl;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoSize(ResizeType, NewWidth, NewHeight: integer): boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      function DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: LRESULT): boolean; override;
    protected
      procedure LoadFromOptions;
      procedure SaveToOptions;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions write fOptions;
    end;

  TFrameOptions = class(TApiTabSheet)
    private
      fOptions: TYTDOptions;
    protected
    public
      procedure LoadFromOptions; virtual; abstract;
      procedure SaveToOptions; virtual; abstract;
      property Options: TYTDOptions read fOptions write fOptions;
    end;
  TFrameOptionsClass = class of TFrameOptions;

implementation

{$RESOURCE *.res}

uses
  {$IFDEF CONVERTERS} guiConverterWINAPI, {$ENDIF}
  guiOptionsWINAPI_Downloads, guiOptionsWINAPI_Main, guiOptionsWINAPI_Network,
  guiOptionsWINAPI_Downloaders;

// from resource.h
const
  IDC_BUTTON_OK = 1000;
  IDC_BUTTON_CANCEL = 1001;
  IDC_PAGE_OPTIONS = 1002;

const
  ACTION_OK = 40000;
  ACTION_CANCEL = 40001;

{ TFormOptions }

constructor TFormOptions.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormOptions.Destroy;
begin
  inherited;
end;

procedure TFormOptions.CreateObjects;

  function AddPage(PageClass: TFrameOptionsClass; const Caption: string): integer;
    var Page: TFrameOptions;
    begin
      Page := PageClass.Create(Self);
      Page.Options := Options;
      Page.Show;
      Result := PageOptions.Add(Page, Caption);
    end;

begin
  PageOptions := TApiTabControl.Create(GetDlgItem(Self.Handle, IDC_PAGE_OPTIONS), True);
  AddPage(TFrameMainOptions, _('Main settings'));
  AddPage(TFrameDownloadOptions, _('Download settings'));
  AddPage(TFrameNetworkOptions, _('Network settings'));
  AddPage(TFrameDownloaderOptions, _('Downloader settings'));
  PageOptions.ResizeTabs;
end;

procedure TFormOptions.DestroyObjects;
begin
  FreeAndNil(PageOptions);
end;

function TFormOptions.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
  // Show the default tab
  PageOptions.ActivePageIndex := 0;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_PAGE_OPTIONS), [akTop, akLeft, akRight, akBottom]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_OK), [akBottom, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_CANCEL), [akBottom, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'OPTIONS_ACTIONS');
end;

function TFormOptions.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFormOptions.DoSize(ResizeType, NewWidth, NewHeight: integer): boolean;
begin
  Result := inherited DoSize(ResizeType, NewWidth, NewHeight);
  PageOptions.ResizeTabs;
end;

function TFormOptions.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1 {, CBN_SELCHANGE}: // Accelerators
      case Identifier of
        ACTION_OK:
          begin
          Close(idOK);
          Result := True;
          end;
        ACTION_CANCEL:
          begin
          Close(idCancel);
          Result := True;
          end;
        end;
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_BUTTON_OK:
          begin
          Close(idOK);
          Result := True;
          end;
        IDC_BUTTON_CANCEL:
          begin
          Close(idCancel);
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

function TFormOptions.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: LRESULT): boolean;
begin
  if (ControlID = IDC_PAGE_OPTIONS) and (Code = TCN_SELCHANGE) then
    begin
    PageOptions.ActivePageIndex := PageOptions.RealActivePageIndex;
    Result := True;
    end
  else
    Result := inherited DoNotify(Control, ControlID, Code, WParam, LParam, NotifyResult);
end;

procedure TFormOptions.LoadFromOptions;
var i: integer;
begin
  for i := 0 to Pred(PageOptions.Count) do
    if PageOptions[i] <> nil then
      TFrameOptions(PageOptions[i]).LoadFromOptions;
end;

procedure TFormOptions.SaveToOptions;
var i: integer;
begin
  for i := 0 to Pred(PageOptions.Count) do
    if PageOptions[i] <> nil then
      TFrameOptions(PageOptions[i]).SaveToOptions;
end;

initialization
   InitCommonControls;

end.
