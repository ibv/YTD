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

unit guiOptionsWINAPI_Nova;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uDownloader, uOptions, guiOptionsWINAPI_Downloader, guiOptionsWINAPI_CommonDownloader;

type
  TFrameDownloaderOptionsPage_Nova = class(TFrameDownloaderOptionsPageCommon)
    protected
      function SubFormClass: TFrameDownloaderOptionsPageClass; override;
    public
      constructor Create(AOwner: TApiForm); override;
    end;

  TFrameDownloaderOptionsPageSpec_Nova = class(TFrameDownloaderOptionsPage)
    protected
      function DoInitDialog: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      EditSecretPassword: THandle;
      procedure CreateObjects; override;
      procedure DestroyObjects; override;
      procedure LabelSecretPasswordClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

uses
  downNova;

const
  IDC_CHECKBOX_LOWQUALITY = 1001;
  IDC_LABEL_SECRETPASSWORD = 1002;
  IDC_EDIT_SECRETPASSWORD = 1003;

{ TFrameDownloaderOptionsPage_Nova }

constructor TFrameDownloaderOptionsPage_Nova.Create(AOwner: TApiForm);
begin
  Create(AOwner, TFrameDownloaderOptionsPageCommon.ClassName);
end;

function TFrameDownloaderOptionsPage_Nova.SubFormClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPageSpec_Nova;
end;

{ TFrameDownloaderOptionsPageSpec_Nova }

constructor TFrameDownloaderOptionsPageSpec_Nova.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPageSpec_Nova.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPageSpec_Nova.CreateObjects;
begin
  inherited;
  EditSecretPassword := GetDlgItem(Self.Handle, IDC_EDIT_SECRETPASSWORD);
end;

procedure TFrameDownloaderOptionsPageSpec_Nova.DestroyObjects;
begin
  EditSecretPassword := 0;
  inherited;
end;

function TFrameDownloaderOptionsPageSpec_Nova.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_LOWQUALITY), [akLeft, akTop, akRight]);
  SetControlAnchors(EditSecretPassword, [akLeft, akTop, akRight]);
end;

function TFrameDownloaderOptionsPageSpec_Nova.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_SECRETPASSWORD:
          begin
          LabelSecretPasswordClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloaderOptionsPageSpec_Nova.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
begin
  inherited;
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_LOWQUALITY, CheckboxConsts[Options.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT)]);
  SetWindowText(EditSecretPassword, PChar(Options.ReadProviderOptionDef(Provider, OPTION_NOVA_SECRET, OPTION_NOVA_SECRET_DEFAULT)));
end;

procedure TFrameDownloaderOptionsPageSpec_Nova.SaveToOptions;
begin
  inherited;
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_LOWQUALITY) of
    BST_CHECKED:
      Options.WriteProviderOption(Provider, OPTION_NOVA_LOWQUALITY, True);
    BST_UNCHECKED:
      Options.WriteProviderOption(Provider, OPTION_NOVA_LOWQUALITY, False);
    end;
  Options.WriteProviderOption(Provider, OPTION_NOVA_SECRET, GetWindowTextAsString(EditSecretPassword));
end;

procedure TFrameDownloaderOptionsPageSpec_Nova.LabelSecretPasswordClick;
begin
  SetFocus(EditSecretPassword);
end;

end.
