(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit guiOptionsWINAPI_Barrandov;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uDownloader, uOptions, guiOptionsWINAPI_Downloader, guiOptionsWINAPI_CommonDownloader;

type
  TFrameDownloaderOptionsPage_Barrandov = class(TFrameDownloaderOptionsPageCommon)
    protected
      function SubFormClass: TFrameDownloaderOptionsPageClass; override;
    public
      constructor Create(AOwner: TApiForm); override;
    end;

  TFrameDownloaderOptionsPageSpec_Barrandov = class(TFrameDownloaderOptionsPage)
    protected
      function DoInitDialog: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      EditSecureToken: THandle;
      procedure CreateObjects; override;
      procedure DestroyObjects; override;
      procedure LabelSecureTokenClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

uses
  downBarrandovTV;

const
  IDC_LABEL_SECURETOKEN = 1001;
  IDC_EDIT_SECURETOKEN = 1002;

{ TFrameDownloaderOptionsPage_Barrandov }

constructor TFrameDownloaderOptionsPage_Barrandov.Create(AOwner: TApiForm);
begin
  Create(AOwner, TFrameDownloaderOptionsPageCommon.ClassName);
end;

function TFrameDownloaderOptionsPage_Barrandov.SubFormClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPageSpec_Barrandov;
end;

{ TFrameDownloaderOptionsPage_Barrandov }

constructor TFrameDownloaderOptionsPageSpec_Barrandov.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPageSpec_Barrandov.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPageSpec_Barrandov.CreateObjects;
begin
  inherited;
  EditSecureToken := GetDlgItem(Self.Handle, IDC_EDIT_SECURETOKEN);
end;

procedure TFrameDownloaderOptionsPageSpec_Barrandov.DestroyObjects;
begin
  EditSecureToken := 0;
  inherited;
end;

function TFrameDownloaderOptionsPageSpec_Barrandov.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  SetControlAnchors(EditSecureToken, [akLeft, akTop, akRight]);
end;

function TFrameDownloaderOptionsPageSpec_Barrandov.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_SECURETOKEN:
          begin
          LabelSecureTokenClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloaderOptionsPageSpec_Barrandov.LoadFromOptions;
begin
  inherited;
  SetWindowText(EditSecureToken, PChar(Options.ReadProviderOptionDef(Provider, OPTION_BARRANDOV_SECURETOKEN, OPTION_BARRANDOV_SECURETOKEN_DEFAULT)));
end;

procedure TFrameDownloaderOptionsPageSpec_Barrandov.SaveToOptions;
begin
  inherited;
  Options.WriteProviderOption(Provider, OPTION_BARRANDOV_SECURETOKEN, GetWindowTextAsString(EditSecureToken));
end;

procedure TFrameDownloaderOptionsPageSpec_Barrandov.LabelSecureTokenClick;
begin
  SetFocus(EditSecureToken);
end;

end.
