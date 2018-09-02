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

unit guiOptionsWINAPI_Joj;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uDownloader, uOptions, guiOptionsWINAPI_Downloader, guiOptionsWINAPI_CommonDownloader;

type
  TFrameDownloaderOptionsPage_Joj = class(TFrameDownloaderOptionsPageCommon)
    protected
      function SubFormClass: TFrameDownloaderOptionsPageClass; override;
    public
      constructor Create(AOwner: TApiForm); override;
    end;

  TFrameDownloaderOptionsPageSpec_Joj = class(TFrameDownloaderOptionsPage)
    protected
      function DoInitDialog: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      EditServer: THandle;
      procedure CreateObjects; override;
      procedure DestroyObjects; override;
      procedure LabelServerClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

uses
  downJoj;

const
  IDC_LABEL_SERVER = 1001;
  IDC_EDIT_SERVER = 1002;

{ TFrameDownloaderOptionsPage_Joj }

constructor TFrameDownloaderOptionsPage_Joj.Create(AOwner: TApiForm);
begin
  Create(AOwner, TFrameDownloaderOptionsPageCommon.ClassName);
end;

function TFrameDownloaderOptionsPage_Joj.SubFormClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPageSpec_Joj;
end;

{ TFrameDownloaderOptionsPage_Joj }

constructor TFrameDownloaderOptionsPageSpec_Joj.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPageSpec_Joj.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPageSpec_Joj.CreateObjects;
begin
  inherited;
  EditServer := GetDlgItem(Self.Handle, IDC_EDIT_SERVER);
end;

procedure TFrameDownloaderOptionsPageSpec_Joj.DestroyObjects;
begin
  EditServer := 0;
  inherited;
end;

function TFrameDownloaderOptionsPageSpec_Joj.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
end;

function TFrameDownloaderOptionsPageSpec_Joj.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_SERVER:
          begin
          LabelServerClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloaderOptionsPageSpec_Joj.LoadFromOptions;
begin
  inherited;
  SetWindowText(EditServer, PChar(Options.ReadProviderOptionDef(Provider, OPTION_JOJ_SERVER, '')));
end;

procedure TFrameDownloaderOptionsPageSpec_Joj.SaveToOptions;
begin
  inherited;
  Options.WriteProviderOption(Provider, OPTION_JOJ_SERVER, GetWindowTextAsString(EditServer));
end;

procedure TFrameDownloaderOptionsPageSpec_Joj.LabelServerClick;
begin
  SetFocus(EditServer);
end;

end.
