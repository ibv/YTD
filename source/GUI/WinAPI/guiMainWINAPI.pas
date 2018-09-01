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

unit guiMainWINAPI;
{$INCLUDE 'ytd.inc'}

interface

{$RESOURCE guiMainWINAPI.res guiMainWINAPI.rc}

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiForm, uApiGraphics,
  SynaCode,
  uLanguages, uMessages, uOptions, uStringUtils,
  guiOptions, uDialogs,
  uDownloadList, uDownloadListItem, uDownloadThread;

type
  TFormMain = class(TApiForm)
    protected
      function DialogProc(var Msg: TMessage): boolean; override;
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
{
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      function DoNotify(Control: THandle; ControlID: DWORD; Code, WParam, LParam: integer; out NotifyResult: integer): boolean; override;
}
    private
      // Moje GDI objekty
      procedure CreateGdiObjects;
      procedure DestroyGdiObjects;
    private
      // Moje soukroma data
    protected
      // Moje properties a pomocne funkce
    protected
      class function DefaultResourceName: string; override;
    public
      constructor Create(const ADialogResourceName: string); override;
      destructor Destroy; override;
    end;

implementation

{ TFormMain }

class function TFormMain.DefaultResourceName: string;
begin
  Result := 'guiMainWINAPI';
end;

constructor TFormMain.Create(const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormMain.Destroy;
begin
  inherited;
end;

procedure TFormMain.CreateGdiObjects;
begin

end;

procedure TFormMain.DestroyGdiObjects;
begin

end;

function TFormMain.DialogProc(var Msg: TMessage): boolean;
begin
  Result := inherited DialogProc(Msg);
end;

function TFormMain.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateGDIObjects;
  // Caption
  SetWindowText(Self.Handle, APPLICATION_TITLE);
end;

function TFormMain.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    DestroyGdiObjects;
end;

end.
