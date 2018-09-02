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

unit guiSetupVCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellApi,
  uLanguages, uMessages, uFunctions, uDialogs, uCompatibility,
  guiFunctions;

type
  TFormSetup = class(TForm)
    LabelDestinationDir: TLabel;
    EditDestinationDir: TEdit;
    BtnDestinationDir: TButton;
    CheckDesktopShortcut: TCheckBox;
    CheckStartMenuShortcut: TCheckBox;
    ButtonInstall: TButton;
    ButtonRun: TButton;
    procedure BtnDestinationDirClick(Sender: TObject);
  private
    function GetDestinationDir: string;
    function GetDesktopShortcut: boolean;
    function GetStartMenuShortcut: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property DestinationDir: string read GetDestinationDir;
    property DesktopShortcut: boolean read GetDesktopShortcut;
    property StartMenuShortcut: boolean read GetStartMenuShortcut;
  end;

implementation

{$R *.DFM}

{ TFormSetup }

constructor TFormSetup.Create(AOwner: TComponent);
begin
  inherited;
  TranslateProperties(self);
  EditDestinationDir.Text := GetSpecialFolder(CSIDL_PROGRAM_FILES) + '\' + APPLICATION_TITLE;
end;

procedure TFormSetup.BtnDestinationDirClick(Sender: TObject);
var Dir: string;
begin
  Dir := EditDestinationDir.Text;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    EditDestinationDir.Text := Dir;
end;

function TFormSetup.GetDestinationDir: string;
begin
  Result := EditDestinationDir.Text;
end;

function TFormSetup.GetDesktopShortcut: boolean;
begin
  Result := CheckDesktopShortcut.Checked;
end;

function TFormSetup.GetStartMenuShortcut: boolean;
begin
  Result := CheckStartMenuShortcut.Checked;
end;

end.
