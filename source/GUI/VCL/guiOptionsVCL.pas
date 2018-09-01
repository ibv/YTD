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

unit guiOptionsVCL;
{$INCLUDE 'ytd.inc'}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, ComCtrls, ShlObj, 
  uLanguages, uMessages, uOptions, uDialogs, guiConsts, guiFunctions;

type
  TFormOptions = class(TForm)
    LabelOverwriteMode: TLabel;
    ComboOverwriteMode: TComboBox;
    LabelDownloadDir: TLabel;
    EditDownloadDir: TEdit;
    BtnDownloadDir: TButton;
    LabelConverter: TLabel;
    ComboConverter: TComboBox;
    BtnOK: TButton;
    btnCancel: TButton;
    ActionList: TActionList;
    actOK: TAction;
    actCancel: TAction;
    actDownloadDir: TAction;
    PageOptions: TPageControl;
    TabMain: TTabSheet;
    CheckPortableMode: TCheckBox;
    CheckCheckNewVersions: TCheckBox;
    LabelLanguage: TLabel;
    EditLanguage: TEdit;
    TabDownloadOptions: TTabSheet;
    CheckSubtitlesEnabled: TCheckBox;
    TabNetworkOptions: TTabSheet;
    CheckUseProxy: TCheckBox;
    LabelProxyHost: TLabel;
    EditProxyHost: TEdit;
    EditProxyPort: TEdit;
    LabelProxyPort: TLabel;
    EditProxyUser: TEdit;
    LabelProxyUser: TLabel;
    EditProxyPass: TEdit;
    LabelProxyPass: TLabel;
    BtnDesktopShortcut: TButton;
    BtnStartMenuShortcut: TButton;
    actDesktopShortcut: TAction;
    actStartMenuShortcut: TAction;
    CheckMonitorClipboard: TCheckBox;
    CheckAutoTryHtmlParser: TCheckBox;
    CheckAutoDownload: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actDownloadDirExecute(Sender: TObject);
    procedure ComboConverterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actDesktopShortcutExecute(Sender: TObject);
    procedure actStartMenuShortcutExecute(Sender: TObject);
  private
    fLoading: boolean;
    fOptions: TYTDOptions;
    {$IFDEF CONVERTERS}
    fConverterIndex: integer;
    {$ENDIF}
  protected
  public
    property Options: TYTDOptions read fOptions write fOptions;
  end;

implementation

{$R *.DFM}

{$IFDEF CONVERTERS}
uses
  guiConverterVCL;
{$ENDIF}

procedure TFormOptions.FormShow(Sender: TObject);
const OverwriteMode: array [TOverwriteMode] of integer = (2, 1, 3, 0);
begin
  fLoading := True;
  try
    // Main options
    CheckPortableMode.Checked := Options.PortableMode;
    CheckCheckNewVersions.Checked := Options.CheckForNewVersionOnStartup;
    CheckMonitorClipboard.Checked := Options.MonitorClipboard;
    EditLanguage.Text := Options.Language;
    // Download options
    CheckAutoDownload.Checked := Options.AutoStartDownloads;
    CheckAutoTryHtmlParser.Checked := Options.AutoTryHtmlParser;
    CheckSubtitlesEnabled.Checked := Options.SubtitlesEnabled;
    EditDownloadDir.Text := Options.DestinationPath;
    ComboOverwriteMode.ItemIndex := OverwriteMode[Options.OverwriteMode];
    {$IFDEF CONVERTERS}
    PrepareConverterComboBox(ComboConverter, Options, Options.SelectedConverterID);
    fConverterIndex := ComboConverter.ItemIndex;
    {$ELSE}
    LabelConverter.Visible := False;
    ComboConverter.Visible := False;
    {$ENDIF}
    // Network
    CheckUseProxy.Checked := Options.ProxyActive;
    EditProxyHost.Text := Options.ProxyHost;
    EditProxyPort.Text := Options.ProxyPort;
    EditProxyUser.Text := Options.ProxyUser;
    EditProxyPass.Text := Options.ProxyPassword;
  finally
    fLoading := False;
    end;
end;

procedure TFormOptions.actOKExecute(Sender: TObject);
const OverwriteMode: array[0..3] of TOverwriteMode = (omAsk, omAlways, omNever, omRename);
{$IFDEF CONVERTERS}
var NewID: string;
{$ENDIF}
begin
  // Main options
  Options.PortableMode := CheckPortableMode.Checked;
  Options.CheckForNewVersionOnStartup := CheckCheckNewVersions.Checked;
  Options.Language := EditLanguage.Text;
  Options.MonitorClipboard := CheckMonitorClipboard.Checked;
  // Download options
  Options.AutoStartDownloads := CheckAutoDownload.Checked;
  Options.AutoTryHtmlParser := CheckAutoTryHtmlParser.Checked;
  Options.SubtitlesEnabled := CheckSubtitlesEnabled.Checked;
  Options.DestinationPath := EditDownloadDir.Text;
  Options.OverwriteMode := OverwriteMode[ComboOverwriteMode.ItemIndex];
  {$IFDEF CONVERTERS}
  if Options.ConvertersActivated then
    if DecodeConverterComboBox(ComboConverter, Options, NewID) then
      Options.SelectedConverterID := NewID;
  {$ENDIF}
  // Network
  Options.ProxyActive := CheckUseProxy.Checked;
  Options.ProxyHost := EditProxyHost.Text;
  Options.ProxyPort := EditProxyPort.Text;
  Options.ProxyUser := EditProxyUser.Text;
  Options.ProxyPassword := EditProxyPass.Text;
end;

procedure TFormOptions.actDownloadDirExecute(Sender: TObject);
var Dir: string;
begin
  Dir := EditDownloadDir.Text;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    EditDownloadDir.Text := Dir;
end;

procedure TFormOptions.ComboConverterChange(Sender: TObject);
begin
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  if not fLoading then
    if not Options.ConvertersActivated then
      begin
      MessageDlg(_(CONVERTERS_INACTIVE_WARNING), mtError, [mbOK], 0);
      ComboConverter.ItemIndex := fConverterIndex;
      Abort;
      end;
  {$ENDIF}
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  {$IFDEF GETTEXT}
  TranslateProperties(self);
  {$ENDIF}
  PageOptions.ActivePageIndex := 0;
end;

procedure TFormOptions.actDesktopShortcutExecute(Sender: TObject);
begin
  CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_DESKTOPDIRECTORY);
end;

procedure TFormOptions.actStartMenuShortcutExecute(Sender: TObject);
begin
  CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_PROGRAMS);
end;

end.
