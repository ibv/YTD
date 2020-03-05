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

unit guiAboutLCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  LCLIntf, LCLType, LMessages,

  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, HttpSend,  ComCtrls,
  {$IFDEF DELPHIXE4_UP}
  UITypes,
  {$ENDIF}
  uLanguages, uMessages, uFunctions, uDownloadClassifier, uDownloader, uOptions,
  uUpgrade,
  guiFunctions ;

const
  WM_FIRSTSHOW = WM_USER + 1;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    LabelYTD: TLabel;
    LabelVersionLabel: TLabel;
    LabelVersion: TLabel;
    LabelNewestVersionLabel: TLabel;
    LabelNewestVersion: TLabel;
    LabelHomepageLabel: TLabel;
    LabelHomepage: TLabel;
    LabelMediaProviders: TLabel;
    ListProviders: TListView;
    LabelDefsVersion: TLabel;
    LabelNewestDefsVersion: TLabel;
    procedure LabelNewestVersionClick(Sender: TObject);
    procedure LabelHomepageClick(Sender: TObject);
    procedure ListProvidersData(Sender: TObject; Item: TListItem);
    procedure LabelNewestDefsVersionClick(Sender: TObject);
  private
    fFirstShow: boolean;
    fDownloadClassifier: TDownloadClassifier;
    fOptions: TYTDOptions;
    fUpgrade: TYTDUpgrade;
    procedure WMFirstShow(var Msg: TMessage); message WM_FIRSTSHOW;
  protected
    procedure DoShow; override;
    procedure DoFirstShow; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    procedure SetUrlStyle(ALabel: TLabel); virtual;
    procedure LoadProviders; virtual;
    procedure NewYTDEvent(Sender: TYTDUpgrade); virtual;
    procedure NewDefsEvent(Sender: TYTDUpgrade); virtual;
    property Upgrade: TYTDUpgrade read fUpgrade;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Options: TYTDOptions read fOptions write fOptions;
    property DownloadClassifier: TDownloadClassifier read fDownloadClassifier write fDownloadClassifier;
  end;

implementation

{$R *.dfm}

uses
  uScriptedDownloader;

{ TFormAbout }

constructor TFormAbout.Create(AOwner: TComponent);
begin
  inherited;
  TranslateProperties(self);
  fFirstShow := True;
  ListProviders.ScrollBars := ssVertical;
end;

destructor TFormAbout.Destroy;
begin
  FreeAndNil(fUpgrade);
  inherited;
end;

procedure TFormAbout.DoShow;
begin
  inherited;
  if fFirstShow then
    begin
    fFirstShow := False;
    PostMessage(Handle, WM_FIRSTSHOW, 0, 0);
    end;
end;

procedure TFormAbout.WMFirstShow(var Msg: TMessage);
begin
  {$IFNDEF FPC}
  DoFirstShow;
  {$ENDIF}
end;

procedure TFormAbout.DoFirstShow;
{$IFNDEF THREADEDVERSION}
var Version, Url: string;
{$ENDIF}
begin
  {$IFDEF FPC}
  inherited;
  {$ENDIF}
  // Show current version
  LabelVersion.Caption := APPLICATION_VERSION;
  if TScriptedDownloader.MainScriptEngine <> nil then
    LabelDefsVersion.Caption := TScriptedDownloader.MainScriptEngine.Version;
  // Homepage
  SetUrlStyle(LabelHomepage);
  // Providers
  LoadProviders;
  // Show available version
  LabelNewestVersion.Caption := {$IFDEF THREADEDVERSION} _('checking...') {$ELSE} _('not found') {$ENDIF} ; // GUI: Check for a new version wasn't made yet - or failed.
  LabelNewestDefsVersion.Caption := {$IFDEF THREADEDVERSION} _('checking...') {$ELSE} _('not found') {$ENDIF} ; // GUI: Check for a new version wasn't made yet - or failed.
  Application.ProcessMessages;
  if Options <> nil then
    begin
    FreeAndNil(fUpgrade);
    fUpgrade := TYTDUpgrade.Create(Options);
    fUpgrade.OnNewYTDFound := NewYTDEvent;
    fUpgrade.OnNewDefsFound := NewDefsEvent;
    fUpgrade.TestUpgrades( {$IFDEF THREADEDVERSION} True {$ELSE} False {$ENDIF} );
    end;
end;

{
procedure TFormAbout.NewVersionEvent(Sender: TObject; const Version, Url: string);
begin
end;
}

procedure TFormAbout.NewYTDEvent(Sender: TYTDUpgrade);
begin
  if Sender.OnlineYTDVersion = '' then
    begin
    LabelNewestVersion.Caption := _('not found');
    LabelNewestVersion.Font.Color := clRed;
    end
  else
    begin
    LabelNewestVersion.Caption := Sender.OnlineYTDVersion;
    LabelNewestVersion.Font.Color := clBlack;
    if Sender.CompareVersions(APPLICATION_VERSION, Sender.OnlineYTDVersion) < 0 then
      begin
      SetUrlStyle(LabelNewestVersion);
      LabelNewestDefsVersion.Visible := False;
      end;
    end;
end;

procedure TFormAbout.NewDefsEvent(Sender: TYTDUpgrade);
begin
  if Sender.OnlineDefsVersion = '' then
    begin
    LabelNewestDefsVersion.Caption := _('not found');
    LabelNewestDefsVersion.Font.Color := clRed;
    end
  else
    begin
    LabelNewestDefsVersion.Caption := Sender.OnlineDefsVersion;
    LabelNewestDefsVersion.Font.Color := clBlack;
    if TScriptedDownloader.MainScriptEngine <> nil then
      if Sender.CompareVersions(TScriptedDownloader.MainScriptEngine.Version, Sender.OnlineDefsVersion) < 0 then
        begin
        SetUrlStyle(LabelNewestDefsVersion);
        end;
    end;
end;

procedure TFormAbout.SetUrlStyle(ALabel: TLabel);
begin
  ALabel.Font.Color := clBlue;
  ALabel.Font.Style := LabelNewestVersion.Font.Style + [fsUnderline];
  ALabel.Cursor := crHandPoint;
end;

procedure TFormAbout.LabelNewestVersionClick(Sender: TObject);
begin
  if fUpgrade <> nil then
    guiFunctions.UpgradeYTD(fUpgrade, Handle);
end;

procedure TFormAbout.LabelNewestDefsVersionClick(Sender: TObject);
begin
  if fUpgrade <> nil then
    if guiFunctions.UpgradeDefs(fUpgrade, Handle, True) then
      begin
      LabelNewestDefsVersion.Font.Assign(LabelDefsVersion.Font);
      LabelNewestDefsVersion.Cursor := LabelDefsVersion.Cursor;
      LabelNewestDefsVersion.Caption := fUpgrade.OnlineDefsVersion;
      Invalidate;
      end;
end;




procedure TFormAbout.LabelHomepageClick(Sender: TObject);
begin
  Run((Sender as TLabel).Caption, Handle);
end;


procedure TFormAbout.LoadProviders;
{$IFDEF FPC}
var i: integer;
{$ENDIF}
begin
  ListProviders.Items.BeginUpdate;
  {$IFDEF FPC}
  ListProviders.Items.Clear;
  if DownloadClassifier <> nil then
    for i := 0 to Pred(DownloadClassifier.NameCount) do
      ListProviders.Items.Add;
  {$ELSE}
  if DownloadClassifier = nil then
    ListProviders.Items.Count := 0
  else
    ListProviders.Items.Count := DownloadClassifier.NameCount;
  {$ENDIF}
  ListProviders.Items.EndUpdate;
  ListProviders.Items.Count := DownloadClassifier.NameCount;
end;


procedure TFormAbout.ListProvidersData(Sender: TObject; Item: TListItem);
begin
  if DownloadClassifier <> nil then
    begin
    Item.Caption := DownloadClassifier.Names[Item.Index];
    Item.SubItems.Clear;
    //Item.SubItems.Add(DownloadClassifier.Names[Item.Index]);
    Item.SubItems.Add(DownloadClassifier.NameClasses[Item.Index]);
    end;

end;


end.
