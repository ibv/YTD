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

unit guiOptionsWINAPI_CommonDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uDownloader, uOptions, guiOptionsWINAPI_Downloader;

type
  TFrameDownloaderOptionsPageCommon = class(TFrameDownloaderOptionsPage)
    protected
      function DoInitDialog: boolean; override;
    private
      EmptyAreaRect: TRect;
      SubForm: TFrameDownloaderOptionsPage;
    private
      fDownloaderClass: TDownloaderClass;
    private
    protected
      procedure CreateObjects; override;
      procedure DestroyObjects; override;
      function GetProvider: string; override;
      function Supports(Feature: TDownloaderFeature): boolean; overload;
      function Supports(Feature: TDownloaderFeature; const ControlIDs: array of integer): boolean; overload;
      function SubFormClass: TFrameDownloaderOptionsPageClass; virtual;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
      property DownloaderClass: TDownloaderClass read fDownloaderClass write fDownloaderClass;
    end;

implementation

{$RESOURCE *.res}

uses
  uCommonDownloader;

// from resource.h
const
  IDC_LABEL_EMPTYAREA = 1001;
  IDC_CHECKBOX_SUBTITLESENABLED = 1002;
  IDC_CHECKBOX_CONVERTSUBTITLES = 1003;

{ TFrameDownloaderOptionsPageCommon }

constructor TFrameDownloaderOptionsPageCommon.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPageCommon.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPageCommon.CreateObjects;
var
  ControlPlacement: TWindowPlacement;
begin
  ShowWindow(GetDlgItem(Self.Handle, IDC_LABEL_EMPTYAREA), SW_HIDE);
  FillChar(ControlPlacement, Sizeof(ControlPlacement), 0);
  ControlPlacement.length := Sizeof(ControlPlacement);
  if GetWindowPlacement(GetDlgItem(Self.Handle, IDC_LABEL_EMPTYAREA), @ControlPlacement) then
    EmptyAreaRect := ControlPlacement.rcNormalPosition
  else
    FillChar(EmptyAreaRect, Sizeof(EmptyAreaRect), 0);
  if SubFormClass <> nil then
    begin
    SubForm := SubFormClass.Create(Self);
    try
      if SubForm is TFrameDownloaderOptionsPageCommon then
        TFrameDownloaderOptionsPageCommon(SubForm).DownloaderClass := Self.DownloaderClass;
      SubForm.Provider := Self.Provider;
      SubForm.Options := Self.Options;
      SubForm.Show;
      MoveWindow(SubForm.Handle, EmptyAreaRect.Left, EmptyAreaRect.Top, EmptyAreaRect.Right - EmptyAreaRect.Left + 1, EmptyAreaRect.Bottom - EmptyAreaRect.Top + 1, False);
      ShowWindow(SubForm.Handle, SW_SHOW);
      SetControlAnchors(SubForm.Handle, [akLeft, akTop, akRight, akBottom]);
    except
      FreeAndNil(SubForm);
      Raise;
      end;
    end
  else
    SubForm := nil;
end;

procedure TFrameDownloaderOptionsPageCommon.DestroyObjects;
begin
  FreeAndNil(SubForm);
end;

function TFrameDownloaderOptionsPageCommon.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  {$IFNDEF SUBTITLES}
  EnableWindow(GetDlgItem(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED), False);
  EnableWindow(GetDlgItem(Self.Handle, IDC_CHECKBOX_CONVERTSUBTITLES), False);
  {$ENDIF}
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED), [akLeft, akTop, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_CONVERTSUBTITLES), [akLeft, akTop, akRight]);
end;

function TFrameDownloaderOptionsPageCommon.GetProvider: string;
begin
  if DownloaderClass <> nil then
    Result := DownloaderClass.Provider
  else
    Result := inherited GetProvider;
end;

function TFrameDownloaderOptionsPageCommon.Supports(Feature: TDownloaderFeature): boolean;
begin
  Result := False;
  if DownloaderClass <> nil then
    if Feature in DownloaderClass.Features then
      Result := True;
end;

function TFrameDownloaderOptionsPageCommon.Supports(Feature: TDownloaderFeature; const ControlIDs: array of integer): boolean;
var i: integer;
begin
  Result := Supports(Feature);
  if not Result then
    for i := 0 to Pred(Length(ControlIDs)) do
      EnableWindow(GetDlgItem(Self.Handle, ControlIDs[i]), False);
end;

function TFrameDownloaderOptionsPageCommon.SubFormClass: TFrameDownloaderOptionsPageClass;
begin
  Result := nil;
end;

procedure TFrameDownloaderOptionsPageCommon.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
begin
  inherited;
  {$IFDEF SUBTITLES}
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED, CheckboxConsts[Options.ReadProviderOptionDef(Provider, OPTION_COMMONDOWNLOADER_SUBTITLESENABLED, True)]);
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_CONVERTSUBTITLES, CheckboxConsts[Options.ReadProviderOptionDef(Provider, OPTION_COMMONDOWNLOADER_CONVERTSUBTITLES, OPTION_COMMONDOWNLOADER_CONVERTSUBTITLES_DEFAULT)]);
  if Supports(dfSubtitles, [IDC_CHECKBOX_SUBTITLESENABLED, IDC_CHECKBOX_CONVERTSUBTITLES]) then
    if Supports(dfSubtitlesConvert, [IDC_CHECKBOX_CONVERTSUBTITLES]) then
      ;
  {$ENDIF}
  if SubForm <> nil then
    begin
    SubForm.Options := Options;
    SubForm.LoadFromOptions;
    end;
end;

procedure TFrameDownloaderOptionsPageCommon.SaveToOptions;
begin
  inherited;
  {$IFDEF SUBTITLES}
  if IsWindowEnabled(GetDlgItem(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED)) then
    case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED) of
      BST_CHECKED:
        Options.WriteProviderOption(Provider, OPTION_COMMONDOWNLOADER_SUBTITLESENABLED, True);
      BST_UNCHECKED:
        Options.WriteProviderOption(Provider, OPTION_COMMONDOWNLOADER_SUBTITLESENABLED, False);
      end;
  if IsWindowEnabled(GetDlgItem(Self.Handle, IDC_CHECKBOX_CONVERTSUBTITLES)) then
    case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_SUBTITLESENABLED) of
      BST_CHECKED:
        Options.WriteProviderOption(Provider, OPTION_COMMONDOWNLOADER_CONVERTSUBTITLES, True);
      BST_UNCHECKED:
        Options.WriteProviderOption(Provider, OPTION_COMMONDOWNLOADER_CONVERTSUBTITLES, False);
      end;
  {$ENDIF}
  if SubForm <> nil then
    begin
    SubForm.Options := Options;
    SubForm.SaveToOptions;
    end;
end;

end.
