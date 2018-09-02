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

unit guiOptionsWINAPI_YouTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uDownloader, uOptions, guiOptionsWINAPI_Downloader, guiOptionsWINAPI_CommonDownloader;

type
  TFrameDownloaderOptionsPage_YouTube = class(TFrameDownloaderOptionsPageCommon)
    protected
      function SubFormClass: TFrameDownloaderOptionsPageClass; override;
    public
      constructor Create(AOwner: TApiForm); override;
    end;

  TFrameDownloaderOptionsPageSpec_YouTube = class(TFrameDownloaderOptionsPage)
    protected
      function DoInitDialog: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      EditPreferredLanguages: THandle;
      EditMaxWidth: THandle;
      EditMaxHeight: THandle;
      CheckAvoidWebM: THandle;
      procedure CreateObjects; override;
      procedure DestroyObjects; override;
      procedure LabelPreferredLanguagesClick;
      procedure LabelMaxWidthClick;
      procedure LabelMaxHeightClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

uses
  downYouTube;

const
  IDC_LABEL_PREFERREDLANGUAGES = 1001;
  IDC_EDIT_PREFERREDLANGUAGES = 1002;
  IDC_LABEL_MAXWIDTH = 1003;
  IDC_EDIT_MAXWIDTH = 1004;
  IDC_LABEL_MAXHEIGHT = 1005;
  IDC_EDIT_MAXHEIGHT = 1006;
  IDC_CHECK_AVOIDWEBM = 1007;

{ TFrameDownloaderOptionsPage_YouTube }

constructor TFrameDownloaderOptionsPage_YouTube.Create(AOwner: TApiForm);
begin
  Create(AOwner, TFrameDownloaderOptionsPageCommon.ClassName);
end;

function TFrameDownloaderOptionsPage_YouTube.SubFormClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPageSpec_YouTube;
end;

{ TFrameDownloaderOptionsPageSpec_YouTube }

constructor TFrameDownloaderOptionsPageSpec_YouTube.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPageSpec_YouTube.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.CreateObjects;
begin
  inherited;
  EditPreferredLanguages := GetDlgItem(Self.Handle, IDC_EDIT_PREFERREDLANGUAGES);
  EditMaxWidth := GetDlgItem(Self.Handle, IDC_EDIT_MAXWIDTH);
  EditMaxHeight := GetDlgItem(Self.Handle, IDC_EDIT_MAXHEIGHT);
  CheckAvoidWebM := GetDlgItem(Self.Handle, IDC_CHECK_AVOIDWEBM);
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.DestroyObjects;
begin
  EditPreferredLanguages := 0;
  EditMaxWidth := 0;
  EditMaxHeight := 0;
  CheckAvoidWebM := 0;
  inherited;
end;

function TFrameDownloaderOptionsPageSpec_YouTube.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  {$IFNDEF SUBTITLES}
  EnableWindow(GetDlgItem(Self.Handle, IDC_LABEL_PREFERREDLANGUAGES), False);
  EnableWindow(GetDlgItem(Self.Handle, IDC_EDIT_PREFERREDLANGUAGES), False);
  {$ENDIF}
  SetControlAnchors(EditPreferredLanguages, [akLeft, akTop, akRight]);
  SetControlAnchors(CheckAvoidWebM, [akLeft, akTop, akRight]);
end;

function TFrameDownloaderOptionsPageSpec_YouTube.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        {$IFDEF SUBTITLES}
        IDC_LABEL_PREFERREDLANGUAGES:
          begin
          LabelPreferredLanguagesClick;
          Result := True;
          end;
        {$ENDIF}
        IDC_LABEL_MAXWIDTH:
          begin
          LabelMaxWidthClick;
          Result := True;
          end;
        IDC_LABEL_MAXHEIGHT:
          begin
          LabelMaxHeightClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
begin
  inherited;
  {$IFDEF SUBTITLES}
  SetWindowText(EditPreferredLanguages, PChar(Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT)));
  //if Supports(dfSubtitles, [IDC_LABEL_PREFERREDLANGUAGES, IDC_EDIT_PREFERREDLANGUAGES]) then
  //  ;
  {$ENDIF}
  SetWindowText(EditMaxWidth, PChar(IntToStr(Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT))));
  SetWindowText(EditMaxHeight, PChar(IntToStr(Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT))));
  CheckDlgButton(Self.Handle, IDC_CHECK_AVOIDWEBM, CheckboxConsts[Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_AVOIDWEBM, OPTION_YOUTUBE_AVOIDWEBM_DEFAULT)]);
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.SaveToOptions;
begin
  inherited;
  {$IFDEF SUBTITLES}
  if IsWindowEnabled(EditPreferredLanguages) then
    Options.WriteProviderOption(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, GetWindowTextAsString(EditPreferredLanguages));
  {$ENDIF}
  Options.WriteProviderOption(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, StrToIntDef(GetWindowTextAsString(EditMaxWidth), 0));
  Options.WriteProviderOption(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, StrToIntDef(GetWindowTextAsString(EditMaxHeight), 0));
  case IsDlgButtonChecked(Self.Handle, IDC_CHECK_AVOIDWEBM) of
    BST_CHECKED:
      Options.WriteProviderOption(Provider, OPTION_YOUTUBE_AVOIDWEBM, True);
    BST_UNCHECKED:
      Options.WriteProviderOption(Provider, OPTION_YOUTUBE_AVOIDWEBM, False);
    end;
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.LabelPreferredLanguagesClick;
begin
  {$IFDEF SUBTITLES}
  SetFocus(EditPreferredLanguages);
  {$ENDIF}
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.LabelMaxWidthClick;
begin
  SetFocus(EditMaxWidth);
end;

procedure TFrameDownloaderOptionsPageSpec_YouTube.LabelMaxHeightClick;
begin
  SetFocus(EditMaxHeight);
end;

end.
