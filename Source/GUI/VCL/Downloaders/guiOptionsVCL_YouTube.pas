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

unit guiOptionsVCL_YouTube;
{$INCLUDE 'ytd.inc'}

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  uDownloader, guiOptionsVCL_Downloader, guiOptionsVCL_CommonDownloader;

type
  TFrameDownloaderOptionsPage_YouTube = class(TFrameDownloaderOptionsPageCommon)
    LabelPreferredLanguages: TLabel;
    EditPreferredLanguages: TEdit;
    LabelMaximumVideoWidth: TLabel;
    EditMaximumVideoWidth: TEdit;
    LabelMaximumVideoHeight: TLabel;
    EditMaximumVideoHeight: TEdit;
    CheckAvoidWebM: TCheckBox;
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromOptions; override;
    procedure SaveToOptions; override;
  end;

implementation

{$R *.DFM}

uses
  downYouTube;

{ TFrameDownloaderOptionsPage_YouTube }

constructor TFrameDownloaderOptionsPage_YouTube.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF SUBTITLES}
  EditPreferredLanguages.Enabled := False;
  {$ENDIF}
end;

destructor TFrameDownloaderOptionsPage_YouTube.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPage_YouTube.LoadFromOptions;
begin
  inherited;
  {$IFDEF SUBTITLES}
  EditPreferredLanguages.Text := Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, OPTION_YOUTUBE_PREFERREDLANGUAGES_DEFAULT);
  if Supports(dfSubtitles, [EditPreferredLanguages, LabelPreferredLanguages]) then
    ;
  {$ENDIF}
  EditMaximumVideoWidth.Text := IntToStr(Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, OPTION_YOUTUBE_MAXVIDEOWIDTH_DEFAULT));
  EditMaximumVideoHeight.Text := IntToStr(Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, OPTION_YOUTUBE_MAXVIDEOHEIGHT_DEFAULT));
  CheckAvoidWebM.Checked := Options.ReadProviderOptionDef(Provider, OPTION_YOUTUBE_AVOIDWEBM, OPTION_YOUTUBE_AVOIDWEBM_DEFAULT);
end;

procedure TFrameDownloaderOptionsPage_YouTube.SaveToOptions;
begin
  inherited;
  {$IFDEF SUBTITLES}
  if EditPreferredLanguages.Enabled then
    Options.WriteProviderOption(Provider, OPTION_YOUTUBE_PREFERREDLANGUAGES, EditPreferredLanguages.Text);
  {$ENDIF}
  Options.WriteProviderOption(Provider, OPTION_YOUTUBE_MAXVIDEOWIDTH, StrToIntDef(EditMaximumVideoWidth.Text, 0));
  Options.WriteProviderOption(Provider, OPTION_YOUTUBE_MAXVIDEOHEIGHT, StrToIntDef(EditMaximumVideoHeight.Text, 0));
  Options.WriteProviderOption(Provider, OPTION_YOUTUBE_AVOIDWEBM, CheckAvoidWebM.Checked);
end;

end.
