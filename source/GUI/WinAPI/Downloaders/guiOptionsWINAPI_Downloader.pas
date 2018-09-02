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

unit guiOptionsWINAPI_Downloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics, uApiTabControl,
  uOptions;

type
  TFrameDownloaderOptionsPage = class(TApiForm)
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
    private
      fOptions: TYTDOptions;
      fProvider: string;
    protected
      procedure CreateObjects; virtual;
      procedure DestroyObjects; virtual;
      function GetProvider: string; virtual;
      procedure SetProvider(const Value: string); virtual;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; virtual;
      procedure SaveToOptions; virtual;
      property Options: TYTDOptions read fOptions write fOptions;
      property Provider: string read GetProvider write SetProvider;
    end;

  TFrameDownloaderOptionsPageClass = class of TFrameDownloaderOptionsPage;

implementation

uses
  uCommonDownloader;

{ TFrameDownloaderOptionsPage }

constructor TFrameDownloaderOptionsPage.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptionsPage.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptionsPage.CreateObjects;
begin
end;

procedure TFrameDownloaderOptionsPage.DestroyObjects;
begin
end;

function TFrameDownloaderOptionsPage.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
end;

function TFrameDownloaderOptionsPage.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFrameDownloaderOptionsPage.GetProvider: string;
begin
  Result := fProvider;
end;

procedure TFrameDownloaderOptionsPage.SetProvider(const Value: string);
begin
  fProvider := Value;
end;

procedure TFrameDownloaderOptionsPage.LoadFromOptions;
begin
end;

procedure TFrameDownloaderOptionsPage.SaveToOptions;
begin
end;

end.
