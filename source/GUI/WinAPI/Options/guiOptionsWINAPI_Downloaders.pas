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

unit guiOptionsWINAPI_Downloaders;
{$INCLUDE 'ytd.inc'}
{$DEFINE TODO}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, guiConsts, uDialogs,
  uDownloadClassifier, uDownloader,
  guiOptionsWINAPI,
  guiDownloaderOptions, guiOptionsWINAPI_Downloader, guiOptionsWINAPI_CommonDownloader;

type
  TFrameDownloaderOptions = class(TFrameOptions)
    protected
    private
      ListPages: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
      procedure CreateDownloaderOptions;
      procedure DestroyDownloaderOptions;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    private
      fCurrentPageIndex: integer;
    protected
      procedure ListPagesChanged;
      function PageCount: integer;
      function Pages(Index: integer): TFrameDownloaderOptionsPage;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

// from resource.h
const
  IDC_LIST_PAGES = 1001;

{ TFrameDownloaderOptions }

constructor TFrameDownloaderOptions.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloaderOptions.Destroy;
begin
  inherited;
end;

procedure TFrameDownloaderOptions.CreateObjects;
begin
  ListPages := GetDlgItem(Self.Handle, IDC_LIST_PAGES);
  CreateDownloaderOptions;
end;

procedure TFrameDownloaderOptions.DestroyObjects;
begin
  DestroyDownloaderOptions;
  ListPages := 0;
end;

procedure TFrameDownloaderOptions.CreateDownloaderOptions;
var
  i, Index, Left, Top, Width, Height: integer;
  DC: TDownloadClassifier;
  FrameClass: TFrameDownloaderOptionsPageClass;
  Frame: TFrameDownloaderOptionsPage;
  FormRect, ListRect: TRect;
begin
  DestroyDownloaderOptions;
  DC := TDownloadClassifier.Create;
  try
    for i := 0 to Pred(DC.ProviderCount) do
      if DC.Providers[i] <> nil then
        begin
        FrameClass := DC.Providers[i].GuiOptionsClass;
        if FrameClass = nil then
          if (DC.Providers[i].Features <> []) and (DC.Providers[i].Features <> [dfDummy]) then
            FrameClass := TFrameDownloaderOptionsPageCommon;
        if FrameClass <> nil then
          begin
          Frame := FrameClass.Create(Self);
          try
            if Frame is TFrameDownloaderOptionsPageCommon then
              TFrameDownloaderOptionsPageCommon(Frame).DownloaderClass := DC.Providers[i];
            Frame.Provider := DC.Providers[i].Provider;
            Frame.Options := Self.Options;
            Frame.Show;
            if GetClientRect(Self.Handle, FormRect) then
              if GetClientRect(ListPages, ListRect) then
                begin
                Left := ListRect.Right + 4;
                Top := ListRect.Top;
                Width := (FormRect.Right - FormRect.Left) - Left;
                Height := (FormRect.Bottom - FormRect.Top) - Top;
                MoveWindow(Frame.Handle, Left, Top, Width, Height, False);
                SetControlAnchors(Frame.Handle, [akLeft, akTop, akRight, akBottom]);
                end;
            ShowWindow(Frame.Handle, SW_HIDE);
            Index := SendMessage(ListPages, LB_ADDSTRING, 0, LPARAM(PChar(DC.Providers[i].Provider)));
            if Index < 0 then
              FreeAndNil(Frame)
            else if SendMessage(ListPages, LB_SETITEMDATA, Index, LPARAM(Frame)) = LB_ERR then
              FreeAndNil(Frame);
          except
            FreeAndNil(Frame);
            Raise;
            end;
          end;
        end;
    if PageCount > 0 then
      begin
      SendMessage(ListPages, LB_SETCURSEL, 0, 0);
      ListPagesChanged;
      end
    else
      begin
      {$IFNDEF TODO}
      'TODO: How to hide this window from the pageControl?'
      {$ENDIF}
      end;
  finally
    FreeAndNil(DC);
    end;
end;

procedure TFrameDownloaderOptions.DestroyDownloaderOptions;
var
  i: integer;
begin
  for i := Pred(PageCount) downto 0 do
    begin
    TObject(Pages(i)).Free;
    SendMessage(ListPages, LB_DELETESTRING, i, 0);
    end;
  fCurrentPageIndex := -1;
end;

function TFrameDownloaderOptions.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
  // Make sure everything can be resized easily
  SetControlAnchors(ListPages, [akTop, akLeft, akBottom]);
end;

function TFrameDownloaderOptions.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFrameDownloaderOptions.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1 {, LBN_SELCHANGE}: // Accelerators
      if WindowHandle = ListPages then
        begin
        ListPagesChanged;
        Result := True;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloaderOptions.LoadFromOptions;
var
  i: integer;
  Page: TFrameDownloaderOptionsPage;
begin
  for i := 0 to Pred(PageCount) do
    begin
    Page := Pages(i);
    if Page <> nil then
      begin
      Page.Options := Options;
      Page.LoadFromOptions;
      end;
    end;
end;

procedure TFrameDownloaderOptions.SaveToOptions;
var
  i: integer;
  Page: TFrameDownloaderOptionsPage;
begin
  for i := 0 to Pred(PageCount) do
    begin
    Page := Pages(i);
    if Page <> nil then
      begin
      Page.Options := Options;
      Page.SaveToOptions;
      end;
    end;
end;

procedure TFrameDownloaderOptions.ListPagesChanged;
var
  Index: LRESULT;
  Page: TFrameDownloaderOptionsPage;
begin
  if (fCurrentPageIndex >= 0) and (fCurrentPageIndex < PageCount) then
    begin
    Page := Pages(fCurrentPageIndex);
    if Page <> nil then
      ShowWindow(Page.Handle, SW_HIDE);
    end;
  Index := SendMessage(ListPages, LB_GETCURSEL, 0, 0);
  if (Index >= 0) and (Index < PageCount) then
    begin
    Page := Pages(Index);
    if Page <> nil then
      ShowWindow(Page.Handle, SW_SHOWNORMAL);
    fCurrentPageIndex := Index;
    end
  else
    fCurrentPageIndex := -1;
end;

function TFrameDownloaderOptions.PageCount: integer;
begin
  Result := SendMessage(ListPages, LB_GETCOUNT, 0, 0);
end;

function TFrameDownloaderOptions.Pages(Index: integer): TFrameDownloaderOptionsPage;
var
  Res: LRESULT;
begin
  if (Index < 0) or (Index >= PageCount) then
    Result := nil
  else
    begin
    Res := SendMessage(ListPages, LB_GETITEMDATA, Index, 0);
    if Res = LB_ERR then
      Result := nil
    else
      Result := TObject(Res) as TFrameDownloaderOptionsPage;
    end;
end;

initialization
   InitCommonControls;

end.
