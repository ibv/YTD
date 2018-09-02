(******************************************************************************

______________________________________________________________________________

libPepak                                                     (c) 2009-11 Pepak
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

unit uDialogs;

interface
{$INCLUDE 'uApi.inc'}

type
  TSelectDirOpt = (sdAllowCreate, sdPerformCreate, sdPrompt);
  TSelectDirOpts = set of TSelectDirOpt;

function InputQuery(const Caption, Prompt: string; var Value: string): boolean;
function InputBox(const Caption, Prompt, Default: string): string;
function SelectDirectory(var Directory: string; Options: TSelectDirOpts; HelpCtx: Longint): boolean;
function SaveDialog(var FileName: string; const InitialDir: string = ''; const Title: string = ''): boolean;
function OpenDialog(var FileName: string; const InitialDir: string = ''; const Title: string = ''; const Filter: string = ''): boolean;

implementation

{$RESOURCE uDialogs.res}

uses
  SysUtils, Windows, Messages, ShlObj, ActiveX, CommDlg,
  uApiFunctions, uApiForm;

const
  INPUTQUERY_LABEL_PROMPT = 1006;
  INPUTQUERY_EDIT_VALUE = 1007;
  INPUTQUERY_BUTTON_OK = 1008;
  INPUTQUERY_BUTTON_CANCEL = 1009;

  INPUTQUERY_ACTION_OK = 40000;
  INPUTQUERY_ACTION_CANCEL = 40001;

type
  TInputQueryForm = class(TApiForm)
    private
    protected
      function DoInitDialog: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
      function DoClose: boolean; override;
    public
      Caption: string;
      Prompt: string;
      Value: string;
    end;

function TInputQueryForm.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  {$IFDEF APIFORM_TRANSLATE}
  Self.Translate;
  {$ENDIF}
  SetWindowText(Self.Handle, PChar(Caption));
  SetDlgItemText(Self.Handle, INPUTQUERY_LABEL_PROMPT, PChar(Prompt));
  SetDlgItemText(Self.Handle, INPUTQUERY_EDIT_VALUE, PChar(Value));
  ShowApiError(SetFocus(GetDlgItem(Self.Handle, INPUTQUERY_EDIT_VALUE)) = 0);
  {$IFDEF APIFORM_ACCELERATORS}
  Accelerators := LoadAccelerators(hInstance, 'DIALOG_INPUTQUERY_ACCELERATORS');
  {$ENDIF}
  SetModalResult(0);
end;

function TInputQueryForm.DoClose: boolean;
begin
  Value := GetControlText(INPUTQUERY_EDIT_VALUE);
  Result := inherited DoClose;
end;

function TInputQueryForm.DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1:
      case Identifier of
        INPUTQUERY_ACTION_OK:
          Result := Close(idOK);
        INPUTQUERY_ACTION_CANCEL:
          Result := Close(idCancel);
        end;
    BN_CLICKED:
      case Identifier of
        INPUTQUERY_BUTTON_OK:
          Result := Close(idOK);
        INPUTQUERY_BUTTON_CANCEL:
          Result := Close(idCancel);
        end;
    end;
end;

function InputQuery(const Caption, Prompt: string; var Value: string): boolean;
var F: TInputQueryForm;
begin
  F := TInputQueryForm.Create;
  try
    F.Caption := Caption;
    F.Prompt := Prompt;
    F.Value := Value;
    Result := F.ShowModal = idOK;
    if Result then
      Value := F.Value;
  finally
    F.Free;
    end;
end;

function InputBox(const Caption, Prompt, Default: string): string;
begin
  Result := '';
  if not InputQuery(Caption, Prompt, Result) then
    Result := Default;
end;

resourcestring
  SSelectDirectory = 'Select a directory';
  
function SelectDirectory(var Directory: string; Options: TSelectDirOpts; HelpCtx: Longint): Boolean;
var Info: TBrowseInfo;
    Path: array[0..MAX_PATH+1] of Char;
    Item: PItemIDList;
begin
  Result := False;
  CoInitialize(nil);
  try
    Path[0] := #0;
    Info.hwndOwner := 0;
    Info.pidlRoot := nil;
    Info.pszDisplayName := nil;
    Info.lpszTitle := PChar(SSelectDirectory);
    Info.ulFlags := 0;
    Info.lpfn := nil;
    Info.lParam := 0;
    Info.iImage := 0;
//    Info.ulFlags := BIF_RETURNONLYFSDIRS ;//+ BIF_EDITBOX + BIF_VALIDATE + $40 {BIF_NEWDIALOGSTYLE};
//    if not (sdAllowCreate in Options) then
//      Info.ulFlags := Info.ulFlags + $200 {BIF_NONEWFOLDERBUTTON};
    Item := {$IFDEF FPC} SHBrowseForFolder(@Info) {$ELSE} SHBrowseForFolder(Info) {$ENDIF};
    if (Item <> nil) then
      try
        if SHGetPathFromIDList(Item, @Path[0]) then
          begin
          Directory := Path;
          Result := True;
          end;
      finally
        CoTaskMemFree(Item);
        end;
  finally
    CoUninitialize;
    end;
end;

function SaveDialog(var FileName: string; const InitialDir, Title: string): boolean;
var OpenFile: TOpenFilename;
    FileNameBuf: array of char;
    FileNameBufSize: DWORD;
begin
  FileNameBufSize := Succ(Length(FileName));
  if FileNameBufSize < MAX_PATH then
    FileNameBufSize := Succ(MAX_PATH);
  SetLength(FileNameBuf, FileNameBufSize);
  StrPCopy(PChar(FileNameBuf), FileName);
  FillChar(OpenFile, Sizeof(OpenFile), 0);
  OpenFile.lStructSize := Sizeof(OpenFile);
  OpenFile.lpstrFile := PChar(FileNameBuf);
  OpenFile.nMaxFile := FileNameBufSize;
  if InitialDir <> '' then
    OpenFile.lpstrInitialDir := PChar(InitialDir);
  if Title <> '' then
    OpenFile.lpstrTitle := PChar(Title);
  OpenFile.Flags := OFN_ENABLESIZING or OFN_EXPLORER or OFN_NOCHANGEDIR or OFN_NOREADONLYRETURN or OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST;
  OpenFile.lpstrDefExt := PChar(Copy(ExtractFileExt(FileName), 2, MaxInt));
  Result := GetSaveFileName( {$IFDEF FPC} @OpenFile {$ELSE} OpenFile {$ENDIF} );
  if Result then
    FileName := OpenFile.lpstrFile;
end;

function OpenDialog(var FileName: string; const InitialDir, Title, Filter: string): boolean;
var OpenFile: TOpenFilename;
    FileNameBuf: array of char;
    FileNameBufSize: DWORD;
begin
  FileNameBufSize := Succ(Length(FileName));
  if FileNameBufSize < MAX_PATH then
    FileNameBufSize := Succ(MAX_PATH);
  SetLength(FileNameBuf, FileNameBufSize);
  StrPCopy(PChar(FileNameBuf), FileName);
  FillChar(OpenFile, Sizeof(OpenFile), 0);
  OpenFile.lStructSize := Sizeof(OpenFile);
  OpenFile.lpstrFile := PChar(FileNameBuf);
  OpenFile.nMaxFile := FileNameBufSize;
  if InitialDir <> '' then
    OpenFile.lpstrInitialDir := PChar(InitialDir);
  if Title <> '' then
    OpenFile.lpstrTitle := PChar(Title);
  if Filter <> '' then
    OpenFile.lpstrFilter := PChar(StringReplace(Filter, '|', #0, [rfReplaceAll]) + #0);
  OpenFile.Flags := OFN_ENABLESIZING or OFN_EXPLORER or OFN_NOCHANGEDIR or OFN_NOREADONLYRETURN or OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST;
  OpenFile.lpstrDefExt := PChar(Copy(ExtractFileExt(FileName), 2, MaxInt));
  Result := GetOpenFileName( {$IFDEF FPC} @OpenFile {$ELSE} OpenFile {$ENDIF} );
  if Result then
    FileName := OpenFile.lpstrFile;
end;

end.
