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

unit uApiFunctions;

interface
{$INCLUDE 'uApi.inc'}

uses
  SysUtils, Classes, Windows, CommCtrl,
  uApiCommon;

//----- Generic Windows functions ----------------------------------------------
type
  EApiError = class(Exception);

procedure ShowApiError(IsError: boolean; const Description: string = ''); overload;
procedure ShowApiError(LastError: DWORD; const Description: string = ''); overload;
function GetWindowTextAsString(hwnd: HWND): string;
function MakePoints(lParam: LPARAM): TPoint;

//----- Clipboard --------------------------------------------------------------
function GetClipboardAsText(Owner: THandle; out Text: string): boolean; overload;
function GetClipboardAsText(Owner: THandle): string; overload;
function SetClipboardAsText(Owner: THandle; const Text: string): THandle;

//----- Listview ---------------------------------------------------------------
function ListViewInsertColumn(ListView: THandle; Index, Subitem: integer; Alignment: TAlignment; Width: integer; const Title: string): integer;
function ListViewGetItemCount(ListView: THandle): integer;
function ListViewIsItemSelected(ListView: THandle; Index: integer): boolean;
function ListViewSelectItem(ListView: THandle; Index: integer; Selected: boolean): boolean;
function ListViewGetSelectedItems(ListView: THandle; out Indexes: TList; MaxCount: integer = 0): boolean;
function ListViewGetSelectedItem(ListView: THandle): integer;
function ListViewSetVirtualItemText(DispInfo: PLVDispInfo; const Text: string): boolean;
function ListViewGetColumnWidth(ListView: THandle; Index: integer): integer;
function ListViewSetColumnWidth(ListView: THandle; Index: integer; Width: integer): boolean;

//----- Toolbar ----------------------------------------------------------------
procedure ToolbarButtonSetEnabled(Toolbar: THandle; Button: WPARAM; Enabled: boolean);

//------------------------------------------------------------------------------

implementation

resourcestring
  WINDOWS_ERROR = 'Windows error %u = %08.8x'#13#10'%s'; // error code, error code, error message
  WINDOWS_ERROR_UNKNOWN = 'Unknown error %u = %08.8x.'; // error code, error code

procedure ShowApiError(IsError: boolean; const Description: string);
begin
  if IsError then
    ShowApiError(GetLastError, Description);
end;

procedure ShowApiError(LastError: DWORD; const Description: string);
var Buf: array[0..32768] of char;
    n: DWORD;
    Msg: string;
begin
  if LastError <> NO_ERROR then
    begin
    n := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, Buf, Sizeof(Buf), nil);
    if n = 0 then
      Msg := Format(WINDOWS_ERROR_UNKNOWN, [LastError, LastError])
    else
      begin
      Buf[n] := #0;
      Msg := Format(WINDOWS_ERROR, [LastError, LastError, string(Buf)]);
      end;
    if Description <> '' then
      Msg := Description + #13#10 + Msg;
    Raise EApiError.Create(Msg);
    end;
end;

function GetWindowTextAsString(hwnd: HWND): string;
var Buf: array of char;
    n: integer;
begin
  Result := '';
  n := GetWindowTextLength(hwnd);
  if n > 0 then
    begin
    Inc(n);
    SetLength(Buf, n);
    n := GetWindowText(hwnd, PChar(Buf), n);
    if n > 0 then
      SetString(Result, PChar(Buf), n);
    end;
end;

function MakePoints(lParam: LPARAM): TPoint;
begin
  Result.x := lParam and $ffff;
  Result.y := lParam shr 16;
end;

const CLIPBOARD_TEXT_FORMAT = {$IFDEF UNICODE} CF_UNICODETEXT {$ELSE} CF_TEXT {$ENDIF} ;

function GetClipboardAsText(Owner: THandle; out Text: string): boolean;
var Data: THandle;
    P: PChar;
begin
  Result := False;
  Text := '';
  if IsClipboardFormatAvailable(CLIPBOARD_TEXT_FORMAT) then
    if OpenClipboard(Owner) then
      try
        Data := GetClipboardData(CLIPBOARD_TEXT_FORMAT);
        if Data <> 0 then
          begin
          P := GlobalLock(Data);
          if P <> nil then
            try
              Text := string(P);
              Result := True;
            finally
              GlobalUnlock(Data);
              end;
          end;
      finally
        CloseClipboard;
        end;
end;

function GetClipboardAsText(Owner: THandle): string;
begin
  if not GetClipboardAsText(Owner, Result) then
    Result := '';
end;

function SetClipboardAsText(Owner: THandle; const Text: string): THandle;
var Data: HGLOBAL;
    DataPtr: Pointer;
    s: string;
begin
  Result := 0;
  if OpenClipboard(Owner) then
    try
      EmptyClipboard;
      s := Text + #0;
      Data := GlobalAlloc(GMEM_MOVEABLE, Length(s) * sizeof(Char));
      if Data <> 0 then
        begin
        DataPtr := GlobalLock(Data);
        try
          Move(s[1], DataPtr^, Length(s) * sizeof(Char));
        finally
          GlobalUnlock(Data);
          end;
        Result := SetClipboardData(CLIPBOARD_TEXT_FORMAT, Data);
        end;
    finally
      CloseClipboard;
      end;
end;

function ListViewInsertColumn(ListView: THandle; Index, Subitem: integer; Alignment: TAlignment; Width: integer; const Title: string): integer;
const Alignments: array[TAlignment] of integer = (LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_CENTER);
var Column: LV_COLUMN;
begin
  Column.mask := LVCF_FMT or LVCF_TEXT;
  if Width > 0 then
    Column.mask := Column.mask or LVCF_WIDTH;
  if Subitem > 0 then
    Column.mask := Column.mask or LVCF_SUBITEM;
  Column.fmt := Alignments[Alignment];
  Column.cx := Width;
  Column.pszText := PChar(Title);
  Column.cchTextMax := 0;
  Result := SendMessage(ListView, LVM_INSERTCOLUMN, Index, integer(@Column));
end;

function ListViewGetItemCount(ListView: THandle): integer;
begin
  Result := SendMessage(ListView, LVM_GETITEMCOUNT, 0, 0);
end;

function ListViewIsItemSelected(ListView: THandle; Index: integer): boolean;
begin
  Result := Longbool(SendMessage(ListView, LVM_GETITEMSTATE, Index, LVIS_SELECTED) and LVIS_SELECTED);
end;

function ListViewSelectItem(ListView: THandle; Index: integer; Selected: boolean): boolean;
const SelectedFlag: array[boolean] of DWORD = (0, LVIS_SELECTED);
var Item: LV_ITEM;
begin
  Result := False;
  Item.state := SelectedFlag[Selected];
  Item.stateMask := LVIS_SELECTED;
  if SendMessage(ListView, LVM_SETITEMSTATE, Index, LPARAM(@Item)) <> 0 then
    Result := True;
end;

function ListViewGetSelectedItems(ListView: THandle; out Indexes: TList; MaxCount: integer): boolean;
var i, n: integer;
begin
  Result := False;
  Indexes := nil;
  n := ListViewGetItemCount(ListView);
  if n > 0 then
    for i := 0 to Pred(n) do
      if ListViewIsItemSelected(ListView, i) then
        begin
        if not Result then
          begin
          Indexes := TList.Create;
          Result := True;
          end;
        Indexes.Add(Pointer(i));
        if MaxCount > 0 then
          if Indexes.Count >= MaxCount then
            Break;
        end;
end;

function ListViewGetSelectedItem(ListView: THandle): integer;
begin
  Result := SendMessage(ListView, LVM_GETNEXTITEM, -1, LVNI_FOCUSED or LVNI_SELECTED);
end;

const LISTVIEW_TEXT_BUFFER_SIZE = 16;
      LISTVIEW_TEXT_BUFFER_ITEMSIZE = 260;
type TListViewTextBufferItem = array[0..LISTVIEW_TEXT_BUFFER_ITEMSIZE-1] of Char;
var ListViewTextBuffer: array[0..LISTVIEW_TEXT_BUFFER_SIZE-1] of TListViewTextBufferItem;
    ListViewTextBufferIndex: integer = 0;
    
function ListViewSetVirtualItemText(DispInfo: PLVDispInfo; const Text: string): boolean;
begin
  Result := Longbool(DispInfo^.item.mask and LVIF_TEXT) and (DispInfo^.item.pszText <> nil) and (DispInfo^.item.cchTextMax > 0);
  if not Result then
    begin
    DispInfo^.item.mask := DispInfo^.item.mask or LVIF_TEXT;
    DispInfo^.item.pszText := @ListViewTextBuffer[ListViewTextBufferIndex, 0];
    DispInfo^.item.cchTextMax := LISTVIEW_TEXT_BUFFER_ITEMSIZE;
    ListViewTextBufferIndex := Succ(ListViewTextBufferIndex) mod LISTVIEW_TEXT_BUFFER_SIZE;
    end;
  StrPLCopy(DispInfo^.item.pszText, Text, DispInfo^.item.cchTextMax-1);
end;

function ListViewGetColumnWidth(ListView: THandle; Index: integer): integer;
begin
  Result := SendMessage(ListView, LVM_GETCOLUMNWIDTH, Index, 0);
end;

function ListViewSetColumnWidth(ListView: THandle; Index: integer; Width: integer): boolean;
begin
  if Width > 0 then
    Result := SendMessage(ListView, LVM_SETCOLUMNWIDTH, Index, Width) <> 0
  else
    Result := False;
end;

procedure ToolbarButtonSetEnabled(Toolbar: THandle; Button: WPARAM; Enabled: boolean);
var Info: TTBButtonInfo;
begin
  FillChar(Info, Sizeof(Info), 0);
  Info.cbSize := Sizeof(Info);
  Info.dwMask := TBIF_STATE;
  if SendMessage(Toolbar, TB_GETBUTTONINFO, Button, LPARAM(@Info)) >= 0 then
    begin
    if Enabled then
      Info.fsState := Info.fsState or TBSTATE_ENABLED
    else
      Info.fsState := Info.fsState and (not TBSTATE_ENABLED);
    SendMessage(Toolbar, TB_SETBUTTONINFO, Button, LPARAM(@Info));
    end;
end;

end.
