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

unit uCompatibility;
{$INCLUDE 'jedi.inc'}

interface

uses
  SysUtils, Classes, Windows, ShellApi;

{$IFNDEF DELPHI7_UP}
type
  EOSError = EWin32Error;

type
  UInt64 = int64; // unsigned not available in Delphi 5
  TByteArray = array[0..65535] of byte; 

function IncludeTrailingPathDelimiter(const Path: string): string;
function ExcludeTrailingPathDelimiter(const Path: string): string;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
type
  TSysCharSet = set of Char;

type
  Utf8String = AnsiString;
  RawByteString = AnsiString;

type
  TSeekOrigin = Word;

const
  soBeginning = 0;
  soCurrent = 1;
  soEnd = 2;

const
  FILE_ATTRIBUTE_REPARSE_POINT = $400;

const
  LVS_EX_DOUBLEBUFFER     = $00010000;
  LVS_EX_LABELTIP         = $00004000; { listview unfolds partly hidden labels if it does not have infotip text }

const
  TBSTYLE_EX_MIXEDBUTTONS       = $00000008;
  TBSTYLE_EX_HIDECLIPPEDBUTTONS = $00000010;
  TBSTYLE_EX_DOUBLEBUFFER       = $00000080;

const
  MB_ERR_INVALID_CHARS = 8;

const
  HoursPerDay = 24;
  MinsPerHour = 60;
  MinsPerDay = HoursPerDay * MinsPerHour;

function CharInSet(C: Char; S: TSysCharSet): boolean;

function StartsText(const SubStr, Str: string): boolean;
{$ENDIF}

const
  WM_XBUTTONDOWN          = $020B;
  WM_XBUTTONDBLCLK        = $020D;

const
  XBUTTON1                = 1;
  XBUTTON2                = 2;

{$IFNDEF DELPHI2010_UP}
const
  CSIDL_PROGRAM_FILES = 38;

const
  SEE_MASK_NOZONECHECKS = $00800000;

const
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11D0-958A-006097C9A090}';

type
  HIMAGELIST = THandle;
  
type
  ITaskbarList = interface(IUnknown)
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;

type
  THUMBBUTTON = record 
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: packed array[0..259] of WCHAR;
    dwFlags: DWORD;
  end;
  tagTHUMBBUTTON = THUMBBUTTON;
  TThumbButton = THUMBBUTTON;
  PThumbButton = ^TThumbButton;

const
  // THUMBBUTTON flags
  THBF_ENABLED        =  $0000;
  THBF_DISABLED       =  $0001;
  THBF_DISMISSONCLICK =  $0002;
  THBF_NOBACKGROUND   =  $0004;
  THBF_HIDDEN         =  $0008;
  THBF_NONINTERACTIVE =  $0010;
  // THUMBBUTTON mask
  THB_BITMAP          =  $0001;
  THB_ICON            =  $0002;
  THB_TOOLTIP         =  $0004;
  THB_FLAGS           =  $0008;
  THBN_CLICKED        =  $1800;

const
  TBPF_NOPROGRESS    = 0; 
  TBPF_INDETERMINATE = $1;
  TBPF_NORMAL        = $2;
  TBPF_ERROR         = $4;
  TBPF_PAUSED        = $8;

  TBATF_USEMDITHUMBNAIL   = $1; 
  TBATF_USEMDILIVEPREVIEW = $2;

type
  ITaskbarList3 = interface(ITaskbarList2) 
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function SetProgressValue(hwnd: HWND; ullCompleted, ullTotal: UInt64): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: Integer): HRESULT; stdcall;
    function RegisterTab(hwndTab, hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab, hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab, hwndMDI: HWND; tbatFlags: Integer): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; var prcClip: TRect): HRESULT; stdcall;
  end;

type
  STPFLAG = Integer;

const
  STPF_NONE                      = 0;
  STPF_USEAPPTHUMBNAILALWAYS     = $1;
  STPF_USEAPPTHUMBNAILWHENACTIVE = $2;
  STPF_USEAPPPEEKALWAYS          = $4;
  STPF_USEAPPPEEKWHENACTIVE      = $8;

type
  ITaskbarList4 = interface(ITaskbarList3)
    ['{C43DC798-95D1-4BEA-9030-BB99E2983A1A}']
    function SetTabProperties(hwndTab: HWND; stpFlags: STPFLAG): HRESULT; stdcall;
  end;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
{$IFNDEF FPC}
type
  NativeInt = {$IFDEF WIN64} int64 {$ELSE} Longint {$ENDIF} ;
  NativeUInt = {$IFDEF WIN64} uint64 {$ELSE} Longword {$ENDIF} ;
{$ENDIF}
{$ENDIF}

{$IFNDEF DELPHIXE5_UP}
const
  REG_QWORD = 11;
{$ENDIF}

{$IFNDEF DELPHIXE5_UP}
const
  INVALID_SET_FILE_POINTER = $ffffffff;
  INVALID_FILE_ATTRIBUTES = $ffffffff;
{$ENDIF}

{$IFDEF FPC}
const
  INVALID_FILE_SIZE = $ffffffff;
{$ENDIF}

implementation

{$IFNDEF DELPHI7_UP}
function IncludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := IncludeTrailingBackslash(Path);
end;

function ExcludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeTrailingBackslash(Path);
end;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
function CharInSet(C: Char; S: TSysCharSet): boolean;
begin
  Result := C in S;
end;

function StartsText(const SubStr, Str: string): boolean;
begin
  Result := AnsiCompareText(SubStr, Copy(Str, 1, Length(SubStr))) = 0;
end;
{$ENDIF}

end.
