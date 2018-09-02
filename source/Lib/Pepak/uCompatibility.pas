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
  SysUtils, Classes, Windows;

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
  INVALID_SET_FILE_POINTER = DWORD(-1);

const
  CSIDL_PROGRAM_FILES = 38;

const
  SEE_MASK_NOZONECHECKS = $00800000;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
{$IFNDEF FPC}
type
  NativeInt = {$IFDEF WIN64} int64 {$ELSE} Longint {$ENDIF} ;
  NativeUInt = {$IFDEF WIN64} uint64 {$ELSE} Longword {$ENDIF} ;
{$ENDIF}
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
