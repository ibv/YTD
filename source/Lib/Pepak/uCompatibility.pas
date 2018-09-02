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

unit uCompatibility;
{$INCLUDE 'jedi.inc'}

interface

{$IFNDEF DELPHI2009_UP}
uses
  SysUtils;

type
  TSysCharSet = set of Char;

type
  EOSError = EWin32Error;

type
  Utf8String = AnsiString;

type
  TSeekOrigin = Word;

const
  soBeginning = 0;
  soCurrent = 1;
  soEnd = 2;

const
  FILE_ATTRIBUTE_REPARSE_POINT = $400;

  LVS_EX_DOUBLEBUFFER     = $00010000;
  LVS_EX_LABELTIP         = $00004000; { listview unfolds partly hidden labels if it does not have infotip text }

  TBSTYLE_EX_MIXEDBUTTONS       = $00000008;
  TBSTYLE_EX_HIDECLIPPEDBUTTONS = $00000010;
  TBSTYLE_EX_DOUBLEBUFFER       = $00000080;

const
  HoursPerDay = 24;
  MinsPerHour = 60;
  MinsPerDay = HoursPerDay * MinsPerHour;

function CharInSet(C: Char; S: TSysCharSet): boolean;

function IncludeTrailingPathDelimiter(const Path: string): string;
function ExcludeTrailingPathDelimiter(const Path: string): string;
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
const
  WM_XBUTTONDOWN          = $020B;
  WM_XBUTTONDBLCLK        = $020D;

  XBUTTON1                = 1;
  XBUTTON2                = 2;
{$ENDIF}

implementation

{$IFNDEF DELPHI2009_UP}
function CharInSet(C: Char; S: TSysCharSet): boolean;
begin
  Result := C in S;
end;

function IncludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := IncludeTrailingBackslash(Path);
end;

function ExcludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeTrailingBackslash(Path);
end;
{$ENDIF}

end.
