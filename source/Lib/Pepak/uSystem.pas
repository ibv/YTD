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

unit uSystem;
{$INCLUDE 'jedi.inc'}

interface

uses
  SysUtils, Classes, Windows;

{$IFDEF WIN32}
function Is64BitWindows: boolean;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
{$ENDIF}

implementation

{$IFDEF WIN32}

type
  TIsWow64ProcessFn = function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  TWow64DisableWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;

var
  Kernel32Dll: THandle = 0;

var
  IsWow64ProcessFn: TIsWow64ProcessFn = nil;
  Wow64DisableWow64FsRedirectionFn: TWow64DisableWow64FsRedirectionFn = nil;
  Wow64RevertWow64FsRedirectionFn: TWow64RevertWow64FsRedirectionFn = nil;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
begin
  if not Assigned(IsWow64ProcessFn) then
    Result := False
  else
    Result := IsWow64ProcessFn(hProcess, Wow64Process);
end;

function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64DisableWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64DisableWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64RevertWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64RevertWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Is64BitWindows: boolean;
var B: BOOL;
begin
  if IsWow64Process(GetCurrentProcess, B) then
    Result := B
  else
    Result := False;
end;

{$ENDIF}

initialization
  {$IFDEF WIN32}
  Kernel32Dll := LoadLibrary('kernel32.dll');
  if Kernel32Dll = 0 then
    begin
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end
  else
    begin
    IsWow64ProcessFn := GetProcAddress(Kernel32Dll, 'IsWow64Process');
    Wow64DisableWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64DisableWow64FsRedirection');
    Wow64RevertWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64RevertWow64FsRedirection');
    end;
  {$ENDIF}

finalization
  {$IFDEF WIN32}
  if Kernel32Dll <> 0 then
    begin
    FreeLibrary(Kernel32Dll);
    Kernel32Dll := 0;
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end;
  {$ENDIF}

end.
