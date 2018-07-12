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

unit uGUID;
{$INCLUDE 'pepak.inc'}

{$DEFINE DYNAMIC}

interface

{$IFDEF DYNAMIC}
uses
  Windows;
{$ENDIF}

type
  RPC_STATUS = integer;

const
  RPC_S_OK = 0;

{$IFDEF DYNAMIC}

  type
    TUuidCreate = function (var Uuid: TGUID): RPC_STATUS; stdcall;
    TUuidToString = function (Uuid: PGUID; out StringUuid: PChar): RPC_STATUS; stdcall;

  var
    UuidCreate: TUuidCreate;
    UuidToString: TUuidToString;

{$ELSE}

  {$EXTERNALSYM UuidCreate}
  function UuidCreate(var Uuid: TGUID): RPC_STATUS; stdcall;
  {$EXTERNALSYM UuidToString}
  function UuidToString(Uuid: PGUID; out StringUuid: PChar): RPC_STATUS; stdcall;

{$ENDIF}

function GenerateUuid: string;

implementation

const
  LibraryFile = 'rpcrt4.dll';
  UuidCreateName = 'UuidCreate';
  UuidToStringName = {$IFDEF UNICODE} 'UuidToStringW' {$ELSE} 'UuidToStringA' {$ENDIF} ;

{$IFDEF DYNAMIC}

  var LibraryHandle: THandle;

{$ELSE}

  function UuidCreate; external LibraryFile name UuidCreateName;
  function UuidToString; external LibraryFile name UuidToStringName;

{$ENDIF}

function GenerateUuid: string;
var GUID: TGUID;
    P: PChar;
begin
  Result := '';
  if UuidCreate(GUID) = RPC_S_OK then
    if UuidToString(@GUID, P) = RPC_S_OK then
      Result := P;
end;

initialization
{$IFDEF DYNAMIC}
  UuidCreate := nil;
  UuidToString := nil;
  LibraryHandle := LoadLibrary(LibraryFile);
  if LibraryHandle <> 0 then
    begin
    @UuidCreate := GetProcAddress(LibraryHandle, UuidCreateName);
    @UuidToString := GetProcAddress(LibraryHandle, UuidToStringName);
    end;
{$ENDIF}

finalization
{$IFDEF DYNAMIC}
  @UuidCreate := nil;
  @UuidToString := nil;
  if LibraryHandle <> 0 then
    begin
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
    end;
{$ENDIF}

end.
