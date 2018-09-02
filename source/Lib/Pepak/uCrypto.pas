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

unit uCrypto;
{$INCLUDE 'pepak.inc'}

interface

uses
  Windows;

// Nebo s pouzitim Crypto API:
// http://stackoverflow.com/questions/7985744/simple-aes-encryption-using-winapi

function AES_Encrypt_ECB(Input, Output, Key: PAnsiChar; Bits: integer): boolean;
function AES_Decrypt_ECB(Input, Output, Key: PAnsiChar; Bits: integer): boolean;
function AES_Decrypt_CTR(const Counter, Input: AnsiString; out Output: AnsiString; Key: PAnsiChar; Bits: integer): boolean;
function AESCTR_Decrypt(const Data, Password: AnsiString; KeyBits: integer): AnsiString;

implementation

const
  LIBEAY32_NAME = 'libeay32.dll';

const
  AES_MAXNR = 14;

type
  AES_KEY = record
    rd_key: array[0..Pred(4*Succ(AES_MAXNR))] of Longint;
    rounds: integer;
    end;

type
  TAESEncryptFunction = procedure(Input, Output: PAnsiChar; var Key: AES_KEY); cdecl;
  TAESSetEncryptKeyFunction = function(UserKey: PAnsiChar; Bits: integer; var Key: AES_KEY): integer; cdecl;
  TAESDecryptFunction = procedure(Input, Output: PAnsiChar; var Key: AES_KEY); cdecl;
  TAESSetDecryptKeyFunction = function(UserKey: PAnsiChar; Bits: integer; var Key: AES_KEY): integer; cdecl;

var
  LibEay32Lock: TRtlCriticalSection;
  LibEay32Handle: THandle = 0;
  AES_encrypt: TAESEncryptFunction = nil;
  AES_set_encrypt_key: TAESSetEncryptKeyFunction = nil;
  AES_decrypt: TAESDecryptFunction = nil;
  AES_set_decrypt_key: TAESSetDecryptKeyFunction = nil;

procedure Init;
begin
  if LibEay32Handle = 0 then
    begin
    EnterCriticalSection(LibEay32Lock);
    try
      if LibEay32Handle = 0 then
        begin
        LibEay32Handle := LoadLibrary(LIBEAY32_NAME);
        if LibEay32Handle <> 0 then
          begin
          AES_encrypt := GetProcAddress(LibEay32Handle, 'AES_encrypt');
          AES_set_encrypt_key := GetProcAddress(LibEay32Handle, 'AES_set_encrypt_key');
          AES_decrypt := GetProcAddress(LibEay32Handle, 'AES_decrypt');
          AES_set_decrypt_key := GetProcAddress(LibEay32Handle, 'AES_set_decrypt_key');
          end;
        end;
    finally
      LeaveCriticalSection(LibEay32Lock);
      end;
    end;
end;

procedure Done;
begin
  EnterCriticalSection(LibEay32Lock);
  try
    if LibEay32Handle <> 0 then
      begin
      FreeLibrary(LibEay32Handle);
      LibEay32Handle := 0;
      AES_encrypt := nil;
      AES_set_encrypt_key := nil;
      AES_decrypt := nil;
      AES_set_decrypt_key := nil;
      end;
  finally
    LeaveCriticalSection(LibEay32Lock);
    end;
end;

function AES_Encrypt_ECB(Input, Output, Key: PAnsiChar; Bits: integer): boolean;
var
  AesKey: AES_KEY;
begin
  Result := False;
  Init;
  if Assigned(AES_set_encrypt_key) and Assigned(AES_encrypt) then
    begin
    AES_set_encrypt_key(Key, Bits, AesKey);
    AES_encrypt(Input, Output, AesKey);
    Result := True;
    end;
end;

function AES_Decrypt_ECB(Input, Output, Key: PAnsiChar; Bits: integer): boolean;
var
  AesKey: AES_KEY;
begin
  Result := False;
  Init;
  if Assigned(AES_set_decrypt_key) and Assigned(AES_decrypt) then
    begin
    AES_set_decrypt_key(Key, Bits, AesKey);
    AES_decrypt(Input, Output, AesKey);
    Result := True;
    end;
end;

function AES_Decrypt_CTR(const Counter, Input: AnsiString; out Output: AnsiString; Key: PAnsiChar; Bits: integer): boolean;
type
  TBlock = array[0..15] of Byte;
  TCounter = array[0..3] of LongWord;
var
  AesKey: AES_KEY;
  Ctr, Decrypted, Encrypted: TBlock;
  i, Position, CtrLength, InputLength: integer;
begin
  Result := False;
  Init;
  if Assigned(AES_set_encrypt_key) and Assigned(AES_encrypt) then
    begin
    AES_set_encrypt_key(Key, Bits, AesKey);
    CtrLength := Length(Counter);
    for i := 0 to High(Ctr) do
      if i >= CtrLength then
        Ctr[i] := 0
      else
        Ctr[i] := Ord(Counter[Succ(i)]);
    Output := Input;
    InputLength := Length(Input);
    Position := 0;
    while Position < InputLength do
      begin
      Move(Ctr[0], Decrypted[0], Sizeof(TBlock));
      AES_encrypt(@Decrypted[0], @Encrypted[0], AesKey);
      for i := 0 to Pred(Sizeof(TBlock)) do
        if Position < InputLength then
          begin
          Inc(Position);
          Output[Position] := AnsiChar(Byte(Input[Position]) xor Encrypted[i]); 
          end
        else
          Break;
      for i := Pred(Length(Ctr)) downto 8 do
        begin
        Inc(Ctr[i]);
        if Ctr[i] <> 0 then
          Break;
        end;
      end;
    Result := True;
    end
end;

function AESCTR_Decrypt(const Data, Password: AnsiString; KeyBits: integer): AnsiString;
const
  BLOCK_LENGTH_BITS = 128;
  BLOCK_LENGTH_BYTES = BLOCK_LENGTH_BITS shr 3;
type
  TBlock = array[0..BLOCK_LENGTH_BYTES-1] of byte;
var
  Key: array of Byte;
  EncBlock: TBlock;
  Decrypted: AnsiString;
  i, pwLength, KeyBytes: integer;
begin
  Result := '';
  // 1. Get the actual encryption key from the password
  KeyBytes := KeyBits shr 3;
  SetLength(Key, KeyBytes);
  pwLength := Length(Password);
  for i := 0 to Pred(KeyBytes) do
    if i >= pwLength then
      Key[i] := 0
    else
      Key[i] := Ord(Password[Succ(i)]);
  if not AES_Encrypt_ECB(@Key[0], @EncBlock[0], @Key[0], KeyBits) then
    Exit;
  for i := 0 to Pred(KeyBytes) do
    Key[i] := EncBlock[i mod BLOCK_LENGTH_BYTES];
  // 2. Decrypt URL in counter mode
  if not AES_Decrypt_CTR(Copy(Data, 1, 8), Copy(Data, 9, MaxInt), Decrypted, @Key[0], KeyBits) then
    Exit;
  Result := Decrypted;
end;

initialization
  InitializeCriticalSection(LibEay32Lock);

finalization
  Done;

  DeleteCriticalSection(LibEay32Lock);

end.
