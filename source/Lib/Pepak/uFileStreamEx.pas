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

unit uFileStreamEx;
{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, Classes, Windows, 
  {$IFDEF FPC} RTLConsts, {$ELSE} {$IFDEF DELPHI7_UP} RTLConsts, {$ELSE} Consts, {$ENDIF} {$ENDIF}
  uCompatibility;

type
  TFileStreamEx = class(THandleStream)
  private
    FHandle: THandle;
    function GetPosition64: int64;
    function GetSize64: int64;
    procedure SetPosition64(const Value: int64);
    procedure SetSize64(const Value: int64); {$IFDEF FPC} reintroduce; {$ENDIF}
  protected
    function GetLastErrorMsg(LastError: DWORD): string; overload;
    function GetLastErrorMsg: string; overload;
  public
    constructor Create(const FileName: WideString; Mode: Word; CreateNewFile: boolean = False);
      // Works similar to TFileStream.Create, except that it always uses the Unicode
      // version of CreateFile and has two ways of creating new file: By using
      // Mode=fmCreate, and by setting CreateNewFile to True; the latter allows
      // for creating of files in shareable mode.
    destructor Destroy; override;
    property Position64: int64 read GetPosition64 write SetPosition64;
      // 64bit version of Position
    property Size64: int64 read GetSize64 write SetSize64;
      // 64bit version of Size
    function Seek64(Offset: int64; Origin: integer): int64;
      // 64bit version of Seek
    function Eof: boolean;
  end;

implementation

{ TFileStreamEx }

constructor TFileStreamEx.Create(const FileName: WideString; Mode: Word; CreateNewFile: boolean);
const
  AccessMode: array[0..2] of DWORD = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of DWORD = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  dwDesiredAccess, dwShareMode, dwCreationDistribution: DWORD;
begin
  if Mode = fmCreate then
    begin
    dwDesiredAccess := GENERIC_READ or GENERIC_WRITE;
    dwShareMode := 0;
    dwCreationDistribution := CREATE_ALWAYS;
    end
  else
    begin
    dwShareMode := ShareMode[(Mode and $f0) shr 4];
    if CreateNewFile then
      begin
      dwDesiredAccess := GENERIC_READ or GENERIC_WRITE;
      dwCreationDistribution := CREATE_ALWAYS;
      end
    else
      begin
      dwDesiredAccess := AccessMode[Mode and $03];
      dwCreationDistribution := OPEN_EXISTING;
      end;
    end;
  FHandle := CreateFileW(PWideChar(FileName), dwDesiredAccess, dwShareMode, nil, dwCreationDistribution, FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    if dwCreationDistribution = CREATE_ALWAYS then
      raise EFCreateError.CreateFmt(SFCreateError + ' (%s)', [FileName, GetLastErrorMsg])
    else
      raise EFOpenError.CreateFmt(SFOpenError + ' (%s)', [FileName, GetLastErrorMsg])
  else
    inherited Create(FHandle);
end;

destructor TFileStreamEx.Destroy;
begin
  if (FHandle <> 0) and (FHandle <> INVALID_HANDLE_VALUE) then
    begin
    CloseHandle(FHandle);
    FHandle := 0;
    end;
  inherited;
end;

function TFileStreamEx.Eof: boolean;
begin
  Result := Position64 >= Size64;
end;

function TFileStreamEx.GetLastErrorMsg(LastError: DWORD): string;
var
  n: DWORD;
  Buffer: array[0..32768] of Char;
begin
  n := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, Buffer, Length(Buffer), nil);
  if n = 0 then
    Result := Format('%d: Unspecified error', [LastError])
  else
    begin
    Buffer[n] := #0;
    Result := Trim(Format('%d: %s', [LastError, string(Buffer)]));
    end;
end;

function TFileStreamEx.GetLastErrorMsg: string;
begin
  Result := GetLastErrorMsg(GetLastError);
end;

function TFileStreamEx.GetPosition64: int64;
begin
  Result := Seek64(0, soFromCurrent);
end;

function TFileStreamEx.GetSize64: int64;
const
  FILE_DEVICE_DISK                = $00000007;
  FILE_READ_ACCESS    = $0001;    // for files and pipes
  METHOD_BUFFERED             = 0;
  IOCTL_DISK_BASE = FILE_DEVICE_DISK;
  IOCTL_DISK_GET_LENGTH_INFO =
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or ($0017 shl 2) or (METHOD_BUFFERED);
var
  LowDWORD, HighDWORD: DWORD;
  BytesReturned, LastError: DWORD;
  Buffer: int64;
begin
  if GetFileType(FHandle) = FILE_TYPE_DISK then
    if DeviceIoControl(FHandle, IOCTL_DISK_GET_LENGTH_INFO, nil, 0, @Buffer, Sizeof(Buffer), BytesReturned, nil) then
      Result := Buffer
    else
      Result := -1
  else
    begin
    LowDWORD := GetFileSize(FHandle, @HighDWORD);
    LastError := GetLastError;
    if (LastError = NO_ERROR) or (LowDWORD <> INVALID_FILE_SIZE) then
      Result := (Int64(HighDWORD) shl 32) or Int64(LowDWORD)
    else
      Result := -1;
    end;
end;

function TFileStreamEx.Seek64(Offset: int64; Origin: integer): int64;
var
  LowDWORD, HighDWORD: DWORD;
  LastError: DWORD;
begin
  LowDWORD := Offset and $ffffffff;
  HighDWORD := Offset shr 32;
  LowDWORD := SetFilePointer(FHandle, LowDWORD, @HighDWORD, Origin);
  LastError := GetLastError;
  if (LastError = NO_ERROR) or (LowDWORD <> INVALID_SET_FILE_POINTER) then
    Result := (Int64(HighDWORD) shl 32) or Int64(LowDWORD)
  else
    Result := -1;
end;

procedure TFileStreamEx.SetPosition64(const Value: int64);
begin
  Seek64(Value, soFromBeginning);
end;

procedure TFileStreamEx.SetSize64(const Value: int64);
begin
  Seek64(Value, soFromBeginning);
  {$IFDEF DELPHI7_UP}
    {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  Win32Check(SetEndOfFile(FHandle));
  {$IFDEF DELPHI7_UP}
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
end;

end.
