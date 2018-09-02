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

unit uFiles;
{$INCLUDE 'pepak.inc'}

interface
{$DEFINE TEXTSTREAMS}
{$DEFINE TEXTFUNCTIONS}

uses
  SysUtils, Classes, Windows,
  uCompatibility, uStrings;

type
  TFileEncoding = (feAuto, feANSI, feOEM, feUTF16, feUTF8);

function LoadFileIntoMemory(const FileName: string): TMemoryStream;
procedure SaveMemoryToFile(const FileName: string; Data: Pointer; DataLength: integer);
procedure AppendMemoryToFile(const FileName: string; Data: Pointer; DataLength: integer);

{$IFDEF TEXTFUNCTIONS}
function GuessFileEncoding(Data: Pointer; DataLength: Integer; out BomLength: integer): TFileEncoding;
function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): TMemoryStream; overload;
function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding): TMemoryStream; overload;
function LoadFileIntoStringW(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): WideString; overload;
function LoadFileIntoStringW(const FileName: string; Encoding: TFileEncoding): WideString; overload;
function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): string; overload;
function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding): string; overload;
procedure SaveMemoryToFileW(const FileName: string; Data: Pointer; DataLength: integer; Encoding: TFileEncoding; BOM: boolean = False; Append: boolean = False);
procedure SaveStringToFileW(const FileName: string; const Data: WideString; Encoding: TFileEncoding = feAuto; BOM: boolean = False; Append: boolean = False);
procedure SaveStringToFile(const FileName: string; const Data: string; Encoding: TFileEncoding = feAuto; BOM: boolean = False; Append: boolean = False);
{$ENDIF}

function FileGetSize(const FileName: string): int64;
function FileGetDateTime(const FileName: string): TDateTime;

{$IFDEF TEXTSTREAMS}
type
  TStreamSize = Longint;

  TBufferedStream = class(TStream)
    private
      fStream: TStream;
      fOwnsStream: boolean;
      fBuffer: PAnsiChar;
      fBufferSize: integer;
      fBufferPosition: int64;
      fBufferFill: integer;
      fBufferDirty: boolean;
      fPosition: int64;
    protected
      procedure Init; virtual;
      procedure SetSize(NewSize: TStreamSize); override;
      procedure ClearBuffer;
      procedure FillBuffer;
      property Stream: TStream read fStream;
      property OwnsStream: boolean read fOwnsStream;
      property Buffer: PAnsiChar read fBuffer;
      property BufferSize: integer read fBufferSize;
      property BufferPosition: int64 read fBufferPosition;
      property BufferFill: integer read fBufferFill;
      property BufferDirty: boolean read fBufferDirty;
      property Position: int64 read fPosition write fPosition;
    public
      constructor Create(AStream: TStream; ABufferSize: integer; AOwnsStream: boolean = False); overload;
      constructor Create(AStream: TStream; AOwnsStream: boolean = False); overload;
      destructor Destroy; override;
      procedure Flush;
      function Read(var OutBuffer; Count: TStreamSize): TStreamSize; override;
      function Write(const InBuffer; Count: TStreamSize): TStreamSize; override;
      function Seek(Offset: TStreamSize; Origin: Word): TStreamSize; override;
    end;

  TTextStream = class(TBufferedStream)
    private
      fDefaultEncoding: TFileEncoding;
      fEncoding: TFileEncoding;
      fBigEndian: boolean;
      fLineSeparator: string;
      procedure SetEncoding(const Value: TFileEncoding);
    protected
      procedure Init; override;
      procedure GuessEncoding;
      property BigEndian: boolean read fBigEndian;
    public
      class procedure AppendLine(const FileName, Msg: string; const Encoding: TFileEncoding = feAuto);
      constructor Create(AHandle: THandle); overload;
      constructor Create(const AFileName: string; const AMode: Word); overload;
      property Encoding: TFileEncoding read fEncoding write SetEncoding;
      property DefaultEncoding: TFileEncoding read fDefaultEncoding write fDefaultEncoding;
      property LineSeparator: string read fLineSeparator write fLineSeparator;
      function Eof: boolean;
      function ReadChar(out Ch: Char): boolean;
      function ReadLine(out Line: string): boolean;
      procedure WriteStr(const S: string);
      procedure WriteLine(const S: string);
    end;
{$ENDIF}

implementation

{$IFDEF TEXTSTREAMS}
const
  DEFAULT_BUFFER_SIZE = 16384;
  BOM_UTF16: array[0..1] of byte = ($ff, $fe);
  BOM_UTF8: array[0..2] of byte = ($ef, $bb, $bf);
  MAX_SIZE_FOR_AUTODETECT = DEFAULT_BUFFER_SIZE;
{$ENDIF}

function LoadFileIntoMemory(const FileName: string): TMemoryStream;
var F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := TMemoryStream.Create;
    try
      Result.CopyFrom(F, 0);
      Result.Position := 0;
    except
      FreeAndNil(Result);
      Raise;
      end;
  finally
    F.Free;
    end;
end;

procedure SaveMemoryToFile(const FileName: string; Data: Pointer; DataLength: integer);
begin
  with TFileStream.Create(FileName, fmCreate) do
    try
      WriteBuffer(Data^, DataLength);
    finally
      Free;
      end;
end;

procedure AppendMemoryToFile(const FileName: string; Data: Pointer; DataLength: integer);
begin
  with TFileStream.Create(FileName, fmOpenRead or fmOpenWrite) do
    try
      Position := Size;
      WriteBuffer(Data^, DataLength);
    finally
      Free;
      end;
end;

{$IFDEF TEXTFUNCTIONS}

function EncodingToCodePage(Encoding: TFileEncoding): integer;
begin
  case Encoding of
    feAuto:
      Raise EConvertError.Create('Invalid encoding specified.');
    feANSI:
      Result := CP_ACP;
    feOEM:
      Result := CP_OEMCP;
    feUTF8:
      Result := CP_UTF8;
    else
      Result := CP_ACP;
    end;
end;

function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): TMemoryStream;
var Source: TMemoryStream;
    CodePage, Size, NewSize, BomLength: integer;
    Content: WideString;
    P: Pointer;
begin
  Source := LoadFileIntoMemory(FileName);
  try
    if Encoding = feAuto then
      Encoding := GuessFileEncoding(Source.Memory, Source.Size, BomLength);
    FoundEncoding := Encoding;
    if Source.Size = 0 then
      Result := Source
    else if Encoding = feUTF16 then
      if BomLength = 0 then
        Result := Source
      else
        begin
        Result := TMemoryStream.Create;
        try
          Source.Position := BomLength;
          Result.CopyFrom(Source, Source.Size - BomLength);
        except
          FreeAndNil(Result);
          Raise;
          end;
        end
    else
      begin
      P := Pointer(NativeUInt(Source.Memory) + NativeUInt(BomLength));
      CodePage := EncodingToCodePage(Encoding);
      Size := MultiByteToWideChar(CodePage, 0 {MB_ERR_INVALID_CHARS}, P, Source.Size - BomLength, nil, 0);
      if Size = 0 then
        Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
      Inc(Size);
      SetLength(Content, Size);
      NewSize := MultiByteToWideChar(CodePage, 0 {MB_ERR_INVALID_CHARS}, P, Source.Size - BomLength, @Content[1], Size);
      if NewSize = 0 then
        Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
      Result := TMemoryStream.Create;
      try
        Result.WriteBuffer(Content[1], NewSize * Sizeof(WideChar));
        Result.Position := 0;
        FreeAndNil(Source);
      except
        FreeAndNil(Result);
        Raise;
        end;
      end;
  except
    FreeAndNil(Source);
    Raise;
    end;
end;

function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding): TMemoryStream;
var
  FoundEncoding: TFileEncoding;
begin
  Result := LoadFileIntoMemoryW(FileName, Encoding, FoundEncoding);
end;

function LoadFileIntoStringW(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): WideString;
var MS: TMemoryStream;
begin
  MS := LoadFileIntoMemoryW(FileName, Encoding, FoundEncoding);
  if MS = nil then
    Result := ''
  else
    try
      if MS.Size = 0 then
        Result := ''
      else
        begin
        SetLength(Result, MS.Size div Sizeof(WideChar));
        Move(MS.Memory^, Result[1], MS.Size);
        end;
    finally
      FreeAndNil(MS);
      end;
end;

function LoadFileIntoStringW(const FileName: string; Encoding: TFileEncoding): WideString;
var
  FoundEncoding: TFileEncoding;
begin
  Result := LoadFileIntoStringW(FileName, Encoding, FoundEncoding);
end;

function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding; out FoundEncoding: TFileEncoding): string;
begin
  Result := string(LoadFileIntoStringW(FileName, Encoding, FoundEncoding));
end;

function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding): string;
var
  FoundEncoding: TFileEncoding;
begin
  Result := LoadFileIntoString(FileName, Encoding, FoundEncoding);
end;

procedure SaveMemoryToFileW(const FileName: string; Data: Pointer; DataLength: integer; Encoding: TFileEncoding; BOM: boolean; Append: boolean);
var Stream: TMemoryStream;
    CodePage, Size, NewSize: integer;
    SavedOK, IsNewFile: boolean;
begin
  SavedOK := False;
  if Encoding = feAuto then
    Encoding := feUTF8;
  if DataLength > 0 then
    if Encoding <> feUTF16 then
      begin
      CodePage := EncodingToCodePage(Encoding);
      Size := WideCharToMultiByte(CodePage, 0 {MB_ERR_INVALID_CHARS}, Data, DataLength div Sizeof(WideChar), nil, 0, nil, nil);
      if Size = 0 then
        Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
      Stream := TMemoryStream.Create;
      try
        Inc(Size);
        Stream.Size := Size;
        NewSize := WideCharToMultiByte(CodePage, 0 {MB_ERR_INVALID_CHARS}, Data, DataLength div Sizeof(WideChar), Stream.Memory, Size, nil, nil);
        if NewSize = 0 then
          Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
        Stream.Size := NewSize;
        IsNewFile := not FileExists(FileName);
        if BOM and (Encoding in [feUTF16, feUTF8]) and (IsNewFile or (not Append)) then
          begin
          case Encoding of
            feUTF16:
              SaveMemoryToFile(FileName, @BOM_UTF16[0], Length(BOM_UTF16));
            feUtf8:
              SaveMemoryToFile(FileName, @BOM_UTF8[0], Length(BOM_UTF8));
            else
              SaveMemoryToFile(FileName, nil, 0);
            end;
          AppendMemoryToFile(FileName, Stream.Memory, Stream.Size);
          end
        else
          if Append and (not IsNewFile) then
            AppendMemoryToFile(FileName, Stream.Memory, Stream.Size)
          else
            SaveMemoryToFile(FileName, Stream.Memory, Stream.Size);
        SavedOK := True;
      finally
        Stream.Free;
        end;
      end;
  if not SavedOK then
    SaveMemoryToFile(FileName, Data, DataLength)
end;

procedure SaveStringToFileW(const FileName: string; const Data: WideString; Encoding: TFileEncoding; BOM: boolean; Append: boolean);
var
  n: integer;
begin
  n := Length(Data);
  if n > 0 then
    SaveMemoryToFileW(FileName, @Data[1], n*Sizeof(WideChar), Encoding, BOM, Append);
end;

procedure SaveStringToFile(const FileName: string; const Data: string; Encoding: TFileEncoding; BOM: boolean; Append: boolean);
begin
  SaveStringToFileW(FileName, WideString(Data), Encoding, BOM, Append);
end;

function GuessFileEncoding(Data: Pointer; DataLength: Integer; out BomLength: integer): TFileEncoding;

  procedure SwitchByteOrder;
    var i: integer;
        b: byte;
    begin
      i := 0;
      while i < DataLength do
        begin
        b := PByteArray(Data)^[i];
        PByteArray(Data)^[i] := PByteArray(Data)^[Succ(i)];
        PByteArray(Data)^[Succ(i)] := b;
        Inc(i, 2);
        end;
    end;

var i: integer;
    OddZeroCount, EvenZeroCount: integer;
begin
  BomLength := 0;
  // Unicode must have at least two bytes
  if DataLength <= 1 then
    begin
    Result := feANSI;
    Exit;
    end;
  // Check BOM
  if (DataLength >= 2) and ((DataLength mod 2) = 0) then
    if (PWordArray(Data)^[0] = $feff) or (PWordArray(Data)^[0] = $fffe) then
      begin
      Result := feUTF16;
      BomLength := 2;
      // If it is big-endian, convert it to little-endian
      if PWordArray(Data)^[0] = $fffe then
        SwitchByteOrder;
      Exit;
      end;
  if (DataLength >= 3) then
    if (PByteArray(Data)^[0] = $ef) and (PByteArray(Data)^[1] = $bb) and (PByteArray(Data)^[2] = $bf) then
      begin
      Result := feUtf8;
      BomLength := 3;
      Exit;
      end;
  // Try to convert the string as UTF8. If it succeeds, assume UTF8
  if MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, Data, DataLength, nil, 0) <> 0 then
    begin
    Result := feUtf8;
    Exit;
    end;
  // I need to decide between Unicode and ANSI.
  // Assumption: ANSI doesn't use binary zero very often, except maybe separating words or lines
  // or as a filler. Unicode will have binary zero in all 7-bit ASCII characters, which will presumably
  // be present in the text (at least, they will be present in HTML/XML tags).
  OddZeroCount := 0;
  EvenZeroCount := 0;
  for i := 0 to Pred(DataLength) do
    if PByteArray(Data)^[i] = 0 then
      if (i mod 2) = 0 then
        Inc(EvenZeroCount)
      else
        Inc(OddZeroCount);
  if (OddZeroCount > 30) and (OddZeroCount > 100*EvenZeroCount) then
    begin
    Result := feUTF16;
    Exit;
    end;
  if (EvenZeroCount > 30) and (EvenZeroCount > 100*OddZeroCount) then
    begin
    Result := feUTF16;
    SwitchByteOrder;
    Exit;
    end;
  // Otherwise it is ANSI
  Result := feAnsi;
end;

{$ENDIF}

function FileGetSize(const FileName: string): int64;
var Handle: THandle;
    FileSizeHigh, FileSizeLow: DWORD;
begin
  Result := -1;
  Handle := CreateFile(PChar(FileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      FileSizeLow := GetFileSize(Handle, @FileSizeHigh);
      if (FileSizeLow <> $ffffffff) or (GetLastError = NO_ERROR) then
        Result := int64(FileSizeHigh) shl 32 + int64(FileSizeLow);
    finally
      CloseHandle(Handle);
      end;
end;

function FileGetDateTime(const FileName: string): TDateTime;
{$IFNDEF DELPHI2007_UP}
var DosTime: integer;
{$ENDIF}
begin
  {$IFDEF DELPHI2007_UP}
  if not FileAge(FileName, Result) then
    Result := 0;
  {$ELSE}
  DosTime := FileAge(FileName);
  if DosTime > 0 then
    Result := FileDateToDateTime(DosTime)
  else
    Result := 0;
  {$ENDIF}
end;

{$IFDEF TEXTSTREAMS}

{ TBufferedStream }

constructor TBufferedStream.Create(AStream: TStream; ABufferSize: integer; AOwnsStream: boolean);
begin
  inherited Create;
  fStream := AStream;
  fOwnsStream := AOwnsStream;
  fBufferSize := ABufferSize;
  GetMem(fBuffer, BufferSize);
  ClearBuffer;
  Init;
end;

constructor TBufferedStream.Create(AStream: TStream; AOwnsStream: boolean);
begin
  Create(AStream, DEFAULT_BUFFER_SIZE, AOwnsStream);
end;

destructor TBufferedStream.Destroy;
begin
  Flush;
  if fOwnsStream then
    FreeAndNil(fStream);
  FreeMem(fBuffer);
  fBuffer := nil;
  inherited;
end;

procedure TBufferedStream.Init;
begin
end;

procedure TBufferedStream.ClearBuffer;
begin
  fBufferPosition := 0;
  fBufferFill := 0;
  fBufferDirty := False;
end;

procedure TBufferedStream.FillBuffer;
begin
  if Stream.Position <> Position then
    Stream.Position := Position;
  fBufferPosition := Position;
  fBufferFill := Stream.Read(Buffer^, BufferSize);
  fBufferDirty := False;
end;

procedure TBufferedStream.Flush;
begin
  if fBufferDirty then
    begin
    if Stream.Position <> BufferPosition then
      Stream.Position := BufferPosition;
    Stream.WriteBuffer(Buffer^, BufferFill);
    fBufferDirty := False;
    end;
end;

procedure TBufferedStream.SetSize(NewSize: Integer);
begin
  Flush;
  ClearBuffer;
  Stream.Size := NewSize;
  Position := Stream.Position;
  inherited;
end;

function TBufferedStream.Seek(Offset: TStreamSize; Origin: Word): TStreamSize;
begin
  case Origin of
    soFromBeginning:
      Position := Offset;
    soFromCurrent:
      Position := Position + Offset;
    soFromEnd:
      Position := Stream.Size + Offset;
    end;
  Result := Position;
end;

function TBufferedStream.Read(var OutBuffer; Count: TStreamSize): TStreamSize;
var
  BufStart: int64;
  BufOfs, BufLen: integer;
  Buf: PAnsiChar;
  Dest: PAnsiChar;
begin
  Result := 0;
  Dest := @OutBuffer;
  if Count > 0 then
    begin
    // Read the rest of the buffer
    BufStart := Position - BufferPosition;
    if (BufStart >= 0) and (BufStart < BufferFill) then
      begin
      BufOfs := BufStart;
      BufLen := BufferFill - BufOfs;
      if BufLen > Count then
        BufLen := Count;
      Buf := Pointer(NativeUInt(Buffer) + NativeUInt(BufOfs));
      Move(Buf^, Dest^, BufLen);
      Position := Position + BufLen;
      Inc(Result, BufLen);
      Dec(Count, BufLen);
      Inc(Dest, BufLen);
      end;
    if Count > 0 then
      begin
      // Flush and clear the current buffer
      Flush;
      ClearBuffer;
      // If the remaining data is larger than the buffer, read it all at once
      if Count > BufferSize then
        begin
        if Stream.Position <> Position then
          Stream.Position := Position;
        Inc(Result, Stream.Read(Dest^, Count));
        Position := Stream.Position;
        end
      // Else fill the buffer from stream and read data from it
      else
        begin
        FillBuffer;
        Move(Buffer^, Dest^, Count);
        Inc(Result, Count);
        Position := Position + Count;
        end;
      end;
    end;
end;

function TBufferedStream.Write(const InBuffer; Count: TStreamSize): TStreamSize;

  procedure UpdateBufferFill;
    var
      BufLen: integer;
    begin
      fBufferDirty := True;
      BufLen := Position - BufferPosition;
      if BufLen > BufferFill then
        begin
        fBufferDirty := True;
        fBufferFill := BufLen;
        end;
    end;

var
  BufStart: int64;
  BufOfs, BufLen: integer;
  Buf: PAnsiChar;
  Src: PAnsiChar;
begin
  Result := 0;
  Src := @InBuffer;
  if Count > 0 then
    begin
    // Write as much as I can into the buffer
    BufStart := Position - BufferPosition;
    if (BufStart >= 0) and (BufStart < BufferSize) then
      begin
      BufOfs := BufStart;
      BufLen := BufferSize - BufOfs;
      if BufLen > Count then
        BufLen := Count;
      Buf := Pointer(NativeUInt(Buffer) + NativeUInt(BufOfs));
      Move(Src^, Buf^, BufLen);
      Position := Position + BufLen;
      Inc(Result, BufLen);
      Dec(Count, BufLen);
      Inc(Src, BufLen);
      UpdateBufferFill;
      end;
    if Count > 0 then
      begin
      // Flush and clear the current buffer
      Flush;
      ClearBuffer;
      // If the remaining data is larger than the buffer, write it all at once
      if Count > BufferSize then
        begin
        if Stream.Position <> Position then
          Stream.Position := Position;
        Inc(Result, Stream.Write(Src^, Count));
        Position := Stream.Position;
        end
      // Else fill the buffer from stream and write data to it
      else
        begin
        FillBuffer;
        Move(Src^, Buffer^, Count);
        Inc(Result, Count);
        Position := Position + Count;
        UpdateBufferFill;
        end;
      end;
    end;
end;

{ TTextStream }

class procedure TTextStream.AppendLine(const FileName, Msg: string; const Encoding: TFileEncoding);
var
  Mode: Word;
  Stream: TTextStream;
begin
  if FileExists(FileName) then
    Mode := fmOpenReadWrite or fmShareDenyWrite
  else
    Mode := fmCreate;
  Stream := TTextStream.Create(FileName, Mode);
  try
    if Encoding <> feAuto then
      Stream.DefaultEncoding := Encoding;
    Stream.Position := Stream.Size;
    Stream.WriteLine(Msg);
  finally
    FreeAndNil(Stream);
    end;
end;

constructor TTextStream.Create(AHandle: THandle);
begin
  Create(THandleStream.Create(AHandle), True);
end;

constructor TTextStream.Create(const AFileName: string; const AMode: Word);
begin
  Create(TFileStream.Create(AFileName, AMode), True);
end;

procedure TTextStream.Init;
begin
  inherited;
  fDefaultEncoding := {$IFDEF UNICODE} feUtf8 {$ELSE} feAnsi {$ENDIF} ;
  fEncoding := feAuto;
  fBigEndian := False;
  fLineSeparator := #13#10;
  if Size <> 0 then
    GuessEncoding;
end;

procedure TTextStream.SetEncoding(const Value: TFileEncoding);
begin
  if Position = 0 then
    case Value of
      feUtf16:
        begin
        WriteBuffer(BOM_UTF16[0], Length(BOM_UTF16));
        fBigEndian := False;
        end;
      feUtf8:
        WriteBuffer(BOM_UTF8[0], Length(BOM_UTF8));
      end;
  fEncoding := Value;
end;

procedure TTextStream.GuessEncoding;
var
  OldPos: int64;
  UTF16Buf: Word;
  StrBuf: RawByteString;
  StrBufSize, OddZeroCount, EvenZeroCount, i, n: integer;
begin
  fEncoding := feAnsi;
  fBigEndian := False;
  OldPos := Position;
  try
    // Unicode must have at least two bytes
    if Size <= 1 then
      Exit;
    // Check for UTF16 via BOM
    if (Size >= 2) and ((Size mod 2) = 0) then
      begin
      Position := 0;
      if Read(UTF16Buf, Sizeof(UTF16Buf)) = Sizeof(UTF16Buf) then
        if UTF16Buf = $feff then
          begin
          fEncoding := feUTF16;
          OldPos := Position;
          Exit;
          end
        else if UTF16Buf = $fffe then
          begin
          fEncoding := feUTF16;
          fBigEndian := True;
          OldPos := Position;
          Exit;
          end
      end;
    // Check for UTF8 via BOM
    if (Size >= 3) then
      begin
      Position := 0;
      SetLength(StrBuf, 3);
      if Read(StrBuf[1], 3) = 3 then
        if (StrBuf[1] = #$ef) and (StrBuf[2] = #$bb) and (StrBuf[3] = #$bf) then
          begin
          fEncoding := feUTF8;
          OldPos := Position;
          Exit;
          end;
      end;
    // Read the first MAX_SIZE_FOR_AUTODETECT characters into a buffer
    Position := 0;
    StrBufSize := MAX_SIZE_FOR_AUTODETECT;
    if StrBufSize > Size then
      StrBufSize := Size;
    SetLength(StrBuf, StrBufSize);
    StrBufSize := Read(StrBuf[1], StrBufSize);
    SetLength(StrBuf, StrBufSize);
    // Try to convert the stream as UTF8. If it succeeds, assume UTF8
    if (Size <= MAX_SIZE_FOR_AUTODETECT) then
      begin
      n := StrBufSize;
      while (n > 0) and ByteBool(Byte(StrBuf[n]) and $80) do
        Dec(n);
      if n > 0 then
        begin
        if MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, PAnsiChar(StrBuf), n, nil, 0) <> 0 then
          begin
          fEncoding := feUtf8;
          Exit;
          end;
        end;
      end;
    // I need to decide between Unicode and ANSI.
    // Assumption: ANSI doesn't use binary zero very often, except maybe separating words or lines
    // or as a filler. Unicode will have binary zero in all 7-bit ASCII characters, which will presumably
    // be present in the text (at least, they will be present in HTML/XML tags).
    OddZeroCount := 0;
    EvenZeroCount := 0;
    for i := 1 to StrBufSize do
      if StrBuf[i] = #0 then
        if (i and 1) <> 0 then // <>, because I use a 1-based index, not 0-based
          Inc(EvenZeroCount)
        else
          Inc(OddZeroCount);
    if (OddZeroCount > 30) and (OddZeroCount > 100*EvenZeroCount) then
      begin
      fEncoding := feUTF16;
      Exit;
      end;
    if (EvenZeroCount > 30) and (EvenZeroCount > 100*OddZeroCount) then
      begin
      fEncoding := feUTF16;
      fBigEndian := True;
      Exit;
      end;
    // Otherwise it is ANSI
    fEncoding := feAnsi;
  finally
    Position := OldPos;
    end;
end;

function TTextStream.Eof: boolean;
begin
  Result := Position >= Size;
end;

function TTextStream.ReadChar(out Ch: Char): boolean;
var
  AChar: AnsiChar;
  WChar: WideChar;
  i, ExtraBytes: integer;
  UTF8: LongWord;
begin
  Result := False;
  if (Position = 0) and (Encoding = feAuto) then
    GuessEncoding;
  case Encoding of
    feUTF16:
      begin
        Result := Read(WChar, Sizeof(WChar)) = Sizeof(WChar);
        if BigEndian then
          WChar := WideChar((Word(WChar) shr 8) or ((Word(WChar) and $ff) shl 8));
        Ch := {$IFNDEF UNICODE} WideToAnsi {$ENDIF} (WChar);
      end;
    feUTF8:
      begin
        if Read(AChar, Sizeof(AChar)) = Sizeof(AChar) then
          begin
          if (Byte(AChar) and $80) = $00 then
            begin
            UTF8 := Byte(AChar) and $7f;
            ExtraBytes := 0;
            end
          else if (Byte(AChar) and $e0) = $c0 then
            begin
            UTF8 := Byte(AChar) and $1f;
            ExtraBytes := 1;
            end
          else if (Byte(AChar) and $f0) = $e0 then
            begin
            UTF8 := Byte(AChar) and $0f;
            ExtraBytes := 2;
            end
          else if (Byte(AChar) and $f8) = $f0 then
            begin
            UTF8 := Byte(AChar) and $07;
            ExtraBytes := 3;
            end
          else if (Byte(AChar) and $fc) = $f8 then
            begin
            UTF8 := Byte(AChar) and $03;
            ExtraBytes := 4;
            end
          else if (Byte(AChar) and $fe) = $fc then
            begin
            UTF8 := Byte(AChar) and $01;
            ExtraBytes := 5;
            end
          else
            begin
            UTF8 := Byte(AChar);
            ExtraBytes := 0;
            end;
          for i := 1 to ExtraBytes do
            if Read(AChar, Sizeof(AChar)) = Sizeof(AChar) then
              if (Byte(AChar) and $c0) <> $80 then
                begin
                Position := Pred(Position);
                Break;
                end
              else
                UTF8 := (UTF8 shl 6) or (Byte(AChar) and $3f);
          Result := True;
          Ch := {$IFNDEF UNICODE} WideToAnsi {$ENDIF} (WideChar(UTF8));
          end;
      end;
    else
      begin
        Result := Read(AChar, Sizeof(AChar)) = Sizeof(AChar);
        Ch := {$IFDEF UNICODE} AnsiToWide {$ENDIF} (AChar);
      end;
    end;
end;

function TTextStream.ReadLine(out Line: string): boolean;
var
  Ch: Char;
  CurPos: int64;
begin
  Result := not Eof;
  Line := '';
  if Result then
    while ReadChar(Ch) do
      if Ch = #13 then
        begin
        CurPos := Position;
        if ReadChar(Ch) then
          if Ch <> #10 then
            Position := CurPos;
        Break;
        end
      else if Ch = #10 then
        Break
      else
        Line := Line + Ch;
end;

procedure TTextStream.WriteStr(const S: string);
var
  Data: Pointer;
  Size: integer;
  AStr: RawByteString;
  WStr: WideString;
begin
  if s <> '' then
    begin
    if Encoding = feAuto then
      Encoding := DefaultEncoding;
    case Encoding of
      feUTF16:
        begin
          WStr := WideString(S);
          Data := @(WStr[1]);
          Size := Length(WStr) * Sizeof(WideChar);
        end;
      feUTF8,
      feAuto:
        begin
          WStr := WideString(S);
          AStr := WideToUTF8(WStr);
          Data := @(AStr[1]);
          Size := Length(AStr) * Sizeof(AnsiChar);
        end;
      feANSI:
        begin
          AStr := {$IFDEF UNICODE} AnsiString {$ENDIF} (S);
          Data := @(AStr[1]);
          Size := Length(AStr) * Sizeof(AnsiChar);
        end;
      feOEM:
        begin
          AStr := {$IFDEF UNICODE} AnsiString {$ENDIF} (S);
          AStr := AnsiToOEM(AStr);
          Data := @(AStr[1]);
          Size := Length(AStr) * Sizeof(AnsiChar);
        end;
      else
        Raise EWriteError.Create('Invalid encoding');
      end;
    WriteBuffer(Data^, Size);
    end;
end;

procedure TTextStream.WriteLine(const S: string);
begin
  WriteStr(S);
  WriteStr(LineSeparator);
end;

{$ENDIF}

end.
