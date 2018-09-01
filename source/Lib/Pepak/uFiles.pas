unit uFiles;

interface

uses
  SysUtils, Classes, Windows;

function LoadFileIntoMemory(const FileName: string): TMemoryStream;
procedure SaveMemoryToFile(const FileName: string; Data: Pointer; DataLength: integer);

type
  TFileEncoding = (feAuto, feANSI, feOEM, feUnicode, feUTF8);

function GuessFileEncoding(Data: Pointer; DataLength: Integer; out BomLength: integer): TFileEncoding;
function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding): TMemoryStream;
procedure SaveMemoryToFileW(const FileName: string; Data: Pointer; DataLength: integer; Encoding: TFileEncoding);
function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding): string;

implementation

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

const MB_ERR_INVALID_CHARS = 8;

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

function LoadFileIntoMemoryW(const FileName: string; Encoding: TFileEncoding): TMemoryStream;
var Source: TMemoryStream;
    CodePage, Size, BomLength: integer;
    P: Pointer;
begin
  Source := LoadFileIntoMemory(FileName);
  try
    if Encoding = feAuto then
      Encoding := GuessFileEncoding(Source.Memory, Source.Size, BomLength);
    if Source.Size = 0 then
      Result := Source
    else if Encoding = feUnicode then
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
      P := PChar(LongWord(Source.Memory) + LongWord(BomLength));
      CodePage := EncodingToCodePage(Encoding);
      Size := MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS, P, Source.Size, nil, 0);
      if Size = 0 then
        Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
      Result := TMemoryStream.Create;
      try
        Result.Size := Size * Sizeof(WideChar);
        if MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS, P, Source.Size, Result.Memory, Size) <> Size then
          Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
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

function LoadFileIntoString(const FileName: string; Encoding: TFileEncoding): string;
var MS: TMemoryStream;
    s: WideString;
begin
  MS := LoadFileIntoMemoryW(FileName, Encoding);
  if MS = nil then
    Result := ''
  else
    try
      if MS.Size = 0 then
        Result := ''
      else
        begin
        SetLength(s, MS.Size div Sizeof(WideChar));
        Move(MS.Memory^, s[1], MS.Size);
        Result := string(s);
        end;
    finally
      FreeAndNil(MS);
      end;
end;

procedure SaveMemoryToFileW(const FileName: string; Data: Pointer; DataLength: integer; Encoding: TFileEncoding);
var Stream: TMemoryStream;
    CodePage, Size: integer;
    SavedOK: boolean;
begin
  SavedOK := False;
  if Encoding = feAuto then
    Encoding := feUTF8;
  if DataLength > 0 then
    if Encoding <> feUnicode then
      begin
      CodePage := EncodingToCodePage(Encoding);
      Size := WideCharToMultiByte(CodePage, MB_ERR_INVALID_CHARS, Data, DataLength div Sizeof(WideChar), nil, 0, nil, nil);
      if Size = 0 then
        Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
      Stream := TMemoryStream.Create;
      try
        Stream.Size := Size;
        if WideCharToMultiByte(CodePage, MB_ERR_INVALID_CHARS, Data, DataLength div Sizeof(WideChar), Stream.Memory, Size, nil, nil) <> Size then
          Raise EConvertError.CreateFmt('Invalid unicode characters encountered(%d)', [GetLastError]);
        SaveMemoryToFile(FileName, Stream.Memory, Stream.Size);
        SavedOK := True;
      finally
        Stream.Free;
        end;
      end;
  if not SavedOK then
    SaveMemoryToFile(FileName, Data, DataLength)
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
      Result := feUnicode;
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
    Result := feUnicode;
    Exit;
    end;
  if (EvenZeroCount > 30) and (EvenZeroCount > 100*OddZeroCount) then
    begin
    Result := feUnicode;
    SwitchByteOrder;
    Exit;
    end;
  // Otherwise it is ANSI
  Result := feAnsi;
end;

end.
