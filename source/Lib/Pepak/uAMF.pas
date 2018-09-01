unit uAMF;
{.DEFINE DEBUG}
{.DEFINE NONSEEKABLESTREAMS}

{$INCLUDE 'jedi.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uStringUtils;

type
  TAMFItem = class;

  EAMFError = class(Exception);

  TAMFItem = class
    private
    protected
      function GetValue: Variant; virtual; abstract;
      procedure SetValue(const AValue: Variant); virtual; abstract;
      function DataType: byte; virtual; abstract;
      procedure LoadFromStream(Stream: TStream); virtual; abstract;
    public
      class function ReadInt16(Stream: TStream): integer; virtual;
      class procedure WriteInt16(Stream: TStream; const AValue: integer); virtual;
      class function ReadInt32(Stream: TStream): LongWord; virtual;
      class procedure WriteInt32(Stream: TStream; const AValue: LongWord); virtual;
      class function ReadUtf(Stream: TStream): WideString; virtual;
      class procedure WriteUtf(Stream: TStream; const AValue: WideString); virtual;
      class function ReadLongUtf(Stream: TStream): WideString; virtual;
      class procedure WriteLongUtf(Stream: TStream; const AValue: WideString); virtual;
    public
      class function CreateFromStream(Stream: TStream): TAMFItem; virtual;
      constructor Create; overload; virtual;
      destructor Destroy; override;
      procedure SaveToStream(Stream: TStream); virtual;
      property Value: Variant read GetValue write SetValue;
    end;

  TAMFCommonArray = class(TAMFItem)
    private
      fList: TStringList;
      fOwnsItems: boolean;
    protected
      function GetValue: Variant; override;
      procedure SetValue(const AValue: Variant); override;
      function GetCount: integer; virtual;
      function GetItems(Index: integer): TAMFItem; virtual;
      procedure SetItems(Index: integer; const AValue: TAMFItem); virtual;
      function GetNames(Index: integer): WideString; virtual;
      procedure SetNames(Index: integer; const AValue: WideString); virtual;
      function GetNamedItems(const Index: WideString): TAMFItem; virtual;
      procedure SetNamedItems(const Index: WideString; const AValue: TAMFItem); virtual;
      procedure SaveHeaderToStream(Stream: TStream); virtual;
      procedure SaveItemNameToStream(Stream: TStream; Index: integer); virtual;
      procedure SaveItemValueToStream(Stream: TStream; Index: integer); virtual;
      procedure SaveFooterToStream(Stream: TStream); virtual;
      property List: TStringList read fList;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure SaveToStream(Stream: TStream); override;
      procedure Clear; virtual;
      function Add(const AName: WideString; AItem: TAMFItem): integer; virtual;
      function Find(const AName: WideString): integer; virtual;
      function FindByPath(const APath: WideString): TAMFItem; virtual;
      property OwnsItems: boolean read fOwnsItems write fOwnsItems;
      property Count: integer read GetCount;
      property Items[Index: integer]: TAMFItem read GetItems write SetItems;
      property Names[Index: integer]: WideString read GetNames write SetNames;
      property NamedItems[const Index: WideString]: TAMFItem read GetNamedItems write SetNamedItems;
    end;

  TAMFNumber = class(TAMFItem)
    private
      fNative: Double;
    protected
      function GetValue: Variant; override;
      procedure SetValue(const AValue: Variant); override;
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      function Swap(V: Double): Double; virtual;
    public
      constructor Create; override;
      constructor Create(const AValue: Double); overload; virtual;
      property Native: Double read fNative write fNative;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFBoolean = class(TAMFItem)
    private
      fNative: Boolean;
    protected
      function GetValue: Variant; override;
      procedure SetValue(const AValue: Variant); override;
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
    public
      constructor Create; override;
      constructor Create(const AValue: boolean); overload; virtual;
      property Native: Boolean read fNative write fNative;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFString = class(TAMFItem)
    private
      fNative: WideString;
      fLong: boolean;
    protected
      function GetValue: Variant; override;
      procedure SetValue(const AValue: Variant); override;
      procedure SetNative(const AValue: WideString); virtual;
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      property Long: boolean read fLong;
    public
      constructor Create; override;
      constructor Create(const AValue: WideString); overload; virtual;
      property Native: WideString read fNative write SetNative;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFObject = class(TAMFCommonArray)
    private
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
    public
    end;

  TAMFNull = class(TAMFItem)
    private
    protected
      function GetValue: Variant; override;
      procedure SetValue(const AValue: Variant); override;
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
    public
      constructor Create; override;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFUndefined = class(TAMFNull)
    private
    protected
      function DataType: byte; override;
    public
    end;

  TAMFEcmaArray = class(TAMFObject)
    private
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      procedure SaveHeaderToStream(Stream: TStream); override;
    public
    end;

  TAMFObjectEnd = class(TAMFNull)
    private
    protected
      function DataType: byte; override;
    public
    end;

  TAMFStrictArray = class(TAMFCommonArray)
    private
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      procedure SaveHeaderToStream(Stream: TStream); override;
      procedure SaveItemNameToStream(Stream: TStream; Index: integer); override;
      procedure SaveFooterToStream(Stream: TStream); override;
    public
    end;

  TAMFDate = class(TAMFNumber)
    private
      fTimeZone: integer;
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      property TimeZone: integer read fTimeZone write fTimeZone;
    public
      constructor Create; override;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFLongString = class(TAMFString)
    private
    protected
      procedure LoadFromStream(Stream: TStream); override;
    public
    end;

  TAMFXML = class(TAMFLongString)
    private
    protected
      function DataType: byte; override;
    public
    end;

  TAMFTypedObject = class(TAMFObject)
    private
      fTypeIdentifier: WideString;
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      procedure SaveHeaderToStream(Stream: TStream); override;
      property TypeIdentifier: WideString read fTypeIdentifier write fTypeIdentifier;
    public
    end;

  TAMFPacketHeader = record
    Name: WideString;
    Required: boolean;
    Content: TAMFItem;
    end;

  TAMFPacketBody = record
    Target: WideString;
    Response: WideString;
    Content: TAMFItem;
    end;

  TAMFPacketHeaderArray = array of TAMFPacketHeader;
  TAMFPacketBodyArray = array of TAMFPacketBody;

  TAMFPacket = class
    private
      fVersion: byte;
      fFlags: byte;
      fHeader: TAMFPacketHeaderArray;
      fBody: TAMFPacketBodyArray;
    protected
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure LoadFromStream(Stream: TStream); virtual;
      procedure SaveToStream(Stream: TStream); virtual;
      procedure LoadFromFile(const FileName: string); virtual;
      procedure SaveToFile(const FileName: string); virtual;
      procedure LoadFromString(const Data: AnsiString); virtual;
      procedure SaveToString(out Data: AnsiString); virtual;
      property Version: byte read fVersion write fVersion;
      property Flags: byte read fFlags write fFlags;
      property Header: TAMFPacketHeaderArray read fHeader write fHeader;
      property Body: TAMFPacketBodyArray read fBody write fBody;
    end;

{$IFDEF DEBUG}
procedure AMFItemDump(const Name: WideString; Item: TAMFItem; const Indent: WideString = '');
procedure AMFPacketDump(const Name: WideString; Packet: TAMFPacket; const Indent: WideString = '');
{$ENDIF}

implementation

const
  AMF_NUMBER = 0;
  AMF_BOOLEAN = 1;
  AMF_STRING = 2;
  AMF_OBJECT = 3;
  AMF_MOVIECLIP = 4;
  AMF_NULL = 5;
  AMF_UNDEFINED = 6;
  AMF_REFERENCE = 7;
  AMF_ECMA_ARRAY = 8;
  AMF_OBJECT_END = 9;
  AMF_STRICT_ARRAY = 10;
  AMF_DATE = 11;
  AMF_LONG_STRING = 12;
  AMF_UNSUPPORTED = 13;
  AMF_RECORDSET = 14;
  AMF_XML_DOC = 15;
  AMF_TYPED_OBJECT = 16;
  AMF_AVMPLUS = 17;
  AMF_INVALID = $ff;

  AMF3_UNDEFINED = 0;
  AMF3_NULL = 1;
  AMF3_FALSE = 2;
  AMF3_TRUE = 3;
  AMF3_INTEGER = 4;
  AMF3_DOUBLE = 5;
  AMF3_STRING = 6;
  AMF3_XML_DOC = 7;
  AMF3_DATE = 8;
  AMF3_ARRAY = 9;
  AMF3_OBJECT = 10;
  AMF3_XML = 11;
  AMF3_BYTE_ARRAY = 12;

{$IFDEF DEBUG}
procedure AMFCommonArrayDump(const Name: WideString; Item: TAMFCommonArray; const Indent: WideString = '');
var i: integer;
    s: WideString;
begin
  Writeln(Format('%s[%s] => %s', [Indent, Name, Item.ClassName]));
  Writeln(Format('%s(', [Indent]));
  for i := 0 to Pred(Item.Count) do
    begin
    s := Item.Names[i];
    if s = '' then
      s := Format('<%d>', [i]);
    AMFItemDump(s, Item.Items[i], Indent + #9);
    end;
  Writeln(Format('%s)', [Indent]));
end;

procedure AMFItemDump(const Name: WideString; Item: TAMFItem; const Indent: WideString = '');
var s: WideString;
begin
  if Item is TAMFCommonArray then
    AMFCommonArrayDump(Name, TAMFCommonArray(Item), Indent)
  else
    begin
    if Item.Value = null then
      s := ''
    else
      s := Item.Value;
    Writeln(Format('%s[%s] => %s', [Indent, Name, s]));
    end;
end;

procedure AMFPacketDump(const Name: WideString; Packet: TAMFPacket; const Indent: WideString = '');
var i: integer;
begin
  Writeln(Format('%s[%s] => %s', [Indent, Name, Packet.ClassName]));
  Writeln(Format('%s'#9'[Version] = 0x%02.2x', [Indent, Packet.Version]));
  Writeln(Format('%s'#9'[Flags] = 0x%02.2x', [Indent, Packet.Flags]));
  Writeln(Format('%s'#9'[Headers] => Array', [Indent]));
  Writeln(Format('%s'#9'(', [Indent]));
  for i := 0 to Pred(Length(Packet.Header)) do
    begin
    Writeln(Format('%s'#9#9'[%d] => %s', [Indent, i, Packet.Header[i].Content.ClassName]));
    Writeln(Format('%s'#9#9'(', [Indent]));
    Writeln(Format('%s'#9#9#9'[Name] = %s', [Indent, Packet.Header[i].Name]));
    Writeln(Format('%s'#9#9#9'[Required] = %d', [Indent, Integer(Packet.Header[i].Required)]));
    Writeln(Format('%s'#9#9#9'[Content] =>', [Indent]));
    Writeln(Format('%s'#9#9#9'(', [Indent]));
    AMFItemDump('', Packet.Header[i].Content, Indent + #9#9#9#9);
    Writeln(Format('%s'#9#9#9')', [Indent]));
    Writeln(Format('%s'#9#9')', [Indent]));
    end;
  Writeln(Format('%s'#9')', [Indent]));
  Writeln(Format('%s'#9'[Body] => Array', [Indent]));
  Writeln(Format('%s'#9'(', [Indent]));
  for i := 0 to Pred(Length(Packet.Body)) do
    begin
    Writeln(Format('%s'#9#9'[%d] => %s', [Indent, i, Packet.Body[i].Content.ClassName]));
    Writeln(Format('%s'#9#9'(', [Indent]));
    Writeln(Format('%s'#9#9#9'[Target] = %s', [Indent, Packet.Body[i].Target]));
    Writeln(Format('%s'#9#9#9'[Response] = %s', [Indent, Packet.Body[i].Response]));
    Writeln(Format('%s'#9#9#9'[Content] =>', [Indent]));
    Writeln(Format('%s'#9#9#9'(', [Indent]));
    AMFItemDump('', Packet.Body[i].Content, Indent + #9#9#9#9);
    Writeln(Format('%s'#9#9#9')', [Indent]));
    Writeln(Format('%s'#9#9')', [Indent]));
    end;
  Writeln(Format('%s'#9')', [Indent]));
end;
{$ENDIF}

{$IFNDEF UNICODE}
function StoreWideStringInAnsiString(const Value: WideString): AnsiString;
var n: integer;
begin
  n := Length(Value) * Sizeof(WideChar);
  SetLength(Result, n);
  Move(Value[1], Result[1], n);
end;

function RestoreWideStringFromAnsiString(const Value: AnsiString): WideString;
var n: integer;
begin
  n := Length(Value) div Sizeof(WideChar);
  SetLength(Result, n);
  Move(Value[1], Result[1], n * Sizeof(WideChar));
end;
{$ENDIF}

{ TAMFItem }

class function TAMFItem.CreateFromStream(Stream: TStream): TAMFItem;
var DT: byte;
begin
  Stream.ReadBuffer(DT, Sizeof(DT));
  case DT of
    AMF_NUMBER:
      Result := TAMFNumber.Create;
    AMF_BOOLEAN:
      Result := TAMFBoolean.Create;
    AMF_STRING:
      Result := TAMFString.Create;
    AMF_OBJECT:
      Result := TAMFObject.Create;
//  AMF_MOVIECLIP:
//    ;
    AMF_NULL:
      Result := TAMFNull.Create;
    AMF_UNDEFINED:
      Result := TAMFUndefined.Create;
//    AMF_REFERENCE:
//      ;
    AMF_ECMA_ARRAY:
      Result := TAMFEcmaArray.Create;
    AMF_OBJECT_END:
      Result := TAMFObjectEnd.Create;
    AMF_STRICT_ARRAY:
      Result := TAMFStrictArray.Create;
    AMF_DATE:
      Result := TAMFDate.Create;
    AMF_LONG_STRING:
      Result := TAMFLongString.Create;
//    AMF_UNSUPPORTED:
//      ;
//    AMF_RECORDSET:
//      ;
    AMF_XML_DOC:
      Result := TAMFXML.Create;
    AMF_TYPED_OBJECT:
      Result := TAMFTypedObject.Create;
//    AMF_AVMPLUS:
//      ;
    else
      Raise EAMFError.CreateFmt('Invalid data type %02.2x', [DT]);
    end;
  Result.LoadFromStream(Stream);
end;

constructor TAMFItem.Create;
begin
  inherited Create;
end;

destructor TAMFItem.Destroy;
begin
  inherited;
end;

procedure TAMFItem.SaveToStream(Stream: TStream);
var DT: byte;
begin
  DT := DataType;
  Stream.WriteBuffer(DT, Sizeof(DT));
end;

class function TAMFItem.ReadInt16(Stream: TStream): integer;
var V: array[0..1] of byte;
begin
  Stream.ReadBuffer(V, Length(V));
  Result := (V[0] shl 8) or (V[1]);
end;

class procedure TAMFItem.WriteInt16(Stream: TStream; const AValue: integer);
var V: array[0..1] of byte;
begin
  if (AValue < 0) or (AValue > $ffff) then
    Raise EAMFError.Create('Value out of bounds.');
  V[0] := (AValue shr 8) and $ff;
  V[1] := AValue and $ff;
  Stream.WriteBuffer(V, Length(V));
end;

class function TAMFItem.ReadInt32(Stream: TStream): LongWord;
var V: array[0..3] of byte;
begin
  Stream.ReadBuffer(V, Length(V));
  Result := (V[0] shl 24) or (V[1] shl 16) or (V[2] shl 8) or (V[3]);
end;

class procedure TAMFItem.WriteInt32(Stream: TStream; const AValue: LongWord);
var V: array[0..3] of byte;
begin
  V[0] := (AValue shr 24) and $ff;
  V[1] := (AValue shr 16) and $ff;
  V[2] := (AValue shr 8) and $ff;
  V[3] := AValue and $ff;
  Stream.WriteBuffer(V, Length(V));
end;

class function TAMFItem.ReadUtf(Stream: TStream): WideString;
var n: integer;
    V: Utf8String;
begin
  n := ReadInt16(Stream);
  if n <= 0 then
    Result := ''
  else
    begin
    SetLength(V, n);
    Stream.ReadBuffer(V[1], n);
    Result := Utf8ToWide(V);
    end;
end;

class procedure TAMFItem.WriteUtf(Stream: TStream; const AValue: WideString);
var V: Utf8String;
begin
  V := WideToUtf8(AValue);
  WriteInt16(Stream, Length(V));
  if V <> '' then
    Stream.WriteBuffer(V[1], Length(V));
end;

class function TAMFItem.ReadLongUtf(Stream: TStream): WideString;
var n: integer;
    V: Utf8String;
begin
  n := ReadInt32(Stream);
  if n <= 0 then
    Result := ''
  else
    begin
    SetLength(V, n);
    Stream.ReadBuffer(V[1], n);
    Result := Utf8ToWide(V);
    end;
end;

class procedure TAMFItem.WriteLongUtf(Stream: TStream; const AValue: WideString);
var V: Utf8String;
begin
  V := WideToUtf8(AValue);
  WriteInt32(Stream, Length(V));
  if V <> '' then
    Stream.WriteBuffer(V[1], Length(V));
end;

{ TAMFNumber }

constructor TAMFNumber.Create;
begin
  inherited;
  Native := 0;
end;

constructor TAMFNumber.Create(const AValue: Double);
begin
  Create;
  Native := AValue;
end;

function TAMFNumber.DataType: byte;
begin
  Result := AMF_NUMBER;
end;

function TAMFNumber.GetValue: Variant;
begin
  Result := Native;
end;

procedure TAMFNumber.LoadFromStream(Stream: TStream);
var V: Double;
begin
  Stream.ReadBuffer(V, Sizeof(V));
  Native := Swap(V);
end;

procedure TAMFNumber.SaveToStream(Stream: TStream);
var V: Double;
begin
  inherited;
  V := Swap(Native);
  Stream.WriteBuffer(V, Sizeof(V));
end;

procedure TAMFNumber.SetValue(const AValue: Variant);
begin
  Native := AValue;
end;

function TAMFNumber.Swap(V: Double): Double;
var P: PByteArray;
    i: integer;
    x: byte;
begin
  Result := V;
  P := @Result;
  for i := 0 to 3 do
    begin
    x := P^[i];
    P^[i] := P^[7-i];
    P^[7-i] := x;
    end;
end;

{ TAMFBoolean }

constructor TAMFBoolean.Create;
begin
  inherited;
  Native := False;
end;

constructor TAMFBoolean.Create(const AValue: boolean);
begin
  Create;
  Native := AValue;
end;

function TAMFBoolean.DataType: byte;
begin
  Result := AMF_BOOLEAN;
end;

function TAMFBoolean.GetValue: Variant;
begin
  Result := Native;
end;

procedure TAMFBoolean.LoadFromStream(Stream: TStream);
var V: Byte;
begin
  Stream.ReadBuffer(V, Sizeof(V));
  Native := V <> 0;
end;

procedure TAMFBoolean.SaveToStream(Stream: TStream);
var V: byte;
begin
  inherited;
  if Native then
    V := 1
  else
    V := 0;
  Stream.WriteBuffer(V, Sizeof(V));
end;

procedure TAMFBoolean.SetValue(const AValue: Variant);
begin
  Native := AValue;
end;

{ TAMFString }

constructor TAMFString.Create;
begin
  inherited;
  Native := '';
end;

constructor TAMFString.Create(const AValue: WideString);
begin
  Create;
  Native := AValue;
end;

function TAMFString.DataType: byte;
begin
  if Long then
    Result := AMF_LONG_STRING
  else
    Result := AMF_STRING;
end;

function TAMFString.GetValue: Variant;
begin
  Result := Native;
end;

procedure TAMFString.LoadFromStream(Stream: TStream);
begin
  Native := ReadUtf(Stream);
end;

procedure TAMFString.SaveToStream(Stream: TStream);
begin
  inherited;
  if Long then
    WriteLongUtf(Stream, Native)
  else
    WriteUtf(Stream, Native);
end;

procedure TAMFString.SetNative(const AValue: WideString);
begin
  fNative := AValue;
  fLong := Length(WideToUtf8(AValue)) >= $10000;
end;

procedure TAMFString.SetValue(const AValue: Variant);
begin
  Native := Value;
end;

{ TAMFCommonArray }

function TAMFCommonArray.Add(const AName: WideString; AItem: TAMFItem): integer;
begin
  {$IFDEF DELPHI2009_UP}
  Result := List.AddObject(AName, AItem);
  {$ELSE}
  Result := List.AddObject(StoreWideStringInAnsiString(AName), AItem);
  {$ENDIF}
end;

procedure TAMFCommonArray.Clear;
var i: integer;
begin
  if OwnsItems then
    for i := 0 to Pred(Count) do
      Items[i].Free;
  List.Clear;
end;

constructor TAMFCommonArray.Create;
begin
  inherited;
  fList := TStringList.Create;
  fOwnsItems := True;
end;

destructor TAMFCommonArray.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

function TAMFCommonArray.Find(const AName: WideString): integer;
begin
  {$IFDEF DELPHI2009_UP}
  Result := List.IndexOf(AName);
  {$ELSE}
  Result := Pred(Count);
  while Result >= 0 do
    if Names[Result] = AName then
      Break
    else
      Dec(Result);
  {$ENDIF}
end;

function TAMFCommonArray.FindByPath(const APath: WideString): TAMFItem;
var Item: TAMFItem;
    Path, s: WideString;
    i: integer;
    Found: boolean;
begin
  Result := nil;
  Item := Self;
  Path := APath;
  Found := True;
  while Found and (Path <> '') and (Item is TAMFCommonArray)do
    begin
    if Path[1] = '/' then
      System.Delete(Path, 1, 1)
    else
      begin
      i := Pos('/', Path);
      if i <= 0 then
        begin
        s := Path;
        Path := '';
        end
      else
        begin
        s := Copy(Path, 1, Pred(i));
        System.Delete(Path, 1, i);
        end;
      Found := False;
      if s = '' then
        Found := True
      else
        for i := 0 to Pred(TAMFCommonArray(Item).Count) do
          if TAMFCommonArray(Item).Names[i] = s then
            begin
            Item := TAMFCommonArray(Item).Items[i];
            Found := True;
            Break;
            end;
      end;
    end;
  if Found then
    Result := Item;
end;

function TAMFCommonArray.GetCount: integer;
begin
  Result := List.Count;
end;

function TAMFCommonArray.GetItems(Index: integer): TAMFItem;
begin
  Result := List.Objects[Index] as TAMFItem;
end;

function TAMFCommonArray.GetNamedItems(const Index: WideString): TAMFItem;
var ix: integer;
begin
  ix := Find(Index);
  if ix >= 0 then
    Result := Items[ix]
  else
    Result := nil;
end;

function TAMFCommonArray.GetNames(Index: integer): WideString;
begin
  {$IFDEF DELPHI2009_UP}
  Result := List[Index];
  {$ELSE}
  Result := RestoreWideStringFromAnsiString(List[Index]);
  {$ENDIF}
end;

function TAMFCommonArray.GetValue: Variant;
begin
  Result := null;
end;

procedure TAMFCommonArray.SaveFooterToStream(Stream: TStream);
var Obj: TAMFObjectEnd;
begin
  WriteUtf(Stream, '');
  Obj := TAMFObjectEnd.Create;
  try
    Obj.SaveToStream(Stream);
  finally
    Obj.Free;
    end;
end;

procedure TAMFCommonArray.SaveHeaderToStream(Stream: TStream);
begin
  // Nothing to do
end;

procedure TAMFCommonArray.SaveItemNameToStream(Stream: TStream; Index: integer);
begin
  WriteUtf(Stream, Names[Index]);
end;

procedure TAMFCommonArray.SaveItemValueToStream(Stream: TStream; Index: integer);
begin
  Items[Index].SaveToStream(Stream);
end;

procedure TAMFCommonArray.SaveToStream(Stream: TStream);
var i: integer;
begin
  inherited;
  SaveHeaderToStream(Stream);
  for i := 0 to Pred(Count) do
    begin
    SaveItemNameToStream(Stream, i);
    SaveItemValueToStream(Stream, i);
    end;
  SaveFooterToStream(Stream);
end;

procedure TAMFCommonArray.SetItems(Index: integer; const AValue: TAMFItem);
begin
  if OwnsItems then
    Items[Index].Free;
  List.Objects[Index] := AValue;
end;

procedure TAMFCommonArray.SetNamedItems(const Index: WideString; const AValue: TAMFItem);
var ix: integer;
begin
  ix := Find(Index);
  if ix >= 0 then
    Items[ix] := AValue
  else
    Add(Index, AValue);
end;

procedure TAMFCommonArray.SetNames(Index: integer; const AValue: WideString);
begin
  {$IFDEF DELPHI2009_UP}
  List[Index] := AValue;
  {$ELSE}
  List[Index] := StoreWideStringInAnsiString(AValue);
  {$ENDIF}
end;

procedure TAMFCommonArray.SetValue(const AValue: Variant);
begin
  Clear;
end;

{ TAMFObject }

function TAMFObject.DataType: byte;
begin
  Result := AMF_OBJECT;
end;

procedure TAMFObject.LoadFromStream(Stream: TStream);
var VObj: TAMFItem;
    VName: WideString;
begin
  Clear;
  OwnsItems := True;
  repeat
    VName := ReadUtf(Stream);
    VObj := CreateFromStream(Stream);
    if not (VObj is TAMFObjectEnd) then
      Add(VName, VObj);
  until VObj is TAMFObjectEnd;
  VObj.Free;
end;

{ TAMFNull }

constructor TAMFNull.Create;
begin
  inherited;
end;

function TAMFNull.DataType: byte;
begin
  Result := AMF_NULL;
end;

function TAMFNull.GetValue: Variant;
begin
  Result := null;
end;

procedure TAMFNull.LoadFromStream(Stream: TStream);
begin
end;

procedure TAMFNull.SaveToStream(Stream: TStream);
begin
  inherited;
end;

procedure TAMFNull.SetValue(const AValue: Variant);
begin
end;

{ TAMFUndefined }

function TAMFUndefined.DataType: byte;
begin
  Result := AMF_UNDEFINED;
end;

{ TAMFEcmaArray }

function TAMFEcmaArray.DataType: byte;
begin
  Result := AMF_ECMA_ARRAY;
end;

procedure TAMFEcmaArray.LoadFromStream(Stream: TStream);
begin
  ReadInt32(Stream); // Discard the length of array
  inherited;
end;

procedure TAMFEcmaArray.SaveHeaderToStream(Stream: TStream);
begin
  inherited;
  WriteInt32(Stream, Count);
end;

{ TAMFObjectEnd }

function TAMFObjectEnd.DataType: byte;
begin
  Result := AMF_OBJECT_END;
end;

{ TAMFStrictArray }

function TAMFStrictArray.DataType: byte;
begin
  Result := AMF_STRICT_ARRAY;
end;

procedure TAMFStrictArray.LoadFromStream(Stream: TStream);
var i, n: integer;
begin
  Clear;
  OwnsItems := True;
  n := ReadInt32(Stream);
  i := 0;
  while n > 0 do
    begin
    Add(IntToStr(i), CreateFromStream(Stream));
    Dec(n);
    Inc(i);
    end;
end;

procedure TAMFStrictArray.SaveFooterToStream(Stream: TStream);
begin
end;

procedure TAMFStrictArray.SaveHeaderToStream(Stream: TStream);
begin
  inherited;
  WriteInt32(Stream, Count);
end;

procedure TAMFStrictArray.SaveItemNameToStream(Stream: TStream; Index: integer);
begin
end;

{ TAMFDate }

constructor TAMFDate.Create;
begin
  inherited;
  fTimeZone := 0;
end;

function TAMFDate.DataType: byte;
begin
  Result := AMF_DATE;
end;

procedure TAMFDate.LoadFromStream(Stream: TStream);
begin
  inherited;
  TimeZone := ReadInt16(Stream);
end;

procedure TAMFDate.SaveToStream(Stream: TStream);
begin
  inherited;
  WriteInt16(Stream, TimeZone);
end;

{ TAMFLongString }

procedure TAMFLongString.LoadFromStream(Stream: TStream);
begin
  inherited;
  Native := ReadLongUtf(Stream);
end;

{ TAMFTypedObject }

function TAMFTypedObject.DataType: byte;
begin
  Result := AMF_TYPED_OBJECT;
end;

procedure TAMFTypedObject.LoadFromStream(Stream: TStream);
begin
  TypeIdentifier := ReadUtf(Stream);
  inherited;
end;

procedure TAMFTypedObject.SaveHeaderToStream(Stream: TStream);
begin
  inherited;
  WriteUtf(Stream, TypeIdentifier);
end;

{ TAMFXML }

function TAMFXML.DataType: byte;
begin
  Result := AMF_XML_DOC;
end;

{ TAMFPacket }

constructor TAMFPacket.Create;
begin
  inherited Create;
  Version := 0;
  Flags := 0;
end;

destructor TAMFPacket.Destroy;
begin
  inherited;
end;

procedure TAMFPacket.LoadFromFile(const FileName: string);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
    end;
end;

procedure TAMFPacket.LoadFromStream(Stream: TStream);
var B1, B2: byte;
    i, n: integer;
    Hdr: TAMFPacketHeader;
    Bdy: TAMFPacketBody;
begin
  Stream.ReadBuffer(B1, Sizeof(B1));
  Stream.ReadBuffer(B2, Sizeof(B2));
  Version := B1;
  Flags := B2;
  n := TAMFItem.ReadInt16(Stream);
  SetLength(fHeader, n);
  for i := 0 to Pred(n) do
    begin
    Hdr.Name := TAMFItem.ReadUtf(Stream);
    Stream.ReadBuffer(B1, Sizeof(B1));
    Hdr.Required := B1 <> 0;
    TAMFItem.ReadInt32(Stream); // Ignore length
    Hdr.Content := TAMFItem.CreateFromStream(Stream);
    Header[i] := Hdr;
    end;
  n := TAMFItem.ReadInt16(Stream);
  SetLength(fBody, n);
  for i := 0 to Pred(n) do
    begin
    Bdy.Target := TAMFItem.ReadUtf(Stream);
    Bdy.Response := TAMFItem.ReadUtf(Stream);
    TAMFItem.ReadInt32(Stream); // Ignore length
    Bdy.Content := TAMFItem.CreateFromStream(Stream);
    Body[i] := Bdy;
    end;
end;

procedure TAMFPacket.LoadFromString(const Data: AnsiString);
var Stream: TStringStream;
begin
  Stream := TStringStream.Create(Data);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
    end;
end;

procedure TAMFPacket.SaveToFile(const FileName: string);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
    end;
end;

procedure TAMFPacket.SaveToStream(Stream: TStream);
var B1, B2: byte;
    i: integer;
    {$IFNDEF NONSEEKABLESTREAMS}
    LengthPosition, BodyStartPosition, BodyEndPosition: integer;
    {$ENDIF}
begin
  B1 := Version;
  B2 := Flags;
  Stream.WriteBuffer(B1, Sizeof(B1));
  Stream.WriteBuffer(B2, Sizeof(B2));
  TAMFItem.WriteInt16(Stream, Length(Header));
  for i := 0 to Pred(Length(Header)) do
    begin
    TAMFItem.WriteUtf(Stream, Header[i].Name);
    if Header[i].Required then
      B1 := 1
    else
      B1 := 0;
    Stream.WriteBuffer(B1, Sizeof(B1));
    {$IFNDEF NONSEEKABLESTREAMS}
    LengthPosition := Stream.Position;
    {$ENDIF}
    TAMFItem.WriteInt32(Stream, 0); // Invalid value
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyStartPosition := Stream.Position;
    {$ENDIF}
    Header[i].Content.SaveToStream(Stream);
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyEndPosition := Stream.Position;
    Stream.Position := LengthPosition;
    TAMFItem.WriteInt32(Stream, BodyEndPosition - BodyStartPosition);
    Stream.Position := BodyEndPosition;
    {$ENDIF}
    end;
  TAMFItem.WriteInt16(Stream, Length(Body));
  for i := 0 to Pred(Length(Body)) do
    begin
    TAMFItem.WriteUtf(Stream, Body[i].Target);
    TAMFItem.WriteUtf(Stream, Body[i].Response);
    {$IFNDEF NONSEEKABLESTREAMS}
    LengthPosition := Stream.Position;
    {$ENDIF}
    TAMFItem.WriteInt32(Stream, 0); // Invalid value
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyStartPosition := Stream.Position;
    {$ENDIF}
    Body[i].Content.SaveToStream(Stream);
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyEndPosition := Stream.Position;
    Stream.Position := LengthPosition;
    TAMFItem.WriteInt32(Stream, BodyEndPosition - BodyStartPosition);
    Stream.Position := BodyEndPosition;
    {$ENDIF}
    end;
end;

procedure TAMFPacket.SaveToString(out Data: AnsiString);
var Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Data := Stream.DataString;
  finally
    Stream.Free;
    end;
end;

end.
