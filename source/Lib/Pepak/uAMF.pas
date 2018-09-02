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

unit uAMF;
{.DEFINE DEBUG}
{.DEFINE NONSEEKABLESTREAMS}
{$DEFINE MINIMIZESIZE}
{.DEFINE VARIANTS}

{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, Classes,
  {$IFDEF VARIANTS} {$IFDEF DELPHI6_UP} Variants, {$ENDIF} {$ENDIF}
  uCompatibility, uStrings;

{$IFNDEF VARIANTS}
const
  AMFNullValue = '';
{$ENDIF}

type
  TAMFItem = class;
  TAMFItemClass = class of TAMFItem;

  EAMFError = class(Exception);

  TAMFValue = {$IFDEF VARIANTS} Variant {$ELSE} string {$ENDIF} ;

  TAMFItem = class
    private
    protected
      function GetValue: TAMFValue; virtual;
      procedure SetValue(const AValue: TAMFValue); virtual;
      function DataType: byte; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF} abstract;
      procedure LoadFromStream(Stream: TStream); {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF} abstract;
    public
    public
      class function CreateFromStream(Stream: TStream): TAMFItem; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      constructor Create; overload; virtual;
      destructor Destroy; override;
      procedure SaveToStream(Stream: TStream); {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function FindByPath(const APath: WideString; ItemType: TAMFItemClass = nil): TAMFItem; virtual;
      function FindValueByPath(const APath: WideString; out Value: TAMFValue; ItemType: TAMFItemClass = nil): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function SetValueByPath(const APath: WideString; const Value: TAMFValue; ItemType: TAMFItemClass = nil): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property Value: TAMFValue read GetValue write SetValue;
    end;

  TAMFCommonArray = class(TAMFItem)
    private
      fList: TStringList;
      fOwnsItems: boolean;
    protected
      procedure SetValue(const AValue: TAMFValue); override;
      function GetCount: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetItems(Index: integer): TAMFItem; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetItems(Index: integer; const AValue: TAMFItem); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetNames(Index: integer): WideString; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetNames(Index: integer; const AValue: WideString); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetNamedItems(const Index: WideString): TAMFItem; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetNamedItems(const Index: WideString; const AValue: TAMFItem); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SaveHeaderToStream(Stream: TStream); {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure SaveItemNameToStream(Stream: TStream; Index: integer); {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure SaveItemValueToStream(Stream: TStream; Index: integer); {$IFNDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure SaveFooterToStream(Stream: TStream); {$IFNDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      property List: TStringList read fList;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure SaveToStream(Stream: TStream); override;
      procedure Clear; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Add(const AName: WideString; AItem: TAMFItem): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Find(const AName: WideString): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function FindByPath(const APath: WideString; ItemType: TAMFItemClass = nil): TAMFItem; override;
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
      function GetValue: TAMFValue; override;
      procedure SetValue(const AValue: TAMFValue); override;
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
      function Swap(V: Double): Double; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
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
      function GetValue: TAMFValue; override;
      procedure SetValue(const AValue: TAMFValue); override;
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
      procedure SetNative(const AValue: WideString); 
    protected
      function GetValue: TAMFValue; override;
      procedure SetValue(const AValue: TAMFValue); override;
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
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
    public
      constructor Create; override;
      procedure SaveToStream(Stream: TStream); override;
    end;

  TAMFReference = class(TAMFItem)
    private
      fReference: LongWord;
    protected
      function DataType: byte; override;
      procedure LoadFromStream(Stream: TStream); override;
    public
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
      procedure LoadFromStream(Stream: TStream); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SaveToStream(Stream: TStream); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure LoadFromFile(const FileName: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SaveToFile(const FileName: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure LoadFromString(const Data: AnsiString); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SaveToString(out Data: AnsiString); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function HasBody(BodyIndex: integer = -1): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property Version: byte read fVersion write fVersion;
      property Flags: byte read fFlags write fFlags;
      property Header: TAMFPacketHeaderArray read fHeader write fHeader;
      property Body: TAMFPacketBodyArray read fBody write fBody;
    end;

{$IFDEF DEBUG}
procedure AMFItemDump(const Name: WideString; Item: TAMFItem; const Indent: WideString = '');
procedure AMFPacketDump(const Name: WideString; Packet: TAMFPacket; const Indent: WideString = '');
{$ENDIF}

function AMFReadInt16(Stream: TStream): integer;
procedure AMFWriteInt16(Stream: TStream; const AValue: integer);
function AMFReadInt32(Stream: TStream): LongWord;
procedure AMFWriteInt32(Stream: TStream; const AValue: LongWord);
function AMFReadUtf(Stream: TStream): WideString;
procedure AMFWriteUtf(Stream: TStream; const AValue: WideString);
function AMFReadLongUtf(Stream: TStream): WideString;
procedure AMFWriteLongUtf(Stream: TStream; const AValue: WideString);

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

function AMFReadInt16(Stream: TStream): integer;
var V: array[0..1] of byte;
begin
  Stream.ReadBuffer(V, Length(V));
  Result := (V[0] shl 8) or (V[1]);
end;

procedure AMFWriteInt16(Stream: TStream; const AValue: integer);
var V: array[0..1] of byte;
begin
  if (AValue < 0) or (AValue > $ffff) then
    Raise EAMFError.Create('Value out of bounds.');
  V[0] := (AValue shr 8) and $ff;
  V[1] := AValue and $ff;
  Stream.WriteBuffer(V, Length(V));
end;

function AMFReadInt32(Stream: TStream): LongWord;
var V: array[0..3] of byte;
begin
  Stream.ReadBuffer(V, Length(V));
  Result := (V[0] shl 24) or (V[1] shl 16) or (V[2] shl 8) or (V[3]);
end;

procedure AMFWriteInt32(Stream: TStream; const AValue: LongWord);
var V: array[0..3] of byte;
begin
  V[0] := (AValue shr 24) and $ff;
  V[1] := (AValue shr 16) and $ff;
  V[2] := (AValue shr 8) and $ff;
  V[3] := AValue and $ff;
  Stream.WriteBuffer(V, Length(V));
end;

function AMFReadUtf(Stream: TStream): WideString;
var n: integer;
    V: Utf8String;
begin
  n := AMFReadInt16(Stream);
  if n <= 0 then
    Result := ''
  else
    begin
    SetLength(V, n);
    Stream.ReadBuffer(V[1], n);
    Result := Utf8ToWide(V);
    end;
end;

procedure AMFWriteUtf(Stream: TStream; const AValue: WideString);
var V: Utf8String;
begin
  V := WideToUtf8(AValue);
  AMFWriteInt16(Stream, Length(V));
  if V <> '' then
    Stream.WriteBuffer(V[1], Length(V));
end;

function AMFReadLongUtf(Stream: TStream): WideString;
var n: integer;
    V: Utf8String;
begin
  n := AMFReadInt32(Stream);
  if n <= 0 then
    Result := ''
  else
    begin
    SetLength(V, n);
    Stream.ReadBuffer(V[1], n);
    Result := Utf8ToWide(V);
    end;
end;

procedure AMFWriteLongUtf(Stream: TStream; const AValue: WideString);
var V: Utf8String;
begin
  V := WideToUtf8(AValue);
  AMFWriteInt32(Stream, Length(V));
  if V <> '' then
    Stream.WriteBuffer(V[1], Length(V));
end;

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
    if Item.Value = AMFNullValue then
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
    AMF_REFERENCE:
      Result := TAMFReference.Create;
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

function TAMFItem.FindByPath(const APath: WideString; ItemType: TAMFItemClass): TAMFItem;
begin
  Result := nil;
  if APath = '' then
    if (ItemType = nil) or (Self is ItemType) then
      Result := Self;
end;

function TAMFItem.FindValueByPath(const APath: WideString; out Value: TAMFValue; ItemType: TAMFItemClass): boolean;
var Item: TAMFItem;
begin
  Item := FindByPath(APath, ItemType);
  if Item = nil then
    begin
    Value := {$IFDEF VARIANTS} null {$ELSE} AMFNullValue {$ENDIF} ;
    Result := False;
    end
  else
    begin
    Value := Item.Value;
    Result := True;
    end;
end;

function TAMFItem.GetValue: TAMFValue;
begin
  Result := AMFNullValue;
end;

procedure TAMFItem.SaveToStream(Stream: TStream);
var DT: byte;
begin
  DT := DataType;
  Stream.WriteBuffer(DT, Sizeof(DT));
end;

procedure TAMFItem.SetValue(const AValue: TAMFValue);
begin
end;

function TAMFItem.SetValueByPath(const APath: WideString; const Value: TAMFValue; ItemType: TAMFItemClass): boolean;
var Item: TAMFItem;
begin
  Item := FindByPath(APath, ItemType);
  if Item = nil then
    Result := False
  else
    begin
    Item.Value := Value;
    Result := True;
    end;
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

function TAMFNumber.GetValue: TAMFValue;
begin
  Result := {$IFNDEF VARIANTS} FloatToStr {$ENDIF} (Native);
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

procedure TAMFNumber.SetValue(const AValue: TAMFValue);
begin
  Native := {$IFNDEF VARIANTS} StrToFloat {$ENDIF} (AValue);
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

function TAMFBoolean.GetValue: TAMFValue;
begin
  Result := {$IFNDEF VARIANTS} IntToStr(Integer(Native)) {$ELSE} Native {$ENDIF} ;
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

procedure TAMFBoolean.SetValue(const AValue: TAMFValue);
begin
  Native := {$IFNDEF VARIANTS} StrToIntDef(AValue, 0) <> 0 {$ELSE} AValue {$ENDIF} ;
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

function TAMFString.GetValue: TAMFValue;
begin
  Result := Native;
end;

procedure TAMFString.LoadFromStream(Stream: TStream);
begin
  Native := AMFReadUtf(Stream);
end;

procedure TAMFString.SaveToStream(Stream: TStream);
begin
  inherited;
  if Long then
    AMFWriteLongUtf(Stream, Native)
  else
    AMFWriteUtf(Stream, Native);
end;

procedure TAMFString.SetNative(const AValue: WideString);
begin
  fNative := AValue;
  fLong := Length(WideToUtf8(AValue)) >= $10000;
end;

procedure TAMFString.SetValue(const AValue: TAMFValue);
begin
  Native := AValue;
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

function TAMFCommonArray.FindByPath(const APath: WideString; ItemType: TAMFItemClass): TAMFItem;
var Path, NextItem, NextPath: WideString;
    i: integer;
begin
  Path := APath;
  while (Path <> '') and (Path[1] = '/') do
    System.Delete(Path, 1, 1);
  if Path = '' then
    Result := inherited FindByPath(Path, ItemType)
  else
    begin
    i := Pos('/', Path);
    if i > 0 then
      begin
      NextItem := Copy(Path, 1, Pred(i));
      NextPath := Copy(Path, Succ(i), MaxInt);
      end
    else
      begin
      NextItem := Path;
      NextPath := '';
      end;
    i := Find(NextItem);
    if i >= 0 then
      Result := Items[i].FindByPath(NextPath, ItemType)
    else
      Result := nil;
    end;
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

procedure TAMFCommonArray.SaveFooterToStream(Stream: TStream);
var Obj: TAMFObjectEnd;
begin
  AMFWriteUtf(Stream, '');
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
  AMFWriteUtf(Stream, Names[Index]);
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

procedure TAMFCommonArray.SetValue(const AValue: TAMFValue);
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
    VName := AMFReadUtf(Stream);
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

procedure TAMFNull.LoadFromStream(Stream: TStream);
begin
end;

procedure TAMFNull.SaveToStream(Stream: TStream);
begin
  inherited;
end;

{ TAMFReference }

function TAMFReference.DataType: byte;
begin
  Result := AMF_REFERENCE;
end;

procedure TAMFReference.LoadFromStream(Stream: TStream);
begin
  inherited;
  fReference := AMFReadInt32(Stream);
end;

procedure TAMFReference.SaveToStream(Stream: TStream);
begin
  inherited;
  AMFWriteInt32(Stream, fReference);
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
  AMFReadInt32(Stream); // Discard the length of array
  inherited;
end;

procedure TAMFEcmaArray.SaveHeaderToStream(Stream: TStream);
begin
  inherited;
  AMFWriteInt32(Stream, Count);
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
  n := AMFReadInt32(Stream);
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
  AMFWriteInt32(Stream, Count);
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
  TimeZone := AMFReadInt16(Stream);
end;

procedure TAMFDate.SaveToStream(Stream: TStream);
begin
  inherited;
  AMFWriteInt16(Stream, TimeZone);
end;

{ TAMFLongString }

procedure TAMFLongString.LoadFromStream(Stream: TStream);
begin
  inherited;
  Native := AMFReadLongUtf(Stream);
end;

{ TAMFTypedObject }

function TAMFTypedObject.DataType: byte;
begin
  Result := AMF_TYPED_OBJECT;
end;

procedure TAMFTypedObject.LoadFromStream(Stream: TStream);
begin
  TypeIdentifier := AMFReadUtf(Stream);
  inherited;
end;

procedure TAMFTypedObject.SaveHeaderToStream(Stream: TStream);
begin
  inherited;
  AMFWriteUtf(Stream, TypeIdentifier);
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

function TAMFPacket.HasBody(BodyIndex: integer): boolean;
var n: integer;
begin
  Result := False;
  n := Length(Body);
  if n > 0 then
    if BodyIndex < 0 then
      Result := True
    else if n > BodyIndex then
      if Body[BodyIndex].Content <> nil then
        Result := True;
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
  n := AMFReadInt16(Stream);
  SetLength(fHeader, n);
  for i := 0 to Pred(n) do
    begin
    Hdr.Name := AMFReadUtf(Stream);
    Stream.ReadBuffer(B1, Sizeof(B1));
    Hdr.Required := B1 <> 0;
    AMFReadInt32(Stream); // Ignore length
    Hdr.Content := TAMFItem.CreateFromStream(Stream);
    Header[i] := Hdr;
    end;
  n := AMFReadInt16(Stream);
  SetLength(fBody, n);
  for i := 0 to Pred(n) do
    begin
    Bdy.Target := AMFReadUtf(Stream);
    Bdy.Response := AMFReadUtf(Stream);
    AMFReadInt32(Stream); // Ignore length
    Bdy.Content := TAMFItem.CreateFromStream(Stream);
    Body[i] := Bdy;
    end;
end;

procedure TAMFPacket.LoadFromString(const Data: AnsiString);
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    //Stream.ReadBuffer((@(Data[1]))^, Length(Data));
    Stream.Size := Length(Data);
    Move(Data[1], Stream.Memory^, Length(Data));
    Stream.Position := 0;
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
  AMFWriteInt16(Stream, Length(Header));
  for i := 0 to Pred(Length(Header)) do
    begin
    AMFWriteUtf(Stream, Header[i].Name);
    if Header[i].Required then
      B1 := 1
    else
      B1 := 0;
    Stream.WriteBuffer(B1, Sizeof(B1));
    {$IFNDEF NONSEEKABLESTREAMS}
    LengthPosition := Stream.Position;
    {$ENDIF}
    AMFWriteInt32(Stream, 0); // Invalid value
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyStartPosition := Stream.Position;
    {$ENDIF}
    Header[i].Content.SaveToStream(Stream);
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyEndPosition := Stream.Position;
    Stream.Position := LengthPosition;
    AMFWriteInt32(Stream, BodyEndPosition - BodyStartPosition);
    Stream.Position := BodyEndPosition;
    {$ENDIF}
    end;
  AMFWriteInt16(Stream, Length(Body));
  for i := 0 to Pred(Length(Body)) do
    begin
    AMFWriteUtf(Stream, Body[i].Target);
    AMFWriteUtf(Stream, Body[i].Response);
    {$IFNDEF NONSEEKABLESTREAMS}
    LengthPosition := Stream.Position;
    {$ENDIF}
    AMFWriteInt32(Stream, 0); // Invalid value
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyStartPosition := Stream.Position;
    {$ENDIF}
    Body[i].Content.SaveToStream(Stream);
    {$IFNDEF NONSEEKABLESTREAMS}
    BodyEndPosition := Stream.Position;
    Stream.Position := LengthPosition;
    AMFWriteInt32(Stream, BodyEndPosition - BodyStartPosition);
    Stream.Position := BodyEndPosition;
    {$ENDIF}
    end;
end;

procedure TAMFPacket.SaveToString(out Data: AnsiString);
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    SetLength(Data, Stream.Size);
    Stream.Seek(0, 0);
    Stream.ReadBuffer(Data[1], Stream.Size);
  finally
    Stream.Free;
    end;
end;

end.
