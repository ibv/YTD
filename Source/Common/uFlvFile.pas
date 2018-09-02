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

unit uFlvFile;
{$INCLUDE 'ytd.inc'}

// Note: Very much incomplete as yet! I did only those parts that I needed.


{$IFNDEF SIMPLIFIEDHDSDOWNLOADER}
  'This is not working yet. Particularly, the packet processing in'
  'TFlvFile.WriteDataPacket is incorrect (I don''t write the correct'
  'number of bytes from a packet.'
{$ENDIF}

interface

uses
  SysUtils, Classes, Windows,
  uCompatibility;

type
  TFlvFile = class
    private
      fStream: TStream;
      fOwnsStream: boolean;
      fHeader: AnsiString;
      fMetadata: AnsiString;
      {$IFNDEF SIMPLIFIEDHDSDOWNLOADER}
      fBaseTimestamp: Int64;
      fPreviousAudioTimestamp: Int64;
      fAACHeaderWritten: boolean;
      fPreviousAACHeader: boolean;
      fPreviousVideoTimestamp: Int64;
      fAVCHeaderWritten: boolean;
      fPreviousAVCHeader: boolean;
      {$ENDIF}
    protected
      property Stream: TStream read fStream;
      property OwnsStream: boolean read fOwnsStream;
    public
      constructor Create;
      destructor Destroy; override;
      procedure CreateNewFile(AStream: TStream; AOwnsStream: boolean = False);
      procedure WriteMetadata(const AMetadata: AnsiString);
      function WriteDataPacket(const AFlvData: AnsiString): boolean;
      procedure Flush;
    end;

type
  THDSSegmentInfo = class;
  THDSSegmentGroup = class;
  THDSFragmentInfo = class;
  THDSFragmentGroup = class;

  THDSMediaInfo = class
    private
      fBootstrap: AnsiString;
      fVersion: Byte;
      fFlags: DWORD;
      fBootstrapVersion: DWORD;
      fTimescale: DWORD;
      fCurrentMediaTime: UInt64;
      fSmtpeTimeCodeOffset: UInt64;
      fMovieID: string;
      fProfile: Byte;
      fLive: boolean;
      fUpdate: boolean;
      fServerEntries: TStringList;
      fQualityEntries: TStringList;
      fDRMData: string;
      fMetadata: string;
      fSegmentGroupList: TList;
      fSegmentList: TList;
      fFragmentGroupList: TList;
      fFragmentList: TList;
    private
      function GetFragment(Index: integer): THDSFragmentInfo;
      function GetFragmentCount: integer;
      function GetFragmentGroup(Index: integer): THDSFragmentGroup;
      function GetFragmentGroupCount: integer;
      function GetSegment(Index: integer): THDSSegmentInfo;
      function GetSegmentCount: integer;
      function GetSegmentGroup(Index: integer): THDSSegmentGroup;
      function GetSegmentGroupCount: integer;
      function GetNumberOfFragments: DWORD;
    protected
      function ParseSegmentBox(var BoxData: PAnsiChar; var BoxSize: integer): boolean;
      function ParseFragmentBox(var BoxData: PAnsiChar; var BoxSize: integer): boolean;
      property SegmentGroupList: TList read fSegmentGroupList;
      property SegmentList: TList read fSegmentList;
      property FragmentGroupList: TList read fFragmentGroupList;
      property FragmentList: TList read fFragmentList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      function Parse: boolean;
      function ParseHeader(var BoxData: PAnsiChar; var BoxSize: integer; out BoxType: string; out PayloadSize: UInt64): boolean; overload;
      function ParseHeader(var BoxData: PAnsiChar; var BoxSize: integer; out BoxType: string): boolean; overload;
      procedure Sort;
      property Bootstrap: AnsiString read fBootstrap write fBootstrap;
      property Version: Byte read fVersion;
      property Flags: DWORD read fFlags;
      property BootstrapVersion: DWORD read fBootstrapVersion;
      property Timescale: DWORD read fTimescale;
      property CurrentMediaTime: UInt64 read fCurrentMediaTime;
      property SmtpeTimeCodeOffset: UInt64 read fSmtpeTimeCodeOffset;
      property MovieID: string read fMovieID;
      property Profile: Byte read fProfile;
      property Live: boolean read fLive;
      property Update: boolean read fUpdate;
      property ServerEntries: TStringList read fServerEntries;
      property QualityEntries: TStringList read fQualityEntries;
      property DRMData: string read fDRMData;
      property Metadata: string read fMetadata;
      property SegmentGroupCount: integer read GetSegmentGroupCount;
      property SegmentGroups[Index: integer]: THDSSegmentGroup read GetSegmentGroup;
      property SegmentCount: integer read GetSegmentCount;
      property Segments[Index: integer]: THDSSegmentInfo read GetSegment;
      property FragmentGroupCount: integer read GetFragmentGroupCount;
      property FragmentGroups[Index: integer]: THDSFragmentGroup read GetFragmentGroup;
      property FragmentCount: integer read GetFragmentCount;
      property Fragments[Index: integer]: THDSFragmentInfo read GetFragment;
      property NumberOfFragments: DWORD read GetNumberOfFragments;
    end;

  THDSSegmentGroup = class
    private
      fVersion: Byte;
      fFlags: DWORD;
      fQualityUrlModifiers: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      property Version: Byte read fVersion;
      property Flags: DWORD read fFlags;
      property QualityUrlModifiers: TStringList read fQualityUrlModifiers;
    end;

  THDSSegmentInfo = class
    private
      fFirstFragment: DWORD;
      fFragmentsPerSegment: DWORD;
      fGroup: THDSSegmentGroup;
    protected
    public
      constructor Create(AFirstFragment, AFragmentsPerSegment: DWORD; AGroup: THDSSegmentGroup);
      destructor Destroy; override;
      property FirstFragment: DWORD read fFirstFragment;
      property FragmentsPerSegment: DWORD read fFragmentsPerSegment;
      property Group: THDSSegmentGroup read fGroup;
    end;

  THDSFragmentGroup = class
    private
      fVersion: Byte;
      fFlags: DWORD;
      fTimescale: DWORD;
      fQualityUrlModifiers: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      property Version: Byte read fVersion;
      property Flags: DWORD read fFlags;
      property Timescale: DWORD read fTimescale;
      property QualityUrlModifiers: TStringList read fQualityUrlModifiers;
    end;

    THDSFragmentInfo = class
    private
      fFragment: DWORD;
      fFragmentTimestamp: UInt64;
      fDuration: DWORD;
      fDiscontinuityIndicator: Byte;
      fGroup: THDSFragmentGroup;
    protected
    public
      constructor Create(AFragment: DWORD; ATimestamp: UInt64; ADuration: DWORD; ADiscontinuityIndicator: Byte; AGroup: THDSFragmentGroup);
      destructor Destroy; override;
      property Fragment: DWORD read fFragment;
      property FragmentTimestamp: UInt64 read fFragmentTimestamp;
      property Duration: DWORD read fDuration;
      property DiscontinuityIndicator: Byte read fDiscontinuityIndicator;
      property Group: THDSFragmentGroup read fGroup;
    end;

function FLVReadByte(var Data: PAnsiChar; var DataSize: integer; out Value: Byte): boolean;
function FLVReadInt24(var Data: PAnsiChar; var DataSize: integer; out Value: DWORD): boolean;
function FLVReadInt32(var Data: PAnsiChar; var DataSize: integer; out Value: DWORD): boolean;
function FLVReadInt64(var Data: PAnsiChar; var DataSize: integer; out Value: UInt64): boolean;
function FLVReadFixedStr(var Data: PAnsiChar; var DataSize: integer; out Value: AnsiString; const ValueLength: integer): boolean; {$IFDEF UNICODE} overload; {$ENDIF}
function FLVReadVariableStr(var Data: PAnsiChar; var DataSize: integer; out Value: AnsiString): boolean; {$IFDEF UNICODE} overload; {$ENDIF}
{$IFDEF UNICODE}
function FLVReadFixedStr(var Data: PAnsiChar; var DataSize: integer; out Value: string; const ValueLength: integer): boolean; overload;
function FLVReadVariableStr(var Data: PAnsiChar; var DataSize: integer; out Value: string): boolean; {$IFDEF UNICODE} overload; {$ENDIF}
{$ENDIF}
function FLVReadMultipleVariableStr(var Data: PAnsiChar; var DataSize: integer; const Count: integer; Value: TStringList): boolean;

implementation

const
  FLV_AUDIO = $08;
  FLV_VIDEO = $09;
  FLV_SCRIPT_DATA = $12;
  FLV_FRAME_TYPE_INFO = $05;
  FLV_CODEC_ID_AVC = $07;
  FLV_CODEC_ID_AAC = $0a;
  FLV_AVC_SEQUENCE_HEADER = $00;
  FLV_AAC_SEQUENCE_HEADER = $00;
  FLV_AVC_SEQUENCE_END = $02;
  FLV_TIMECODE_DURATION = 8;

  DWORD_UNSET = $ffffffff;

function FixedStringFromMemory(const Data; Length: integer): AnsiString;
begin
  SetLength(Result, Length);
  if Length > 0 then
    Move(Data, Result[1], Length);
end;

function FLVReadInteger(var Data: PAnsiChar; var DataSize: integer; var Buffer; BufferSize: integer): boolean;
type
  TAnsiCharArray = array[0..65535] of AnsiChar;
begin
  Result := DataSize >= BufferSize;
  if Result then
    while BufferSize > 0 do
      begin
      Dec(BufferSize);
      TAnsiCharArray(Buffer)[BufferSize] := Data^;
      Inc(Data);
      Dec(DataSize);
      end;
end;

function FLVReadByte(var Data: PAnsiChar; var DataSize: integer; out Value: Byte): boolean;
begin
  Result := FLVReadInteger(Data, DataSize, Value, Sizeof(Value));
end;

function FLVReadInt24(var Data: PAnsiChar; var DataSize: integer; out Value: DWORD): boolean;
begin
  Result := FLVReadInteger(Data, DataSize, Value, 3);
  if Result then
    Value := Value and $00ffffff;
end;

function FLVReadInt32(var Data: PAnsiChar; var DataSize: integer; out Value: DWORD): boolean;
begin
  Result := FLVReadInteger(Data, DataSize, Value, Sizeof(Value));
end;

function FLVReadInt64(var Data: PAnsiChar; var DataSize: integer; out Value: UInt64): boolean;
begin
  Result := FLVReadInteger(Data, DataSize, Value, Sizeof(Value));
end;

function FLVReadFixedStr(var Data: PAnsiChar; var DataSize: integer; out Value: AnsiString; const ValueLength: integer): boolean;
begin
  Result := DataSize >= ValueLength;
  if Result and (ValueLength > 0) then
    begin
    Value := FixedStringFromMemory(Data^, ValueLength);
    Inc(Data, ValueLength);
    Dec(DataSize, ValueLength);
    end
  else
    Value := '';
end;

function FLVReadVariableStr(var Data: PAnsiChar; var DataSize: integer; out Value: AnsiString): boolean;
var
  P: PAnsiChar;
  Len: integer;
begin
  Result := DataSize >= 0;
  P := Data;
  Len := 0;
  while (DataSize > 0) do
    if Data^ = #0 then
      begin
      Inc(Data);
      Dec(DataSize);
      Break;
      end
    else
      begin
      Inc(Data);
      Dec(DataSize);
      end;
  if Len > 0 then
    Value := FixedStringFromMemory(P^, Len)
  else
    Value := '';
end;

{$IFDEF UNICODE}
function FLVReadFixedStr(var Data: PAnsiChar; var DataSize: integer; out Value: string; const ValueLength: integer): boolean;
var
  s: AnsiString;
begin
  Result := FLVReadFixedStr(Data, DataSize, s, ValueLength);
  if Result then
    Value := string(s);
end;

function FLVReadVariableStr(var Data: PAnsiChar; var DataSize: integer; out Value: string): boolean;
var
  s: AnsiString;
begin
  Result := FLVReadVariableStr(Data, DataSize, s);
  if Result then
    Value := string(s);
end;
{$ENDIF}

function FLVReadMultipleVariableStr(var Data: PAnsiChar; var DataSize: integer; const Count: integer; Value: TStringList): boolean;
var
  i: integer;
  s: string;
begin
  Result := True;
  for i := 1 to Count do
    if FLVReadVariableStr(Data, DataSize, s) then
      Value.Add(s)
    else
      begin
      Result := False;
      Break;
      end;
end;

function FLVWriteInteger(const Value; ValueSize: integer): AnsiString;
var
  P: PAnsiChar;
begin
  P := @Value;
  SetLength(Result, ValueSize);
  while ValueSize > 0 do
    begin
    Result[ValueSize] := P^;
    Dec(ValueSize);
    Inc(P);
    end;
end;

function FLVWriteByte(const Value: Byte): AnsiString;
begin
  Result := FLVWriteInteger(Value, Sizeof(Value));
end;

function FLVWriteInt24(const Value: DWORD): AnsiString;
begin
  Result := FLVWriteInteger(Value, 3);
end;

function FLVWriteInt32(const Value: DWORD): AnsiString;
begin
  Result := FLVWriteInteger(Value, Sizeof(Value));
end;

function FLVWriteTimestamp(const Value: DWORD): AnsiString;
begin
  Result := FLVWriteInt24(Value and $00ffffff) + FLVWriteByte(Value shr 24);
end;

{ TFlvFile }

constructor TFlvFile.Create;
begin
  inherited Create;
end;

destructor TFlvFile.Destroy;
begin
  Flush;
  if OwnsStream then
    FreeAndNil(fStream);
  inherited;
end;

procedure TFlvFile.Flush;
begin
  if fHeader <> '' then
    begin
    Stream.WriteBuffer(fHeader[1], Length(fHeader));
    fHeader := '';
    if fMetadata <> '' then
      begin
      Stream.WriteBuffer(fMetadata[1], Length(fMetadata));
      fMetadata := '';
      end;
    end;
end;

procedure TFlvFile.CreateNewFile(AStream: TStream; AOwnsStream: boolean);
begin
  fStream := AStream;
  fOwnsStream := AOwnsStream;
  {$IFDEF SIMPLIFIEDHDSDOWNLOADER}
  fHeader := #$46#$4c#$56#$01#$05#$00#$00#$00#$09#$00#$00#$00#$00;
  {$ELSE}
  fHeader := #$46#$4c#$56#$01#$00#$00#$00#$00#$09#$00#$00#$00#$00;
  {$ENDIF}
  fMetadata := '';
  {$IFNDEF SIMPLIFIEDHDSDOWNLOADER}
  fBaseTimestamp := DWORD_UNSET;
  fPreviousAudioTimestamp := 0;
  fAACHeaderWritten := False;
  fPreviousAACHeader := False;
  fPreviousVideoTimestamp := 0;
  fAVCHeaderWritten := False;
  fPreviousAVCHeader := False;
  {$ENDIF}
end;

procedure TFlvFile.WriteMetadata(const AMetadata: AnsiString);
{.$DEFINE NEEDS09ASLASTBYTEOFAMETADATA} // Unsure if this is needed. The PHP script does it.
var
  n: integer;
begin
  if AMetadata = '' then
    fMetadata := ''
  else
    begin
    n := Length(AMetadata);
    fMetadata := ''
       + FLVWriteByte(FLV_SCRIPT_DATA)
       + FLVWriteInt24(n)
       + FLVWriteInt24(0)
       + FLVWriteInt32(0)
       {$IFDEF NEEDS09ASLASTBYTEOFAMETADATA}
       + Copy(AMetadata, 1, Pred(n))
       + FLVWriteByte($09)
       {$ELSE}
       + AMetadata
       {$ENDIF}
       + FLVWriteInt32(n + 11) // 11 bytes before AMetadata
       ;
    end;
end;

function TFlvFile.WriteDataPacket(const AFlvData: AnsiString): boolean;
{$IFNDEF SIMPLIFIEDHDSDOWNLOADER}
var
  FlvPtr, PacketPayloadPtr: PAnsiChar;
  FlvSize, PacketPayloadSize: integer;
  PacketType, TimestampHigh: Byte;
  Timestamp, PacketSize: DWORD;
  Packet: AnsiString;
  FrameInfo, CodecID, AACPacketType, FrameType, AVCPacketType: Byte;
  Skip: boolean;
{$ENDIF}
begin
  {$IFNDEF SIMPLIFIEDHDSDOWNLOADER}
  if AFlvData = '' then
    begin
    Result := True;
    Exit;
    end;
  Result := False;
  FlvPtr := @(AFlvData[1]);
  FlvSize := Length(AFlvData);
  Packet := '';
  while FlvSize > 0 do
    begin
    if not (True
      and FLVReadByte(FlvPtr, FlvSize, PacketType)
      and FLVReadInt24(FlvPtr, FlvSize, PacketSize)
      and FLVReadInt24(FlvPtr, FlvSize, Timestamp)
      and FLVReadByte(FlvPtr, FlvSize, TimestampHigh)
    ) then
      Exit;
    PacketPayloadPtr := FlvPtr;
    PacketPayloadSize := FlvSize;
    Timestamp := (Timestamp or (DWORD(TimestampHigh) shl 24)) and $7fffffff;
    if (fBaseTimestamp = DWORD_UNSET) and (PacketType in [FLV_AUDIO, FLV_VIDEO]) then
      fBaseTimestamp := Timestamp;
    if (fBaseTimestamp > 1000) then
      Timestamp := Timestamp - fBaseTimestamp;
    if PacketSize > 0 then
      case PacketType of
        FLV_AUDIO:
          begin
            if fHeader <> '' then
              fHeader[5] := AnsiChar(Byte(fHeader[5]) or 1);
            if Timestamp > (fPreviousAudioTimestamp - FLV_TIMECODE_DURATION*5) then
              begin
              if not FLVReadByte(FlvPtr, FlvSize, FrameInfo) then
                Exit;
              CodecID := (FrameInfo and $f0) shr 4;
              Skip := False;
              if CodecID = FLV_CODEC_ID_AAC then
                begin
                if not FLVReadByte(FlvPtr, FlvSize, AACPacketType) then
                  Exit;
                if AACPacketType = FLV_AAC_SEQUENCE_HEADER then
                  if fAACHeaderWritten then
                    Skip := True // Nothing to do - header already written
                  else
                    begin
                    fAACHeaderWritten := True;
                    fPreviousAACHeader := True;
                    end
                else
                  if not fAACHeaderWritten then
                    Skip := True; // Can't write AAC packet before the sequence header
                if not Skip then
                  begin
                  if (not fPreviousAACHeader) and (Timestamp <= fPreviousAudioTimestamp) then
                    // Fix the timestamp if older than the previous one
                    Timestamp := FLV_TIMECODE_DURATION + fPreviousAudioTimestamp;
                  if (CodecID = FLV_CODEC_ID_AAC) and (AACPacketType <> FLV_AAC_SEQUENCE_HEADER) then
                    fPreviousAACHeader := False;
                  Packet := Packet
                    + FLVWriteByte(PacketType)
                    + FLVWriteInt24(PacketSize)
                    + FLVWriteTimestamp(Timestamp)
                    + FixedStringFromMemory(PacketPayloadPtr, PacketPayloadSize);
                  fPreviousAudioTimestamp := Timestamp;
                  end;
                end;
              end
            else
              ; // Nothing to do: Skipping the packet
          end;
        FLV_VIDEO:
          begin
            if fHeader <> '' then
              fHeader[5] := AnsiChar(Byte(fHeader[5]) or 4);
            if Timestamp > (fPreviousVideoTimestamp - FLV_TIMECODE_DURATION*5) then
              begin
              if not FLVReadByte(FlvPtr, FlvSize, FrameInfo) then
                Exit;
              FrameType := (FrameInfo and $f0) shr 4;
              CodecID := FrameInfo and $0f;
              Skip := False;
              if FrameType = FLV_FRAME_TYPE_INFO then
                Skip := True
              else
                if CodecID = FLV_CODEC_ID_AVC then
                  begin
                  if not FLVReadByte(FlvPtr, FlvSize, AVCPacketType) then
                    Exit;
                  if AVCPacketType = FLV_AVC_SEQUENCE_HEADER then
                    if fAVCHeaderWritten then
                      Skip := True // Nothing to do - header already written
                    else
                      begin
                      fAVCHeaderWritten := True;
                      fPreviousAVCHeader := True;
                      end
                  else
                    if not fAVCHeaderWritten then
                      Skip := True; // Can't write AVC packet before the sequence header
                  end;
              if not Skip then
                begin
                if (not fPreviousAVCHeader) and (Timestamp <= fPreviousVideoTimestamp) and (CodecID = FLV_CODEC_ID_AVC) and (AVCPacketType <> FLV_AVC_SEQUENCE_END) then
                  // Fix the timestamp if older than the previous one
                  Timestamp := FLV_TIMECODE_DURATION + fPreviousVideoTimestamp;
                if (CodecID = FLV_CODEC_ID_AVC) and (AVCPacketType <> FLV_AVC_SEQUENCE_HEADER) then
                  fPreviousAVCHeader := False;
                Packet := Packet
                  + FLVWriteByte(PacketType)
                  + FLVWriteInt24(PacketSize)
                  + FLVWriteTimestamp(Timestamp)
                  + FixedStringFromMemory(PacketPayloadPtr, PacketSize);
                fPreviousVideoTimestamp := Timestamp;
                end;
              end
            else
              ; // Nothing to do: Skipping the packet
          end;
        FLV_SCRIPT_DATA:
          begin
          end;
        else
          Exit; // Unknown packet type
        end;
      FlvPtr := PacketPayloadPtr; Inc(FlvPtr, PacketSize);
      FlvSize := PacketPayloadSize; Dec(FlvSize, PacketSize);
      end;
  {$ENDIF}
  if {$IFDEF SIMPLIFIEDHDSDOWNLOADER} AFlvData {$ELSE} Packet {$ENDIF} <> '' then
    begin
    Flush;
    Stream.WriteBuffer( {$IFDEF SIMPLIFIEDHDSDOWNLOADER} AFlvData[1] {$ELSE} Packet[1] {$ENDIF} , Length( {$IFDEF SIMPLIFIEDHDSDOWNLOADER} AFlvData {$ELSE} Packet {$ENDIF} ));
    end;
  Result := True;
end;

{ THDSMediaInfo }

constructor THDSMediaInfo.Create;
begin
  inherited Create;
  fServerEntries := TStringList.Create;
  fQualityEntries := TStringList.Create;
  fSegmentGroupList := TList.Create;
  fSegmentList := TList.Create;
  fFragmentGroupList := TList.Create;
  fFragmentList := TList.Create;
end;

destructor THDSMediaInfo.Destroy;
begin
  Clear;
  FreeAndNil(fServerEntries);
  FreeAndNil(fQualityEntries);
  FreeAndNil(fSegmentGroupList);
  FreeAndNil(fSegmentList);
  FreeAndNil(fFragmentGroupList);
  FreeAndNil(fFragmentList);
  inherited;
end;

procedure THDSMediaInfo.Clear;
var
  i: integer;
begin
  fVersion := 0;
  fFlags := 0;
  fBootstrapVersion := 0;
  fTimescale := 0;
  fCurrentMediaTime := 0;
  fSmtpeTimeCodeOffset := 0;
  fMovieID := '';
  fProfile := 0;
  fLive := False;
  fUpdate := False;
  fServerEntries.Clear;
  fQualityEntries.Clear;
  for i := 0 to Pred(SegmentCount) do
    Segments[i].Free;
  fSegmentList.Clear;
  for i := 0 to Pred(FragmentCount) do
    Fragments[i].Free;
  fFragmentList.Clear;
end;

function THDSMediaInfo.GetSegmentGroupCount: integer;
begin
  Result := SegmentGroupList.Count;
end;

function THDSMediaInfo.GetSegmentGroup(Index: integer): THDSSegmentGroup;
begin
  Result := THDSSegmentGroup(SegmentGroupList[Index]);
end;

function THDSMediaInfo.GetSegmentCount: integer;
begin
  Result := SegmentList.Count;
end;

function THDSMediaInfo.GetSegment(Index: integer): THDSSegmentInfo;
begin
  Result := THDSSegmentInfo(SegmentList[Index]);
end;

function THDSMediaInfo.GetFragmentGroupCount: integer;
begin
  Result := FragmentGroupList.Count;
end;

function THDSMediaInfo.GetFragmentGroup(Index: integer): THDSFragmentGroup;
begin
  Result := THDSFragmentGroup(FragmentGroupList[Index]);
end;

function THDSMediaInfo.GetFragmentCount: integer;
begin
  Result := FragmentList.Count;
end;

function THDSMediaInfo.GetFragment(Index: integer): THDSFragmentInfo;
begin
  Result := THDSFragmentInfo(FragmentList[Index]);
end;

function THDSMediaInfo.ParseHeader(var BoxData: PAnsiChar; var BoxSize: integer; out BoxType: string; out PayloadSize: UInt64): boolean;
var
  Size32: DWORD;
  Size64: UInt64;
  BoxTypeA: AnsiString;
begin
  // Note: It may prove to be necessary to do something with the Size32/Size64.
  Result := False;
  if FLVReadInt32(BoxData, BoxSize, Size32) then
    if FLVReadFixedStr(BoxData, BoxSize, BoxTypeA, 4) then
      begin
      BoxType := {$IFDEF UNICODE} string {$ENDIF} (BoxTypeA);
      if Size32 = 1 then
        begin
        Result := FLVReadInt64(BoxData, BoxSize, Size64);
        PayloadSize := Size64 - 16;
        end
      else
        begin
        PayloadSize := Size32 - 8;
        Result := True;
        end;
      end;
end;

function THDSMediaInfo.ParseHeader(var BoxData: PAnsiChar; var BoxSize: integer; out BoxType: string): boolean;
var
  Dummy: UInt64;
begin
  Result := ParseHeader(BoxData, BoxSize, BoxType, Dummy);
end;

function THDSMediaInfo.Parse: boolean;
var
  BootstrapPtr: PAnsiChar;
  BootstrapSize: integer;
  BoxType: string;
  ProfileAndFlags, ServerEntryCount, QualityEntryCount, SegmentCount, FragmentCount: Byte;
  i: integer;
begin
  Result := False;
  Clear;
  if fBootstrap = '' then
    Exit;
  BootstrapPtr := @(fBootstrap[1]);
  BootstrapSize := Length(fBootstrap);
  if not ParseHeader(BootstrapPtr, BootstrapSize, BoxType) then
    Exit;
  if BoxType <> 'abst' then
    Exit;
  if not (True
    and FLVReadByte(BootstrapPtr, BootstrapSize, fVersion)
    and FLVReadInt24(BootstrapPtr, BootstrapSize, fFlags)
    and FLVReadInt32(BootstrapPtr, BootstrapSize, fBootstrapVersion)
    and FLVReadByte(BootstrapPtr, BootstrapSize, ProfileAndFlags)
    and FLVReadInt32(BootstrapPtr, BootstrapSize, fTimescale)
    and FLVReadInt64(BootstrapPtr, BootstrapSize, fCurrentMediaTime)
    and FLVReadInt64(BootstrapPtr, BootstrapSize, fSmtpeTimeCodeOffset)
    and FLVReadVariableStr(BootstrapPtr, BootstrapSize, fMovieID)
    and FLVReadByte(BootstrapPtr, BootstrapSize, ServerEntryCount)
    and FLVReadMultipleVariableStr(BootstrapPtr, BootstrapSize, ServerEntryCount, ServerEntries)
    and FLVReadByte(BootstrapPtr, BootstrapSize, QualityEntryCount)
    and FLVReadMultipleVariableStr(BootstrapPtr, BootstrapSize, QualityEntryCount, QualityEntries)
    and FLVReadVariableStr(BootstrapPtr, BootstrapSize, fDRMData)
    and FLVReadVariableStr(BootstrapPtr, BootstrapSize, fMetadata)
    and FLVReadByte(BootstrapPtr, BootstrapSize, SegmentCount)
  ) then
    Exit;
  fProfile := (ProfileAndFlags and $c0) shr 6;
  fLive := Bytebool(ProfileAndFlags and $20);
  fUpdate := Bytebool(ProfileAndFlags and $10);
  for i := 1 to SegmentCount do
    if not ParseSegmentBox(BootstrapPtr, BootstrapSize) then
      Exit;
  if not FLVReadByte(BootstrapPtr, BootstrapSize, FragmentCount) then
    Exit;
  for i := 1 to FragmentCount do
    if not ParseFragmentBox(BootstrapPtr, BootstrapSize) then
      Exit;
  Result := True;
end;

function THDSMediaInfo.ParseSegmentBox(var BoxData: PAnsiChar; var BoxSize: integer): boolean;
var
  BoxType: string;
  QualityUrlModifiersCount: Byte;
  SegmentCount, FirstSegment, FragmentsPerSegment: DWORD;
  Group: THDSSegmentGroup;
begin
  Result := False;
  if not ParseHeader(BoxData, BoxSize, BoxType) then
    Exit;
  if BoxType <> 'asrt' then
    Exit;
  Group := THDSSegmentGroup.Create;
  SegmentGroupList.Add(Group);
  if not (True
    and FLVReadByte(BoxData, BoxSize, Group.fVersion)
    and FLVReadInt24(BoxData, BoxSize, Group.fFlags)
    and FLVReadByte(BoxData, BoxSize, QualityUrlModifiersCount)
    and FLVReadMultipleVariableStr(BoxData, BoxSize, QualityUrlModifiersCount, Group.QualityUrlModifiers)
    and FLVReadInt32(BoxData, BoxSize, SegmentCount)
  ) then
    Exit;
  while SegmentCount > 0 do
    if not (True
      and FLVReadInt32(BoxData, BoxSize, FirstSegment)
      and FLVReadInt32(BoxData, BoxSize, FragmentsPerSegment)
    ) then
      Exit
    else
      begin
      SegmentList.Add(THDSSegmentInfo.Create(FirstSegment, FragmentsPerSegment, Group));
      Dec(SegmentCount);
      end;
  Result := True;
end;

function THDSMediaInfo.ParseFragmentBox(var BoxData: PAnsiChar; var BoxSize: integer): boolean;
var
  BoxType: string;
  QualityUrlModifiersCount: Byte;
  FragmentCount, FirstFragment, Duration: DWORD;
  Timestamp: UInt64;
  DiscontinuityIndicator: Byte;
  Group: THDSFragmentGroup;
begin
  Result := False;
  if not ParseHeader(BoxData, BoxSize, BoxType) then
    Exit;
  if BoxType <> 'afrt' then
    Exit;
  Group := THDSFragmentGroup.Create;
  FragmentGroupList.Add(Group);
  if not (True
    and FLVReadByte(BoxData, BoxSize, Group.fVersion)
    and FLVReadInt24(BoxData, BoxSize, Group.fFlags)
    and FLVReadInt32(BoxData, BoxSize, Group.fTimescale)
    and FLVReadByte(BoxData, BoxSize, QualityUrlModifiersCount)
    and FLVReadMultipleVariableStr(BoxData, BoxSize, QualityUrlModifiersCount, Group.QualityUrlModifiers)
    and FLVReadInt32(BoxData, BoxSize, FragmentCount)
  ) then
    Exit;
  while FragmentCount > 0 do
    if not (True
      and FLVReadInt32(BoxData, BoxSize, FirstFragment)
      and FLVReadInt64(BoxData, BoxSize, Timestamp)
      and FLVReadInt32(BoxData, BoxSize, Duration)
    ) then
      Exit
    else
      begin
      if Duration <> 0 then
        DiscontinuityIndicator := 0
      else if not FLVReadByte(BoxData, BoxSize, DiscontinuityIndicator) then
        Exit;
      // This IF may in fact be wrong
      if FirstFragment <> 0 then
        FragmentList.Add(THDSFragmentInfo.Create(FirstFragment, Timestamp, Duration, DiscontinuityIndicator, Group));
      Dec(FragmentCount);
      end;
  Result := True;
end;

function SegmentSorter(Item1, Item2: Pointer): Integer;
var
  Seg1, Seg2: THDSSegmentInfo;
begin
  Seg1 := THDSSegmentInfo(Item1);
  Seg2 := THDSSegmentInfo(Item2);
  if Seg1.FirstFragment = Seg2.FirstFragment then
    if Seg1.FragmentsPerSegment = Seg2.FragmentsPerSegment then
      Result := 0
    else
      if Seg1.FragmentsPerSegment < Seg2.FragmentsPerSegment then
        Result := -1
      else
        Result := 1
  else
    if Seg1.FirstFragment < Seg2.FirstFragment then
      Result := -1
    else
      Result := 1;
end;

function FragmentSorter(Item1, Item2: Pointer): Integer;
var
  Frag1, Frag2: THDSFragmentInfo;
begin
  // Note: Maybe Timestamp would be better than Fragment?
  Frag1 := THDSFragmentInfo(Item1);
  Frag2 := THDSFragmentInfo(Item2);
  if Frag1.Fragment = Frag2.Fragment then
    Result := 0
  else
    if Frag1.Fragment < Frag2.Fragment then
      Result := -1
    else
      Result := 1;
end;

procedure THDSMediaInfo.Sort;
begin
  SegmentList.Sort(SegmentSorter);
  FragmentList.Sort(FragmentSorter);
end;

function THDSMediaInfo.GetNumberOfFragments: DWORD;
var
  i: integer;
  Frag: DWORD;
begin
  Result := 0;
  for i := 0 to Pred(SegmentCount) do
    begin
    Frag := Segments[i].FirstFragment + Segments[i].FragmentsPerSegment;
    if Frag > Result then
      Result := Frag;
    end;
end;

{ THDSSegmentGroup }

constructor THDSSegmentGroup.Create;
begin
  inherited Create;
  fQualityUrlModifiers := TStringList.Create;
end;

destructor THDSSegmentGroup.Destroy;
begin
  FreeAndNil(fQualityUrlModifiers);
  inherited;
end;

{ THDSSegmentInfo }

constructor THDSSegmentInfo.Create(AFirstFragment, AFragmentsPerSegment: DWORD; AGroup: THDSSegmentGroup);
begin
  inherited Create;
  fFirstFragment := AFirstFragment;
  fFragmentsPerSegment := AFragmentsPerSegment;
  fGroup := AGroup;
end;

destructor THDSSegmentInfo.Destroy;
begin
  inherited;
end;

{ THDSFragmentGroup }

constructor THDSFragmentGroup.Create;
begin
  inherited Create;
  fQualityUrlModifiers := TStringList.Create;
end;

destructor THDSFragmentGroup.Destroy;
begin
  FreeAndNil(fQualityUrlModifiers);
  inherited;
end;

{ THDSFragmentInfo }

constructor THDSFragmentInfo.Create(AFragment: DWORD; ATimestamp: UInt64; ADuration: DWORD; ADiscontinuityIndicator: Byte; AGroup: THDSFragmentGroup);
begin
  inherited Create;
  fFragment := AFragment;
  fFragmentTimestamp := ATimestamp;
  fDuration := ADuration;
  fDiscontinuityIndicator := ADiscontinuityIndicator;
  fGroup := AGroup;
end;

destructor THDSFragmentInfo.Destroy;
begin
  inherited;
end;

end.
