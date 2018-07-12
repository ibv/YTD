{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    This code provide some types and object for build dxgettext with
    FreePascal 1.9.x (previous version not support the WideString)

  link:
    http://dybdahl.dk/dxgettext/
    http://www.freepascal.org

  History
   jan  06 2004 - Create this header
------------------------------------------------------------------------------}

{$MODE DELPHI}{$H+}

unit gnugettext_fpc;

interface

uses
  Classes, SysUtils,SyncObjs,TypInfo;

type
{$IFDEF VER1_0}
  Sorry this code it's not compatible with your FreePascal version.
{$ENDIF}

  TCSStringList= Class(TStringList)
  private
    fCaseSensitive : Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
    
  public
    constructor Create;
    function Find(const S: string; out Index: Integer): Boolean; override;

    property CaseSensitive : Boolean read fCaseSensitive write SetCaseSensitive;
  end;
  
  TSimpleRWSync = class(TObject)
  private
    fLock: TCriticalSection;
    
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

 TMultiReadExclusiveWriteSynchronizer = TSimpleRWSync;
 
 {Code find in gnugettextD5.pas}
 function Utf8Decode(const S: UTF8String): WideString;
 function Utf8Encode(const WS: WideString): UTF8String;

 function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
 function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;

implementation

function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := GetPropList(PTypeInfo(AObject.ClassInfo), PropList);
end;

{ TSimpleRWSync }

constructor TSimpleRWSync.Create;
begin
  inherited Create;
  fLock:=TCriticalSection.Create;
end;

destructor TSimpleRWSync.Destroy;
begin
  fLock.Free;
  inherited Destroy;
end;

function TSimpleRWSync.BeginWrite: Boolean;
begin
  fLock.Enter;
  Result := True;
end;

procedure TSimpleRWSync.EndWrite;
begin
  fLock.Leave;
end;

procedure TSimpleRWSync.BeginRead;
begin
  fLock.Enter;
end;

procedure TSimpleRWSync.EndRead;
begin
  fLock.Leave;
end;


function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;
  count := 0;
  i := 0;
  if Dest <> nil then begin
    while (i < SourceChars) and (count < MaxDestBytes) do begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then begin
        Dest[count] := Char(c);
        Inc(count);
      end else
      if c > $7FF then begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count + 1] := Char($80 or ((c shr 6) and $3F));
        Dest[count + 2] := Char($80 or (c and $3F));
        Inc(count, 3);
      end else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count + 1] := Char($80 or (c and $3F));
        Inc(count, 2);
      end;
    end;
    if count >= MaxDestBytes then
      count := MaxDestBytes - 1;
    Dest[count] := #0;
  end else begin
    while i < SourceChars do begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count + 1; // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then begin
    while (i < SourceBytes) and (count < MaxDestChars) do begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then
      count := MaxDestChars - 1;
    Dest[count] := #0;
  end else begin
    while (i < SourceBytes) do begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count + 1;
end;

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp) + 1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then
    Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp) + 1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

{ TCSStringList }
constructor TCSStringList.Create;
begin
  inherited Create;
  fCaseSensitive:=False;
end;

procedure TCSStringList.SetCaseSensitive(const AValue: Boolean);
begin
  if AValue<>fCaseSensitive then
    fCaseSensitive:=AValue;
end;


function TCSStringList.Find(const S: string; out Index: Integer): Boolean;
{ Searches for the first string <= S, returns True if exact match,
  sets index to the index f the found string. }
Var I,L,R,Temp : Longint;

begin
  Result:=False;
  // Use binary search.
  L:=0;
  R:=Count-1;
  While L<=R do
  begin
    I:=(L+R) div 2;
    if fCaseSensitive then
      Temp:=AnsiCompareStr(Get(i),S)
    else
      Temp:=AnsiCompareText(Get(i),S);
    If Temp<0 then
      L:=I+1
    else
    begin
      R:=I-1;
      If Temp=0 then
      begin
        Result:=True;
        If Duplicates<>DupAccept then L:=I;
      end;
    end;
  end;
  Index:=L;
end;

end.

