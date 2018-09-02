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

unit uXml;
{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, Classes,
  uCompatibility,
  NativeXml;

type
  TXmlString = UTF8String;

  TXmlNode = NativeXml.TXmlNode;

  TXmlDoc = class(TNativeXml)
  private
    protected
      procedure LoadFromBinaryData(Data: Pointer; Length: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    public
      procedure LoadFromBinaryString(const Xml: AnsiString); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function SaveToBinaryString: AnsiString; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function NodeByPath(const Path: string; out Node: TXmlNode): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function NodeByPathAndAttr(const Path, AttributeName, AttributeValue: string; out Node: TXmlNode): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ValueByPath(const Path: string; const Default: string = ''): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetIndentation(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property Xml: AnsiString read SaveToBinaryString write LoadFromBinaryString;
    end;

function XmlValueIncludingCData(Node: TXmlNode): string;
function XmlAttribute(Node: TXmlNode; const Attribute: string; out Value: string): boolean; overload;
function XmlAttribute(Node: TXmlNode; const Attribute: string): string; overload;
function XmlNodeByID(Node: TXmlNode; const ID: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByID(Node: TXmlDoc; const ID: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPath(Node: TXmlNode; const Path: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPath(Node: TXmlDoc; const Path: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathAndAttr(Node: TXmlNode; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathAndAttr(Node: TXmlDoc; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathCreate(Node: TXmlNode; const Path: string): TXmlNode; overload;
function XmlNodeByPathCreate(Node: TXmlDoc; const Path: string): TXmlNode; overload;
function XmlValueByPath(Node: TXmlNode; const Path: string; out Value: string): boolean; overload;
function XmlValueByPath(Node: TXmlNode; const Path: string): string; overload;
function XmlValueByPath(Node: TXmlDoc; const Path: string; out Value: string): boolean; overload;
function XmlValueByPath(Node: TXmlDoc; const Path: string): string; overload;
function XmlGetNamespace(Node: TXmlNode): string; overload;
function XmlGetNamespace(Node: TXmlDoc): string; overload;

implementation

type
  THackXmlNode = class(TXmlNode);

function XmlValueIncludingCData(Node: TXmlNode): string;

  function UnicodeStringOrMalformedUtf8(Node: TXmlNode): string;
    begin
      Result := Node.ValueAsUnicodeString;
      if (Result = '') and (Node.ValueAsString <> '') then
        Result := string(Node.ValueAsString);
    end;

var CData: TXmlNode;
begin
  if Node = nil then
    Result := ''
  else
    begin
    CData := Node.NodeByElementType(xeCData);
    if CData = nil then
      Result := Trim(UnicodeStringOrMalformedUtf8(Node))
    else
      Result := UnicodeStringOrMalformedUtf8(CData);
    end;
end;

function XmlAttribute(Node: TXmlNode; const Attribute: string; out Value: string): boolean;
begin
  Result := Node.HasAttribute( {$IFDEF UNICODE} TXmlString {$ENDIF} (Attribute));
  if Result then
    Value := Node.AttributeByNameWide[ {$IFDEF UNICODE} TXmlString {$ENDIF} (Attribute)];
end;

function XmlAttribute(Node: TXmlNode; const Attribute: string): string;
begin
  if not XmlAttribute(Node, Attribute, Result) then
    Result := '';
end;

function XmlNodeByID(Node: TXmlNode; const ID: string; out FoundNode: TXmlNode): boolean;
var
  i: Integer;
begin
  Result := False;
  FoundNode := nil;
  if Node <> nil then
    begin
    if Node.HasAttribute('id') then
      if Node.AttributeByNameWide['id'] = ID then
        begin
        FoundNode := Node;
        Result := True;
        end;
    if not Result then
      for i := 0 to Pred(Node.NodeCount) do
        if XmlNodeByID(Node.Nodes[i], ID, FoundNode) then
          begin
          Result := True;
          Break;
          end;
    end;
end;

function XmlNodeByID(Node: TXmlDoc; const ID: string; out FoundNode: TXmlNode): boolean;
begin
  Result := XmlNodeByID(Node.Root, ID, FoundNode);
end;

function XmlNodeByPathAndAttr(Node: TXmlNode; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean;

  function TestNodeForAttr(Node: TXmlNode; const AttributeName, AttributeValue: string): boolean;
    var
      AttrValue: string;
    begin
      Result := False;
      if Node <> nil then
        if AttributeName = '' then
          Result := True
        else
          if XmlAttribute(Node, AttributeName, AttrValue) then
            Result := (AttrValue = AttributeValue);
    end;

var NodePath, NodeName: string;
    i: integer;
begin
  Result := False;
  FoundNode := nil;
  if Node <> nil then
    if Path = '' then
      begin
      if TestNodeForAttr(Node, AttributeName, AttributeValue) then
        begin
        FoundNode := Node;
        Result := True;
        end;
      end
    else
      begin
      NodePath := Path;
      while (Node <> nil) do
        begin
        i := Pos('/', NodePath);
        if i <= 0 then
          begin
          for i := 0 to Pred(Node.NodeCount) do
            if string(Node.Nodes[i].Name) = NodePath then
              if TestNodeForAttr(Node.Nodes[i], AttributeName, AttributeValue) then
                begin
                FoundNode := Node.Nodes[i];
                Result := True;
                Break;
                end;
          Break;
          end
        else
          begin
          NodeName := Copy(NodePath, 1, Pred(i));
          System.Delete(NodePath, 1, i);
          Node := Node.NodeByName(TXmlString(NodeName));
          end;
        end;
      end;
end;

function XmlNodeByPathAndAttr(Node: TXmlDoc; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean;
begin
  Result := XmlNodeByPathAndAttr(Node.Root, Path, AttributeName, AttributeValue, FoundNode);
end;

function XmlNodeByPath(Node: TXmlNode; const Path: string; out FoundNode: TXmlNode): boolean;
begin
  Result := XmlNodeByPathAndAttr(Node, Path, '', '', FoundNode)
end;

function XmlNodeByPath(Node: TXmlDoc; const Path: string; out FoundNode: TXmlNode): boolean;
begin
  Result := XmlNodeByPath(Node.Root, Path, FoundNode);
end;

function XmlNodeByPathCreate(Node: TXmlNode; const Path: string): TXmlNode;
var NodeName, NodePath: string;
    i: integer;
    ExistingNode, NewNode: TXmlNode;
begin
  Result := Node;
  NodePath := Path;
  while NodePath <> '' do
    begin
    i := Pos('/', NodePath);
    if i <= 0 then
      begin
      NodeName := NodePath;
      NodePath := '';
      end
    else
      begin
      NodeName := Copy(NodePath, 1, Pred(i));
      System.Delete(NodePath, 1, i);
      end;
    if XmlNodeByPath(Result, NodeName, ExistingNode) then
      Result := ExistingNode
    else
      begin
      NewNode := TXmlNode.CreateName(Result.Document, TXmlString(NodeName));
      Result.NodeAdd(NewNode);
      Result := NewNode;
      end;
    end;
end;

function XmlNodeByPathCreate(Node: TXmlDoc; const Path: string): TXmlNode;
begin
  Result := XmlNodeByPathCreate(Node.Root, Path);
end;

function XmlValueByPath(Node: TXmlNode; const Path: string; out Value: string): boolean;
var
  ValueNode: TXmlNode;
begin
  Result := XmlNodeByPath(Node, Path, ValueNode);
  if Result then
    Value := XmlValueIncludingCData(ValueNode);
end;

function XmlValueByPath(Node: TXmlDoc; const Path: string; out Value: string): boolean;
begin
  Result := XmlValueByPath(Node.Root, Path, Value);
end;

function XmlValueByPath(Node: TXmlNode; const Path: string): string;
begin
  if not XmlValueByPath(Node, Path, Result) then
    Result := '';
end;

function XmlValueByPath(Node: TXmlDoc; const Path: string): string;
begin
  Result := XmlValueByPath(Node.Root, Path);
end;

function XmlGetNamespace(Node: TXmlNode): string;
var
  Name: string;
  ix: integer;
begin
  Name := {$IFDEF DELPHI5_UP} string {$ENDIF} (Node.Name);
  ix := Pos(':', Name);
  if ix > 0 then
    Result := Copy(Name, 1, ix)
  else
    Result := '';
end;

function XmlGetNamespace(Node: TXmlDoc): string;
begin
  Result := XmlGetNamespace(Node.Root);
end;

{ TXmlDoc }

procedure TXmlDoc.LoadFromBinaryData(Data: Pointer; Length: integer);
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Length;
    Move(Data^, Stream.Memory^, Length);
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
    end;
end;

procedure TXmlDoc.LoadFromBinaryString(const Xml: AnsiString);
begin
  LoadFromBinaryData(@Xml[1], Length(Xml));
end;

function TXmlDoc.SaveToBinaryString: AnsiString;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    SetLength(Result, Stream.Size);
    Move(Stream.Memory^, (@(Result[1]))^, Stream.Size);
  finally
    FreeAndNil(Stream);
    end;
end;

function TXmlDoc.NodeByPath(const Path: string; out Node: TXmlNode): boolean;
begin
  Result := XmlNodeByPath(Self, Path, Node);
end;

function TXmlDoc.NodeByPathAndAttr(const Path, AttributeName, AttributeValue: string; out Node: TXmlNode): boolean;
begin
  Result := XmlNodeByPathAndAttr(Self, Path, AttributeName, AttributeValue, Node);
end;

function TXmlDoc.ValueByPath(const Path, Default: string): string;
begin
  if not XmlValueByPath(Self, Path, Result) then
    Result := Default;
end;

procedure TXmlDoc.SetIndentation(const Value: string);
begin
  IndentString := TXmlString(Value);
  XmlFormat := xfReadable;
end;

end.
