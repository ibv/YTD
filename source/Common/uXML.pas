(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uCompatibility,
  NativeXml;

type
  TXmlNode = NativeXml.TXmlNode;

  TXmlDoc = class(TNativeXml)
    protected
      procedure LoadFromBinaryData(Data: Pointer; Length: integer); virtual;
    public
      procedure LoadFromBinaryString(const Xml: AnsiString); virtual;
      function SaveToBinaryString: AnsiString; virtual;
      function NodeByPath(const Path: string; out Node: TXmlNode): boolean; virtual;
      function NodeByPathAndAttr(const Path, AttributeName, AttributeValue: string; out Node: TXmlNode): boolean; virtual;
      function ValueByPath(const Path: string; const Default: string = ''): string; virtual;
      procedure SetIndentation(const Value: string); virtual;
      property Xml: AnsiString read SaveToBinaryString write LoadFromBinaryString;
    end;

function XmlValueIncludingCData(Node: TXmlNode): string;
function XmlNodeByPath(Node: TXmlNode; const Path: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPath(Node: TXmlDoc; const Path: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathAndAttr(Node: TXmlNode; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathAndAttr(Node: TXmlDoc; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean; overload;
function XmlNodeByPathCreate(Node: TXmlNode; const Path: string): TXmlNode; overload;
function XmlNodeByPathCreate(Node: TXmlDoc; const Path: string): TXmlNode; overload;
function XmlValueByPath(Node: TXmlNode; const Path: string; const Default: string = ''): string; overload;
function XmlValueByPath(Node: TXmlDoc; const Path: string; const Default: string = ''): string; overload;

implementation

function XmlValueIncludingCData(Node: TXmlNode): string;
var CData: TXmlNode;
begin
  if Node = nil then
    Result := ''
  else
    begin
    CData := Node.NodeByElementType(xeCData);
    if CData = nil then
      Result := Trim(Node.ValueAsUnicodeString)
    else
      Result := CData.ValueAsUnicodeString;
    end;
end;

function XmlNodeByPathAndAttr(Node: TXmlNode; const Path, AttributeName, AttributeValue: string; out FoundNode: TXmlNode): boolean;

  function TestNodeForAttr(Node: TXmlNode; const AttributeName, AttributeValue: string): boolean;
    begin
      Result := False;
      if Node <> nil then
        if AttributeName = '' then
          Result := True
        else
          if Node.HasAttribute(UTF8String(AttributeName)) then
            if Node.AttributeByNameWide[UTF8String(AttributeName)] = AttributeValue then
              Result := True;
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
          Node := Node.NodeByName(UTF8String(NodeName));
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
  Result := XmlNodeByPathAndAttr(Node, Path, '', '', FoundNode);
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
      NewNode := TXmlNode.CreateName(Result.Document, UTF8String(NodeName));
      Result.NodeAdd(NewNode);
      Result := NewNode;
      end;
    end;
end;

function XmlNodeByPathCreate(Node: TXmlDoc; const Path: string): TXmlNode;
begin
  Result := XmlNodeByPathCreate(Node.Root, Path);
end;

function XmlValueByPath(Node: TXmlNode; const Path: string; const Default: string): string;
var ValueNode: TXmlNode;
begin
  if XmlNodeByPath(Node, Path, ValueNode) then
    Result := XmlValueIncludingCData(ValueNode)
  else
    Result := Default;
end;

function XmlValueByPath(Node: TXmlDoc; const Path: string; const Default: string): string;
begin
  Result := XmlValueByPath(Node.Root, Path, Default);
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
    Stream.Free;
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
    Stream.Free;
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
  Result := XmlValueByPath(Self, Path, Default);
end;

procedure TXmlDoc.SetIndentation(const Value: string);
begin
  IndentString := UTF8String(Value);
  XmlFormat := xfReadable;
end;

end.
