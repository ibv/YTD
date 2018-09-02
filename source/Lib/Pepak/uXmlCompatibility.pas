unit uXmlCompatibility;
{$INCLUDE 'jedi.inc'}
{$IFDEF DELPHI6_UP}
  {$DEFINE DELPHIXML}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IFDEF DELPHIXML}
  XmlDoc, XmlIntf, ActiveX,
  {$ELSE}
  NativeXml,
  {$ENDIF}
  uCompatibility;

{$IFDEF DELPHIXML}
type
  TXmlDoc = TXmlDocument;
  TXmlNode = IXmlNode;
{$ELSE}
type
  TXmlDoc = NativeXml.TNativeXml;
  TXmlNode = NativeXml.TXmlNode;
{$ENDIF}

function XmlCreateFromString(Owner: TComponent; const Xml: AnsiString; out Doc: TXmlDoc; const RootName: string = ''): boolean;
function XmlRoot(Doc: TXmlDoc): TXmlNode;
function XmlNodeName(Node: TXmlNode): string;
function XmlText(Node: TXmlNode): string; overload;
function XmlText(Node: TXmlNode; const Path: string): string; overload;
function XmlAttribute(Node: TXmlNode; const Name: string): string; overload;
function XmlAttribute(Node: TXmlNode; const Path, Name: string): string; overload;
procedure XmlSetAttribute(Node: TXmlNode; const Name, Value: string);
function XmlChildCount(Node: TXmlNode): integer;
function XmlChild(Node: TXmlNode; Index: integer): TXmlNode;
function XmlAddChild(Node: TXmlNode; const Name: string): TXmlNode;
function XmlFindNode(Root: TXmlNode; Path: string; out Node: TXmlNode): boolean;
function XmlSave(Doc: TXmlDoc): Utf8String;

implementation

function XmlRoot(Doc: TXmlDoc): TXmlNode;
begin
  Result := {$IFDEF DELPHIXML} Doc.DocumentElement {$ELSE} Doc.Root {$ENDIF} ;
end;

function XmlNodeName(Node: TXmlNode): string;
{$IFNDEF DELPHIXML}
var i: integer;
{$ENDIF}
begin
  {$IFDEF DELPHIXML}
  Result := Node.LocalName;
  {$ELSE}
  Result := Node.Name;
  i := Pos(':', Result);
  if i > 0 then
    System.Delete(Result, 1, i);
  {$ENDIF}
end;

function XmlText(Node: TXmlNode): string;
begin
  Result := {$IFDEF DELPHIXML} Node.Text {$ELSE} string(Node.ValueAsUnicodeString) {$ENDIF} ;
end;

function XmlText(Node: TXmlNode; const Path: string): string;
var N: TXmlNode;
begin
  Result := '';
  if XmlFindNode(Node, Path, N) then
    Result := XmlText(N);
end;

function XmlAttribute(Node: TXmlNode; const Name: string): string;
begin
  Result := {$IFDEF DELPHIXML} Node.Attributes[Name] {$ELSE} string(Node.AttributeByNameWide[Name]) {$ENDIF} ;
end;

function XmlAttribute(Node: TXmlNode; const Path, Name: string): string;
var N: TXmlNode;
begin
  Result := '';
  if XmlFindNode(Node, Path, N) then
    Result := XmlAttribute(N, Name);
end;

procedure XmlSetAttribute(Node: TXmlNode; const Name, Value: string);
begin
  {$IFDEF DELPHIXML}
  Node.Attributes[Name] := Value;
  {$ELSE}
  Node.AttributeByNameWide[Name] := WideString(Value);
  {$ENDIF}
end;

function XmlChildCount(Node: TXmlNode): integer;
begin
  Result := {$IFDEF DELPHIXML} Node.ChildNodes.Count {$ELSE} Node.NodeCount {$ENDIF} ;
end;

function XmlChild(Node: TXmlNode; Index: integer): TXmlNode;
begin
  Result := {$IFDEF DELPHIXML} Node.ChildNodes[Index] {$ELSE} Node.Nodes[Index] {$ENDIF} ;
end;

function XmlAddChild(Node: TXmlNode; const Name: string): TXmlNode;
begin
  Result := {$IFDEF DELPHIXML} Node.AddChild(Name) {$ELSE} Node.NodeNew(Name) {$ENDIF} ;
end;

function XmlCreateFromString(Owner: TComponent; const Xml: AnsiString; out Doc: TXmlDoc; const RootName: string = ''): boolean;
{$IFNDEF DELPHIXML}
var Stream: TMemoryStream;
{$ENDIF}
begin
  Result := False;
  Doc := TXmlDoc.Create {$IFDEF DELPHIXML} (Owner) {$ENDIF} ;
  try
    {$IFDEF DELPHIXML}
    Doc.Options := Doc.Options - [doAttrNull];
    Doc.LoadFromXml(Xml);
    {$ELSE}
    Stream := TMemoryStream.Create;
    try
      if Xml <> '' then
        begin
        Stream.WriteBuffer(Xml[1], Length(Xml)*Sizeof(Xml[1]));
        Stream.Position := 0;
        end;
      Doc.LoadFromStream(Stream);
    finally
      Stream.Free;
      end;
    {$ENDIF}
    if RootName = '' then
      Result := True
    else if (XmlRoot(Doc) <> nil) and (XmlNodeName(XmlRoot(Doc)) = RootName) then
      Result := True;
  finally
    if not Result then
      FreeAndNil(Doc);
    end;
end;

function XmlFindNode(Root: TXmlNode; Path: string; out Node: TXmlNode): boolean;
var N: TXmlNode;
    i: integer;
    NodeName: string;
    Found: boolean;
begin
  Result := False;
  Node := nil;
  if Root <> nil then
    begin
    N := Root;
    while Path <> '' do
      begin
      i := Pos('/', Path);
      if i <= 0 then
        begin
        NodeName := Path;
        Path := '';
        end
      else
        begin
        NodeName := Copy(Path, 1, Pred(i));
        Path := Copy(Path, Succ(i), MaxInt);
        end;
      if NodeName = '' then
        Continue;
      Found := False;
      for i := 0 to XmlChildCount(N) - 1 do
        if XmlNodeName(XmlChild(N, i)) = NodeName then
          begin
          N := XmlChild(N, i);
          Found := True;
          Break;
          end;
      if not Found then
        Exit;
      end;
    if Path = '' then
      begin
      Node := N;
      Result := True;
      end;
    end;
end;

function XmlSave(Doc: TXmlDoc): Utf8String;
begin
  {$IFDEF DELPHIXML}
  Doc.SaveToXml(Result);
  {$ELSE}
  Result := Doc.WriteToString;
  {$ENDIF}
end;

end.
