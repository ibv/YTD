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

unit uScripts;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXML, HttpSend, blcksock,
  uDownloader, uOptions;

type
  EScriptedDownloaderError = class(EDownloaderError);
  EScriptedDownloaderScriptError = class(EScriptedDownloaderError);

  TScriptVariable = class;
  TScriptVariables = class;
  TScriptEngine = class;

  TScriptVariable = class
    private
      fName: string;
      fValue: string;
      fXml: TXmlDoc;
      procedure SetValue(const AValue: string);
      function GetXml: TXmlDoc;
    protected
    public
      constructor Create(const AName: string);
      destructor Destroy; override;
      property Name: string read fName;
      property Value: string read fValue write SetValue;
      property Xml: TXmlDoc read GetXml;
    end;

  TScriptVariables = class
    private
      fList: TList;
      function GetCount: integer;
      function GetValue(const Name: string): string;
      function GetVariable(Index: integer): TScriptVariable;
      procedure SetValue(const Name, Value: string);
      function GetExists(const Name: string): boolean;
    function GetXml(const Name: string): TXmlDoc;
    protected
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      function Find(const Name: string; out Index: integer): boolean;
      property Count: integer read GetCount;
      property Variables[Index: integer]: TScriptVariable read GetVariable;
      property Values[const Name: string]: string read GetValue write SetValue; default;
      property Xml[const Name: string]: TXmlDoc read GetXml;
      property Exists[const Name: string]: boolean read GetExists;
    end;

  TScriptEngine = class
  private
    fFileName: string;
    fXml: TXmlDoc;
    fUrlsNode: TXmlNode;
    fScriptsNode: TXmlNode;
    {$IFDEF XXX}
    fAdultUrlsNode: TXmlNode;
    fAdultScriptsNode: TXmlNode;
    {$ENDIF}
    fRegExpsNode: TXmlNode;
    fVersion: string;
    fUpgradeUrl: string;
    fRegExpCache: TRegExpCache;
  protected
    procedure InitData;
    function InternalGetScriptForUrl(const Url: string; Urls: TXmlNode; out Node: TXmlNode; out MovieID: string): boolean;
    property Xml: TXmlDoc read fXml;
    property FileName: string read fFileName;
    property Scripts: TXmlNode read fScriptsNode;
    property RegExpsNode: TXmlNode read fRegExpsNode;
    property Urls: TXmlNode read fUrlsNode;
    {$IFDEF XXX}
    property AdultScripts: TXmlNode read fAdultScriptsNode;
    property AdultUrls: TXmlNode read fAdultUrlsNode;
    {$ENDIF}
    property RegExpCache: TRegExpCache read fRegExpCache;
    property UpgradeUrl: string read fUpgradeUrl;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string = '');
    procedure SaveToStream(Stream: TStream);
    function GetScriptForUrl(const Url: string; out Node: TXmlNode; out MovieID: string): boolean;
    function GetScript(const ID: string; out Node: TXmlNode): boolean;
    function GetRegExp(const ID: string; out Node: TXmlNode): boolean;
    property Version: string read fVersion;
  end;

procedure ScriptError(const Msg: string; Node: TXmlNode);

implementation

uses
  uCompatibility,
  uFunctions,
  uStrings,
  uMessages;

type
  THackXmlNode = class(TXmlNode);

procedure ScriptError(const Msg: string; Node: TXmlNode);
var
  Str: TMemoryStream;
  Tag: AnsiString;
begin
  Tag := '';
  if Node <> nil then
    begin
    Str := TMemoryStream.Create;
    try
      THackXmlNode(Node).WriteToStream(Str);
      Str.Position := 0;
      SetLength(Tag, Str.Size);
      if Str.Size > 0 then
        Str.Read(Tag[1], Str.Size);
    finally
      FreeAndNil(Str);
      end;
    end;
  if Tag = '' then
    Raise EScriptedDownloaderScriptError.Create(ERR_SCRIPTS_ERROR + Msg)
  else
    Raise EScriptedDownloaderScriptError.Create(ERR_SCRIPTS_ERROR + Msg + EOLN + AnsiEncodedUTF8ToString(Tag));
end;

{ TScriptVariable }

constructor TScriptVariable.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
  fValue := '';
  fXml := nil;
end;

destructor TScriptVariable.Destroy;
begin
  FreeAndNil(fXml);
  inherited;
end;

function TScriptVariable.GetXml: TXmlDoc;
begin
  if fXml = nil then
    begin
    fXml := TXmlDoc.Create;
    try
      fXml.ReadFromString(StringToUtf8(Value, False));
    except
      FreeAndNil(fXml);
      Raise;
      end;
    end;
  Result := fXml;
end;

procedure TScriptVariable.SetValue(const AValue: string);
begin
  if fValue <> AValue then
    begin
    fValue := AValue;
    FreeAndNil(fXml);
    end;
end;

{ TScriptVariables }

constructor TScriptVariables.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TScriptVariables.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

procedure TScriptVariables.Clear;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Variables[i].Free;
  fList.Clear;
end;

function TScriptVariables.GetCount: integer;
begin
  Result := fList.Count;
end;

function TScriptVariables.GetVariable(Index: integer): TScriptVariable;
begin
  Result := TScriptVariable(fList[Index]);
end;

function TScriptVariables.Find(const Name: string; out Index: integer): boolean;
var
  L, H, I, C: Integer;
  Item: TScriptVariable;
begin
  Result := False;
  L := 0;
  H := Pred(Count);
  while L <= H do
    begin
    I := (L + H) shr 1;
    Item := Variables[I];
    C := AnsiCompareStr(Item.Name, Name);
    if C < 0
      then L := Succ(I)
    else
      begin
      H := Pred(I);
      if C = 0 then
        begin
        Result := True;
        L := I;
        end;
      end;
    end;
  Index := L;
end;

function TScriptVariables.GetValue(const Name: string): string;
var
  Index: integer;
begin
  if Find(Name, Index) then
    Result := Variables[Index].Value
  else
    Raise EScriptedDownloaderScriptError.CreateFmt(ERR_SCRIPTS_VARIABLE_NOT_FOUND, [Name]);
end;

function TScriptVariables.GetXml(const Name: string): TXmlDoc;
var
  Index: integer;
begin
  if Find(Name, Index) then
    Result := Variables[Index].Xml
  else
    Raise EScriptedDownloaderScriptError.CreateFmt(ERR_SCRIPTS_VARIABLE_NOT_FOUND, [Name]);
end;

procedure TScriptVariables.SetValue(const Name, Value: string);
var
  Index: integer;
  NewVar: TScriptVariable;
begin
  if Find(Name, Index) then
    Variables[Index].Value := Value
  else
    begin
    NewVar := TScriptVariable.Create(Name);
    NewVar.Value := Value;
    fList.Insert(Index, NewVar);
    end;
end;

function TScriptVariables.GetExists(const Name: string): boolean;
var
  Index: integer;
begin
  Result := Find(Name, Index);
end;

{ TScript }

constructor TScriptEngine.Create;
begin
  inherited Create;
  fXml := TXmlDoc.Create;
  fRegExpCache := TRegExpCache.Create;
  InitData;
end;

destructor TScriptEngine.Destroy;
begin
  FreeAndNil(fXml);
  FreeAndNil(fRegExpCache);
  inherited;
end;

function TScriptEngine.GetRegExp(const ID: string; out Node: TXmlNode): boolean;
begin
  Result := XmlNodeByPathAndAttr(RegExpsNode, 'regexp', 'id', ID, Node);
end;

function TScriptEngine.GetScript(const ID: string; out Node: TXmlNode): boolean;
begin
  if XmlNodeByPathAndAttr(Scripts, 'script', 'id', ID, Node) then
    Result := True
  {$IFDEF XXX}
  else if XmlNodeByPathAndAttr(AdultScripts, 'script', 'id', ID, Node) then
    Result := True
  {$ENDIF}
  else
    Result := False;
end;

function TScriptEngine.InternalGetScriptForUrl(const Url: string; Urls: TXmlNode; out Node: TXmlNode; out MovieID: string): boolean;
var
  UrlNode: TXmlNode;
  ScriptID, UrlRegExp, SubexpressionName: string;
  RE: TRegExp;
  i: integer;
begin
  Result := False;
  for i := 0 to Pred(Urls.NodeCount) do
    begin
    UrlNode := Urls.Nodes[i];
    if UrlNode.Name = 'url' then
      begin
      UrlRegExp := XmlValueIncludingCData(UrlNode);
      if UrlRegExp = '' then
        ScriptError(ERR_SCRIPTS_EMPTY_URL_ENCOUNTERED, UrlNode)
      else
        begin
        if XmlAttribute(UrlNode, 'direct') = '' then
          UrlRegExp := '^https?://(?:[a-z0-9-]+\.)*' + UrlRegExp;
        RE := RegExpCache.GetRegExp(UrlRegExp);
        if RE.Match(Url) then
          begin
          ScriptID := XmlAttribute(UrlNode, 'script');
          if ScriptID = '' then
            ScriptError(ERR_SCRIPTS_SCRIPT_MUST_BE_NONEMPTY, UrlNode)
          else if not GetScript(ScriptID, Node) then
            ScriptError(Format(ERR_SCRIPTS_SCRIPT_NOT_FOUND, [ScriptID]), UrlNode)
          else
            begin
            SubexpressionName := XmlAttribute(UrlNode, 'subexpression');
            if SubexpressionName = '' then
              MovieID := Url
            else if not RE.SubexpressionByName(SubexpressionName, MovieID) then
              ScriptError(Format(ERR_SCRIPTS_SUBEXPRESSION_NOT_FOUND, [SubexpressionName]), UrlNode);
            Result := True;
            Break;
            end;
          end;
        end;
      end;
    end;
end;

function TScriptEngine.GetScriptForUrl(const Url: string; out Node: TXmlNode; out MovieID: string): boolean;
begin
  if InternalGetScriptForUrl(Url, Urls, Node, MovieID) then
    Result := True
  {$IFDEF XXX}
  else if InternalGetScriptForUrl(Url, AdultUrls, Node, MovieID) then
    Result := True
  {$ENDIF}
  else
    Result := False;
end;

procedure TScriptEngine.InitData;
var
  VersionNode, UpgradeUrlNode: TXmlNode;
  {$IFNDEF XXX}
  DeleteNode: TXmlNode;
  {$ENDIF}
begin
  if not XmlNodeByPath(Xml, 'version', VersionNode) then
    fVersion := ''
  else
    fVersion := XmlValueIncludingCData(VersionNode);
  if not XmlNodeByPath(Xml, 'upgrade_url', UpgradeUrlNode) then
    fUpgradeUrl := ''
  else
    fUpgradeUrl := XmlValueIncludingCData(UpgradeUrlNode);
  if not XmlNodeByPath(Xml, 'urls', fUrlsNode) then
    fUrlsNode := Xml.Root.NodeNew('urls');
  {$IFDEF XXX}
  if not XmlNodeByPath(Urls, 'adult', fAdultUrlsNode) then
    fAdultUrlsNode := Urls.NodeNew('adult');
  {$ELSE}
  if XmlNodeByPath(Urls, 'adult', DeleteNode) then
    DeleteNode.Delete;
  {$ENDIF}
  if not XmlNodeByPath(Xml, 'scripts', fScriptsNode) then
    fScriptsNode := Xml.Root.NodeNew('scripts');
  {$IFDEF XXX}
  if not XmlNodeByPath(Scripts, 'adult', fAdultScriptsNode) then
    fAdultScriptsNode := Scripts.NodeNew('adult');
  {$ELSE}
  if XmlNodeByPath(Scripts, 'adult', DeleteNode) then
    DeleteNode.Delete;
  {$ENDIF}
  if not XmlNodeByPath(Xml, 'regexps', fRegExpsNode) then
    fRegExpsNode := Xml.Root.NodeNew('regexps');
end;

procedure TScriptEngine.LoadFromFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FS);
    fFileName := FileName;
  finally
    FreeAndNil(FS);
    end;
end;

procedure TScriptEngine.LoadFromStream(Stream: TStream);
var
  TestXml: TXmlDoc;
begin
  Xml.LoadFromStream(Stream);
  TestXml := TXmlDoc.Create;
  try
    TestXml.LoadFromStream(Stream);
    FreeAndNil(fXml);
    fXml := TestXml;
    TestXml := nil;
    InitData;
  finally
    FreeAndNil(TestXml);
    end;
end;

procedure TScriptEngine.SaveToFile(const FileName: string);
begin
  if FileName = '' then
    Xml.SaveToFile(Self.FileName)
  else
    Xml.SaveToFile(FileName);
end;

procedure TScriptEngine.SaveToStream(Stream: TStream);
begin
  Xml.SaveToStream(Stream);
end;

end.
