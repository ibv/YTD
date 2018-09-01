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

unit uOptions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, ShlObj, FileCtrl,
  uLanguages, uXml;

type
  TOverwriteMode = (omNever, omAlways, omRename, omAsk);

  {$IFDEF THREADEDVERSION}
  TGetNewestVersionEvent = procedure(Sender: TObject; const Version, Url: string) of object;
  {$ENDIF}

  {$IFDEF CONVERTERS}
  TConverterVisibility = (cvVisible, cvMinimized, cvHidden);

  TConverter = record
    ID: string;
    Title: string;
    ExePath: string;
    CommandLine: string;
    Visibility: TConverterVisibility;
    end;
  {$ENDIF}

  TYTDOptions = class
    private
      fXml: TXmlDoc;
      fXmlFileName: string;
      fMainXmlFileName: string;
      fUserXmlFileName: string;
    protected
      function Load(IgnoreErrors: boolean = True): boolean; virtual;
      function TranslateNodeName(const Name: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetOption(const Path: string; const Default: string = ''): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetOption(const Path, Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function XmlToBoolean(const Value: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function XmlToBoolean(const Value: string; Default: boolean): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function BooleanToXml(const Value: boolean): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property Xml: TXmlDoc read fXml;
      property XmlFileName: string read fXmlFileName write fXmlFileName;
    protected
      function GetPortableMode: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetPortableMode(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetProxyActive: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetProxyActive(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetProxyHost: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetProxyHost(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetProxyPort: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetProxyPort(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetProxyUser: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetProxyUser(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetProxyPassword: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetProxyPassword(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetLanguage: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetLanguage(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetOverwriteMode: TOverwriteMode; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetOverwriteMode(const Value: TOverwriteMode); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetDestinationPath: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetDestinationPath(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetErrorLog: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetErrorLog(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetAutoStartDownloads: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetAutoStartDownloads(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetCheckForNewVersionOnStartup: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetCheckForNewVersionOnStartup(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF CONVERTERS}
        function GetSelectedConverterID: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        procedure SetSelectedConverterID(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        function GetMaxConversionThreads: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        procedure SetMaxConversionThreads(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        {$IFDEF CONVERTERSMUSTBEACTIVATED}
        function GetConvertersActivated: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        procedure SetConvertersActivated(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$IFDEF SUBTITLES}
      function GetSubtitlesEnabled: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetSubtitlesEnabled(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Init; virtual;
      procedure Save(IgnoreErrors: boolean = True); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadProviderOption(const Provider, Option: string; out Value: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteProviderOption(const Provider, Option, Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ReadUrlList(List: TStringList); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteUrlList(List: TStringList); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF CONVERTERS}
      procedure ReadConverterIDList(List: TStrings); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadConverter(const ID: string; out Converter: TConverter): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
      function GetNewestVersion(out Version, Url: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF THREADEDVERSION}
      procedure GetNewestVersionInBackground(OnDone: TGetNewestVersionEvent); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
      property FileName: string read fXmlFileName;
    public
      property PortableMode: boolean read GetPortableMode write SetPortableMode;
      property ProxyActive: boolean read GetProxyActive write SetProxyActive;
      property ProxyHost: string read GetProxyHost write SetProxyHost;
      property ProxyPort: string read GetProxyPort write SetProxyPort;
      property ProxyUser: string read GetProxyUser write SetProxyUser;
      property ProxyPassword: string read GetProxyPassword write SetProxyPassword;
      property Language: string read GetLanguage write SetLanguage;
      property OverwriteMode: TOverwriteMode read GetOverwriteMode write SetOverwriteMode;
      property DestinationPath: string read GetDestinationPath write SetDestinationPath;
      property ErrorLog: string read GetErrorLog write SetErrorLog;
      property AutoStartDownloads: boolean read GetAutoStartDownloads write SetAutoStartDownloads;
      property CheckForNewVersionOnStartup: boolean read GetCheckForNewVersionOnStartup write SetCheckForNewVersionOnStartup;
      {$IFDEF CONVERTERS}
        property SelectedConverterID: string read GetSelectedConverterID write SetSelectedConverterID;
        property MaxConversionThreads: integer read GetMaxConversionThreads write SetMaxConversionThreads;
        {$IFDEF CONVERTERSMUSTBEACTIVATED}
        property ConvertersActivated: boolean read GetConvertersActivated write SetConvertersActivated;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF SUBTITLES}
      property SubtitlesEnabled: boolean read GetSubtitlesEnabled write SetSubtitlesEnabled;
      {$ENDIF}
    end;

implementation

uses
  uCompatibility, HttpSend;

const
  OverwriteModeStrings: array[TOverwriteMode] of string
    = ('never', 'always', 'rename', 'ask');

{$IFDEF CONVERTERS}
const
  ConverterVisibilityStrings: array[TConverterVisibility] of string
    = ('visible', 'minimized', 'hidden');
{$ENDIF}

const
  XML_ROOTNAME = 'ytd';
  XML_URLLIST_ITEM = 'item';
  XML_CONVERTERLIST_ITEM = 'converter';
  XML_CONVERTERLIST_ITEM_ID = 'id';
  XML_CONVERTERLIST_ITEM_TITLE = 'title';
  XML_CONVERTERLIST_ITEM_EXEPATH = 'exe_path';
  XML_CONVERTERLIST_ITEM_COMMANDLINE = 'command_line';
  {$IFDEF CONVERTERS}
  XML_CONVERTERLIST_ITEM_VISIBILITY = 'visibility';
  {$ENDIF}

const
  XML_PATH_PROVIDEROPTION = 'modules/%s/%s';
  XML_PATH_PORTABLEMODE = 'config/portable_mode';
  XML_PATH_PROXYACTIVE = 'config/proxy_server/active';
  XML_PATH_PROXYHOST = 'config/proxy_server/host';
  XML_PATH_PROXYPORT = 'config/proxy_server/port';
  XML_PATH_PROXYUSER = 'config/proxy_server/user';
  XML_PATH_PROXYPASSWORD = 'config/proxy_server/password';
  XML_PATH_LANGUAGE = 'config/language';
  XML_PATH_OVERWRITEMODE = 'config/overwrite_mode';
  XML_PATH_DESTINATIONPATH = 'config/destination_path';
  XML_PATH_ERRORLOG = 'config/error_log';
  XML_PATH_AUTOSTARTDOWNLOADS = 'gui/auto_start_downloads';
  XML_PATH_CHECKFORNEWVERSIONONSTARTUP = 'gui/check_for_new_version';
  XML_PATH_URLLIST = 'download_list';
  XML_PATH_CONVERTERLIST = 'converters';
  XML_PATH_SELECTEDCONVERTER = XML_PATH_CONVERTERLIST + '/selected';
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  XML_PATH_CONVERTERSACTIVATED = XML_PATH_CONVERTERLIST + '/activated';
  {$ENDIF}
  XML_PATH_MAXCONVERSIONTHREADS = XML_PATH_CONVERTERLIST + '/max_threads';
  {$IFDEF SUBTITLES}
  XML_PATH_SUBTITLESENABLED = 'config/subtitles_enabled';
  {$ENDIF}

const
  XML_DEFAULT_PORTABLEMODE = False;
  XML_DEFAULT_PROXYACTIVE = False;
  XML_DEFAULT_PROXYHOST = '';
  XML_DEFAULT_PROXYPORT = '3128';
  XML_DEFAULT_PROXYUSER = '';
  XML_DEFAULT_PROXYPASSWORD = '';
  XML_DEFAULT_LANGUAGE = '';
  XML_DEFAULT_OVERWRITEMODE = omAsk;
  XML_DEFAULT_DESTINATIONPATH = '';
  XML_DEFAULT_ERRORLOG = '';
  XML_DEFAULT_AUTOSTARTDOWNLOADS = True;
  XML_DEFAULT_CHECKFORNEWVERSIONONSTARTUP = True;
  XML_DEFAULT_SELECTEDCONVERTER = '';
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  XML_DEFAULT_CONVERTERSACTIVATED = False;
  {$ENDIF}
  XML_DEFAULT_MAXCONVERSIONTHREADS = 1;
  {$IFDEF CONVERTERS}
  XML_DEFAULT_CONVERTERVISIBILITY = cvMinimized;
  {$ENDIF}
  {$IFDEF SUBTITLES}
  XML_DEFAULT_SUBTITLESENABLED = True;
  {$ENDIF}

{ TYTDOptions }

constructor TYTDOptions.Create;
var PathBuf: array[0..MAX_PATH+1] of Char;
begin
  inherited Create;
  fXml := TXmlDoc.Create;
  fMainXmlFileName := ChangeFileExt(ParamStr(0), '.xml');
  fUserXmlFileName := '';
  if SHGetSpecialFolderPath(0, PathBuf, CSIDL_APPDATA, False) then
    fUserXmlFileName := PathBuf + '\YouTube Downloader\ytd.xml';
  Init;
end;

destructor TYTDOptions.Destroy;
begin
  FreeAndNil(fXml);
  inherited;
end;

procedure TYTDOptions.Init;
begin
  XmlFileName := fMainXmlFileName;
  Xml.Clear;
  Xml.Root.Name := XML_ROOTNAME;
  if (not Load(False)) or (not PortableMode) then
    if (fUserXmlFileName <> '') and FileExists(fUserXmlFileName) then
      begin
      XmlFileName := fUserXmlFileName;
      Load(False);
      PortableMode := False;
      end;
end;

function TYTDOptions.Load(IgnoreErrors: boolean): boolean;
begin
  Result := False;
  try
    if FileExists(XmlFileName) then
      begin
      Xml.LoadFromFile(XmlFileName);
      Result := True;
      end;
  except
    if not IgnoreErrors then
      Raise;
    end;
end;

procedure TYTDOptions.Save(IgnoreErrors: boolean);
var Dir: string;
begin
  try
    Dir := ExtractFilePath(XmlFileName);
    if Dir <> '' then
      ForceDirectories(ExcludeTrailingPathDelimiter(Dir));
    Xml.SetIndentation(#9);
    Xml.SaveToFile(XmlFileName);
  except
    if not IgnoreErrors then
      Raise;
    end;
end;

function TYTDOptions.TranslateNodeName(const Name: string): string;
var i: integer;
begin
  Result := Name;
  for i := 1 to Length(Result) do
    if not CharInSet(Result[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Result[i] := '_';
end;

function TYTDOptions.ReadProviderOption(const Provider, Option: string; out Value: string): boolean;
var Node: TXmlNode;
begin
  if Xml.NodeByPath(Format(XML_PATH_PROVIDEROPTION, [TranslateNodeName(Provider), TranslateNodeName(Option)]), Node) then
    begin
    Result := True;
    Value := XmlValueIncludingCData(Node);
    end
  else
    begin
    Result := False;
    Value := '';
    end;
end;

procedure TYTDOptions.WriteProviderOption(const Provider, Option, Value: string);
begin
  SetOption(Format(XML_PATH_PROVIDEROPTION, [TranslateNodeName(Provider), TranslateNodeName(Option)]), Value);
end;

function TYTDOptions.BooleanToXml(const Value: boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

function TYTDOptions.XmlToBoolean(const Value: string): boolean;
begin
  Result := XmlToBoolean(Value, False);
end;

function TYTDOptions.XmlToBoolean(const Value: string; Default: boolean): boolean;
var n: integer;
begin
  n := StrToIntDef(Value, -1);
  if n = 0 then
    Result := False
  else if n > 0 then
    Result := True
  else
    Result := Default;
end;

function TYTDOptions.GetOption(const Path: string; const Default: string = ''): string;
var Node: TXmlNode;
begin
  if Xml.NodeByPath(Path, Node) then
    Result := XmlValueIncludingCData(Node)
  else
    Result := Default;
end;

procedure TYTDOptions.SetOption(const Path, Value: string);
begin
  XmlNodeByPathCreate(Xml, Path).ValueAsUnicodeString := Value;
end;

function TYTDOptions.GetPortableMode: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_PORTABLEMODE), XML_DEFAULT_PORTABLEMODE);
end;

procedure TYTDOptions.SetPortableMode(const Value: boolean);
begin
  SetOption(XML_PATH_PORTABLEMODE, BooleanToXml(Value));
  if Value then
    XmlFileName := fMainXmlFileName
  else
    begin
    if XmlFileName = fMainXmlFileName then
      Save;
    XmlFileName := fUserXmlFileName;
    end;
end;

function TYTDOptions.GetProxyActive: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_PROXYACTIVE), XML_DEFAULT_PROXYACTIVE);
end;

procedure TYTDOptions.SetProxyActive(const Value: boolean);
begin
  SetOption(XML_PATH_PROXYACTIVE, BooleanToXml(Value));
end;

function TYTDOptions.GetProxyHost: string;
begin
  Result := GetOption(XML_PATH_PROXYHOST, XML_DEFAULT_PROXYHOST);
end;

procedure TYTDOptions.SetProxyHost(const Value: string);
begin
  SetOption(XML_PATH_PROXYHOST, Value);
end;

function TYTDOptions.GetProxyPort: string;
begin
  Result := GetOption(XML_PATH_PROXYPORT, XML_DEFAULT_PROXYPORT);
end;

procedure TYTDOptions.SetProxyPort(const Value: string);
begin
  SetOption(XML_PATH_PROXYPORT, Value);
end;

function TYTDOptions.GetProxyUser: string;
begin
  Result := GetOption(XML_PATH_PROXYUSER, XML_DEFAULT_PROXYUSER);
end;

procedure TYTDOptions.SetProxyUser(const Value: string);
begin
  SetOption(XML_PATH_PROXYUSER, Value);
end;

function TYTDOptions.GetProxyPassword: string;
begin
  Result := GetOption(XML_PATH_PROXYPASSWORD, XML_DEFAULT_PROXYPASSWORD);
end;

procedure TYTDOptions.SetProxyPassword(const Value: string);
begin
  SetOption(XML_PATH_PROXYPASSWORD, Value);
end;

function TYTDOptions.GetLanguage: string;
begin
  Result := GetOption(XML_PATH_LANGUAGE, XML_DEFAULT_LANGUAGE);
end;

procedure TYTDOptions.SetLanguage(const Value: string);
begin
  SetOption(XML_PATH_LANGUAGE, Value);
end;

function TYTDOptions.GetOverwriteMode: TOverwriteMode;
var s: string;
    i: TOverwriteMode;
begin
  s := GetOption(XML_PATH_OVERWRITEMODE);
  Result := XML_DEFAULT_OVERWRITEMODE;
  for i := Low(OverwriteModeStrings) to High(OverwriteModeStrings) do
    if AnsiCompareText(s, OverwriteModeStrings[i]) = 0 then
      begin
      Result := i;
      Break;
      end;
end;

procedure TYTDOptions.SetOverwriteMode(const Value: TOverwriteMode);
begin
  SetOption(XML_PATH_OVERWRITEMODE, OverwriteModeStrings[Value]);
end;

function TYTDOptions.GetDestinationPath: string;
begin
  Result := GetOption(XML_PATH_DESTINATIONPATH, XML_DEFAULT_DESTINATIONPATH);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TYTDOptions.SetDestinationPath(const Value: string);
begin
  SetOption(XML_PATH_DESTINATIONPATH, Value);
end;

function TYTDOptions.GetErrorLog: string;
begin
  Result := GetOption(XML_PATH_ERRORLOG, XML_DEFAULT_ERRORLOG);
end;

procedure TYTDOptions.SetErrorLog(const Value: string);
begin
  SetOption(XML_PATH_ERRORLOG, Value);
end;

function TYTDOptions.GetAutoStartDownloads: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_AUTOSTARTDOWNLOADS), XML_DEFAULT_AUTOSTARTDOWNLOADS);
end;

procedure TYTDOptions.SetAutoStartDownloads(const Value: boolean);
begin
  SetOption(XML_PATH_AUTOSTARTDOWNLOADS, BooleanToXml(Value));
end;

function TYTDOptions.GetCheckForNewVersionOnStartup: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_CHECKFORNEWVERSIONONSTARTUP), XML_DEFAULT_CHECKFORNEWVERSIONONSTARTUP);
end;

procedure TYTDOptions.SetCheckForNewVersionOnStartup(const Value: boolean);
begin
  SetOption(XML_PATH_CHECKFORNEWVERSIONONSTARTUP, BooleanToXml(Value));
end;

{$IFDEF SUBTITLES}
function TYTDOptions.GetSubtitlesEnabled: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_SUBTITLESENABLED), XML_DEFAULT_SUBTITLESENABLED);
end;

procedure TYTDOptions.SetSubtitlesEnabled(const Value: boolean);
begin
  SetOption(XML_PATH_SUBTITLESENABLED, BooleanToXml(Value));
end;
{$ENDIF}

procedure TYTDOptions.ReadUrlList(List: TStringList);
var Node: TXmlNode;
    i: integer;
    Url: string;
begin
  List.Clear;
  if Xml.NodeByPath(XML_PATH_URLLIST, Node) then
    for i := 0 to Pred(Node.NodeCount) do
      if Node.Nodes[i].Name = XML_URLLIST_ITEM then
        begin
        Url := Trim(XmlValueIncludingCData(Node.Nodes[i]));
        if Url <> '' then
          List.Add(Url);
        end;
end;

procedure TYTDOptions.WriteUrlList(List: TStringList);
var Node, NewNode: TXmlNode;
    i: integer;
begin
  Node := XmlNodeByPathCreate(Xml, XML_PATH_URLLIST);
  Node.NodesClear;
  for i := 0 to Pred(List.Count) do
    if List[i] <> '' then
      begin
      NewNode := TXmlNode.CreateName(Node.Document, XML_URLLIST_ITEM);
      NewNode.ValueAsUnicodeString := List[i];
      Node.NodeAdd(NewNode);
      end;
end;

{$IFDEF CONVERTERS}
procedure TYTDOptions.ReadConverterIDList(List: TStrings);
var Node: TXmlNode;
    i: integer;
begin
  List.Clear;
  if Xml.NodeByPath(XML_PATH_CONVERTERLIST, Node) then
    for i := 0 to Pred(Node.NodeCount) do
      if Node.Nodes[i].Name = XML_CONVERTERLIST_ITEM then
        if Node.Nodes[i].HasAttribute(XML_CONVERTERLIST_ITEM_ID) then
          List.Add(Node.Nodes[i].AttributeByNameWide[XML_CONVERTERLIST_ITEM_ID]);
end;

function TYTDOptions.ReadConverter(const ID: string; out Converter: TConverter): boolean;
var Node: TXmlNode;
    Vis: string;
    Visibility: TConverterVisibility;
begin
  if ID = '' then
    Result := False
  else if not Xml.NodeByPathAndAttr(XML_PATH_CONVERTERLIST + '/' + XML_CONVERTERLIST_ITEM, XML_CONVERTERLIST_ITEM_ID, ID, Node) then
    Result := False
  else
    begin
    Converter.ID :=  Node.AttributeByNameWide[XML_CONVERTERLIST_ITEM_ID];
    Converter.Title := XmlValueByPath(Node, XML_CONVERTERLIST_ITEM_TITLE);
    Converter.ExePath := XmlValueByPath(Node, XML_CONVERTERLIST_ITEM_EXEPATH);
    Converter.CommandLine := XmlValueByPath(Node, XML_CONVERTERLIST_ITEM_COMMANDLINE);
    Vis := XmlValueByPath(Node, XML_CONVERTERLIST_ITEM_VISIBILITY);
    Converter.Visibility := XML_DEFAULT_CONVERTERVISIBILITY;
    for Visibility := Low(ConverterVisibilityStrings) to High(ConverterVisibilityStrings) do
      if AnsiCompareText(Vis, ConverterVisibilityStrings[Visibility]) = 0 then
        begin
        Converter.Visibility := Visibility;
        Break;
        end;
    Result := (Converter.ID = ID) and (Converter.Title <> '') and (Converter.ExePath <> '');
    end;
  if not Result then
    begin
    Converter.ID := '';
    Converter.Title := '';
    Converter.ExePath := '';
    Converter.CommandLine := '';
    Converter.Visibility := XML_DEFAULT_CONVERTERVISIBILITY;
    end;
end;

function TYTDOptions.GetSelectedConverterID: string;
begin
  Result := GetOption(XML_PATH_SELECTEDCONVERTER, XML_DEFAULT_SELECTEDCONVERTER);
end;

procedure TYTDOptions.SetSelectedConverterID(const Value: string);
begin
  SetOption(XML_PATH_SELECTEDCONVERTER, Value);
end;

function TYTDOptions.GetMaxConversionThreads: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_MAXCONVERSIONTHREADS), XML_DEFAULT_MAXCONVERSIONTHREADS);
end;

procedure TYTDOptions.SetMaxConversionThreads(const Value: integer);
begin
  SetOption(XML_PATH_MAXCONVERSIONTHREADS, IntToStr(Value));
end;

{$IFDEF CONVERTERSMUSTBEACTIVATED}
function TYTDOptions.GetConvertersActivated: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_CONVERTERSACTIVATED), XML_DEFAULT_CONVERTERSACTIVATED);
end;

procedure TYTDOptions.SetConvertersActivated(const Value: boolean);
begin
  SetOption(XML_PATH_CONVERTERSACTIVATED, BooleanToXml(Value));
end;
{$ENDIF}

{$ENDIF}

{$IFDEF THREADEDVERSION}
type
  TGetNewestVersionThread = class(TThread)
    private
      fVersion: string;
      fUrl: string;
      procedure SyncReportVersion;
    protected
      Options: TYTDOptions;
      OnDone: TGetNewestVersionEvent;
      procedure Execute; override;
    public
      constructor Create(AOptions: TYTDOptions; AOnDone: TGetNewestVersionEvent); virtual;
    end;

constructor TGetNewestVersionThread.Create(AOptions: TYTDOptions; AOnDone: TGetNewestVersionEvent);
begin
  inherited Create(True);
  Options := AOptions;
  OnDone := AOnDone;
  FreeOnTerminate := True;
  Resume;
end;

procedure TGetNewestVersionThread.Execute;
var Version, Url: string;
begin
  if Assigned(OnDone) and Assigned(Options) then
    if Options.GetNewestVersion(Version, Url) then
      begin
      fVersion := Version;
      fUrl := Url;
      {$IFDEF DELPHITHREADS}
      Synchronize(SyncReportVersion);
      {$ELSE}
      SyncReportVersion;
      {$ENDIF}
      end;
end;

procedure TGetNewestVersionThread.SyncReportVersion;
begin
  OnDone(Options, fVersion, fUrl);
end;

procedure TYTDOptions.GetNewestVersionInBackground(OnDone: TGetNewestVersionEvent);
begin
  TGetNewestVersionThread.Create(Self, OnDone);
end;

{$ENDIF}

function TYTDOptions.GetNewestVersion(out Version, Url: string): boolean;

  function FindHeader(Http: THttpSend; Header: string; out Value: string): boolean;
    var i: integer;
        HdrLen: integer;
    begin
      Result := False;
      Header := Trim(Header) + ':';
      HdrLen := Length(Header);
      for i := 0 to Pred(Http.Headers.Count) do
        if AnsiCompareText(Copy(Http.Headers[i], 1, HdrLen), Header) = 0 then
          begin
          Value := Trim(Copy(Http.Headers[i], Succ(HdrLen), MaxInt));
          Result := True;
          Break;
          end;
    end;

var Http: THttpSend;
    Ver: string;
    n: integer;
begin
  Result := False;
  Version := '';
  Url := 'http://ytd.pepak.net' {$IFNDEF XXX} + '/?lite=1' {$ENDIF};
  Http := THttpSend.Create;
  try
    if ProxyActive then
      begin
      Http.ProxyHost := ProxyHost;
      Http.ProxyPort := ProxyPort;
      Http.ProxyUser := ProxyUser;
      Http.ProxyPass := ProxyPassword;
      end;
    if Http.HttpMethod('HEAD', Url) then
      if (Http.ResultCode >= 200) and (Http.ResultCode < 400) then
        if FindHeader(Http, 'X-YTD-Version', Version) then
          Result := True
        else if FindHeader(Http, 'Location', Url) then
          begin
          Ver := ChangeFileExt(Url, '');
          n := Length(Ver);
          if n >= 4 then
            if CharInSet(Ver[n-3], ['0'..'9']) and (Ver[n-2] = '.') and CharInSet(Ver[n-1], ['0'..'9']) and CharInSet(Ver[n], ['0'..'9']) then
              begin
              Version := Copy(Ver, n-3, 4);
              Result := True;
              end;
          end;
  finally
    Http.Free;
    end;
end;

end.
