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

unit uOptions;
{$INCLUDE 'ytd.inc'}

interface

uses
  {$ifdef mswindows}
    Windows, ActiveX, ShellApi,
    ShlObj,
  {$ELSE}
    LCLIntf, LCLType, fileutil,
  {$ENDIF}

  SysUtils, Classes, {$IFNDEF DELPHI7_UP} FileCtrl, {$ENDIF}
  HttpSend,
  uXml;

type
  TOverwriteMode = (omNever, omAlways, omRename, omAsk);
  TIndexForNames = (ifnNone, ifnStart, ifnEnd);

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
      function IgnoreInitErrors: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function TranslateNodeName(const Name: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetOption(const Path: string; const Default: string = ''): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetOption(const Path, Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function XmlToBoolean(const Value: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function XmlToBoolean(const Value: string; Default: boolean): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function BooleanToXml(const Value: boolean): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property Xml: TXmlDoc read fXml;
      property XmlFileName: string read fXmlFileName write fXmlFileName;
      property MainXmlFileName: string read fMainXmlFileName;
      property UserXmlFileName: string read fUserXmlFileName;
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
      function GetAutoDeleteFinishedDownloads: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetAutoDeleteFinishedDownloads(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetAutoTryHtmlParser: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetAutoTryHtmlParser(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetDownloadRetryCount: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetDownloadRetryCount(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetIgnoreMissingOpenSSL: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetIgnoreMissingOpenSSL(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetIgnoreMissingRtmpDump: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetIgnoreMissingRtmpDump(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetIgnoreMissingMSDL: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetIgnoreMissingMSDL(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetAddIndexToNames: TIndexForNames;
      procedure SetAddIndexToNames(const Value: TIndexForNames);
      function GetScriptFileName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}

      function GetStartSound: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetStartSound(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetStartSoundFile: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetStartSoundFile(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetEndSound: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetEndSound(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetEndSoundFile: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetEndSoundFile(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}


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
      function GetDownloadToTempFiles: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetDownloadToTempFiles(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetDownloadToProviderSubdirs: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetDownloadToProviderSubdirs(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Init; virtual;
      procedure Save(IgnoreErrors: boolean = True); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadProviderOption(const Provider, Option: string; out Value: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadProviderOptionDef(const Provider, Option: string; const Default: string): string; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadProviderOptionDef(const Provider, Option: string; const Default: integer): integer; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadProviderOptionDef(const Provider, Option: string; const Default: boolean): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteProviderOption(const Provider, Option, Value: string); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteProviderOption(const Provider, Option: string; Value: integer); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteProviderOption(const Provider, Option: string; Value: boolean); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ReadUrlList(List: TStringList); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteUrlList(List: TStringList); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF CONVERTERS}
      procedure ReadConverterIDList(List: TStrings); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ReadConverter(const ID: string; out Converter: TConverter): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function WriteConverter(const Converter: TConverter): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
      function CreateHttp: THttpSend; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
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
      property StartSound: boolean read GetStartSound write SetStartSound;
      property EndSound: boolean read GetEndSound write SetEndSound;
      property StartSoundFile: string read GetStartSoundFile write SetStartSoundFile;
      property EndSoundFile: string read GetEndSoundFile write SetEndSoundFile;
      property AutoDeleteFinishedDownloads: boolean read GetAutoDeleteFinishedDownloads write SetAutoDeleteFinishedDownloads;
      property AutoTryHtmlParser: boolean read GetAutoTryHtmlParser write SetAutoTryHtmlParser;
      property DownloadRetryCount: integer read GetDownloadRetryCount write SetDownloadRetryCount;
      property IgnoreMissingOpenSSL: boolean read GetIgnoreMissingOpenSSL write SetIgnoreMissingOpenSSL;
      property IgnoreMissingRtmpDump: boolean read GetIgnoreMissingRtmpDump write SetIgnoreMissingRtmpDump;
      property IgnoreMissingMSDL: boolean read GetIgnoreMissingMSDL write SetIgnoreMissingMSDL;
      property AddIndexToNames: TIndexForNames read GetAddIndexToNames write SetAddIndexToNames;
      property ScriptFileName: string read GetScriptFileName;
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
      property DownloadToTempFiles: boolean read GetDownloadToTempFiles write SetDownloadToTempFiles;
      property DownloadToProviderSubdirs: boolean read GetDownloadToProviderSubdirs write SetDownloadToProviderSubdirs;
    end;

implementation

uses
  {$IFDEF SETUP}
  uFunctions,
  {$ENDIF}
  uCompatibility,
  uSystem;

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
  XML_PATH_DELETFINISHEDDOWNLOADS = 'gui/auto_delete_finished_downloads';
  XML_PATH_STARTSOUND = 'gui/start_sound';
  XML_PATH_ENDSOUND = 'gui/end_sound';
  XML_PATH_STARTSOUNDFILE = 'gui/start_sound_file';
  XML_PATH_ENDSOUNDFILE = 'gui/end_sound_file';
  XML_PATH_AUTOTRYHTMLPARSER = 'config/auto_try_html_parser';
  XML_PATH_DOWNLOADRETRYCOUNT = 'config/download_retry_count';
  XML_PATH_SCRIPTFILENAME = 'config/script_filename';
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
  XML_PATH_DOWNLOADTOTEMPFILES = 'config/download_to_temp_files';
  XML_PATH_DOWNLOADTOPROVIDERSUBDIRS = 'config/download_to_provider_subdirectories';
  XML_PATH_IGNOREMISSINGOPENSSL = 'config/ignore_missing_openssl';
  XML_PATH_IGNOREMISSINGRTMPDUMP = 'config/ignore_missing_rtmpdump';
  XML_PATH_IGNOREMISSINGMSDL = 'config/ignore_missing_msdl';
  XML_PATH_ADDINDEXTONAMES = 'config/add_index_to_names';

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
  XML_DEFAULT_STARTSOUND = False;
  XML_DEFAULT_ENDSOUND = False;
  XML_DEFAULT_STARTSOUNDFILE = '';
  XML_DEFAULT_ENDSOUNDFILE = '';
  XML_DEFAULT_DELETFINISHEDDOWNLOADS = False;
  XML_DEFAULT_AUTOTRYHTMLPARSER = True;
  XML_DEFAULT_DOWNLOADRETRYCOUNT = 0;
  XML_DEFAULT_SCRIPTFILENAME = 'ytd-defs.xml';
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
  XML_DEFAULT_DOWNLOADTOTEMPFILES = False;
  XML_DEFAULT_DOWNLOADTOPROVIDERSUBDIRS = False;
  XML_DEFAULT_IGNOREMISSINGOPENSSL = True;
  XML_DEFAULT_IGNOREMISSINGRTMPDUMP = True;
  XML_DEFAULT_IGNOREMISSINGMSDL = True;
  XML_DEFAULT_ADDINDEXTONAMES = ifnNone;

resourcestring
  MSG_CANT_OVERWRITE_FILE = 'Cannot overwrite file "%s".';
  MSG_CANT_CREATE_FILE = 'Cannot create file "%s".';

{ TYTDOptions }

constructor TYTDOptions.Create;
///var PathBuf: array[0..MAX_PATH+1] of Char;
///    LocalXml, ExeXml: string;
begin
  inherited Create;
  fXml := TXmlDoc.Create;
  {LocalXml := ExtractFileName(ChangeFileExt(ParamStr(0), '.xml'));
  ExeXml := ChangeFileExt(ParamStr(0), '.xml');
  if FileExists(LocalXml) then
    fMainXmlFileName := LocalXml
  else if FileExists(ExeXml) then
    fMainXmlFileName := ExeXml
  else
    fMainXmlFileName := LocalXml;}
  ///fMainXmlFileName := GetAppConfigDir(False)+'ytd.xml';
  fMainXmlFileName := 'ytd.xml';

  fUserXmlFileName := '';
  ///if SHGetSpecialFolderPath(0, PathBuf, CSIDL_APPDATA, False) then
  ///  fUserXmlFileName := PathBuf + '\YTD\ytd.xml';
  Init;
end;

destructor TYTDOptions.Destroy;
begin
  FreeAndNil(fXml);
  inherited;
end;

function TYTDOptions.IgnoreInitErrors: boolean;
begin
  Result := True;
end;

procedure TYTDOptions.Init;
begin
  XmlFileName := fMainXmlFileName;
  Xml.Clear;
  Xml.Root.Name := XML_ROOTNAME;
  try
    if (not Load(IgnoreInitErrors)) or (not PortableMode) then
      if (fUserXmlFileName <> '') and (FileExists(fUserXmlFileName) or (not PortableMode)) then
        begin
        XmlFileName := fUserXmlFileName;
        Load(IgnoreInitErrors);
        PortableMode := False;
        end;
  finally
    if Xml.Root = nil then
      Xml.Clear;
    if Xml.Root.Name = '' then
      Xml.Root.Name := XML_ROOTNAME;
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
var
  Dir, TempFN: string;
begin
  try
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(XmlFileName));
    if Dir <> '' then
      ForceDirectories(ExpandFileName(Dir));
    Xml.SetIndentation(#9);
    TempFN := XmlFileName + '.new';
    if FileExists(TempFN) then
      SysUtils.DeleteFile(TempFN);
    Xml.SaveToFile(TempFN);
    if FileExists(TempFN) then
      begin
      if FileExists(XmlFileName) then
        if not SysUtils.DeleteFile(XmlFileName) then
          if not IgnoreErrors then
            begin
            SysUtils.DeleteFile(TempFN);
            Raise EInOutError.CreateFmt(MSG_CANT_OVERWRITE_FILE, [XmlFileName]);
            end;
      if not RenameFile(TempFN, XmlFileName) then
        Raise EInOutError.CreateFmt(MSG_CANT_CREATE_FILE, [XmlFileName]);
      end;
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
  if Result <> '' then
    if CharInSet(Result[1], ['0'..'9']) then
      Result := '_' + Result;
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

function TYTDOptions.ReadProviderOptionDef(const Provider, Option: string; const Default: string): string;
var s: string;
begin
  if ReadProviderOption(Provider, Option, s) then
    Result := s
  else
    Result := Default;
end;

function TYTDOptions.ReadProviderOptionDef(const Provider, Option: string; const Default: integer): integer;
var s: string;
begin
  if ReadProviderOption(Provider, Option, s) then
    Result := StrToIntDef(s, Default)
  else
    Result := Default;
end;

function TYTDOptions.ReadProviderOptionDef(const Provider, Option: string; const Default: boolean): boolean;
var s: string;
begin
  if ReadProviderOption(Provider, Option, s) then
    Result := XmlToBoolean(s, Default)
  else
    Result := Default;
end;

procedure TYTDOptions.WriteProviderOption(const Provider, Option, Value: string);
begin
  SetOption(Format(XML_PATH_PROVIDEROPTION, [TranslateNodeName(Provider), TranslateNodeName(Option)]), Value);
end;

procedure TYTDOptions.WriteProviderOption(const Provider, Option: string; Value: integer);
begin
  WriteProviderOption(Provider, Option, IntToStr(Value));
end;

procedure TYTDOptions.WriteProviderOption(const Provider, Option: string; Value: boolean);
begin
  WriteProviderOption(Provider, Option, BooleanToXml(Value));
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

function TYTDOptions.GetAutoDeleteFinishedDownloads: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_DELETFINISHEDDOWNLOADS), XML_DEFAULT_DELETFINISHEDDOWNLOADS);
end;

procedure TYTDOptions.SetAutoDeleteFinishedDownloads(const Value: boolean);
begin
  SetOption(XML_PATH_DELETFINISHEDDOWNLOADS, BooleanToXml(Value));
end;

function TYTDOptions.GetAutoTryHtmlParser: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_AUTOTRYHTMLPARSER), XML_DEFAULT_AUTOTRYHTMLPARSER);
end;

procedure TYTDOptions.SetAutoTryHtmlParser(const Value: boolean);
begin
  SetOption(XML_PATH_AUTOTRYHTMLPARSER, BooleanToXml(Value));
end;

function TYTDOptions.GetDownloadRetryCount: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_DOWNLOADRETRYCOUNT), XML_DEFAULT_DOWNLOADRETRYCOUNT);
end;

procedure TYTDOptions.SetDownloadRetryCount(const Value: integer);
begin
  SetOption(XML_PATH_DOWNLOADRETRYCOUNT, IntToStr(Value));
end;

function TYTDOptions.GetScriptFileName: string;
const
  DEFS_FILENAME = 'ytd-defs.xml';
var
  MainFileName, ProfileDir: string;
begin
  Result := GetOption(XML_PATH_SCRIPTFILENAME, '');
  if Result = '' then
    begin
    MainFileName := ExtractFilePath(ParamStr(0)) + DEFS_FILENAME;
    if PortableMode then
      Result := MainFileName
    else
      begin
      ProfileDir := ExtractFilePath(XmlFileName);
      Result := ProfileDir + DEFS_FILENAME;
      {$ifndef fpc}
      if (not FileExists(Result)) or (GetFileDateTime(Result) < GetFileDateTime(MainFileName)) then
      {$else}
      if (not FileExists(Result)) or (FileAge(Result) < FileAge(MainFileName)) then

      {$endif}
        begin
        if not DirectoryExists(ProfileDir) then
          CreateDir(ProfileDir);
        CopyFile(PChar(MainFileName), PChar(Result), False);
        end;
      end;
    end;
end;



function TYTDOptions.GetStartSound: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_STARTSOUND), XML_DEFAULT_STARTSOUND);
end;

procedure TYTDOptions.SetStartSound(const Value: boolean);
begin
  SetOption(XML_PATH_STARTSOUND, BooleanToXml(Value));
end;


function TYTDOptions.GetStartSoundFile: string;
begin
  Result := GetOption(XML_PATH_STARTSOUNDFILE, XML_DEFAULT_STARTSOUNDFILE);
end;


procedure TYTDOptions.SetStartSoundFile(const Value: string);
begin
  SetOption(XML_PATH_STARTSOUNDFILE, Value);
end;


function TYTDOptions.GetEndSound: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_ENDSOUND), XML_DEFAULT_ENDSOUND);
end;

procedure TYTDOptions.SetEndSound(const Value: boolean);
begin
  SetOption(XML_PATH_ENDSOUND, BooleanToXml(Value));
end;

function TYTDOptions.GetEndSoundFile: string;
begin
  Result := GetOption(XML_PATH_ENDSOUNDFILE, XML_DEFAULT_ENDSOUNDFILE);
end;

procedure TYTDOptions.SetEndSoundFile(const Value: string);
begin
  SetOption(XML_PATH_ENDSOUNDFILE, Value);
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

function TYTDOptions.GetDownloadToTempFiles: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_DOWNLOADTOTEMPFILES), XML_DEFAULT_DOWNLOADTOTEMPFILES);
end;

procedure TYTDOptions.SetDownloadToTempFiles(const Value: boolean);
begin
  SetOption(XML_PATH_DOWNLOADTOTEMPFILES, BooleanToXml(Value));
end;

function TYTDOptions.GetDownloadToProviderSubdirs: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_DOWNLOADTOPROVIDERSUBDIRS), XML_DEFAULT_DOWNLOADTOPROVIDERSUBDIRS);
end;

procedure TYTDOptions.SetDownloadToProviderSubdirs(const Value: boolean);
begin
  SetOption(XML_PATH_DOWNLOADTOPROVIDERSUBDIRS, BooleanToXml(Value));
end;

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
          List.Add({$IFDEF FPC} string {$ENDIF} (Node.Nodes[i].AttributeByNameWide[XML_CONVERTERLIST_ITEM_ID]));
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
    Converter.ID := {$IFDEF FPC} string {$ENDIF} (Node.AttributeByNameWide[XML_CONVERTERLIST_ITEM_ID]);
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

function TYTDOptions.WriteConverter(const Converter: TConverter): boolean;
var
  Node: TXmlNode;
begin
  if Converter.ID = '' then
    Result := False
  else
    begin
    if not Xml.NodeByPathAndAttr(XML_PATH_CONVERTERLIST + '/' + XML_CONVERTERLIST_ITEM, XML_CONVERTERLIST_ITEM_ID, Converter.ID, Node) then
      begin
      Node := XmlNodeByPathCreate(Xml, XML_PATH_CONVERTERLIST);
      Node := Node.NodeNew(XML_CONVERTERLIST_ITEM);
      Node.AttributeByNameWide[XML_CONVERTERLIST_ITEM_ID] := Converter.ID;
      end;
    XmlNodeByPathCreate(Node, XML_CONVERTERLIST_ITEM_TITLE).ValueAsUnicodeString := Converter.Title;
    XmlNodeByPathCreate(Node, XML_CONVERTERLIST_ITEM_EXEPATH).ValueAsUnicodeString := Converter.ExePath;
    XmlNodeByPathCreate(Node, XML_CONVERTERLIST_ITEM_COMMANDLINE).ValueAsUnicodeString := Converter.CommandLine;
    XmlNodeByPathCreate(Node, XML_CONVERTERLIST_ITEM_VISIBILITY).ValueAsUnicodeString := ConverterVisibilityStrings[Converter.Visibility];
    Result := True;
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

function TYTDOptions.CreateHttp: THttpSend;
begin
  Result := THttpSend.Create;
  try
    if ProxyActive then
      begin
      Result.ProxyHost := ProxyHost;
      Result.ProxyPort := ProxyPort;
      Result.ProxyUser := ProxyUser;
      Result.ProxyPass := ProxyPassword;
      end;
  except
    FreeAndNil(Result);
    Raise;
    end;
end;

function TYTDOptions.GetIgnoreMissingOpenSSL: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_IGNOREMISSINGOPENSSL), XML_DEFAULT_IGNOREMISSINGOPENSSL);
end;

procedure TYTDOptions.SetIgnoreMissingOpenSSL(const Value: boolean);
begin
  SetOption(XML_PATH_IGNOREMISSINGOPENSSL, BooleanToXml(Value));
end;

function TYTDOptions.GetIgnoreMissingRtmpDump: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_IGNOREMISSINGRTMPDUMP), XML_DEFAULT_IGNOREMISSINGRTMPDUMP);
end;

procedure TYTDOptions.SetIgnoreMissingRtmpDump(const Value: boolean);
begin
  SetOption(XML_PATH_IGNOREMISSINGRTMPDUMP, BooleanToXml(Value));
end;

function TYTDOptions.GetIgnoreMissingMSDL: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_IGNOREMISSINGMSDL), XML_DEFAULT_IGNOREMISSINGMSDL);
end;

procedure TYTDOptions.SetIgnoreMissingMSDL(const Value: boolean);
begin
  SetOption(XML_PATH_IGNOREMISSINGMSDL, BooleanToXml(Value));
end;

function TYTDOptions.GetAddIndexToNames: TIndexForNames;
var
  Opt: string;
begin
  Opt := GetOption(XML_PATH_ADDINDEXTONAMES);
  if Opt = 'none' then
    Result := ifnNone
  else if Opt = 'start' then
    Result := ifnStart
  else if Opt = 'end' then
    Result := ifnEnd
  else
    Result := XML_DEFAULT_ADDINDEXTONAMES;
end;

procedure TYTDOptions.SetAddIndexToNames(const Value: TIndexForNames);
begin
  case Value of
    ifnNone:  SetOption(XML_PATH_ADDINDEXTONAMES, 'none');
    ifnStart: SetOption(XML_PATH_ADDINDEXTONAMES, 'start');
    ifnEnd:   SetOption(XML_PATH_ADDINDEXTONAMES, 'end');
    end;
end;

end.
