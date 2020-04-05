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

unit guiFunctions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,  ShellApi,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  {$IFNDEF DELPHI7_UP} FileCtrl, {$ENDIF}
  {$IFDEF SETUP}
    uSetup, uSystem,
    {$IFNDEF GUI_WINAPI}
      Forms, Dialogs,
      {$IFDEF DELPHIXE4_UP}
      UITypes,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  SynaCode, SynaUtil,
  uFunctions, uLanguages, uDownloadList, uMessages, uStrings, uOptions, uUpgrade;

function GetProgressStr(DoneSize, TotalSize: int64): string;
procedure ReportBug(DownloadList: TDownloadList; Index: integer);
function IsHttpProtocol(const Url: string): boolean;
procedure UpgradeYTD(Upgrade: TYTDUpgrade; OwnerHandle: THandle);
function UpgradeDefs(Upgrade: TYTDUpgrade; OwnerHandle: THandle; ReportUpgradedProviders: boolean): boolean;
function DownloadAndInstallExternalLibrary(const Url: string; OwnerHandle: THandle; Options: TYTDOptions): boolean;
procedure CheckForExternalLibraries(OwnerHandle: THandle; Options: TYTDOptions);

{$IFDEF SINGLEINSTANCE}
procedure RegisterMainInstance(const MainFormHandle: THandle);
procedure UnregisterMainInstance(const MainFormHandle: THandle);
function FindMainInstance(out MainFormHandle: THandle): boolean;
{$ENDIF}

implementation

uses
  uScriptedDownloader, uRtmpDownloader, uMSDownloader;


function GetProgressStr(DoneSize, TotalSize: int64): string;
var n: int64;
begin
  Result := PrettySize(DoneSize);
  if TotalSize > 0 then
    begin
    n := 1000*DoneSize div TotalSize;
    Result := Format('%s (%d.%d%%)', [Result, n div 10, n mod 10]);
    end
end;


procedure ReportBug(DownloadList: TDownloadList; Index: integer);
var
  BugReportUrl, DefsVersion: string;
begin
  DefsVersion := '';
  if TScriptedDownloader.MainScriptEngine <> nil then
    DefsVersion := TScriptedDownloader.MainScriptEngine.Version;
  {BugReportUrl := Format(BUGREPORT_URL,
                       [ APPLICATION_VERSION,
                         DefsVersion,
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList.Urls[Index]))),
                         EncodeUrl(AnsiString(StringToUtf8(DownloadList[Index].Downloader.LastErrorMsg)))
                       ]);
  Run(BugReportUrl);}
  BugReportUrl := Format(BUGREPORT,
                       [ APPLICATION_VERSION,
                         DefsVersion,
                         (AnsiString(StringToUtf8(DownloadList.Urls[Index]))),
                         (AnsiString(StringToUtf8(DownloadList[Index].Downloader.LastErrorMsg)))
                       ]);
  with TFileStream.create('bugreport.txt',fmCreate) do
    try
      writeBuffer(BugReportUrl[1],length(BugReportUrl));
    finally
      free;
    end;



end;

function IsHttpProtocol(const Url: string): boolean;
var Protocol, User, Password, Host, Port, Path, Params: string;
begin
  ParseUrl(Url, Protocol, User, Password, Host, Port, Path, Params);
  if AnsiCompareText(Protocol, 'http') = 0 then
    Result := True
  else if AnsiCompareText(Protocol, 'https') = 0 then
    Result := True
  else
    Result := False;
end;

procedure UpgradeYTD(Upgrade: TYTDUpgrade; OwnerHandle: THandle);
begin
  if Upgrade <> nil then
    begin
    {$IFDEF SETUP}
    if Upgrade.OnlineYTDUrl = '' then
      Upgrade.TestUpgrades(False, False);
    if Upgrade.OnlineYTDUrl <> '' then
      if MessageBox(OwnerHandle, PChar(MSG_DOWNLOAD_OR_UPGRADE), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONQUESTION or $00002000{MB_TASKMODAL}) = idYes then
        if (Upgrade.OnlineYTD <> nil) or Upgrade.DownloadYTDUpgrade(False, False) then
          if Upgrade.UpgradeYTD then
            {$IFDEF GUI_WINAPI}
            ExitProcess(0)
            {$ELSE}
            Application.Terminate
            {$ENDIF}
          else
            {$IFDEF GUI_WINAPI}
            MessageBox(OwnerHandle, PChar(MSG_FAILED_TO_UPGRADE), PChar(APPLICATION_TITLE), MB_OK or MB_ICONSTOP or MB_TASKMODAL)
            {$ELSE}
            MessageDlg(MSG_FAILED_TO_UPGRADE, mtError, [mbOK], 0)
            {$ENDIF}
        else
          {$IFDEF GUI_WINAPI}
          MessageBox(OwnerHandle, PChar(MSG_FAILED_TO_DOWNLOAD_UPGRADE + Upgrade.OnlineYTDUrl), PChar(APPLICATION_TITLE), MB_OK or MB_ICONSTOP or MB_TASKMODAL)
          {$ELSE}
          MessageDlg(MSG_FAILED_TO_DOWNLOAD_UPGRADE + Upgrade.OnlineYTDUrl, mtError, [mbOK], 0)
          {$ENDIF}
      else
    else
    {$ENDIF}
    if Upgrade.OnlineYTDUrl <> '' then
      Run(Upgrade.OnlineYTDUrl, OwnerHandle);
    end;
end;

function UpgradeDefs(Upgrade: TYTDUpgrade; OwnerHandle: THandle; ReportUpgradedProviders: boolean): boolean;
const
  MAX_PROVIDERS_TO_SHOW = 10;
var
  LastUpgrade: TDateTime;
  LastUpgradeVersion: integer;
  Providers: TStringList;
  i, n: integer;
begin
  Result := False;
  if Upgrade <> nil then
    begin
    if Upgrade.OnlineDefs = nil then
      Upgrade.DownloadDefsUpgrade(False, False);
    if Upgrade.OnlineDefs <> nil then
      if Upgrade.OnlineDefs.Size > 0 then
        if TScriptedDownloader.MainScriptEngine <> nil then
          begin
          TScriptedDownloader.MainScriptEngine.GetUpgradedScriptVersion(LastUpgrade, LastUpgradeVersion);
          TScriptedDownloader.MainScriptEngine.LoadFromStream(Upgrade.OnlineDefs);
          TScriptedDownloader.MainScriptEngine.SaveToFile;
          Result := True;
          if ReportUpgradedProviders then
            begin
            Providers := TStringList.Create;
              try
                if TScriptedDownloader.MainScriptEngine.GetUpgradedScriptsSince(LastUpgrade, LastUpgradeVersion, Providers) then
                  begin
                  n := Providers.Count;
                  if n > 0 then
                    begin
                    if n > MAX_PROVIDERS_TO_SHOW then
                      begin
                      for i := Pred(n) downto MAX_PROVIDERS_TO_SHOW do
                        Providers.Delete(i);
                      Providers.Add(Format(_('... and %d others'), [n-MAX_PROVIDERS_TO_SHOW]));
                      end;
                    end;
                  {$IFDEF GUI_WINAPI}
                  MessageBox(0, PChar(_(MSG_PROVIDER_DEFINITIONS_UPGRADED) + EOLN + EOLN + Providers.Text), PChar(APPLICATION_TITLE), MB_OK or MB_TASKMODAL);
                  {$ELSE}
                  MessageDlg(_(MSG_PROVIDER_DEFINITIONS_UPGRADED) + EOLN + EOLN + Providers.Text, mtInformation, [mbOK], 0);
                  {$ENDIF}
                  end;
              finally
                FreeAndNil(Providers);
                end;
              end;
          end;
    end;
end;

{$IFDEF SINGLEINSTANCE}

const
  MAXIMUM_INSTANCE_COUNT = 256;

type
  TInstanceInfo = packed record
    Count: integer;
    MainFormHandles: array[0..MAXIMUM_INSTANCE_COUNT-1] of THandle;
    end;
  PInstanceInfo = ^TInstanceInfo;
  
var
  InstanceMappingHandle: THandle = 0;
  InstanceInfo: PInstanceInfo = nil;

function GetInstanceMutex(out Mutex: THandle): boolean;
var
  MutexName: string;
begin
  Result := False;
  MutexName := StringReplace(ParamStr(0), '\', '/', [rfReplaceAll]) + ('*mutex' + {$IFDEF WIN64} '*x64' {$ELSE} '*x32' {$ENDIF} );
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if Mutex <> 0 then
    if WaitForSingleObject(Mutex, 1000) = WAIT_FAILED then
      begin
      ReleaseMutex(Mutex);
      CloseHandle(Mutex);
      end
    else
      Result := True;
end;

procedure RegisterMainInstance(const MainFormHandle: THandle);
var
  Dummy, Mutex: THandle;
begin
  if (MainFormHandle = INVALID_HANDLE_VALUE) or (MainFormHandle = 0) then
    Exit;
  FindMainInstance(Dummy);
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        if InstanceInfo^.Count < MAXIMUM_INSTANCE_COUNT then
          begin
          InstanceInfo^.MainFormHandles[InstanceInfo^.Count] := MainFormHandle;
          InstanceInfo^.Count := Succ(InstanceInfo^.Count);
          end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

procedure UnregisterMainInstance(const MainFormHandle: THandle);
var
  Mutex: THandle;
  i: integer;
begin
  if (MainFormHandle = INVALID_HANDLE_VALUE) or (MainFormHandle = 0) then
    Exit;
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        for i := 0 to Pred(InstanceInfo^.Count) do
          if InstanceInfo^.MainFormHandles[i] = MainFormHandle then
            begin
            InstanceInfo^.MainFormHandles[i] := 0;
            Break;
            end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

function FindMainInstance(out MainFormHandle: THandle): boolean;
var
  MappingName: string;
  Mutex: THandle;
  i: integer;
begin
  Result := False;
  if InstanceMappingHandle = 0 then
    begin
    InstanceInfo := nil;
    MappingName := StringReplace(ParamStr(0), '\', '/', [rfReplaceAll]) + ('*mapping' + {$IFDEF WIN64} '*x64' {$ELSE} '*x32' {$ENDIF} );
    InstanceMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, Sizeof(TInstanceInfo), PChar(MappingName));
    if InstanceMappingHandle <> 0 then
      InstanceInfo := MapViewOfFile(InstanceMappingHandle, FILE_MAP_ALL_ACCESS, 0, 0, Sizeof(TInstanceInfo));
    end;
  if InstanceInfo <> nil then
    if GetInstanceMutex(Mutex) then
      try
        for i := 0 to Pred(InstanceInfo^.Count) do
          if InstanceInfo^.MainFormHandles[i] <> 0 then
            begin
            MainFormHandle := InstanceInfo^.MainFormHandles[i];
            Result := True;
            Break;
            end;
      finally
        ReleaseMutex(Mutex);
        CloseHandle(Mutex);
        end;
end;

{$ENDIF}

function DownloadAndInstallExternalLibrary(const Url: string; OwnerHandle: THandle; Options: TYTDOptions): boolean;
var
  LibData: TMemoryStream;
  Dir: string;
begin
  Result := False;
  LibData := TMemoryStream.Create;
  try
    if DownloadFromHttp(Url, Options, LibData) then
      begin
      LibData.Position := 0;
      if not Result then
        if Unzip(LibData, ExtractFilePath(ExpandFileName(ParamStr(0)))) then
          Result := True;
      {$IFDEF SETUP}
      if not Result then
        begin
        ///Dir := SystemTempFile(SystemTempDir, 'YTDLib');
        Dir := 'YTDLib';
        ForceDirectories(Dir);
        if not Unzip(LibData, Dir) then
          {$ifdef mswindows}
          ///ForceDeleteDirectory(Dir)
          {.$else}
          RemoveDir(Dir)
          {$endif}
        else
          Result := Run(ParamStr(0), SETUP_PARAM_INSTALL_LIBRARY + ' ' + AnsiQuotedStr(Dir, '"'), 0, True);
        end;
      {$ENDIF}
      end;
  finally
    FreeAndNil(LibData);
    end;
end;

procedure CheckForExternalLibraries(OwnerHandle: THandle; Options: TYTDOptions);
var
  NeedsRestart: boolean;
begin
  NeedsRestart := False;
  // OpenSSL
  if not IsSSLAvailable then
    if not Options.IgnoreMissingOpenSSL then
      if MessageBox(OwnerHandle, PChar(MSG_OPENSSL_NOT_FOUND + #10#10 + MSG_OPENSSL_NOT_FOUND_ACTION_SUFFIX), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONWARNING or $00002000{MB_TASKMODAL}) = idYes then
        if DownloadAndInstallExternalLibrary(MY_OPENSSL_URL, OwnerHandle, Options) then
          NeedsRestart := True
        else
          Run(OPENSSL_URL);
  // RtmpDump
  if not TRtmpDownloader.CheckForPrerequisites then
    if not Options.IgnoreMissingRtmpDump then
      if MessageBox(OwnerHandle, PChar(MSG_RTMPDUMP_NOT_FOUND + #10#10 + MSG_RTMPDUMP_NOT_FOUND_ACTION_SUFFIX), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONWARNING or $00002000{MB_TASKMODAL}) = idYes then
        if DownloadAndInstallExternalLibrary(MY_RTMPDUMP_URL, OwnerHandle, Options) then
          NeedsRestart := True
        else
          Run(RTMPDUMP_URL);
  // RtmpDump
  if not TMSDownloader.CheckForPrerequisites then
    if not Options.IgnoreMissingMSDL then
      if MessageBox(OwnerHandle, PChar(MSG_MSDL_NOT_FOUND + #10#10 + MSG_MSDL_NOT_FOUND_ACTION_SUFFIX), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONWARNING or $00002000{MB_TASKMODAL}) = idYes then
        if DownloadAndInstallExternalLibrary(MY_MSDL_URL, OwnerHandle, Options) then
          NeedsRestart := True
        else
          Run(MSDL_URL);
  // Restart?
  if NeedsRestart then
    MessageBox(OwnerHandle, PChar(MSG_EXTERNAL_LIBS_WERE_DOWNLOADED), PChar(APPLICATION_TITLE), MB_ICONINFORMATION or MB_OK or $00002000{MB_TASKMODAL})
end;

initialization

finalization
  {$IFDEF SINGLEINSTANCE}
    if InstanceInfo <> nil then
      if UnmapViewOfFile(InstanceInfo) then
        InstanceInfo := nil;
    if InstanceMappingHandle <> INVALID_HANDLE_VALUE then
      if CloseHandle(InstanceMappingHandle) then
        InstanceMappingHandle := INVALID_HANDLE_VALUE;
  {$ENDIF}

end.
