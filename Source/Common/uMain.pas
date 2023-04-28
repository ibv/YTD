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

unit uMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$INCLUDE 'ytd.inc'}

{uses FileUtil;}

interface

procedure Main;



implementation

uses
  {$ifdef mswindows}
    CommCtrl, Windows,
    ShlObj,
  {$ELSE}
    LCLIntf, LCLType, LMessages, FileUtil,
  {$ENDIF}
  SysUtils, Messages,
  {$IFDEF SETUP}

    {$IFNDEF DELPHI7_UP}
    FileCtrl,
    {$ENDIF}
    uSetup,
    uCompatibility,
    {$IFDEF SETUP_GUI}
      {$IFDEF GUI_WINAPI}
        guiSetupWINAPI,
      {$ELSE}
        {$IFNDEF GUI_LCL}
  	  guiSetupVCL,
  	{$ELSE}
          guiSetupLCL,
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF CLI}
    uYTD,
    uConsoleApp,
  {$ENDIF}
  {$IFDEF GUI}
    guiConsts,
    guiFunctions,
    {$IFDEF GUI_WINAPI}
      guiMainWINAPI,
    {$ELSE}
      Forms,
      {$IFNDEF GUI_LCL}
        guiMainVCL,
      {$ELSE}
        guiMainLCL,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uSystem, uFunctions, uMessages;

type
  TStartupType = (
    {$IFDEF CLI}
      stCLI, // Running in command-line mode
    {$ENDIF}
    {$IFDEF GUI}
      stGUI, // Automatical decision, running in GUI
      stGUIexplicit, // Running in GUI, required by the user
    {$ENDIF}
    {$IFDEF SETUP}
      stInstall, // Running from the installer, need to install YTD
      stUninstallPhase1, // First phase of the uninstallation: Copy YTD to temp directory and continue uninstall
      stUninstallPhase2, // Second phase of the uninstallation: Remove YTD files from the installation directory
      stInstallLibrary, // Need to install
    {$ENDIF}
      stNone
    );

const
  RESCODE_OK = 0;
  RESCODE_INSTALL_FAILED = 253;
  RESCODE_UNINSTALL_FAILED = 252;

var
  StartedFromIDE: boolean;
  {$IFDEF GUI}
    {$IFDEF CLI}
    RunExternal: boolean;
    {$ENDIF}
  {$ENDIF}
  ErrorMsg: string;
  {$IFDEF SETUP}
  InstallDir: string;
  DesktopShortcut, StartMenuShortcut, RestartYTD: boolean;
  {$ENDIF}

function FindStartupType( {$IFDEF SETUP} var InstallDir: string; var DesktopShortcut, StartMenuShortcut, RestartYTD: boolean {$ENDIF} ): TStartupType;
{$IFDEF SETUP}
var
  i: integer;
  Param: string;
  {$IFDEF SETUP_GUI}
  F: TFormSetup;
  {$ENDIF}
{$ENDIF}
begin
  Result := Low(TStartupType);
  // No parameters runs GUI if available, otherwise CLI
  {$IFDEF GUI}
  if ParamCount = 0 then
    Result := stGUI
  else
  {$ENDIF}
  // Otherwise check for startup-type parameters
  {$IFDEF SETUP}
  for i := 1 to ParamCount do
    begin
    Param := ParamStr(i);
    if False then
      begin
      end
    {$IFDEF GUI}
    else if AnsiCompareText(Param, SETUP_PARAM_GUI) = 0 then
      begin
      Result := stGUIexplicit;
      Break;
      end
    {$ENDIF}
    {$IFDEF SETUP_GUI}
    else if AnsiCompareText(Param, SETUP_PARAM_SETUP) = 0then
      begin
      {$IFNDEF DEBUG}
        {$IFNDEF FPC}
          FreeConsole;
          IsConsole := False;
        {$ENDIF}
      {$ENDIF}
      F := TFormSetup.Create(nil);
      try
        case F.ShowModal of
          idOK:
            begin
            Result := stInstall;
            InstallDir := F.DestinationDir;
            DesktopShortcut := F.DesktopShortcut;
            StartMenuShortcut := F.StartMenuShortcut;
            RestartYTD := True;
            end;
          idIgnore:
            Result := {$IFDEF GUI} stGUI {$ELSE} {$IFDEF CLI} stCli {$ELSE} stNone {$ENDIF} {$ENDIF} ;
          else
            Result := stNone;
          end;
      finally
        FreeAndNil(F);
        end;
      Break;
      end
    {$ENDIF}
    else if AnsiCompareText(Param, SETUP_PARAM_UNINSTALL) = 0 then
      begin
      InstallDir := '';
      if i < ParamCount then
        InstallDir := ParamStr(Succ(i));
      if (InstallDir <> '') and FileExists(InstallDir){ *Převedeno z FileExists* } then
        Result := stUninstallPhase2
      else
        Result := stUninstallPhase1;
      end
    else if AnsiCompareText(Param, SETUP_PARAM_INSTALL_LIBRARY) = 0 then
      begin
      Result := stInstallLibrary;
      InstallDir := ParamStr(Succ(i));
      Break;
      end
    else if (AnsiCompareText(Param, SETUP_PARAM_UPGRADE) = 0) or (AnsiCompareText(Param, SETUP_PARAM_UPGRADE_GUI) = 0) or (AnsiCompareText(Param, SETUP_PARAM_INSTALL) = 0) or (AnsiCompareText(Param, SETUP_PARAM_INSTALL_GUI) = 0) then
      begin
      if i < ParamCount then
        begin
        Result := stInstall;
        InstallDir := ParamStr(Succ(i));
        DesktopShortcut := (AnsiCompareText(Param, SETUP_PARAM_INSTALL) = 0) or (AnsiCompareText(Param, SETUP_PARAM_INSTALL_GUI) = 0);
        StartMenuShortcut := (AnsiCompareText(Param, SETUP_PARAM_INSTALL) = 0) or (AnsiCompareText(Param, SETUP_PARAM_INSTALL_GUI) = 0);
        RestartYTD := (AnsiCompareText(Param, SETUP_PARAM_UPGRADE_GUI) = 0) or (AnsiCompareText(Param, SETUP_PARAM_INSTALL_GUI) = 0);
        Sleep(500); // to give some time for the caller to quit
        Break;
        end;
      end;
    end;
  {$ENDIF}
end;

{$IFDEF CLI}
procedure RunCLI;
begin
  ExitCode := ExecuteConsoleApp(TYTD);
  if StartedFromIDE then
    begin
    Writeln;
    Write(MSG_PRESS_ANY_KEY_TO_QUIT);
    Readln;
    end;
end;
{$ENDIF}

{$IFDEF GUI}
procedure RunGUI;
{$IFDEF SINGLEINSTANCE}
var
  MainInstance: THandle;
  CopyData: TCopyDataStruct;
  Param: string;
  ParamW: WideString;
  i: integer;
{$ENDIF}
begin
  {$IFDEF SINGLEINSTANCE}
    if FindMainInstance(MainInstance) then
      begin
      for i := 1 to ParamCount do
        if AnsiCompareText(ParamStr(i), '-new') = 0 then
          begin
          MainInstance := 0;
          Break;
          end;
      if MainInstance <> 0 then
        begin
        for i := 1 to ParamCount do
          begin
          Param := ParamStr(i);
          if Param <> '' then
            if Param[1] <> '-' then
              begin
              ParamW := WideString(Param);
              CopyData.dwData := COPYDATA_URL;
              CopyData.cbData := Length(ParamW) * Sizeof(WideChar);
              CopyData.lpData := @(ParamW[1]);
              SendMessage(MainInstance, WM_COPYDATA, hInstance, LPARAM(@CopyData));
              end;
          end;
        SetForegroundWindow(MainInstance);
        Exit;
        end;
      end;
  {$ENDIF}
  {$IFNDEF DEBUG}
    {$IFNDEF FPC}
      FreeConsole;
      IsConsole := False;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF GUI_WINAPI}
    with TFormMain.Create do
      try
        ShowModal;
      finally
        Free;
        {$IFDEF DIRTYHACKS}
        ExitProcess(ExitCode);
          // Without ExitProcess(), YTD will crash with an access violation
          // in one of the finalization sections (don't know which)
          // Steps to reproduce:
          //   - Run YTD in GUI mode
          //   - Ask for download of https://www.youtube.com/watch?v=k7xkS_h8u0c
          //   - The download fails due to an unsupported obfuscation scheme
          //   - Exit YTD
          // It seems that the AV occurs in a unit two removed after uSystem.pas
          // (in version 1.56) and that it was caused by memory overwrite because
          // when System.FinalizeUnits calls TProc(P)(), procedure System._IntfClear
          // which has one argument is being called.
          // This is dependent on GUI_WINAPI, it doesn't occur in VCL.
        {$ENDIF}
        end;
  {$ELSE}
    Application.Initialize;
    Application.Title := 'YTD';
    Application.CreateForm(TFormYTD, FormYTD);
    Application.Run;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF SETUP}
procedure RunInstall(const InstallDir: string; DesktopShortcut, StartMenuShortcut, RestartYTD: boolean);

  function CopyFiles(const SourceDir, DestinationDir: string): boolean;
    var SR: TSearchRec;
    begin
      Result := True;
      ForceDirectories(ExpandFileName(DestinationDir));
      if FindFirst(SourceDir + '*.*', faAnyFile, SR) = 0 then
        try
          repeat
            if Longbool(SR.Attr and faDirectory) then
              begin
              if (SR.Name <> '.') and (SR.Name <> '..') then
                if not CopyFiles(SourceDir + SR.Name + '\', DestinationDir + SR.Name + '\') then
                  Result := False;
              end
            else
              begin
              if not CopyFile(PChar(SourceDir + SR.Name), PChar(DestinationDir + SR.Name), False) then
                Result := False;
              end;
          until FindNext(SR) <> 0;
        finally
          SysUtils.FindClose(SR);
          end;
    end;

var OK: boolean;
    InstDir, InstExe: string;
begin
  OK := False;
  ExitCode := RESCODE_INSTALL_FAILED;
  InstDir := IncludeTrailingPathDelimiter(InstallDir);
  InstExe := InstDir + ExtractFileName(ParamStr(0));
  if InstallDir <> '' then
    begin
    OK := CopyFiles(ExtractFilePath(ParamStr(0)), InstDir);
    if OK then
      begin
      if DesktopShortcut then
        ///CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_DESKTOPDIRECTORY, InstExe, SETUP_PARAM_GUI);
      if StartMenuShortcut then
        ///CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_COMMON_PROGRAMS, InstExe, SETUP_PARAM_GUI);
      ///RegisterUninstallApplication(APPLICATION_UNINSTALL_ID, 'YTD (pepak)', AnsiQuotedStr(InstExe, '"') + ' --uninstall');
      end;
    end;
  if not OK then
    begin
    {$IFDEF FPC}
      Writeln(ERR_INSTALL_FAILED);
    {$ELSE}
      {$IFDEF CLI}
      if TConsoleApp.HasConsole = csOwnConsole then
        Writeln(ERR_INSTALL_FAILED)
      else
      {$ENDIF}
        MessageBox(0, PChar(ERR_INSTALL_FAILED), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or MB_TASKMODAL);
    {$ENDIF}
    end
  else
    begin
    ExitCode := RESCODE_OK;
    if RestartYTD then
      Run(InstExe, '', ExcludeTrailingPathDelimiter(InstDir));
    end;
end;

procedure RunInstallLibrary(const InstallSource: string);
var
  SrcDir, DestDir, FailedList: string;
  SR: TSearchRec;
begin
  ExitCode := RESCODE_INSTALL_FAILED;
  SrcDir := ExcludeTrailingPathDelimiter(InstallSource);
  if DirectoryExists(SrcDir) then
    if FindFirst(SrcDir + '\*.*', faAnyFile and (not faDirectory), SR) = 0 then
      try
        SrcDir := IncludeTrailingPathDelimiter(SrcDir);
        DestDir := ExtractFilePath(ParamStr(0));
        FailedList := '';
        repeat
          if not CopyFile(PChar(SrcDir + SR.Name), PChar(DestDir + SR.Name), False) then
            FailedList := FailedList + #13#10 + SR.Name;
        until FindNext(SR) <> 0;
        if FailedList <> '' then
          begin
          {$IFDEF CLI}
          if TConsoleApp.HasConsole = csOwnConsole then
            Writeln(Format(ERR_INSTALL_LIBRARY_FAILED, [FailedList]))
          else
          {$ENDIF}
            MessageBox(0, PChar(Format(ERR_INSTALL_LIBRARY_FAILED, [FailedList])), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or $00002000{MB_TASKMODAL});
          end
        else
          begin
          ExitCode := RESCODE_OK;
          end;
      finally
        SysUtils.FindClose(SR);
      end;
end;

procedure RunUninstallPhase1;
var
  Dir, FileName, FN: string;
begin
  ExitCode := RESCODE_UNINSTALL_FAILED;
  if MessageBox(0, PChar(MSG_WANT_TO_UNINSTALL), PChar(APPLICATION_TITLE), MB_YESNOCANCEL or MB_ICONQUESTION or $00002000{MB_TASKMODAL}) = idYes then
    begin
    ///Dir := IncludeTrailingPathDelimiter(SystemTempFile(SystemTempDir, 'YTD'));
    Dir := IncludeTrailingPathDelimiter('YTD');
    CreateDir(Dir);
    FileName := Dir + ExtractFileName(ParamStr(0));
    if CopyFile(PChar(ParamStr(0)), PChar(FileName), False) then
      begin
      {$IFDEF DELPHI6_UP}
        {$MESSAGE HINT 'Rewrite to not use setup.exe anymore - run YTD elevated directly'}
      {$ENDIF}
      FN := 'setup.exe';
      if FileExists(ExtractFilePath(ParamStr(0)) + FN) then
        if CopyFile(PChar(FN), PChar(Dir + FN), False) then
          FileName := Dir + FN;
      if FileExists(FileName) then
        if Run(FileName, SETUP_PARAM_UNINSTALL + ' ' + AnsiQuotedStr(ParamStr(0), '"')) then
          begin
          {$IFDEF DIRTYHACKS}
          Sleep(100); // Without this, the FileName doesn't actually execute. Might be necessary to use CreateProcess?
          {$ENDIF}
          ExitCode := RESCODE_OK;
          end;
      end;
    end;
end;

procedure RunUninstallPhase2(const ExeFileName: string);
const
  MAX_DELETE_TRIES = 5;
  DELAY_BETWEEN_TRIES = 100;
var
  Dir: string;
  i: integer;
begin
  ExitCode := RESCODE_UNINSTALL_FAILED;
  if InstallDir <> '' then
    if FileExists(ExeFileName) then
      if FileContentsSame(ExeFileName, ParamStr(0)) then
        begin
        Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(ExeFileName));
        if (Dir <> '') and (ExtractFilePath(Dir) <> '') then
          begin
          for i := 1 to MAX_DELETE_TRIES do
            ///if ForceDeleteDirectory(Dir) then
            if RemoveDir(Dir) then
              Break
            else
              Sleep(DELAY_BETWEEN_TRIES);
          ///DeleteShortcut(APPLICATION_SHORTCUT, '', CSIDL_DESKTOPDIRECTORY, ExeFileName);
          ///DeleteShortcut(APPLICATION_SHORTCUT, '', CSIDL_COMMON_PROGRAMS, ExeFileName);
          ///UnregisterUninstallApplication(APPLICATION_UNINSTALL_ID);
          ExitCode := RESCODE_OK;
          MessageBox(0, PChar(MSG_UNINSTALL_COMPLETE), PChar(APPLICATION_TITLE), MB_OK or $00002000{MB_TASKMODAL});
          end;
        end;
end;
{$ENDIF}

procedure Main;
begin
  try
    ExitCode := RESCODE_OK;
    ///InitCommonControls; // Needed because of the manifest file
    // Test for IDE
    StartedFromIDE := False;
    {$IFNDEF FPC}
      {$IFDEF DELPHI7_UP}
        {$WARN SYMBOL_PLATFORM OFF}
      {$ENDIF}
      if DebugHook <> 0 then
        StartedFromIDE := True;
      {$IFDEF DELPHI7_UP}
        {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
    {$ENDIF}
    // Determine the startup type and parameters
    Randomize;
    {$IFDEF SETUP}
    InstallDir := '';
    DesktopShortcut := False;
    StartMenuShortcut := False;
    RestartYTD := False;
    {$ENDIF}
    case FindStartupType( {$IFDEF SETUP} InstallDir, DesktopShortcut, StartMenuShortcut, RestartYTD {$ENDIF} ) of
      {$IFDEF CLI}
      stCLI:
        RunCLI;
      {$ENDIF}
      {$IFDEF GUI}
      stGUIexplicit:
        RunGUI;
      stGUI:
        begin
        {$IFDEF CLI}
          RunExternal := (not StartedFromIDE);
          {$IFNDEF FPC}
            if RunExternal then
              begin
              FreeConsole;
              if not TConsoleApp.ParentHasConsole then
                RunExternal := False;
              end;
          {$ENDIF}
          if (not RunExternal) or (not Run(ParamStr(0), {$IFDEF SETUP} SETUP_PARAM_GUI {$ELSE} '' {$ENDIF} )) then
        {$ENDIF}
          RunGUI;
        end;
      {$ENDIF}
      {$IFDEF SETUP}
      stInstall:
        RunInstall(InstallDir, DesktopShortcut, StartMenuShortcut, RestartYTD);
      stInstallLibrary:
        RunInstallLibrary(InstallDir);
      stUninstallPhase1:
        RunUninstallPhase1;
      stUninstallPhase2:
        RunUninstallPhase2(InstallDir);
      {$ENDIF}
      end;
  except
    on E: Exception do
      begin
      ErrorMsg := Format(ERR_EXCEPTION_MESSAGE, [E.ClassName, E.Message]);
      {$IFDEF FPC}
        Writeln(ErrorMsg);
      {$ELSE}
        {$IFDEF CLI}
        if TConsoleApp.HasConsole = csOwnConsole then
          Writeln(ErrorMsg)
        else
        {$ENDIF}
          MessageBox(0, PChar(ErrorMsg), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or MB_TASKMODAL);
      {$ENDIF}
      ExitCode := 255;
      end;
    end;
end;

end.
