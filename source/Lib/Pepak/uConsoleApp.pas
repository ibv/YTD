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

unit uConsoleApp;
{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uStrings, uFiles;

type
  TConsoleApp = class;
  TConsoleAppClass = class of TConsoleApp;

  EConsoleAppError = class(Exception);

  TConsoleColor = (ccBlack, ccBlue, ccGreen, ccCyan, ccRed, ccMagenta, ccBrown, ccLightGray,
                   ccDarkGray, ccLightBlue, ccLightGreen, ccLightCyan, ccLightRed, ccLightMagenta, ccYellow, ccWhite);
  TConsoleState = (csNoConsole, csOwnConsole, csParentConsole);

  TConsoleCtrlEvent = procedure(Sender: TConsoleApp; CtrlType: DWORD; var Handled: boolean) of object;

  TConsoleApp = class
    private
      fCurrentParamIndex: integer;
      fStdIn: THandle;
      fStdOut: THandle;
      fStdOutRedirected: boolean;
      fTextAttr: byte;
      fLineSeparator: string;
      fOnConsoleCtrl: TConsoleCtrlEvent;
      fConvertWritesToOEM: boolean;
      function GetTextAttr: Byte;
      procedure SetTextAttr(const Value: Byte); overload;
      function GetTextBackground: TConsoleColor;
      procedure SetTextBackground(const Value: TConsoleColor);
      function GetTextColor: TConsoleColor;
      procedure SetTextColor(const Value: TConsoleColor);
      function GetCursorCol: integer;
      function GetCursorRow: integer;
      procedure SetCursorCol(const Value: integer);
      procedure SetCursorRow(const Value: integer);
      function GetConsoleWindowHandle: THandle;
    protected // Application info
      function AppTitle: string; virtual; abstract;
      function AppVersion: string; virtual; abstract;
      function DoExecute: integer; virtual; abstract;
    protected // Parameter handling methods
      procedure ParamInitialize; virtual;
      function ParamGetNext(out Value: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected // Console
      procedure InitConsole; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure DoneConsole; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetTextAttribute(const Text, Background: TConsoleColor); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure Write(const Msg: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure Writeln(const Msg: string = ''); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WriteColored(Color: TConsoleColor; const Msg: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure WritelnColored(Color: TConsoleColor; const Msg: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure DoConsoleCtrl(const CtrlType: DWORD; var Handled: boolean); virtual;
      function ReadPassword: string;
      property StdIn: THandle read fStdIn;
      property StdOut: THandle read fStdOut;
      property StdOutRedirected: boolean read fStdOutRedirected;
      property TextAttribute: Byte read GetTextAttr write SetTextAttr;
      property TextColor: TConsoleColor read GetTextColor write SetTextColor;
      property TextBackground: TConsoleColor read GetTextBackground write SetTextBackground;
      property CursorRow: integer read GetCursorRow write SetCursorRow;
      property CursorCol: integer read GetCursorCol write SetCursorCol;
      property LineSeparator: string read fLineSeparator write fLineSeparator;
      property ConvertWritesToOEM: boolean read fConvertWritesToOEM write fConvertWritesToOEM;
      property ConsoleWindowHandle: THandle read GetConsoleWindowHandle;
    protected // Output methods
      procedure ShowHeader; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ShowError(const Msg: string); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ShowError(const Msg: string; const Params: array of const); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ShowError(Error: Exception); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ShowSyntax(const Error: string = ''); overload; virtual;
      procedure ShowSyntax(const Error: string; const Params: array of const); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure Log(const LogFileName: string; const Msg: string); overload; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure Log(const LogFileName: string; const Msg: string; const Params: array of const); overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected // Files
      function ProcessWildCardFile(const FileName: string; const SearchRec: TSearchRec): boolean; virtual; abstract;
      function ProcessWildCard(const WildCard: string; Recursive: boolean): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    public
      constructor Create; virtual;
      destructor Destroy; override;
      function Execute: DWORD; virtual;
      property OnConsoleCtrl: TConsoleCtrlEvent read fOnConsoleCtrl write fOnConsoleCtrl;
    public
      class function HasConsole: TConsoleState; virtual;
      class function ParentHasConsole: boolean; virtual;
    end;

const
  RESCODE_OK = 0;
  RESCODE_ABORT = 254;
  RESCODE_EXCEPTION = 255;

function ExecuteConsoleApp(AppClass: TConsoleAppClass): integer;

implementation

const ATTACH_PARENT_PROCESS = $FFFFFFFF;

type TGetConsoleWindowFn = function (): THandle; stdcall;
     TAttachConsoleFn = function(dwProcessId: DWORD): BOOL; stdcall;

var KernelDLL: THandle = 0;
    GetConsoleWindow: TGetConsoleWindowFn = nil;
    AttachConsole: TAttachConsoleFn = nil;
    ConsoleApps: TList = nil;

function ExecuteConsoleApp(AppClass: TConsoleAppClass): integer;
var App: TConsoleApp;
    {$IFDEF FORCECONSOLE}
    ConsoleState: TConsoleState;
    {$ENDIF}
begin
  {$IFDEF FORCECONSOLE}
  if System.IsConsole then
    ConsoleState := csOwnConsole
  else
    ConsoleState := TConsoleApp.HasConsole;
  if ConsoleState <> csOwnConsole then
    if (ConsoleState = csParentConsole) and (@AttachConsole <> nil) then
      begin
      AttachConsole(ATTACH_PARENT_PROCESS);
      Writeln;
      end
    else
      AllocConsole;
  try
  {$ENDIF}
    try
      App := AppClass.Create;
      try
        Result := App.Execute;
      finally
        App.Free;
        end;
    except
      on E: EAbort do
        begin
        Result := RESCODE_ABORT;
        end;
      on E: Exception do
        begin
        Writeln('ERROR ' + E.ClassName + ': ' + E.Message);
        Result := RESCODE_EXCEPTION;
        end;
      end;
  {$IFDEF FORCECONSOLE}
  finally
    if ConsoleState <> csOwnConsole then
      FreeConsole;
    end;
  {$ENDIF}
end;

{ TConsoleApp }

constructor TConsoleApp.Create;
begin
  inherited;
  if ConsoleApps <> nil then
    ConsoleApps.Add(Self);
  fLineSeparator := #13#10;
  fConvertWritesToOEM := True;
  // Console
  InitConsole;
  // Parameters
  ParamInitialize;
end;

destructor TConsoleApp.Destroy;
var i: integer;
begin
  DoneConsole;
  if ConsoleApps <> nil then
    for i := Pred(ConsoleApps.Count) downto 0 do
      if ConsoleApps[i] = Self then
        ConsoleApps.Delete(i);
  inherited;
end;

function TConsoleApp.Execute: DWORD;
var Err: string;
begin
  ShowHeader;
  try
    Result := DoExecute;
  except
    on EAbort do
      Raise;
    on E: Exception do
      begin
      Err := Format('ERROR %s: %s', [E.ClassName, E.Message]);
      ShowError(Err);
      Raise EAbort.Create(Err);
      end;
    end;
end;

procedure TConsoleApp.InitConsole;
var
  BufferInfo: TConsoleScreenBufferInfo;
  n: DWORD;
begin
  Reset(Input);
  Rewrite(Output);
  fStdIn := TTextRec(Input).Handle; //fStdIn := GetStdHandle(STD_INPUT_HANDLE);
  fStdOut := TTextRec(Output).Handle; //fStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(fStdOut, BufferInfo) then
    fTextAttr := BufferInfo.wAttributes
  else
    fTextAttr := (Integer(ccBlack) shl 4) or (Integer(ccLightGray));
  fStdOutRedirected := not WriteConsole(StdOut, @BufferInfo, 0, n, nil);
end;

procedure TConsoleApp.DoneConsole;
begin
end;

procedure TConsoleApp.Write(const Msg: string);
var
  MsgOEM: AnsiString;
  {$IFDEF UNICODE}
  BytesWritten: DWORD;
  {$ENDIF}
begin
  if StdOutRedirected or (not ConvertWritesToOEM) then
    System.Write(Msg)
  else
    begin
    MsgOEM := AnsiToOEM ({$IFDEF UNICODE} AnsiString {$ENDIF} (Msg));
    {$IFDEF UNICODE}
    WriteFile(StdOut, MsgOEM[1], Length(MsgOEM), BytesWritten, nil);
    {$ELSE}
    System.Write(MsgOEM);
    {$ENDIF}
    end;
end;

procedure TConsoleApp.Writeln(const Msg: string);
begin
  if Msg <> '' then
    Write(Msg);
  Write(LineSeparator);
end;

procedure TConsoleApp.WriteColored(Color: TConsoleColor; const Msg: string);
var Attr: Byte;
begin
  Attr := TextAttribute;
  try
    TextColor := Color;
    Write(Msg);
  finally
    TextAttribute := Attr;
    end;
end;

procedure TConsoleApp.WritelnColored(Color: TConsoleColor; const Msg: string);
begin
  WriteColored(Color, Msg);
  Write(LineSeparator);
end;

procedure TConsoleApp.ShowHeader;
var Attr: Byte;
begin
  Attr := TextAttribute;
  try
    TextColor := ccWhite;
    Write(Format('%-42.42s (c) 2014 Pepak, ', [AppTitle + ' v' + AppVersion  {$IFDEF UNICODE} + ' (Unicode)' {$ELSE} + ' (ANSI)' {$ENDIF} ]));
    WriteColored(ccLightCyan, 'http://www.pepak.net');
    Writeln;
    Writeln(StringOfChar('-', 79));
  finally
    TextAttribute := Attr;
    end;
end;

procedure TConsoleApp.Log(const LogFileName: string; const Msg: string);
begin
  try
    if LogFileName = '' then
      Writeln(Msg)
    else
      TTextStream.AppendLine(LogFileName, Msg);
  except
    on Exception do
      ;
    end;
end;

procedure TConsoleApp.Log(const LogFileName: string; const Msg: string; const Params: array of const);
begin
  Log(LogFileName, Format(Msg, Params));
end;

procedure TConsoleApp.ShowError(Error: Exception);
begin
  ShowError('%s: %s', [Error.ClassName, Error.Message]);
end;

procedure TConsoleApp.ShowError(const Msg: string; const Params: array of const);
begin
  ShowError(Format(Msg, Params));
end;

procedure TConsoleApp.ShowError(const Msg: string);
begin
  WriteColored(ccLightRed, Msg);
  Writeln;
end;

procedure TConsoleApp.ShowSyntax(const Error: string; const Params: array of const);
begin
  ShowSyntax(Format(Error, Params));
end;

procedure TConsoleApp.ShowSyntax(const Error: string);
begin
  if Error <> '' then
    begin
    ShowError(Error);
    Writeln;
    end;
  Write('Syntax: ');
  WriteColored(ccWhite, ExtractFileName(ParamStr(0)) + ' ');
end;

procedure TConsoleApp.ParamInitialize;
begin
  fCurrentParamIndex := 0;
end;

function TConsoleApp.ParamGetNext(out Value: string): boolean;
begin
  Inc(fCurrentParamIndex);
  Result := fCurrentParamIndex <= ParamCount;
  if Result then
    Value := ParamStr(fCurrentParamIndex);
end;

function TConsoleApp.GetTextAttr: Byte;
begin
  Result := fTextAttr;
end;

procedure TConsoleApp.SetTextAttr(const Value: Byte);
begin
  fTextAttr := Value;
  SetConsoleTextAttribute(StdOut, Value);
end;

procedure TConsoleApp.SetTextAttribute(const Text, Background: TConsoleColor);
begin
  TextAttribute := ((Integer(Background) shl 4) or Integer(Text));
end;

function TConsoleApp.GetTextBackground: TConsoleColor;
begin
  Result := TConsoleColor(TextAttribute shr 4);
end;

procedure TConsoleApp.SetTextBackground(const Value: TConsoleColor);
begin
  SetTextAttribute(TextColor, Value);
end;

function TConsoleApp.GetTextColor: TConsoleColor;
begin
  Result := TConsoleColor(TextAttribute and 15);
end;

procedure TConsoleApp.SetTextColor(const Value: TConsoleColor);
begin
  SetTextAttribute(Value, TextBackground);
end;

function TConsoleApp.GetConsoleWindowHandle: THandle;
begin
  if @GetConsoleWindow <> nil then
    Result := GetConsoleWindow
  else
    Result := 0;
end;

function TConsoleApp.GetCursorCol: integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(StdOut, BufferInfo) then
    Result := BufferInfo.dwCursorPosition.X
  else
    Result := -1;
end;

procedure TConsoleApp.SetCursorCol(const Value: integer);
var
  Coord: TCoord;
begin
  Coord.X := Value;
  Coord.Y := CursorRow;
  SetConsoleCursorPosition(StdOut, Coord);
end;

function TConsoleApp.GetCursorRow: integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(StdOut, BufferInfo) then
    Result := BufferInfo.dwCursorPosition.Y
  else
    Result := -1;
end;

procedure TConsoleApp.SetCursorRow(const Value: integer);
var
  Coord: TCoord;
begin
  Coord.X := CursorCol;
  Coord.Y := Value;
  SetConsoleCursorPosition(StdOut, Coord);
end;

function TConsoleApp.ProcessWildCard(const WildCard: string; Recursive: boolean): integer;
var SR: TSearchRec;
    Path, Mask: string;
begin
  Result := 0;
  Path := ExtractFilePath(WildCard);
  if FindFirst(WildCard, faAnyFile, SR) = 0 then
    try
      repeat
        if not LongBool(SR.Attr and ({$IFNDEF DELPHI7_UP} faVolumeID or {$ENDIF} faDirectory)) then
          if ProcessWildCardFile(Path + SR.Name, SR) then
            Inc(Result);
      until FindNext(SR) <> 0;
    finally
      SysUtils.FindClose(SR);
      end;
  if Recursive then
    if FindFirst(Path + '*.*', faAnyFile, SR) = 0 then
      try
        Mask := ExtractFileName(WildCard);
        repeat
          if LongBool(SR.Attr and faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
            Result := Result + ProcessWildCard(Path + SR.Name + '\' + Mask, Recursive);
        until FindNext(SR) <> 0;
      finally
        SysUtils.FindClose(SR);
        end;
end;

function TConsoleApp.ReadPassword: string;
var
  HaveMode: boolean;
  OldConsoleMode: DWORD;
begin
  HaveMode := GetConsoleMode(StdIn, OldConsoleMode);
  try
    SetConsoleMode(StdIn, OldConsoleMode and (not ENABLE_ECHO_INPUT));
    Readln(Result);
  finally
    if HaveMode then
      SetConsoleMode(StdIn, OldConsoleMode);
  end;
end;

class function TConsoleApp.ParentHasConsole: boolean;
begin
  Result := False;
  if @AttachConsole <> nil then
    if AttachConsole(ATTACH_PARENT_PROCESS) then
      try
        if GetConsoleWindow <> 0 then
          Result := True;
      finally
        FreeConsole;
        end;
end;

class function TConsoleApp.HasConsole: TConsoleState;
begin
  Result := csNoConsole;
  if @GetConsoleWindow <> nil then
    begin
    if GetConsoleWindow <> 0 then
      Result := csOwnConsole // Console exists within this application
    else
      if TConsoleApp.ParentHasConsole then
        Result := csParentConsole;
    end;
end;

procedure TConsoleApp.DoConsoleCtrl(const CtrlType: DWORD; var Handled: boolean);
begin
  if Assigned(OnConsoleCtrl) then
    OnConsoleCtrl(Self, CtrlType, Handled);
end;

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
var i: integer;
    Handled: boolean;
begin
  Handled := False;
  for i := 0 to Pred(ConsoleApps.Count) do
    if ConsoleApps[i] <> nil then
      if TObject(ConsoleApps[i]) is TConsoleApp then
        TConsoleApp(ConsoleApps[i]).DoConsoleCtrl(dwCtrlType, Handled);
  Result := Handled;
end;

initialization
  KernelDLL := LoadLibrary('kernel32.dll');
  if KernelDLL <> 0 then
    begin
    GetConsoleWindow := GetProcAddress(KernelDLL, 'GetConsoleWindow');
    AttachConsole := GetProcAddress(KernelDLL, 'AttachConsole');
    end;
  ConsoleApps := TList.Create;
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);

finalization
  FreeLibrary(KernelDLL);
  KernelDLL := 0;
  GetConsoleWindow := nil;
  AttachConsole := nil;
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, False);
  FreeAndNil(ConsoleApps);

end.
