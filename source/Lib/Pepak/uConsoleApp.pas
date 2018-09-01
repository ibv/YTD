unit uConsoleApp;
{$INCLUDE 'uConsoleApp.inc'}

interface

uses
  SysUtils, Classes, Windows;

type
  TConsoleColor = (ccBlack, ccBlue, ccGreen, ccCyan, ccRed, ccMagenta, ccBrown, ccLightGray,
                   ccDarkGray, ccLightBlue, ccLightGreen, ccLightCyan, ccLightRed, ccLightMagenta, ccYellow, ccWhite);
  TConsoleState = (csNoConsole, csOwnConsole, csParentConsole);

  TConsoleAppClass = class of TConsoleApp;
  TConsoleApp = class
    private
      fCurrentParamIndex: integer;
      fStdIn: THandle;
      fStdOut: THandle;
      fStdOutRedirected: boolean;
      fTextAttr: byte;
      function GetTextAttr: Byte;
      procedure SetTextAttr(const Value: Byte); overload;
      function GetTextBackground: TConsoleColor;
      procedure SetTextBackground(const Value: TConsoleColor);
      function GetTextColor: TConsoleColor;
      procedure SetTextColor(const Value: TConsoleColor);
    protected // Application info
      function AppTitle: string; virtual; abstract;
      function AppVersion: string; virtual; abstract;
      function DoExecute: integer; virtual; abstract;
    protected // Parameter handling methods
      procedure ParamInitialize; virtual;
      function ParamGetNext(out Value: string): boolean; virtual;
    protected // Console
      procedure InitConsole; virtual;
      procedure SetTextAttribute(const Text, Background: TConsoleColor); virtual;
      procedure WriteColored(Color: TConsoleColor; const Msg: string); virtual;
      property StdIn: THandle read fStdIn;
      property StdOut: THandle read fStdOut;
      property StdOutRedirected: boolean read fStdOutRedirected;
      property TextAttribute: Byte read GetTextAttr write SetTextAttr;
      property TextColor: TConsoleColor read GetTextColor write SetTextColor;
      property TextBackground: TConsoleColor read GetTextBackground write SetTextBackground;
    protected // Output methods
      procedure ShowHeader; virtual;
      procedure ShowError(const Msg: string); overload; virtual;
      procedure ShowError(const Msg: string; const Params: array of const); overload; virtual;
      procedure ShowSyntax(const Error: string = ''); overload; virtual;
      procedure ShowSyntax(const Error: string; const Params: array of const); overload; virtual;
      procedure Log(const LogFileName: string; const Msg: string); overload; virtual;
      procedure Log(const LogFileName: string; const Msg: string; const Params: array of const); overload; virtual;
    protected // Files
      function ProcessWildCardFile(const FileName: string; const SearchRec: TSearchRec): boolean; virtual; abstract;
      function ProcessWildCard(const WildCard: string; Recursive: boolean): integer; virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      function Execute: DWORD; virtual;
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
  // Parameters
  ParamInitialize;
  // Console
  InitConsole;
end;

destructor TConsoleApp.Destroy;
begin
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
  fStdIn := TTextRec(Input).Handle;
  fStdOut := TTextRec(Output).Handle;
  if GetConsoleScreenBufferInfo(fStdOut, BufferInfo) then
    fTextAttr := BufferInfo.wAttributes
  else
    fTextAttr := (Integer(ccBlack) shl 4) or (Integer(ccLightGray));
  fStdOutRedirected := not WriteConsole(StdOut, @BufferInfo, 0, n, nil);
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

procedure TConsoleApp.ShowHeader;
var Attr: Byte;
begin
  Attr := TextAttribute;
  try
    TextColor := ccWhite;
    Write(Format('%-42.42s (c) 2010 Pepak, ', [AppTitle + ' v' + AppVersion]));
    WriteColored(ccLightCyan, 'http://www.pepak.net');
    Writeln;
    Writeln(StringOfChar('-', 79));
  finally
    TextAttribute := Attr;
    end;
end;

procedure TConsoleApp.Log(const LogFileName: string; const Msg: string);
var T: TextFile;
begin
  try
    AssignFile(T, LogFileName);
    if FileExists(LogFileName) then
      Append(T)
    else
      Rewrite(T);
    try
      Writeln(T, Msg);
    finally
      CloseFile(T);
      end;
  except
    on Exception do
      ;
    end;
end;

procedure TConsoleApp.Log(const LogFileName: string; const Msg: string; const Params: array of const);
begin
  Log(LogFileName, Format(Msg, Params));
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

function TConsoleApp.ProcessWildCard(const WildCard: string; Recursive: boolean): integer;
var SR: TSearchRec;
    Path, Mask: string;
begin
  Result := 0;
  Path := ExtractFilePath(WildCard);
  if FindFirst(WildCard, faAnyFile, SR) = 0 then
    try
      repeat
        if not LongBool(SR.Attr and faDirectory) then
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

initialization
  KernelDLL := LoadLibrary('kernel32.dll');
  if KernelDLL <> 0 then
    begin
    GetConsoleWindow := GetProcAddress(KernelDLL, 'GetConsoleWindow');
    AttachConsole := GetProcAddress(KernelDLL, 'AttachConsole');
    end;

finalization
  FreeLibrary(KernelDLL);
  KernelDLL := 0;
  GetConsoleWindow := nil;
  AttachConsole := nil;

end.
