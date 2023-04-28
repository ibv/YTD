unit uLog;
{$INCLUDE 'pepak.inc'}
{$INCLUDE 'uLog.inc'}
{$INCLUDE 'ytd.inc'}

{$IFNDEF DEBUG}
  {$UNDEF DEBUGLOG}
{$ENDIF}

interface

uses
  SysUtils,
  {$ifdef mswindows}
    Windows,
  {$endif}
  Classes;

procedure Log(const Msg: string); overload;
procedure Log(const Msg: string; const Params: array of const); overload;
//procedure LogPanelItem(const Item: TFarPanelItem; const Indent: string = '');
//procedure LogPanel(const Panel: TFarPanelInfo; const Indent: string = '');

implementation

{$IFDEF DEBUGLOG}
var
  LogFileName: string = '';
{$ENDIF}

procedure Log(const Msg: string);
{$IFDEF DEBUGLOG}
var
  T: TextFile;
{$ENDIF}
begin
  {$IFDEF DEBUGLOG}
  try
    if LogFileName = '' then
      begin
      ///LogFileName := GetModuleName(hInstance) + '.log';
      LogFileName := 'ytd.log';
      if FileExists(LogFileName) then
        SysUtils.DeleteFile(LogFileName);
      end;
    AssignFile(T, LogFileName);
    if FileExists(LogFileName) then
      Append(T)
    else
      Rewrite(T);
    try
      Writeln(T, FormatDateTime('d.m.yyyy hh:nn:ss ', now)+Msg);
    finally
      CloseFile(T);
    end;
  except
    // Do nothing
  end;
  {$ENDIF}
end;

procedure Log(const Msg: string; const Params: array of const);
begin
  {$IFDEF DEBUGLOG}
  try
    Log(Format(Msg, Params));
  except
    on E: Exception do
      Log(Format('Error %s logging "%s": %s', [E.ClassName, Msg, E.Message]));
    end;
  {$ENDIF}
end;

(*
procedure LogPanelItem(const Item: TFarPanelItem; const Indent: string = '');
begin
  {$IFDEF DEBUGLOG}
  Log('%sFileName: "%s"', [Indent, Item.FileName]);
  Log('%sFileSize: "%d"', [Indent, Item.FileSize]);
  {$ENDIF}
end;

procedure LogPanel(const Panel: TFarPanelInfo; const Indent: string = '');
{$IFDEF DEBUGLOG}
var
  i: integer;
{$ENDIF}
begin
  {$IFDEF DEBUGLOG}
  Log('%sDirectory: "%s"', [Indent, Panel.Directory]);
  {$IFDEF FAR3_UP}
  Log('%s - Directory param: "%s"', [Indent, Panel.DirectoryParam]);
  Log('%s - Directory file: "%s"', [Indent, Panel.DirectoryFile]);
  {$ENDIF}
  Log('%s - Number of files: %d', [Indent, Panel.Items.Count]);
  Log('%s - Number of selected files: %d', [Indent, Panel.SelectedItemsCount]);
  for i := 0 to Pred(Panel.Items.Count) do
    LogPanelItem(Panel.Items[i], Format('%s  - file %d: ', [Indent, i+1]));
  {$ENDIF}
end;
*)

end.
