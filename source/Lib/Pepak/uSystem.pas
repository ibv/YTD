unit uSystem;
{$INCLUDE 'jedi.inc'}

interface

uses
  SysUtils, Classes, Windows;

{$IFDEF WIN32}
function Is64BitWindows: boolean;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
{$ENDIF}

implementation

{$IFDEF WIN32}

type
  TIsWow64ProcessFn = function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  TWow64DisableWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirectionFn = function(var OldValueDONOTCHANGE: Pointer): BOOL; stdcall;

var
  Kernel32Dll: THandle = 0;

var
  IsWow64ProcessFn: TIsWow64ProcessFn = nil;
  Wow64DisableWow64FsRedirectionFn: TWow64DisableWow64FsRedirectionFn = nil;
  Wow64RevertWow64FsRedirectionFn: TWow64RevertWow64FsRedirectionFn = nil;

function IsWow64Process(hProcess: THandle; var Wow64Process: BOOL): BOOL;
begin
  if not Assigned(IsWow64ProcessFn) then
    Result := False
  else
    Result := IsWow64ProcessFn(hProcess, Wow64Process);
end;

function Wow64DisableWow64FsRedirection(var OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64DisableWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64DisableWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Wow64RevertWow64FsRedirection(OldValueDONOTCHANGE: Pointer): BOOL;
begin
  if not Assigned(Wow64RevertWow64FsRedirectionFn) then
    Result := False
  else
    Result := Wow64RevertWow64FsRedirectionFn(OldValueDONOTCHANGE);
end;

function Is64BitWindows: boolean;
var B: BOOL;
begin
  if IsWow64Process(GetCurrentProcess, B) then
    Result := B
  else
    Result := False;
end;

{$ENDIF}

initialization
  {$IFDEF WIN32}
  Kernel32Dll := LoadLibrary('kernel32.dll');
  if Kernel32Dll = 0 then
    begin
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end
  else
    begin
    IsWow64ProcessFn := GetProcAddress(Kernel32Dll, 'IsWow64Process');
    Wow64DisableWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64DisableWow64FsRedirection');
    Wow64RevertWow64FsRedirectionFn := GetProcAddress(Kernel32Dll, 'Wow64RevertWow64FsRedirection');
    end;
  {$ENDIF}

finalization
  {$IFDEF WIN32}
  if Kernel32Dll <> 0 then
    begin
    FreeLibrary(Kernel32Dll);
    Kernel32Dll := 0;
    IsWow64ProcessFn := nil;
    Wow64DisableWow64FsRedirectionFn := nil;
    Wow64RevertWow64FsRedirectionFn := nil;
    end;
  {$ENDIF}

end.
