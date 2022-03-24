unit uplaysound;
{
Copyright (C)2014 minesadorada@charcodelvalle.com
  Modified GPL
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs
  , FileUtil{$IFDEF WINDOWS}, mmsystem{$ELSE}, asyncprocess, process{$ENDIF}, aboutplaysound;

type
  TPlayStyle = (psAsync, psSync);

  Tplaysound = class(TAboutPlaySound)
  private
    { Private declarations }
    {$IFNDEF WINDOWS}
    SoundPlayerAsyncProcess: Tasyncprocess;
    SoundPlayerSyncProcess: Tprocess;
    {$ENDIF}
    fPlayCommand:String;
    fDefaultPlayCommand: String;
    fPathToSoundFile: string;
    fPlayStyle: TPlayStyle;
  protected
    { Protected declarations }
    function GetPlayCommand: String;
    procedure PlaySound(const szSoundFilename: string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; reintroduce;
    // This is the default method
    procedure Execute;
    procedure StopSound;
  published
    { Published declarations }
    // This is normally set at runtime
    property SoundFile: string read fPathToSoundFile write fPathToSoundFile;
    // Default is Async
    property PlayStyle: TPlayStyle read fPlayStyle write fPlayStyle default psASync;
    // This is automatically determined when the component loads
    property PlayCommand:String read fPlayCommand write fPlayCommand;
  end;

procedure Register;

implementation

uses
  LazFileUtils;

resourcestring
  C_UnableToPlay = 'Unable to play ';

function GetNonWindowsPlayCommand:String;
begin
  Result := '';
  // Try play
  if (FindDefaultExecutablePath('play') <> '') then
    Result := 'play';
  // Try aplay
  if (result = '') then
    if (FindDefaultExecutablePath('aplay') <> '') then
      Result := 'aplay -q';
  // Try paplay
  if (Result = '') then
    if (FindDefaultExecutablePath('paplay') <> '') then
      Result := 'paplay';
  // Try mplayer
  if (Result = '') then
    if (FindDefaultExecutablePath('mplayer') <> '') then
      Result := 'mplayer -really-quiet';
  // Try CMus
  if (Result = '') then
    if (FindDefaultExecutablePath('CMus') <> '') then
      Result := 'CMus';
  // Try pacat
  if (Result = '') then
    if (FindDefaultExecutablePath('pacat') <> '') then
      Result := 'pacat -p';
  // Try ffplay
  if (Result = '') then
    if (FindDefaultExecutablePath('ffplay') <> '') then
      result := 'ffplay -autoexit -nodisp';
  // Try cvlc
  if (Result = '') then
    if (FindDefaultExecutablePath('cvlc') <> '') then
      result := 'cvlc -q --play-and-exit';
  // Try canberra-gtk-play
  if (Result = '') then
    if (FindDefaultExecutablePath('canberra-gtk-play') <> '') then
      Result := 'canberra-gtk-play -c never -f';
  // Try Macintosh command?
  if (Result = '') then
    if (FindDefaultExecutablePath('afplay') <> '') then
      Result := 'afplay';
end;


constructor Tplaysound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlayStyle := psASync;
//  fPathToSoundFile := ProgramDirectory;
  {$IFDEF WINDOWS}
  fDefaultPlayCommand := 'sndPlaySound';
  {$ELSE}
  fDefaultPlayCommand := GetNonWindowsPlayCommand; // Linux, Mac etc.
  {$ENDIF}
  if (fDefaultPlayCommand <> '') then FPlayCommand:=fDefaultPlayCommand;
  // About Dialog properties
  AboutBoxComponentName := 'PlaySound';
  AboutBoxWidth := 400;
  AboutBoxHeight := 400;
  AboutBoxBackgroundColor := clCream;
  //AboutBoxFontName (string)
  //AboutBoxFontSize (integer)
  AboutBoxVersion := '0.0.8';
  AboutBoxAuthorname := 'Gordon Bamber';
  AboutBoxOrganisation := 'Public Domain';
  AboutBoxAuthorEmail := 'minesadorada@charcodelvalle.com';
  AboutBoxLicenseType := 'MODIFIEDGPL';
  AboutBoxDescription := 'Plays WAVE sounds in Windows or Linux' + LineEnding +
                         'Public methods: Execute and StopSound';
end;

destructor Tplaysound.Destroy;
begin
  {$IFNDEF WINDOWS}
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
  {$ENDIF}
  inherited;
end;

procedure Tplaysound.Execute;
begin
  if not FileExists(fPathToSoundFile) then
    Exit;
  {$IFDEF WINDOWS}
  fDefaultPlayCommand := 'sndPlaySound';
  {$ELSE}
  fDefaultPlayCommand := GetNonWindowsPlayCommand; // Linux, Mac etc.
  {$ENDIF}
  Try
    PlaySound(fPathToSoundFile);
  Except
    On E: Exception do
          E.CreateFmt(C_UnableToPlay +
           '%s Message:%s', [fPathToSoundFile, E.Message]);
  end;
end;

function TPlaySound.GetPlayCommand: String;
begin
  if FPlayCommand = '' then
    Result := FDefaultPlayCommand
  else
    Result := FPlayCommand;
end;

procedure Tplaysound.PlaySound(const szSoundFilename: string);
var
{$IFDEF WINDOWS}
  flags: word;
{$ELSE}
  L: TStrings;
  i: Integer;
  playCmd: String;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  if fPlayStyle = psASync then
    flags := SND_ASYNC or SND_NODEFAULT
  else
    flags := SND_SYNC or SND_NODEFAULT;
  try
    sndPlaySound(PChar(szSoundFilename), flags);
  except
    ShowMessage(C_UnableToPlay + szSoundFilename);
  end;
{$ELSE}
  // How to play in Linux? Use generic Linux commands
  // Use asyncprocess to play sound as SND_ASYNC
  // proceed if we managed to find a valid command
  playCmd := GetPlayCommand;
  if (playCmd <> '') then
  begin
    L := TStringList.Create;
    try
      L.Delimiter := ' ';
      L.DelimitedText := playCmd;
      if fPlayStyle = psASync then
      begin
        if SoundPlayerAsyncProcess = nil then
          SoundPlayerAsyncProcess := TaSyncProcess.Create(nil);
        SoundPlayerAsyncProcess.CurrentDirectory := ExtractFileDir(szSoundFilename);
        SoundPlayerAsyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayerAsyncProcess.Parameters.Clear;
        for i := 1 to L.Count-1 do
          SoundPlayerAsyncProcess.Parameters.Add(L[i]);
        SoundPlayerAsyncProcess.Parameters.Add(szSoundFilename);
        try
          SoundPlayerAsyncProcess.Execute;
        except
          On E: Exception do
            E.CreateFmt('Playstyle=paASync: ' + C_UnableToPlay +
              '%s Message:%s', [szSoundFilename, E.Message]);
        end;
      end
      else
      begin
        if SoundPlayerSyncProcess = nil then
          SoundPlayerSyncProcess := TProcess.Create(nil);
        SoundPlayerSyncProcess.CurrentDirectory := ExtractFileDir(szSoundFilename);
        SoundPlayerSyncProcess.Executable := FindDefaultExecutablePath(L[0]);
        SoundPlayersyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do
          SoundPlayerSyncProcess.Parameters.Add(L[i]);
        SoundPlayerSyncProcess.Parameters.Add(szSoundFilename);
        try
          SoundPlayerSyncProcess.Execute;
          SoundPlayersyncProcess.WaitOnExit;
        except
          On E: Exception do
            E.CreateFmt('Playstyle=paSync: ' + C_UnableToPlay +
              '%s Message:%s', [szSoundFilename, E.Message]);
        end;
      end;
    finally
      L.Free;
    end;
  end
  else
    raise Exception.CreateFmt('The play command %s does not work on your system',
      [fPlayCommand]);
{$ENDIF}
end;

procedure Tplaysound.StopSound;
begin
{$IFDEF WINDOWS}
   sndPlaySound(nil, 0);
{$ELSE}
  if SoundPlayerSyncProcess <> nil then SoundPlayerSyncProcess.Terminate(1);
  if SoundPlayerAsyncProcess <> nil then SoundPlayerAsyncProcess.Terminate(1);
{$ENDIF}
end;

procedure Register;
begin
  RegisterComponents('LazControls', [Tplaysound]);
  {$I playsound_icon.lrs}
end;

end.
