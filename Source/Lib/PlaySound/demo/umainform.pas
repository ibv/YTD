unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, uplaysound;

type

  { Tmainform }

  Tmainform = class(TForm)
    BitBtn1: TBitBtn;
    cmd_StopSound: TButton;
    cmd_Async: TButton;
    cmd_Sync: TButton;
    playsound1: Tplaysound;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure cmd_AsyncClick(Sender: TObject);
    procedure cmd_StopSoundClick(Sender: TObject);
    procedure cmd_SyncClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }

procedure Tmainform.cmd_AsyncClick(Sender: TObject);
// No gap between sounds. App remains responsive
begin
  playsound1.PlayStyle:=psASync;
 {$IFDEF WINDOWS}
  playsound1.SoundFile:='doorbell.wav';
  playsound1.Execute;
  playsound1.SoundFile:='telephone.wav';
  playsound1.Execute;
 {$ELSE}
  // Sound file taken from PropertyGrid
  playsound1.Execute;
 {$ENDIF}
end;

procedure Tmainform.cmd_StopSoundClick(Sender: TObject);
begin
  playsound1.StopSound;
end;

procedure Tmainform.cmd_SyncClick(Sender: TObject);
begin
  playsound1.PlayStyle:=psSync;
 {$IFDEF WINDOWS}
  playsound1.SoundFile:='doorbell.wav';
  playsound1.Execute;
  playsound1.SoundFile:='telephone.wav';
  playsound1.Execute;
 {$ELSE}
  // Sound file taken from PropertyGrid
  playsound1.Execute;
 {$ENDIF}
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  Caption:=Application.Title;
  Icon:=Application.Icon;
end;

end.

