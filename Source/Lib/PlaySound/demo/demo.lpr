program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, poweredby, umainform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='PlaySound component demo';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.

