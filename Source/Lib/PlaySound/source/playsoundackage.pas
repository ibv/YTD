{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit playsoundackage;

interface

uses
  uplaysound, aboutplaysound, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uplaysound', @uplaysound.Register);
  RegisterUnit('aboutplaysound', @aboutplaysound.Register);
end;

initialization
  RegisterPackage('playsoundackage', @Register);
end.
