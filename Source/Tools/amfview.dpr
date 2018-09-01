program amfview;
{$apptype console}

uses
  SysUtils, Classes,
  uAMF;

var Packet: TAMFPacket;
    FS: TFileStream;
    FN: string;
begin
  if ParamCount < 1 then
    begin
    Writeln('amfpost.exe <url> <amf-packet>');
    ExitCode := 1;
    Exit;
    end;
  try
    FN := ParamStr(1);
    Packet := TAMFPacket.Create;
    try
      FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
      try
        Packet.LoadFromStream(FS);
      finally
        FS.Free;
        end;
      AMFPacketDump('Request Packet', Packet);
    finally
      Packet.Free;
      end;
  except
    on E: Exception do
      begin
      ExitCode := 2;
      Writeln(Format('Error %s: %s', [E.ClassName, E.Message]));
      end;
    end;
end.
