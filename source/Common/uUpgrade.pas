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

unit uUpgrade;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  HttpSend,
  uCompatibility, uOptions, {$IFDEF SETUP} uSetup, {$ENDIF} uFunctions;

type
  TYTDUpgrade = class;
  
  TYTDUpgradeEvent = procedure(Sender: TYTDUpgrade) of object;

  TYTDUpgrade = class
    private
      fOptions: TYTDOptions;
      fOnNewYTDFound: TYTDUpgradeEvent;
      fOnNewDefsFound: TYTDUpgradeEvent;
      fOnlineYTD: TStream;
      fOnlineYTDUrl: string;
      fOnlineYTDVersion: string;
      fOnlineDefs: TStream;
      fOnlineDefsUrl: string;
      fOnlineDefsVersion: string;
    protected
      function GetNewestVersionUrl(const BaseUrl: string; out Version, Url: string): boolean;
      function GetNewestVersion(const BaseUrl: string; out Version, Url: string; out Data: TStream): boolean;
      procedure NotifyNewYTDFound;
      procedure NotifyNewDefsFound;
    public
      constructor Create(AOptions: TYTDOptions);
      destructor Destroy; override;
      procedure Clear;
      procedure ClearYTD;
      procedure ClearDefs;
      function TestUpgrades(InBackground: boolean; Notify: boolean = True): boolean;
      function DownloadYTDUpgrade(InBackground: boolean; Notify: boolean = True): boolean;
      function DownloadDefsUpgrade(InBackground: boolean; Notify: boolean = True): boolean;
      function UpgradeYTD: boolean;
      function CompareVersions(const Version1, Version2: string): integer;
      property Options: TYTDOptions read fOptions;
      property OnNewYTDFound: TYTDUpgradeEvent read fOnNewYTDFound write fOnNewYTDFound;
      property OnNewDefsFound: TYTDUpgradeEvent read fOnNewDefsFound write fOnNewDefsFound;
      property OnlineYTD: TStream read fOnlineYTD;
      property OnlineYTDUrl: string read fOnlineYTDUrl;
      property OnlineYTDVersion: string read fOnlineYTDVersion;
      property OnlineDefs: TStream read fOnlineDefs;
      property OnlineDefsUrl: string read fOnlineDefsUrl;
      property OnlineDefsVersion: string read fOnlineDefsVersion;
    end;

implementation

const
  BASE_UPGRADE_URL = 'http://ytd.pepak.net/';
  NEWEST_VERSION_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = BASE_UPGRADE_URL {$IFNDEF XXX} + 'lite' {$ELSE} {$IFDEF UNICODE} + 'unicode' {$ELSE} + 'ansi' {$ENDIF} {$ENDIF} ;
  NEWEST_DEFS_VERSION_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = BASE_UPGRADE_URL + 'defs' ;

{$IFDEF THREADEDVERSION}
type
  TYTDUpgradeThreadTask = (uttTestUpgrades, uttDownloadDefsUpgrade, uttDownloadYTDUpgrade);

  TYTDUpgradeThread = class(TThread)
    private
      fYTDUpgrade: TYTDUpgrade;
      fTask: TYTDUpgradeThreadTask;
      fOnlineYTD: TStream;
      fOnlineYTDVersion: string;
      fOnlineYTDUrl: string;
      fOnlineDefs: TStream;
      fOnlineDefsVersion: string;
      fOnlineDefsUrl: string;
    protected
      procedure Execute; override;
      procedure NotifyUser;
      property YTDUpgrade: TYTDUpgrade read fYTDUpgrade;
      property Task: TYTDUpgradeThreadTask read fTask;
    public
      constructor Create(AYTDUpgrade: TYTDUpgrade; ATask: TYTDUpgradeThreadTask);
    end;

{ TYTDUpgradeThread }

constructor TYTDUpgradeThread.Create(AYTDUpgrade: TYTDUpgrade; ATask: TYTDUpgradeThreadTask);
begin
  fYTDUpgrade := AYTDUpgrade;
  fTask := ATask;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TYTDUpgradeThread.Execute;
var
  Version, Url: string;
  Data: TStream;
begin
  try
    fOnlineYTDVersion := '';
    fOnlineYTDUrl := '';
    fOnlineYTD := nil;
    fOnlineDefsVersion := '';
    fOnlineDefsUrl := '';
    fOnlineDefs := nil;
    try
      case Task of
        uttTestUpgrades:
          begin
            if YTDUpgrade.GetNewestVersionUrl(NEWEST_VERSION_URL, Version, Url) then
              begin
              fOnlineYTDVersion := Version;
              fOnlineYTDUrl := Url;
              end;
            if YTDUpgrade.GetNewestVersionUrl(NEWEST_DEFS_VERSION_URL, Version, Url) then
              begin
              fOnlineDefsVersion := Version;
              fOnlineDefsUrl := Url;
              end;
          end;
        uttDownloadYTDUpgrade:
          begin
            if YTDUpgrade.GetNewestVersion(NEWEST_VERSION_URL, Version, Url, Data) then
              begin
              fOnlineYTDVersion := Version;
              fOnlineYTDUrl := Url;
              fOnlineYTD := Data;
              end;
          end;
        uttDownloadDefsUpgrade:
          begin
            if YTDUpgrade.GetNewestVersion(NEWEST_DEFS_VERSION_URL, Version, Url, Data) then
              begin
              fOnlineDefsVersion := Version;
              fOnlineDefsUrl := Url;
              fOnlineDefs := Data;
              end;
          end;
        end;
      {$IFDEF DELPHITHREADS}
      Synchronize(NotifyUser);
      {$ELSE}
      NotifyUser;
      {$ENDIF}
      fOnlineYTD := nil; // Will be deleted by TYTDUpgrade
      fOnlineDefs := nil; // Will be deleted by TYTDUpgrade
    finally
      FreeAndNil(fOnlineYTD);
      FreeAndNil(fOnlineDefs);
      end;
  except
    ; // Do nothing
    end;
end;

procedure TYTDUpgradeThread.NotifyUser;
begin
  case fTask of
    uttTestUpgrades:
      YTDUpgrade.Clear;
    uttDownloadDefsUpgrade:
      YTDUpgrade.ClearDefs;
    uttDownloadYTDUpgrade:
      YTDUpgrade.ClearYTD;
    end;
  if fTask in [uttTestUpgrades, uttDownloadYTDUpgrade] then
    begin
    YTDUpgrade.fOnlineYTDVersion := Self.fOnlineYTDVersion;
    YTDUpgrade.fOnlineYTDUrl := Self.fOnlineYTDUrl;
    YTDUpgrade.fOnlineYTD := Self.fOnlineYTD;
    YTDUpgrade.NotifyNewYTDFound;
    end;
  if fTask in [uttTestUpgrades, uttDownloadDefsUpgrade] then
    begin
    YTDUpgrade.fOnlineDefsVersion := Self.fOnlineDefsVersion;
    YTDUpgrade.fOnlineDefsUrl := Self.fOnlineDefsUrl;
    YTDUpgrade.fOnlineDefs := Self.fOnlineDefs;
    YTDUpgrade.NotifyNewDefsFound;
    end;
end;

{$ENDIF}

{ TYTDUpgrade }

procedure TYTDUpgrade.Clear;
begin
  ClearYTD;
  ClearDefs;
end;

procedure TYTDUpgrade.ClearDefs;
begin
  FreeAndNil(fOnlineDefs);
  fOnlineDefsVersion := '';
  fOnlineDefsUrl := '';
end;

procedure TYTDUpgrade.ClearYTD;
begin
  FreeAndNil(fOnlineYTD);
  fOnlineYTDVersion := '';
  fOnlineYTDUrl := '';
end;

constructor TYTDUpgrade.Create(AOptions: TYTDOptions);
begin
  inherited Create;
  fOptions := AOptions;
end;

destructor TYTDUpgrade.Destroy;
begin
  Clear;
  inherited;
end;

procedure TYTDUpgrade.NotifyNewYTDFound;
  // Note: Must be called in a thread-safe manner (e.g. form Synchronize)
begin
  if Assigned(OnNewYTDFound) then
    OnNewYTDFound(Self);
end;

procedure TYTDUpgrade.NotifyNewDefsFound;
  // Note: Must be called in a thread-safe manner (e.g. form Synchronize)
begin
  if Assigned(OnNewDefsFound) then
    OnNewDefsFound(Self);
end;

function TYTDUpgrade.GetNewestVersionUrl(const BaseUrl: string; out Version, Url: string): boolean;
  // Note: Must be thread-safe
var
  Http: THttpSend;
  Ver: string;
  n: integer;
begin
  Result := False;
  Version := '';
  Url := BaseUrl;
  try
    Http := Options.CreateHttp;
    try
      if Http.HttpMethod('HEAD', Url) then
        if (Http.ResultCode >= 200) and (Http.ResultCode < 400) then
          if FindHttpHeader(Http, 'X-YTD-Version', Version) then
            Result := True
          else if FindHttpHeader(Http, 'Location', Url) then
            begin
            Ver := ChangeFileExt(Url, '');
            n := Length(Ver);
            while (n >= 1) and CharInSet(Ver[n], ['0'..'9']) do
              Dec(n);
            if (n >= 1) and (Ver[n] = '.') then
              repeat
                Dec(n);
                if (n < 1) or (not CharInSet(Ver[n], ['0'..'9'])) then
                  begin
                  Version := Copy(Ver, Succ(n), MaxInt);
                  Result := True;
                  Break;
                  end;
              until False;
            end;
    finally
      FreeAndNil(Http);
      end;
  except
    Version := '';
    end;
end;

function TYTDUpgrade.GetNewestVersion(const BaseUrl: string; out Version, Url: string; out Data: TStream): boolean;
var
  Http: THttpSend;
  Again: boolean;
begin
  Result := False;
  Version := '';
  Data := nil;
  try
    if GetNewestVersionUrl(BaseUrl, Version, Url) then
      begin
      Http := Options.CreateHttp;
      try
        repeat
          Again := False;
          Http.Clear;
          if Http.HttpMethod('HEAD', Url) then
            if CheckRedirect(HTTP, Url) then
              Again := True
            else if (Http.ResultCode >= 200) and (Http.ResultCode < 300) then
              begin
              Http.Clear;
              if Http.HttpMethod('GET', Url) then
                begin
                Data := TMemoryStream.Create;
                try
                  Data.CopyFrom(Http.Document, 0);
                  Data.Position := 0;
                  Result := True;
                except
                  FreeAndNil(Data);
                  Raise;
                  end;
                end;
              end;
        until not Again;
      finally
        FreeAndNil(Http);
        end;
      end;
  except
    Version := '';
    FreeAndNil(Data);
    end;
end;

function TYTDUpgrade.TestUpgrades(InBackground, Notify: boolean): boolean;
var
  Version, Url: string;
begin
  Result := False;
  Clear;
  {$IFDEF THREADEDVERSION}
  if InBackground then
    begin
    TYTDUpgradeThread.Create(Self, uttTestUpgrades);
    Exit;
    end;
  {$ENDIF}
  if GetNewestVersionUrl(NEWEST_VERSION_URL, Version, Url) then
    begin
    fOnlineYTDVersion := Version;
    fOnlineYTDUrl := Url;
    Result := True;
    if Notify then
      NotifyNewYTDFound;
    end;
  if GetNewestVersionUrl(NEWEST_DEFS_VERSION_URL, Version, Url) then
    begin
    fOnlineDefsVersion := Version;
    fOnlineDefsUrl := Url;
    Result := True;
    if Notify then
      NotifyNewDefsFound;
    end;
end;

function TYTDUpgrade.DownloadYTDUpgrade(InBackground, Notify: boolean): boolean;
var
  Version, Url: string;
  Data: TStream;
begin
  Result := False;
  ClearYTD;
  {$IFDEF THREADEDVERSION}
  if InBackground then
    begin
    TYTDUpgradeThread.Create(Self, uttDownloadDefsUpgrade);
    Exit;
    end;
  {$ENDIF}
  if GetNewestVersion(NEWEST_VERSION_URL, Version, Url, Data) then
    begin
    fOnlineYTDVersion := Version;
    fOnlineYTDUrl := Url;
    fOnlineYTD := Data;
    Result := True;
    if Notify then
      NotifyNewYTDFound;
    end;
end;

function TYTDUpgrade.DownloadDefsUpgrade(InBackground, Notify: boolean): boolean;
var
  Version, Url: string;
  Data: TStream;
begin
  Result := False;
  ClearDefs;
  {$IFDEF THREADEDVERSION}
  if InBackground then
    begin
    TYTDUpgradeThread.Create(Self, uttDownloadDefsUpgrade);
    Exit;
    end;
  {$ENDIF}
  if GetNewestVersion(NEWEST_DEFS_VERSION_URL, Version, Url, Data) then
    begin
    fOnlineDefsVersion := Version;
    fOnlineDefsUrl := Url;
    fOnlineDefs := Data;
    Result := True;
    if Notify then
      NotifyNewDefsFound;
    end;
end;

function TYTDUpgrade.UpgradeYTD: boolean;
{$IFDEF SETUP}
var
  FileName, Ext: string;
  FS: TFileStream;
{$ENDIF}
begin
  Result := False;
  {$IFDEF SETUP}
  if OnlineYTD <> nil then
    if OnlineYTD.Size > 0 then
      begin
      Ext := ExtractFileExt(OnlineYTDUrl);
      if AnsiCompareText(Ext, '.exe') = 0 then
        begin
        FileName := GetTempDir + 'ytd-upgrade' + Ext;
        FS := TFileStream.Create(FileName, fmCreate);
        try
          FS.CopyFrom(OnlineYTD, 0);
        finally
          FreeAndNil(FS);
          end;
        Result := Run(FileName, Format('%s "%s"', [SETUP_PARAM_UPGRADE, ExtractFilePath(ParamStr(0))]));
        end;
      end;
  {$ELSE}
  if OnlineYTDUrl <> '' then
    Result := Run(OnlineYTDUrl);
  {$ENDIF}
end;

function TYTDUpgrade.CompareVersions(const Version1, Version2: string): integer;

  function ExtractVersionPartAsInteger(var Version: string; out Part: integer): boolean;
    var i: integer;
        s: string;
    begin
      i := Pos('.', Version);
      if i > 0 then
        begin
        s := Copy(Version, 1, Pred(i));
        Delete(Version, 1, i);
        end
      else
        begin
        s := Version;
        Version := '';
        end;
      try
        if s = '' then
          begin
          Part := 0;
          Result := False;
          end
        else
          begin
          Part := StrToInt(s);
          Result := True;
          end;
      except
        on EConvertError do
          begin
          Part := 0;
          Result := False;
          end;
        end;
    end;

var V1, V2: string;
    Num1, Num2: integer;
    Found1, Found2: boolean;
begin
  Result := 0;
  V1 := Version1;
  V2 := Version2;
  repeat
    Found1 := ExtractVersionPartAsInteger(V1, Num1);
    Found2 := ExtractVersionPartAsInteger(V2, Num2);
    if not (Found1 or Found2) then
      Break
    else
      if Num1 < Num2 then
        begin
        Result := -1;
        Break;
        end
      else if Num1 > Num2 then
        begin
        Result := 1;
        Break;
        end;
  until False;
end;

end.
