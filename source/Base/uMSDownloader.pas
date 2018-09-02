(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit uMSDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uFunctions,
  uDownloader, uCommonDownloader, uExternalDownloader,
  Msdl_Dll;

type
  EMSDownloaderError = class(EExternalDownloaderError);

  TMSDownloader = class(TExternalDownloader)
    private
      fMsdlOptions: TMsdlOptions;
    protected
      procedure ClearMsdlOptions; virtual;
      procedure AddMsdlOption(ShortOption: char; const Argument: string = ''); virtual;
      procedure SetMsdlOption(ShortOption: char; const Argument: string = ''); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure OnMsdlDownloadProgress(DownloadedSize, TotalSize: integer; var DoAbort: integer); virtual;
      property MsdlOptions: TMsdlOptions read fMsdlOptions;
    protected
      function GetContentUrl: string; override;
      procedure SetProxyUrl;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
      //function Prepare: boolean; override;
    end;

implementation

uses
  uFiles, uMessages;

procedure MsdlDownloadProgressCallback(Tag: integer; DownloadedSize, TotalSize: integer; var DoAbort: integer); cdecl;
begin
  TMSDownloader(Tag).OnMsdlDownloadProgress(DownloadedSize, TotalSize, DoAbort);
end;

{ TMSDownloader }

constructor TMSDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TMSDownloader.Destroy;
begin
  inherited;
end;

function TMSDownloader.GetContentUrl: string;
var
  s: string;
  i: integer;
begin
  SetProxyUrl;
  for i := 0 to Pred(Length(MsdlOptions)) do
    if MsdlOptions[i].Argument = '' then
      s := Format('%s -%s', [s, MsdlOptions[i].ShortOption])
    else
      s := Format('%s -%s "%s"', [s, MsdlOptions[i].ShortOption, MsdlOptions[i].Argument]);
  Result := Format('msdl %s -o "%s" "%s"', [s, FileName, MovieUrl]);
end;

procedure TMSDownloader.SetProxyUrl;
var
  ProxyString: string;
begin
  if Options.ProxyActive and (Options.ProxyHost <> '') then
    begin
    ProxyString := Options.ProxyHost + ':' + Options.ProxyPort;
    if Options.ProxyUser <> '' then
      if Options.ProxyPassword <> '' then
        ProxyString := Options.ProxyUser + ':' + Options.ProxyPassword + '@' + ProxyString
      else
        ProxyString := Options.ProxyUser + '@' + ProxyString;
    SetMsdlOption('y', ProxyString); // Note: MSDL has no option 'y', it's an extra option of MSDL_DLL
    end;
end;

procedure TMSDownloader.ClearMsdlOptions;
begin
  SetLength(fMsdlOptions, 0);
end;

procedure TMSDownloader.AddMsdlOption(ShortOption: char; const Argument: string);
var n: integer;
begin
  n := Length(fMsdlOptions);
  SetLength(fMsdlOptions, Succ(n));
  fMsdlOptions[n].ShortOption := AnsiChar(ShortOption);
  fMsdlOptions[n].Argument := AnsiString(Argument);
end;

procedure TMSDownloader.SetMsdlOption(ShortOption: char; const Argument: string);
var i: integer;
begin
  for i := 0 to Pred(Length(fMsdlOptions)) do
    if fMsdlOptions[i].ShortOption = AnsiChar(ShortOption) then
      begin
      fMsdlOptions[i].ShortOption := AnsiChar(ShortOption);
      fMsdlOptions[i].Argument := AnsiString(Argument);
      Exit;
      end;
  AddMsdlOption(ShortOption, Argument);
end;

procedure TMSDownloader.OnMsdlDownloadProgress(DownloadedSize, TotalSize: integer; var DoAbort: integer);
begin
  DownloadedBytes := DownloadedSize;
  TotalBytes := TotalSize;
  DoProgress;
  if Aborted then
    DoAbort := 1
  else
    DoAbort := 0;
end;

function TMSDownloader.Download: boolean;
var LogFileName: string;
    RetCode: integer;
    FN, FinalFN: string;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  SetProxyUrl;
  FinalFN := FileName;
  if Options.DownloadToTempFiles then
    FN := FinalFN + '.part'
  else
    FN := FinalFN;
  SetMsdlOption('v');
  SetMsdlOption('o', FN);
  LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
  if FileExists(LogFileName) then
    DeleteFile(PChar(LogFileName));
  SetMsdlOption('l', LogFileName);
  SetMsdlOption(MSDL_OPTION_URL, MovieURL);
  if not Msdl_Init then
    SetLastErrorMsg(Format(ERR_FAILED_TO_LOAD_DLL, ['msdl_dll.dll']))
  else
    begin
    SetLastErrorMsg(Format(ERR_SEE_LOGFILE, [LogFileName]));
    RetCode := Msdl_Download(Integer(Self), MsdlDownloadProgressCallback, MsdlOptions);
    if (RetCode >= 0) and (FileExists(FN)) and (FileGetSize(FN) > 65536) then
      Result := True;
    if Result then
      if FN <> FinalFN then
        begin
        if FileExists(FinalFN) then
          DeleteFile(PChar(FinalFN));
        if FileExists(FN) then
          if RenameFile(FN, FinalFN) then
            FN := FinalFN;
        end;
    end;
end;

end.
