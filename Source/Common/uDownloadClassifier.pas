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

unit uDownloadClassifier;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  uPCRE,
  uDownloader;

type
  TDownloadClassifier = class
    private
      fUrl: string;
      fDownloader: TDownloader;
      fOwnsDownloader: boolean;
    protected
      function GetProviderCount: integer; virtual;
      function GetProviders(Index: integer): TDownloaderClass; virtual;
      function GetNameCount: integer; virtual;
      function GetNames(Index: integer): string; virtual;
      function GetNameClasses(Index: integer): string; virtual;
      function FindDownloader(const AUrl: string; out DownloaderClass: TDownloaderClass; out MovieID, Provid: string): boolean;
      procedure SetUrl(const Value: string); virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear; virtual;
      property Url: string read fUrl write SetUrl;
      property Downloader: TDownloader read fDownloader;
      property OwnsDownloader: boolean read fOwnsDownloader write fOwnsDownloader;
      property ProviderCount: integer read GetProviderCount;
      property Providers[Index: integer]: TDownloaderClass read GetProviders;
      property NameCount: integer read GetNameCount;
      property Names[Index: integer]: string read GetNames;
      property NameClasses[Index: integer]: string read GetNameClasses;
    end;

procedure RegisterDownloader(Downloader: TDownloaderClass);

implementation

var RegisteredDownloaders: TList;
var RegisteredProviders: TStringList;
var RegisteredProviderNames: TStringList;

procedure RegisterDownloader(Downloader: TDownloaderClass);
var i: integer;
begin
  RegisteredDownloaders.Add(Downloader);
  for i := 0 to Pred(RegisteredProviders.Count) do
    if AnsiCompareText(RegisteredProviders[i], Downloader.Provider) = 0 then
      begin
      RegisteredProviderNames[i] := RegisteredProviderNames[i] + ', ' + Downloader.ClassName;
      Exit;
      end;
  RegisteredProviders.Add(Downloader.Provider);
  RegisteredProviderNames.Add(Downloader.ClassName);
end;

{ TDownloadClassifier }

constructor TDownloadClassifier.Create;
begin
  inherited Create;
  Clear;
end;

destructor TDownloadClassifier.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDownloadClassifier.Clear;
begin
  fUrl := '';
  if OwnsDownloader then
    FreeAndNil(fDownloader)
  else
    fDownloader := nil;
end;

function TDownloadClassifier.FindDownloader(const AUrl: string; out DownloaderClass: TDownloaderClass; out MovieID, Provid: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Pred(ProviderCount) do
    if Providers[i].IsSupportedUrl(AUrl, MovieID, Provid) then
      begin
      DownloaderClass := Providers[i];
      Result := True;
      Break;
      end;
end;

procedure TDownloadClassifier.SetUrl(const Value: string);
var DC: TDownloaderClass;
    i: integer;
    ID: string;
    b: boolean;
    provider: string;
begin
  Clear;
  i := Pos('#', Value);
  if i <= 0 then
    fUrl := Value
  else
    fUrl := Copy(Value, 1, Pred(i));
  b := FindDownloader(fUrl, DC, ID, provider);
  if (not b) and (fUrl <> Value) then
    begin
    fUrl := Value;
    b := FindDownloader(fUrl, DC, ID, provider);
    end;
  if b then
  begin
    fDownloader := DC.Create(ID);
    fDownLoader.ProviderName := provider;
  end;
end;

function TDownloadClassifier.GetProviderCount: integer;
begin
  Result := RegisteredDownloaders.Count;
end;

function TDownloadClassifier.GetProviders(Index: integer): TDownloaderClass;
begin
  Result := TDownloaderClass(RegisteredDownloaders[Index]);
end;

function TDownloadClassifier.GetNameCount: integer;
begin
  Result := RegisteredProviders.Count;
end;

function TDownloadClassifier.GetNames(Index: integer): string;
begin
  Result := RegisteredProviders[Index];
end;

function TDownloadClassifier.GetNameClasses(Index: integer): string;
begin
  Result := RegisteredProviderNames[Index];
end;

initialization
  RegisteredDownloaders := TList.Create;
  RegisteredProviders := TStringList.Create;
  RegisteredProviderNames := TStringList.Create;

finalization
  FreeAndNil(RegisteredDownloaders);
  FreeAndNil(RegisteredProviders);
  FreeAndNil(RegisteredProviderNames);

end.
