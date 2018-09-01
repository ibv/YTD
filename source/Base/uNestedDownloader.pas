(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit uNestedDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, blcksock, 
  uDownloader, uCommonDownloader,
  uOptions;

type
  TNestedDownloader = class(TCommonDownloader)
    private
      fNestedDownloader: TDownloader;
      {$IFDEF MULTIDOWNLOADS}
      fFirstItem: boolean;
      {$ENDIF}
    protected
      NestedIDRegExp: TRegExp;
      NestedUrlRegExp: TRegExp;
    protected
      function GetFileName: string; override;
      function GetThisFileName: string; virtual;
      procedure SetNestedDownloader(Value: TDownloader); virtual;
      function CreateNestedDownloaderFromID(const MovieID: string): boolean; virtual;
      function CreateNestedDownloaderFromURL(var Url: string): boolean; virtual;
      function CreateNestedDownloaderFromDownloader(Downloader: TDownloader): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure NestedFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      property NestedDownloader: TDownloader read fNestedDownloader write SetNestedDownloader;
      {$IFDEF MULTIDOWNLOADS}
      property FirstItem: boolean read fFirstItem write fFirstItem;
      {$ENDIF}
      {$IFDEF SUBTITLES}
    public
      function GetSubtitlesFileName: string; override;
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uMessages,
  uDownloadClassifier;
  
{ TNestedDownloader }

constructor TNestedDownloader.Create(const AMovieID: string);
begin
  inherited;
  fNestedDownloader := nil;
end;

destructor TNestedDownloader.Destroy;
begin
  FreeAndNil(fNestedDownloader);
  RegExFreeAndNil(NestedIDRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  inherited;
end;

procedure TNestedDownloader.SetNestedDownloader(Value: TDownloader);
begin
  FreeAndNil(fNestedDownloader);
  fNestedDownloader := Value;
end;

function TNestedDownloader.CreateNestedDownloaderFromID(const MovieID: string): boolean;
begin
  Result := False;
end;

function TNestedDownloader.CreateNestedDownloaderFromURL(var Url: string): boolean;
var DC: TDownloadClassifier;
begin
  Result := False;
  DC := TDownloadClassifier.Create;
  try
    DC.OwnsDownloader := False;
    DC.Url := Url;
    if DC.Downloader <> nil then
      begin
      Result := CreateNestedDownloaderFromDownloader(DC.Downloader);
      if Result then
        MovieURL := Url;
      end;
  finally
    DC.Free;
    end;
end;

function TNestedDownloader.CreateNestedDownloaderFromDownloader(Downloader: TDownloader): boolean;
begin
  NestedDownloader := Downloader;
  NestedDownloader.Options := Options;
  NestedDownloader.OnProgress := OnProgress;
  NestedDownloader.OnFileNameValidate := NestedFileNameValidate;
  Result := True;
end;

function TNestedDownloader.GetFileName: string;
var NestedFN: string;
begin
  Result := GetThisFileName;
  if NestedDownloader <> nil then
    begin
    NestedFN := NestedDownloader.FileName;
    {$IFDEF MULTIDOWNLOADS}
    if not FirstItem then
      Result := NestedDownloader.FileName
    else
    {$ENDIF}
      Result := {ChangeFileExt(}Result{, '')} + ExtractFileExt(NestedFN);
    end;
end;

function TNestedDownloader.GetThisFileName: string;
begin
  Result := inherited GetFileName;
end;

{$IFDEF SUBTITLES}
function TNestedDownloader.GetSubtitlesFileName: string;
begin
  Result := ChangeFileExt(GetThisFileName, fSubtitlesExt);
end;

function TNestedDownloader.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := False;
  if NestedDownloader <> nil then
    if NestedDownloader is TCommonDownloader then
      begin
      Result := TCommonDownloader(NestedDownloader).ReadSubtitles(Page, PageXml, Http);
      if Result then
        begin
        fSubtitles := TCommonDownloader(NestedDownloader).Subtitles;
        fSubtitlesExt := TCommonDownloader(NestedDownloader).SubtitlesExt;
        end;
      end;
  if not Result then
    Result := inherited ReadSubtitles(Page, PageXml, Http);
end;
{$ENDIF}

function TNestedDownloader.Prepare: boolean;
begin
  NestedDownloader := nil;
  Result := False;
  if inherited Prepare then
    if NestedDownloader <> nil then
      Result := NestedDownloader.Prepare;
end;

function TNestedDownloader.Download: boolean;
begin
  inherited Download;
  Result := False;
  if NestedDownloader <> nil then
    begin
    {$IFDEF SUBTITLES}
    //WriteSubtitles;
    {$ENDIF}
    NestedDownloader.Options := Options;
    NestedDownloader.OnProgress := OnProgress;
    NestedDownloader.OnFileNameValidate := NestedFileNameValidate;
    Result := NestedDownloader.ValidateFileName and NestedDownloader.Download;
    end;
end;

{$IFDEF MULTIDOWNLOADS}
function TNestedDownloader.First: boolean;
begin
  if NestedDownloader <> nil then
    Result := NestedDownloader.First
  else
    Result := False;
  FirstItem := Result;
end;

function TNestedDownloader.Next: boolean;
begin
  if NestedDownloader <> nil then
    Result := NestedDownloader.Next
  else
    Result := False;
  FirstItem := False;
end;
{$ENDIF}

function TNestedDownloader.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var ID, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO));
  if (NestedIDRegExp <> nil) and GetRegExpVar(NestedIDRegExp, Page, 'ID', ID) and (ID <> '') and CreateNestedDownloaderFromID(ID) then
    begin
    if NestedUrlRegExp <> nil then
      if GetRegExpVar(NestedUrlRegExp, Page, 'URL', Url) then
        MovieURL := Url;
    SetPrepared(True);
    Result := True;
    end
  else if (NestedUrlRegExp <> nil) and GetRegExpVar(NestedUrlRegExp, Page, 'URL', Url) and (Url <> '') and CreateNestedDownloaderFromURL(Url) then
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

procedure TNestedDownloader.NestedFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean);
begin
  FileName := GetThisFileName;
  Valid := ValidateFileName(FileName);
end;

end.
