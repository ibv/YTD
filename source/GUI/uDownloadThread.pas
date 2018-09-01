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

unit uDownloadThread;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uDownloader, uPlaylistDownloader;

type
  TDownloadThread = class;

  EDownloadThreadError = class(Exception);

  TDownloadThreadState = (dtsWaiting, dtsPreparing, dtsDownloading, dtsFinished, dtsFailed, dtsAborted);

  {$IFDEF CONVERTERS}
  TConvertThreadState = (ctsWaiting, ctsConverting, ctsFinished, ctsFailed, ctsFailedRun);
  {$ENDIF}

  TDTStateChangeEvent = procedure(Sender: TDownloadThread; State: TDownloadThreadState) of object;
  TDTDownloadProgressEvent = procedure(Sender: TDownloadThread; TotalSize, DownloadedSize: int64) of object;
  TDTDownloaderFileNameValidateEvent = procedure(Sender: TDownloadThread; var FileName: string; var Valid: boolean) of object;
  TDTExceptionEvent = procedure(Sender: TDownloadThread; Error: Exception) of object;

  TDownloadThread = class(TThread)
    private
      fDownloader: TDownloader;
      fState: TDownloadThreadState;
      fOnStateChange: TDTStateChangeEvent;
      fOnDownloadProgress: TDTDownloadProgressEvent;
      fOnFileNameValidate: TDTDownloaderFileNameValidateEvent;
      fOnException: TDTExceptionEvent;
    protected
      procedure SetState(Value: TDownloadThreadState); virtual;
      procedure ReportState; virtual;
    protected
      CallersDownloaderProgressEvent: TDownloaderProgressEvent;
      CallersDownloaderFileNameValidateEvent: TDownloaderFileNameValidateEvent;
      ReportSender: TObject;
      ReportTotalSize: int64;
      ReportDownloadedSize: int64;
      ValidateSender: TObject;
      ValidateFileName: string;
      ValidateValid: boolean;
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
      procedure DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      procedure SyncReportProgress; virtual;
      procedure SyncValidate; virtual;
    protected
      ReportException: Exception;
      procedure ReportError; virtual;
    protected
      procedure Execute; override;
      property Downloader: TDownloader read fDownloader;
      property State: TDownloadThreadState read fState write SetState;
    public
      constructor Create(ADownloader: TDownloader; CreateSuspended: Boolean); virtual;
      destructor Destroy; override;
    public
      property OnStateChange: TDTStateChangeEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TDTDownloadProgressEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnFileNameValidate: TDTDownloaderFileNameValidateEvent read fOnFileNameValidate write fOnFileNameValidate;
      property OnException: TDTExceptionEvent read fOnException write fOnException;
    end;

implementation

{ TDownloadThread }

constructor TDownloadThread.Create(ADownloader: TDownloader; CreateSuspended: Boolean);
begin
  fDownloader := ADownloader;
  fState := dtsWaiting;
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TDownloadThread.Destroy;
begin
  inherited;
end;

procedure TDownloadThread.Execute;
begin
  try
    CallersDownloaderProgressEvent := Downloader.OnProgress;
    CallersDownloaderFileNameValidateEvent := Downloader.OnFileNameValidate;
    try
      Downloader.OnProgress := DownloaderProgress;
      Downloader.OnFileNameValidate := DownloaderFileNameValidate;
      repeat
        if Terminated then
          Break;
        State := dtsPreparing;
        if Downloader is TPlaylistDownloader then
          begin
          if Downloader.Prepare then
            State := dtsFinished
          else
            Raise EDownloadThreadError.Create(Downloader.LastErrorMsg);
          end
        else
          begin
          if (not Downloader.Prepare) {$IFDEF MULTIDOWNLOADS} or (not Downloader.First) {$ENDIF} then
            Raise EDownloadThreadError.Create(Downloader.LastErrorMsg);
          if Terminated then
            Break;
          State := dtsDownloading;
          {$IFDEF MULTIDOWNLOADS}
          repeat
          {$ENDIF}
          if (not Downloader.ValidateFileName) or (not Downloader.Download) then
            if Terminated then
              Break
            else
              Raise EDownloadThreadError.Create(Downloader.LastErrorMsg);
          if Terminated then
            Break;
          {$IFDEF MULTIDOWNLOADS}
          until not Downloader.Next;
          {$ENDIF}
          State := dtsFinished;
          end;
      until True;
      if Terminated then
        begin
        State := dtsAborted;
        Downloader.AbortTransfer;
        end;
    finally
      Downloader.OnProgress := CallersDownloaderProgressEvent;
      Downloader.OnFileNameValidate := CallersDownloaderFileNameValidateEvent;
      end;
  except
    on E: Exception do
      try
        ReportException := E;
        if Assigned(OnException) then
          Synchronize(ReportError);
      finally
        State := dtsFailed;
        end;
    end;
end;

procedure TDownloadThread.SetState(Value: TDownloadThreadState);
begin
  fState := Value;
  if Assigned(OnStateChange) then
    Synchronize(ReportState);
end;

procedure TDownloadThread.ReportState;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self, Self.State);
end;

procedure TDownloadThread.DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean);
begin
  if Assigned(CallersDownloaderProgressEvent) or Assigned(OnDownloadProgress) then
    begin
    ReportSender := Sender;
    ReportTotalSize := TotalSize;
    ReportDownloadedSize := DownloadedSize;
    Synchronize(SyncReportProgress);
    end;
  if Terminated then
    DoAbort := True;
end;

procedure TDownloadThread.SyncReportProgress;
var DoAbort: boolean;
begin
  DoAbort := False;
  if Assigned(CallersDownloaderProgressEvent) then
    CallersDownloaderProgressEvent(ReportSender, ReportTotalSize, ReportDownloadedSize, DoAbort);
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self, ReportTotalSize, ReportDownloadedSize);
  if DoAbort then
    Terminate;
end;

procedure TDownloadThread.DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean);
begin
  if Assigned(CallersDownloaderFileNameValidateEvent) or Assigned(OnFileNameValidate) then
    begin
    ValidateSender := Sender;
    ValidateFileName := FileName;
    ValidateValid := Valid;
    Synchronize(SyncValidate);
    FileName := ValidateFileName;
    Valid := ValidateValid;
    end;
end;

procedure TDownloadThread.SyncValidate;
begin
  if Assigned(CallersDownloaderFileNameValidateEvent) then
    CallersDownloaderFileNameValidateEvent(ValidateSender, ValidateFileName, ValidateValid);
  if Assigned(OnFileNameValidate) then
    OnFileNameValidate(Self, ValidateFileName, ValidateValid);
end;

procedure TDownloadThread.ReportError;
begin
  if Assigned(OnException) then
    OnException(Self, ReportException);
end;

end.
