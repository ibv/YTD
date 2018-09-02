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

unit uHttpDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2007_UP} Windows, {$ENDIF}
  uPCRE, uXml, uHttp, HttpSend, blcksock, ssl_openssl,
  uDownloader, uCommonDownloader;

type
  THttpDownloader = class(TCommonDownloader)
    private
      fVideoDownloader: THttpSend;
      fBytesTransferred: int64;
      {$IFDEF MULTIDOWNLOADS}
      fNameList: TStringList;
      fUrlList: TStringList;
      fDownloadIndex: integer;
      {$ENDIF}
      fCookies: TStringList;
      fHeaders: TStringList;
    protected
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      function GetContentUrl: string; override;
      function BeforePrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function BeforeDownload(Http: THttpSend): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ClearHttpData; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property VideoDownloader: THttpSend read fVideoDownloader write fVideoDownloader;
      property BytesTransferred: int64 read fBytesTransferred write fBytesTransferred;
      property Cookies: TStringList read fCookies;
      property Headers: TStringList read fHeaders;
      {$IFDEF MULTIDOWNLOADS}
      property NameList: TStringList read fNameList;
      property UrlList: TStringList read fUrlList;
      property DownloadIndex: integer read fDownloadIndex write fDownloadIndex;
      {$ENDIF}
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      procedure AbortTransfer; override;
      {$IFDEF MULTIDOWNLOADS}
      function ValidatePrepare: boolean; override;
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uLanguages,
  uMessages,
  uSystem;

{ THttpDownloader }

constructor THttpDownloader.Create(const AMovieID: string);
begin
  inherited;
  fCookies := TStringList.Create;
  fHeaders := TStringList.Create;
  {$IFDEF MULTIDOWNLOADS}
  fNameList := TStringList.Create;
  fUrlList := TStringList.Create;
  {$ENDIF}
end;

destructor THttpDownloader.Destroy;
begin
  FreeAndNil(fCookies);
  FreeAndNil(fHeaders);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fNameList);
  FreeAndNil(fUrlList);
  {$ENDIF}
  inherited;
end;

procedure THttpDownloader.ClearHttpData;
begin
  {$IFDEF MULTIDOWNLOADS}
  NameList.Clear;
  UrlList.Clear;
  DownloadIndex := 0;
  {$ENDIF}
  BytesTransferred := 0;
end;

function THttpDownloader.Prepare: boolean;
begin
  ClearHttpData;
  Result := inherited Prepare;
end;

function THttpDownloader.BeforePrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, PageXml, Http);
  Cookies.Assign(Http.Cookies);
end;

function THttpDownloader.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := True;
  Http.Cookies.Assign(Cookies);
  Http.Headers.Assign(Headers);
  SetReferer(Http, Referer);
end;

function THttpDownloader.Download: boolean;
const
  MINIMUM_SIZE_TO_KEEP = 10240;
var
  FN, FinalFN: string;
begin
  inherited Download;
  BytesTransferred := 0;
  Result := False;
  if MovieURL = '' then
    SetLastErrorMsg(ERR_DOWNLOAD_EMPTY_URL)
  else
    begin
    VideoDownloader := CreateHttp;
    try
      if not BeforeDownload(VideoDownloader) then
        SetLastErrorMsg(ERR_DOWNLOAD_NOT_INITIALIZED)
      else
        try
          FinalFN := FileName;
          if Options.DownloadToTempFiles then
            FN := FinalFN + '.part'
          else
            FN := FinalFN;
          if FileExists(FN) then
            DeleteFile(PChar(FN));
          VideoDownloader.OutputStream := TFileStream.Create(FN, fmCreate or fmShareDenyWrite);
          try
            // For some reason, Delphi don't allow fmCreate with sharing flags
            {$IFDEF SHAREABLEFILES}
            VideoDownloader.OutputStream.Free;
            VideoDownloader.OutputStream := nil;
            VideoDownloader.OutputStream := TFileStream.Create(FN, fmOpenWrite or fmShareDenyWrite);
            VideoDownloader.Sock.OnStatus := SockStatusMonitor;
            {$ENDIF}
            BytesTransferred := 0;
            if not DownloadPage(VideoDownloader, MovieURL) then
              SetLastErrorMsg(ERR_DOWNLOAD_FAILED)
            else if (VideoDownloader.ResultCode < 200) or (VideoDownloader.ResultCode >= 300) then
              SetLastErrorMsg(Format(ERR_HTTP_RESULT_CODE, [VideoDownloader.ResultCode]))
            else
              Result := True;
          finally
            VideoDownloader.Sock.OnStatus := nil;
            VideoDownloader.OutputStream.Free;
            VideoDownloader.OutputStream := nil;
            if Result then
              if (not FileExists(FN)) or (GetFileSize(FN) < MINIMUM_SIZE_TO_KEEP) then
                begin
                SetLastErrorMsg(ERR_HTTP_NO_DATA_READ);
                Result := False;
                end;
            if not Result then
              if FileExists(FN) and (GetFileSize(FN) < MINIMUM_SIZE_TO_KEEP) then
                DeleteFile(PChar(FN));
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
        except
          if FileExists(FN) then
            DeleteFile(PChar(FN));
          Raise;
          end;
    finally
      VideoDownloader.Free;
      VideoDownloader := nil;
      end;
    end;
end;

{$IFDEF MULTIDOWNLOADS}
function THttpDownloader.ValidatePrepare: boolean;
var
  DownIndex: integer;
begin
  DownIndex := DownloadIndex;
  try
    Result := inherited ValidatePrepare;
  finally
    DownloadIndex := DownIndex;
    end;
end;

function THttpDownloader.First: boolean;
begin
  if ValidatePrepare then
    if UrlList.Count <= 0 then
      Result := MovieURL <> ''
    else
      begin
      DownloadIndex := -1;
      Result := Next;
      end
  else
    Result := False;
end;

function THttpDownloader.Next: boolean;
begin
  Result := False;
  if ValidatePrepare then
    begin
    DownloadIndex := Succ(DownloadIndex);
    if (DownloadIndex >= 0) and (DownloadIndex < UrlList.Count) then
      begin
      SetName(NameList[DownloadIndex]);
      SetFileName('');
      MovieURL := UrlList[DownloadIndex];
      Result := True;
      end;
    end;
end;
{$ENDIF}

procedure THttpDownloader.AbortTransfer;
begin
  inherited;
  if (VideoDownloader <> nil) and (VideoDownloader.Sock <> nil) then
    VideoDownloader.Sock.AbortSocket;
end;

function THttpDownloader.GetDownloadedSize: int64;
begin
  Result := BytesTransferred;
end;

function THttpDownloader.GetTotalSize: int64;
begin
  if VideoDownloader <> nil then
    Result := VideoDownloader.DownloadSize
  else
    Result := -1;
end;

function THttpDownloader.GetContentUrl: string;
const
  PARAM_SEPARATOR = {$IFDEF DEBUG} #13#10 {$ELSE} ' ' {$ENDIF} ;
var
  Http: THttpSend;
  s: string;
  i: integer;
begin
  Http := THttpSend.Create;
  try
    if not BeforeDownload(Http) then
      Result := inherited GetContentUrl
    else
      begin
      s := '';
      for i := 0 to Pred(Headers.Count) do
        s := Format('%s' + PARAM_SEPARATOR + '--header="%s"', [s, Headers[i]]);
      for i := 0 to Pred(Cookies.Count) do
        s := Format('%s' + PARAM_SEPARATOR + '--header="Cookie: %s"', [s, Cookies[i]]);
      Result := Format('wget' + PARAM_SEPARATOR + '--user-agent="%s"%s' + PARAM_SEPARATOR + '-O "%s"' + PARAM_SEPARATOR + '"%s"', [Http.UserAgent, s, FileName, inherited GetContentUrl]);
      end;
  finally
    FreeAndNil(Http);
    end;
end;

procedure THttpDownloader.SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  SetLastErrorMsg(_(SockStatusReasons[Reason]));
  if (Reason = HR_ReadCount) then
    BytesTransferred := BytesTransferred + StrToInt64(Value);
  if not (Reason in [HR_SocketClose, HR_Error]) then
    DoProgress;
end;

end.
