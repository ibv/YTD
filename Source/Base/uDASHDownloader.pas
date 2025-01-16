(******************************************************************************

______________________________________________________________________________

YTD v1.63                                                    (c) 2023  ibv
https://ibv.github.io/YTD/
______________________________________________________________________________


Copyright (c) 2019 ibv (https://ibv.github.io/YTD/)
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

unit uDASHdownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ENDIF}
  {$IFDEF fpc}
  LCLIntf, LCLType,
  {$ENDIF}
  uPCRE, uXml, uHttp, HttpSend, blcksock,
  uDownloader, uCommonDownloader,uMPD;

type
  TDASHDownloader = class(TCommonDownloader)
    private
      fCookies: TStringList;
      fRetryCount: integer;
      fVideoDownloader: THttpSend;
      fFragments: TStringList;
      fFragmentsDownloaded: int64;
      fDownloadedThisFragment: int64;
      fDownloadedPreviousFragments: int64;
      fAborted: boolean;
      fMaxBitRate: integer;
      fMPD: TMPDObject;
      VideoEndNumber: integer;
    protected
      QualityRegExp: TRegExp;
    protected
      function CreateHttp: THttpSend; override;
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      function GetContentUrl: string; override;
      procedure SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function InitDownloadInfo: boolean;
      procedure CleanupDownloadInfo;
      property Cookies: TStringList read fCookies;
      property VideoDownloader: THttpSend read fVideoDownloader;
      property Fragments: TStringList read fFragments;
      property Aborted: boolean read fAborted write fAborted;
      property MaxBitRate: integer read fMaxBitRate write fMaxBitRate;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      procedure AbortTransfer; override;
      property RetryCount: integer read fRetryCount write fRetryCount;
      property MPD: TMPDObject read fMPD;
    end;

implementation

uses
  SynaUtil, strutils,
  uLanguages, uMessages, uFunctions;

const

  //--
  OPTION_DASH_VIDEO_SUPPORT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'dash_video_support';
  OPTION_DASH_AUDIO_SUPPORT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'dash_audio_support';



{ TDASHDownloader }

constructor TDASHDownloader.Create(const AMovieID: string);
begin
  inherited;
  fCookies := TStringList.Create;
  fVideoDownloader := nil;
  fFragments := TStringList.Create;
  fRetryCount := 3;
  fMaxBitRate := MaxInt;
  VideoEndNumber:=-2;
end;

destructor TDASHDownloader.Destroy;
begin
  FreeAndNil(fCookies);
  FreeAndNil(fVideoDownloader);
  FreeAndNil(fFragments);
  FreeAndNil(fMPD);
  inherited;
end;

function TDASHDownloader.CreateHttp: THttpSend;
begin
  Result := inherited CreateHttp;
  Result.Cookies.Assign(Cookies);
end;

function TDASHDownloader.GetContentUrl: string;
begin
  Result := Format('DASH "%s"', [MovieURL]);
end;



function TDASHDownloader.InitDownloadInfo: boolean;
var
  Http: THttpSend;
  init, id: string;
  i,j : integer;
  ATimeT,ATimeD,
    SegTime,Segnumber : integer;
  Xml: TXmlDoc;
  Prot, User, Pass, Host, Port, Path, Para, URI: string;
begin
  Result := False;
  CleanupDownloadInfo;
  Http := CreateHttp;
  try
    if not DownloadXml(Http, MovieUrl, Xml) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else
    begin
      id:='';
      fMPD := TMPDObject.Create(Xml);
      id   := fMPD.GetBestID(fMaxBitRate);
      // without baseurl
      if fMPD.BaseURL = '' then
      begin
        URI := ParseURL( MovieURL, Prot, User, Pass, Host, Port, Path, Para);
        if (port='80') or (port='443') then
          port:=''
        else
          port:=':'+port;
        fMPD.BaseURL := Prot+'://'+host+port+copy(path,1,LastDelimiter('/',Path));
      end;
      if not Result then
      begin
        fVideoDownloader := CreateHttp;
        fVideoDownloader.Cookies.Assign(Http.Cookies);
        fVideoDownloader.Sock.OnStatus := SockStatusMonitor;
        fDownloadedThisFragment := 0;
        fDownloadedPreviousFragments := 0;
        Result := True;
      end;

      // video stream
      if Options.ReadProviderOptionDef(Provider, OPTION_DASH_VIDEO_SUPPORT, false) then
      begin
        Fragments.Add(GetRelativeUrl(fMPD.BaseURL, fMPD.VideoInit));
        if AnsiContainsStr(fMPD.VideoMedia, '$Time$') then
        begin
          // time based segment
          SegNumber := fMPD.VideoStartNumber;
          SegTime := 0;
          for i:=0 to fMPD.VideoSegmentList.Count-1 do
          begin
            ATimeT := TXMLNode(fMPD.VideoSegmentList[i]).ReadAttributeInteger('t',0);
            ATimeD := TXMLNode(fMPD.VideoSegmentList[i]).ReadAttributeInteger('d',0);
            if ATimeT >0 then SegTime := ATimeT;
            //
            init := StringReplace(fMPD.VideoMedia,'$Time$',format('%d', [SegTime]),[]);
            Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
            //
            inc(SegNumber);
            for j:=0 to TXMLNode(fMPD.VideoSegmentList[i]).ReadAttributeInteger('r',0)-1 do
            begin
              inc(SegTime,ATimeD);
              init := StringReplace(fMPD.VideoMedia,'$Time$',format('%d', [SegTime]),[]);
              Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
              inc(SegNumber);
            end;
            inc(SegTime,ATimeD);
          end;
          VideoEndNumber := SegNumber;
        end
        else
        for i:=0 to fMPD.VideoEndNumber do
        begin
          // pro CT '%06d'
          if pos('%',fMPD.VideoMedia) >0 then
           init := StringReplace(fMPD.VideoMedia,'%'+fMPD.fm+'$',format('%.'+fMPD.fm, [i+fMPD.VideoStartNumber]),[]);
          Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
        end;
      end;

      // audio stream
      if Options.ReadProviderOptionDef(Provider, OPTION_DASH_AUDIO_SUPPORT, false) then
      begin
        //id   := fMPD.GetBestID(128000,false);
        id   := fMPD.GetBestID(256000,false);
        Fragments.Add(GetRelativeUrl(fMPD.BaseURL, fMPD.AudioInit));
        if AnsiContainsStr(fMPD.AudioMedia, '$Time$') then
        begin
          // time based segment
          SegNumber := fMPD.AudioStartNumber;
          SegTime := 0;
          for i:=0 to fMPD.AudioSegmenList.Count-1 do
          begin
            ATimeT := TXMLNode(fMPD.AudioSegmenList[i]).ReadAttributeInteger('t',0);
            ATimeD := TXMLNode(fMPD.AudioSegmenList[i]).ReadAttributeInteger('d',0);
            if ATimeT > 0 then SegTime := ATimeT;
            //
            init := StringReplace(fMPD.AudioMedia,'$Time$',format('%d', [SegTime]),[]);
            Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
            //
            inc(SegNumber);
            for j:=0 to TXMLNode(fMPD.AudioSegmenList[i]).ReadAttributeInteger('r',0)-1 do
            begin
              inc(SegTime,ATimeD);
              init := StringReplace(fMPD.AudioMedia,'$Time$',format('%d', [SegTime]),[]);
              Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
              inc(SegNumber);
            end;
            inc(SegTime,ATimeD);
          end;
        end
        else
        for i:=0 to fMPD.AudioEndNumber do
        begin
          // pro CT '%06d'
          if pos('%',fMPD.AudioMedia) >0 then
           init := StringReplace(fMPD.AudioMedia,'%'+fMPD.fm+'$',format('%.'+fMPD.fm, [i+fMPD.AudioStartNumber]),[]);
          Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
        end;
      end;
    end;
  finally
    FreeAndNil(Http);
    if not Result then
      CleanupDownloadInfo;
    end;
end;

procedure TDASHDownloader.CleanupDownloadInfo;
begin
  Fragments.Clear;
  FreeAndNil(fVideoDownloader);
  Aborted := False;
end;

function TDASHDownloader.Prepare: boolean;
begin
  Result := inherited Prepare;
  CleanupDownloadInfo;
end;

function TDASHDownloader.Download: boolean;
var
  FinalFN, FN: string;
  Stream: TFileStream;
  FragmentDownloaded: boolean;
  FragmentData: AnsiString;
  i, Retry: integer;
begin
  inherited Download;
  Result := False;
  if MovieURL = '' then
    SetLastErrorMsg(ERR_DOWNLOAD_EMPTY_URL)
  else if not InitDownloadInfo then
    SetLastErrorMsg(ERR_DOWNLOAD_NOT_INITIALIZED)
  else if Fragments.Count < 1 then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else
    begin
    FinalFN := FileName;
    if Options.DownloadToTempFiles then
      FN := FinalFN + '.part'
    else
      FN := FinalFN;
    if FileExists(FN) then
      DeleteFile(PChar(FN));
    try
      SetLastErrorMsg(ERR_HTTP_NO_DATA_READ);
      Stream := nil;
      try
        for i := 0 to Pred(Fragments.Count) do
        begin
          FragmentDownloaded := False;
          Retry := RetryCount;
          if  (i=VideoEndNumber+1) and Options.ReadProviderOptionDef(Provider, OPTION_DASH_AUDIO_SUPPORT, false) then
          begin
            FN:=ChangeFileExt(FN, '.mpa');
            FInalFN:=FN;
            FreeAndNil(Stream);
          end;

          while Retry >= 0 do
            if DownloadBinary(VideoDownloader, Fragments[i], FragmentData) then
            begin
              FragmentDownloaded := True;
              Inc(fFragmentsDownloaded);
              fDownloadedPreviousFragments := fDownloadedPreviousFragments + fDownloadedThisFragment;
              fDownloadedThisFragment := 0;

              if FragmentData <> '' then
              begin
                if Stream = nil then
                begin
                  Stream := TFileStream.Create(FN, fmCreate);
                  {$IFDEF SHAREABLEFILES}
                  FreeAndNil(Stream);
                  Stream := TFileStream.Create(FN, fmOpenWrite or fmShareDenyWrite);
                  {$ENDIF}
                end;
                Stream.WriteBuffer(FragmentData[1], Length(FragmentData));
              end;

              Break;
            end
            else
              Dec(Retry);
          if not FragmentDownloaded then
            Exit;
        end;
        Result := True;
      finally
        FreeAndNil(Stream);
        end;
    finally
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
end;

procedure TDASHDownloader.AbortTransfer;
begin
  inherited;
  Aborted := True;
  if (VideoDownloader <> nil) and (VideoDownloader.Sock <> nil) then
    VideoDownloader.Sock.AbortSocket;
end;

procedure TDASHDownloader.SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  SetLastErrorMsg(_(SockStatusReasons[Reason]));
  if (Reason = HR_ReadCount) then
    fDownloadedThisFragment := fDownloadedThisFragment + StrToInt64(Value);
  if not (Reason in [HR_SocketClose, HR_Error]) then
    DoProgress;
end;

function TDASHDownloader.GetDownloadedSize: int64;
begin
  Result := fDownloadedPreviousFragments + fDownloadedThisFragment;
end;

function TDASHDownloader.GetTotalSize: int64;
var
  AverageSizePerFragment: int64;
begin
  Result := fDownloadedPreviousFragments;
  if (VideoDownloader <> nil) then
    begin
    Result := Result + VideoDownloader.DownloadSize;
    AverageSizePerFragment := Result div Succ(fFragmentsDownloaded);
    end
  else
    if fFragmentsDownloaded = 0 then
      AverageSizePerFragment := 0
    else
      AverageSizePerFragment := Result div fFragmentsDownloaded;
  Result := Result + (Fragments.Count - fFragmentsDownloaded) * AverageSizePerFragment;
end;

end.
