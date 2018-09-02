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

unit uHDSDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, uHttp, uFlvFile, uCompatibility, HttpSend, blcksock,
  uDownloader, uCommonDownloader;

type
  THDSDownloader = class(TCommonDownloader)
    private
      fCookies: TStringList;
      fManifestNamespace: string;
      fRetryCount: integer;
      fVideoDownloader: THttpSend;
      fManifest: TXmlDoc;
      fMediaInfo: THDSMediaInfo;
      fStreamUrl: string;
      fStreamMetadata: AnsiString;
      fAborted: Boolean;
      fFragmentsToDownload: int64;
      fFragmentsDownloaded: int64;
      fDownloadedThisFragment: int64;
      fDownloadedPreviousFragments: int64;
    protected
      function CreateHttp: THttpSend; override;
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      function GetFileNameExt: string; override;
      function GetContentUrl: string; override;
      procedure SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadManifest(Http: THttpSend; const ManifestUrl: string; out Manifest: TXmlDoc): boolean;
      function ParseManifest(const Manifest: TXmlDoc; const ManifestUrl: string; out StreamID, Url: string; out Bootstrap, Metadata: AnsiString): boolean;
      function GetMediaInfo(const Bootstrap, Metadata: AnsiString; out Info: THDSMediaInfo): boolean;
      function InitDownloadInfo: boolean;
      procedure CleanupDownloadInfo;
      function DownloadFragment(Http: THttpSend; MediaInfo: THDSMediaInfo; const StreamUrl: string; Fragment: DWORD; out FragmentData: AnsiString): boolean;
      function VerifyFragment(MediaInfo: THDSMediaInfo; const FragmentData: AnsiString; out FlvData: AnsiString): boolean;
      property Cookies: TStringList read fCookies;
      property ManifestNamespace: string read fManifestNamespace write fManifestNamespace;
      property VideoDownloader: THttpSend read fVideoDownloader;
      property Manifest: TXmlDoc read fManifest;
      property MediaInfo: THDSMediaInfo read fMediaInfo;
      property StreamUrl: string read fStreamUrl write fStreamUrl;
      property StreamMetadata: AnsiString read fStreamMetadata write fStreamMetadata;
      property Aborted: boolean read fAborted write fAborted;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      procedure AbortTransfer; override;
      property RetryCount: integer read fRetryCount write fRetryCount;
    end;

implementation

uses
  SynaUtil, SynaCode,
  uLanguages, uMessages, uFunctions;

{ THDSDownloader }

constructor THDSDownloader.Create(const AMovieID: string);
begin
  inherited;
  fCookies := TStringList.Create;
  fVideoDownloader := nil;
  fManifest := nil;
  fMediaInfo := nil;
  fRetryCount := 3;
end;

destructor THDSDownloader.Destroy;
begin
  FreeAndNil(fCookies);
  FreeAndNil(fVideoDownloader);
  FreeAndNil(fManifest);
  FreeAndNil(fMediaInfo);
  inherited;
end;

function THDSDownloader.CreateHttp: THttpSend;
begin
  Result := inherited CreateHttp;
  Result.Cookies.Assign(Cookies);
end;

function THDSDownloader.GetFileNameExt: string;
begin
  Result := '.flv';
end;

function THDSDownloader.GetContentUrl: string;
begin
  Result := Format('php AdobeHDS.php --manifest "%s"', [MovieURL]); 
end;

function THDSDownloader.DownloadManifest(Http: THttpSend; const ManifestUrl: string; out Manifest: TXmlDoc): boolean;
begin
  Result := DownloadXml(Http, ManifestUrl, Manifest);
end;

function THDSDownloader.ParseManifest(const Manifest: TXmlDoc; const ManifestUrl: string; out StreamID, Url: string; out Bootstrap, Metadata: AnsiString): boolean;
var
  Http: THttpSend;
  MediaNode, BootstrapNode: TXmlNode;
  i, Bitrate, BestBitrate: integer;
  sBaseURL, sStreamID, sStreamUrl, sBitrate, sInfoID, sBootstrapUrl, sMetadata: string;
begin
  Result := False;
  BestBitrate := -1;
  if not GetXmlVar(Manifest, ManifestNamespace + 'baseURL', sBaseUrl) then
    sBaseUrl := ExtractUrlPath(ManifestUrl);
  for i := 0 to Pred(Manifest.Root.NodeCount) do
    if Manifest.Root.Nodes[i].Name = TXmlString(ManifestNameSpace + 'media') then
      begin
      MediaNode := Manifest.Root.Nodes[i];
      GetXmlAttr(MediaNode, '', 'streamId', sStreamID);
      if GetXmlAttr(MediaNode, '', 'url', sStreamUrl) then
        begin
        if GetXmlAttr(MediaNode, '', 'bootstrapInfoId', sInfoID) then
          begin
          if not XmlNodeByPathAndAttr(Manifest, ManifestNamespace + 'bootstrapInfo', 'id', sInfoID, BootstrapNode) then
            BootstrapNode := nil;
          end
        else
          if not XmlNodeByPath(Manifest, ManifestNamespace + 'bootstrapInfo', BootstrapNode) then
            BootstrapNode := nil;
        if BootstrapNode <> nil then
          begin
          Bootstrap := '';
          if GetXmlAttr(BootstrapNode, '', 'url', sBootstrapUrl) then
            begin
            if not IsHttpProtocol(sBootstrapUrl) then
              sBootstrapUrl := sBaseUrl + sBootstrapUrl;
            Http := CreateHttp;
            try
              if not DownloadBinary(Http, sBootstrapUrl, Bootstrap) then
                Bootstrap := '';
            finally
              FreeAndNil(Http);
              end;
            end
          else
            Bootstrap := DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (XmlValueIncludingCData(BootstrapNode)));
          if Bootstrap <> '' then
            begin
            if GetXmlVar(MediaNode, 'metadata', sMetadata) then
              Metadata := DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (sMetadata))
            else
              Metadata := '';
            if GetXmlAttr(MediaNode, '', 'bitrate', sBitrate) then
              Bitrate := StrToIntDef(sBitrate, 0)
            else
              Bitrate := 0;
            if Bitrate > BestBitrate then
              begin
              StreamID := sStreamID;
              if IsHttpProtocol(sStreamUrl) then
                Url := sStreamUrl
              else
                Url := sBaseUrl + sStreamUrl;
              BestBitrate := Bitrate;
              Result := True;
              end;
            end;
          end;
        end;
      end;
end;

function THDSDownloader.GetMediaInfo(const Bootstrap, Metadata: AnsiString; out Info: THDSMediaInfo): boolean;
begin
  Result := False;
  Info := THDSMediaInfo.Create;
  try
    Info.Bootstrap := Bootstrap;
    Result := Info.Parse;
    if Result then
      Info.Sort;
  finally
    if not Result then
      FreeAndNil(Info);
    end;
end;

function THDSDownloader.InitDownloadInfo: boolean;
var
  Http: THttpSend;
  StrID, StrURL: string;
  Bootstrap, Metadata: AnsiString;
begin
  Result := False;
  CleanupDownloadInfo;
  Http := CreateHttp;
  try
    if not DownloadManifest(Http, MovieURL, fManifest) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else if not ParseManifest(fManifest, MovieURL, StrID, StrURL, Bootstrap, Metadata) then
      SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
    else if not GetMediaInfo(Bootstrap, Metadata, fMediaInfo) then
      SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
    else
      begin
      StreamUrl := StrURL;
      StreamMetadata := Metadata;
      fVideoDownloader := CreateHttp;
      fVideoDownloader.Cookies.Assign(Http.Cookies);
      fVideoDownloader.Sock.OnStatus := SockStatusMonitor;
      fFragmentsToDownload := fMediaInfo.NumberOfFragments;
      fFragmentsDownloaded := 0;
      fDownloadedThisFragment := 0;
      fDownloadedPreviousFragments := 0;
      Result := True;
      end;
  finally
    FreeAndNil(Http);
    if not Result then
      CleanupDownloadInfo;
    end;
end;

procedure THDSDownloader.CleanupDownloadInfo;
begin
  FreeAndNil(fManifest);
  FreeAndNil(fMediaInfo);
  FreeAndNil(fVideoDownloader);
  StreamUrl := '';
  StreamMetadata := '';
  Aborted := False;
end;

function THDSDownloader.Prepare: boolean;
begin
  Result := inherited Prepare;
  CleanupDownloadInfo;
end;

function THDSDownloader.DownloadFragment(Http: THttpSend; MediaInfo: THDSMediaInfo; const StreamUrl: string; Fragment: DWORD; out FragmentData: AnsiString): boolean;
var
  {$IFDEF DEBUG}
  Stream: TStream;
  {$ELSE}
  Segment: DWORD;
  Url: string;
  {$ENDIF}
begin
  {$IFDEF DEBUG}
    Stream := TFileStream.Create('..\flv_prvni_packet.bin', fmOpenRead or fmShareDenyNone);
    SetLength(FragmentData, Stream.Size);
    Stream.ReadBuffer(FragmentData[1], Stream.Size);
    Result := True;
    Stream.Free;
  {$ELSE}
  Result := False;
  fDownloadedThisFragment := 0;
  for Segment := 0 to Pred(MediaInfo.SegmentCount) do
    with MediaInfo.Segments[Segment] do
      if FirstFragment <= Fragment then
        if Fragment < (FirstFragment + FragmentsPerSegment) then
          begin
          Url := Format('%sSeg%u-Frag%u%s', [StreamUrl, Succ(Segment), Fragment, Token]);
          Result := DownloadBinary(Http, Url, FragmentData);
          Break;
          end;
  {$ENDIF}
end;

function THDSDownloader.VerifyFragment(MediaInfo: THDSMediaInfo; const FragmentData: AnsiString; out FlvData: AnsiString): boolean;
var
  FragmentPtr: PAnsiChar;
  FragmentSize: Integer;
  BoxType: string;
  PayloadSize: UInt64;
begin
  Result := False;
  FlvData := '';
  if FragmentData <> '' then
    begin
    FragmentPtr := @(FragmentData[1]);
    FragmentSize := Length(FragmentData);
    while FragmentSize > 0 do
      if not MediaInfo.ParseHeader(FragmentPtr, FragmentSize, BoxType, PayloadSize) then
        Break
      else if BoxType = 'mdat' then
        begin
        Result := FragmentSize >= PayloadSize;
        if Result then
          begin
          SetLength(FlvData, PayloadSize);
          Move(FragmentPtr^, FlvData[1], PayloadSize);
          end;
        Exit;
        end
      else
        begin
        Inc(FragmentPtr, PayloadSize);
        Dec(FragmentSize, PayloadSize);
        end;
    end;
end;

function THDSDownloader.Download: boolean;
var
  FinalFN, FN: string;
  Output: TFlvFile;
  Stream: TFileStream;
  FragmentDownloaded: boolean;
  Fragment, FragmentCount: DWORD;
  FragmentData, FlvData: AnsiString;
  Retry: integer;
begin
  inherited Download;
  Result := False;
  if MovieURL = '' then
    SetLastErrorMsg(ERR_DOWNLOAD_EMPTY_URL)
  else if not InitDownloadInfo then
    SetLastErrorMsg(ERR_DOWNLOAD_NOT_INITIALIZED)
  else if MediaInfo.SegmentCount < 1 then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else if MediaInfo.FragmentCount < 1 then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else
    begin
    FinalFN := {$IFDEF DEBUG} 'debug.flv' {$ELSE} FileName {$ENDIF} ;
    if Options.DownloadToTempFiles then
      FN := FinalFN + '.part'
    else
      FN := FinalFN;
    if FileExists(FN) then
      DeleteFile(PChar(FN));
    try
      SetLastErrorMsg(ERR_HTTP_NO_DATA_READ);
      Output := nil;
      try
        FragmentCount := MediaInfo.NumberOfFragments;
        for Fragment := 0 to Pred(FragmentCount) do
          begin
          if Aborted then
            Exit;
          if (Fragment = 0)  then
            begin
            Dec(fFragmentsToDownload);
            end
          else
            begin
            FragmentDownloaded := False;
            Retry := RetryCount;
            while Retry >= 0 do
              begin
              if DownloadFragment(VideoDownloader, MediaInfo, StreamUrl, Fragment, FragmentData) then
                if VerifyFragment(MediaInfo, FragmentData, FlvData) then
                  begin
                  FragmentDownloaded := True;
                  Inc(fFragmentsDownloaded);
                  fDownloadedPreviousFragments := fDownloadedPreviousFragments + fDownloadedThisFragment;
                  fDownloadedThisFragment := 0;
                  if Output = nil then
                    begin
                    Stream := TFileStream.Create(FN, fmCreate);
                    {$IFDEF SHAREABLEFILES}
                    FreeAndNil(Stream);
                    Stream := TFileStream.Create(FN, fmOpenWrite or fmShareDenyWrite);
                    {$ENDIF}
                    Output := TFlvFile.Create;
                    Output.CreateNewFile(Stream, True);
                    Output.WriteMetadata(StreamMetadata);
                    end;
                  Output.WriteDataPacket(FlvData);
                  Break;
                  end;
              Dec(Retry);
              end;
            if not FragmentDownloaded then
              Exit;
            end;
          {$IFDEF DEBUG}
          Break;
          {$ENDIF}
          end;
        Result := True;
      finally
        FreeAndNil(Output);
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

procedure THDSDownloader.AbortTransfer;
begin
  inherited;
  Aborted := True;
  if (VideoDownloader <> nil) and (VideoDownloader.Sock <> nil) then
    VideoDownloader.Sock.AbortSocket;
end;

procedure THDSDownloader.SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  SetLastErrorMsg(_(SockStatusReasons[Reason]));
  if (Reason = HR_ReadCount) then
    fDownloadedThisFragment := fDownloadedThisFragment + StrToInt64(Value);
  if not (Reason in [HR_SocketClose, HR_Error]) then
    DoProgress;
end;

function THDSDownloader.GetDownloadedSize: int64;
begin
  Result := fDownloadedPreviousFragments + fDownloadedThisFragment;
end;

function THDSDownloader.GetTotalSize: int64;
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
  Result := Result + (fFragmentsToDownload - fFragmentsDownloaded) * AverageSizePerFragment;
end;

end.
