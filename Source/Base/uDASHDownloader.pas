(******************************************************************************

______________________________________________________________________________

YTD v1.63                                                    (c) 2019  ibv
https://ibv.github.io/YTD/
______________________________________________________________________________


Copyright (c) 201 ibv (https://ibv.github.io/YTD/)
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
  {$ELSE}
  LCLIntf, LCLType, {LMessages,}
  {$ENDIF}
  uPCRE, uXml, uHttp, HttpSend, blcksock,
  uDownloader, uCommonDownloader;

type
  TMPDObject = class
    private
      fBaseURL:  String;
      fAudioList: TList;
      fVideoList: TList;
      fXml: TXmlDoc;
      fMedia,fInit,
      fAMedia,fAInit: string;
      fAudioStartNumber,
      fAudioEndNumber,
      fVideoStartNumber,
      fVideoEndNumber : integer;

      procedure ParseMPD;
    public
      fm:  string;
      constructor Create(Xml: TXmlDoc);
      destructor Destroy;

      function GetBestID(BandWidth:integer;Video:boolean = true):string;

      property AudioList: TList read fAudioList;
      property VideoList: TList read fVideoList;
      property BaseURL: String read fBaseURL write fBaseURL;
      property VideoMedia: string read fMedia;
      property VideoInit: string read fInit;
      property VideoStartNumber: integer read fVideoStartNumber;
      property VideoEndNumber: integer read fVideoEndNumber;
      property AudioMedia: string read fAMedia;
      property AudioInit: string read fAInit;
      property AudioStartNumber: integer read fAudioStartNumber;
      property AudioEndNumber: integer read fAudioEndNumber;

  end;


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
  strutils,
  uLanguages, uMessages;



{ TMPDObject }

constructor TMPDObject.Create(Xml:TXmlDoc);
begin
  fXml := Xml;
  fAudioList:=TList.Create;
  fVideoList:=TList.Create;
  fBaseURL := '';
  fMedia:='';
  fInit:='';
  fAMedia:='';
  fAInit:='';
  fVideoStartNumber:=0;
  fVideoEndNumber:=0;
  fAudioStartNumber:=0;
  fAudioEndNumber:=0;

  ParseMPD;
end;


destructor TMPDObject.Destroy;
begin
  FreeAndNil(fAudioList);
  FreeAndNil(fVideoList);
end;


procedure TMPDObject.ParseMPD;
var
  Node,Node1 : TXmlNode;
  lang: string;
begin
  if fXml.NodeByPath('BaseURL', Node) then
      BaseURL := XmlValueIncludingCData(Node);

  if fxml.NodeByPathAndAttr('Period/AdaptationSet','mimeType','video/mp4',Node1) then
  begin
    if XmlNodeByPath(Node1,'SegmentTemplate',Node) then
    begin
      fmedia := XmlAttribute(Node, 'media');
      finit := XmlAttribute(Node, 'initialization');
      fVideostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fVideoList);
    fVideoEndNumber := TXMLNode(fVideoList[0]).ReadAttributeInteger('r',0);
    inc(fVideoEndNumber,fVideoList.count-1);

    Node1.FindNodes('Representation',fVideoList);
  end;

  if fxml.NodeByPathAndAttr('Period/AdaptationSet','mimeType','audio/mp4',Node1) then
  begin
    lang := XmlAttribute(Node1, 'lang');

    if XmlNodeByPath(Node1,'SegmentTemplate',Node) then
    begin
      famedia := XmlAttribute(Node, 'media');
      fainit := XmlAttribute(Node, 'initialization');
      fAudiostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fAudioList);
    fAudioEndNumber := TXMLNode(fAudioList[0]).ReadAttributeInteger('r',0);
    inc(fAudioEndNumber,fAudioList.count-1);

    Node1.FindNodes('Representation',fAudioList);

  end;
end;


function TMPDObject.GetBestID(BandWidth:integer;Video:boolean = true):string;
var
  i,j,k,quality: integer;
  id,media,init:string;
  Node: TXmlNode;
  List: TList;
begin
  result:='';
  List:=fVideoList;
  media:=fmedia;
  init :=finit;
  if not Video then
  begin
    List:=fAudioList;
    media:=famedia;
    init:=fainit;
  end;
  for i:=0 to List.Count-1 do
  begin
     Node := TXMLNode(List[i]);
     id := XmlAttribute(node, 'id');
     Quality := Node.ReadAttributeInteger('bandwidth',0);
     if (BandWidth-Quality <= 0)  then  break;
  end;
  if id <> '' then result:=id;

  Init := StringReplace(init,'$RepresentationID$',id,[]);
  Media := StringReplace(media,'$RepresentationID$',id,[]);
  // time based segment
  if AnsiContainsStr(media, '$Time') then
  begin
    // ToDo
  end
  else
  // id based segment
  if AnsiContainsStr(media, '$Number') then
  begin
    media := StringReplace(media,'$Number','',[]);
    j := pos('%',media);
    k := LastDelimiter('$', media);
    fm := copy(media,j+1,k-j-1);
  end;

  if Video then
  begin
    fmedia:=media;
    finit:=init;
  end
  else
  begin
    famedia:=media;
    fainit:=init;
  end;


end;




{ TDASHDownloader }

constructor TDASHDownloader.Create(const AMovieID: string);
begin
  inherited;
  fCookies := TStringList.Create;
  fVideoDownloader := nil;
  fFragments := TStringList.Create;
  fRetryCount := 3;
  fMaxBitRate := MaxInt;
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
  i : integer;
  Xml: TXmlDoc;
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
      // video stream
      id   := fMPD.GetBestID(fMaxBitRate);
      Fragments.Add(GetRelativeUrl(fMPD.BaseURL, fMPD.VideoInit));
      for i:=0 to fMPD.VideoEndNumber do
      begin
        if not Result then
        begin
          fVideoDownloader := CreateHttp;
          fVideoDownloader.Cookies.Assign(Http.Cookies);
          fVideoDownloader.Sock.OnStatus := SockStatusMonitor;
          fDownloadedThisFragment := 0;
          fDownloadedPreviousFragments := 0;
          Result := True;
        end;
        init := StringReplace(fMPD.VideoMedia,'%'+fMPD.fm+'$',format('%.'+fMPD.fm, [i+fMPD.VideoStartNumber]),[]);
        Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
      end;

      // audio stream
      id   := fMPD.GetBestID(128000,false);
      Fragments.Add(GetRelativeUrl(fMPD.BaseURL, fMPD.AudioInit));
      for i:=0 to fMPD.AudioEndNumber do
      begin
        init := StringReplace(fMPD.AudioMedia,'%'+fMPD.fm+'$',format('%.'+fMPD.fm, [i+fMPD.AudioStartNumber]),[]);
        Fragments.Add(GetRelativeUrl(fMPD.BaseURL, init));
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

          if i=fMPD.VideoEndNumber+1 then
          begin
            FN:=ChangeFileExt(FN, '.mpa');
            FInalFN:=FN;
            FreeAndNil(Stream);
          end;

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
