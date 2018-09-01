unit uHttpDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, blcksock,
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
    protected
      function GetTotalSize: int64; override;
      function GetDownloadedSize: int64; override;
      procedure SetPrepared(Value: boolean); override;
      procedure SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string); virtual;
      function BeforeDownload(Http: THttpSend): boolean; virtual;
      property VideoDownloader: THttpSend read fVideoDownloader write fVideoDownloader;
      property BytesTransferred: int64 read fBytesTransferred write fBytesTransferred;
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
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;

implementation

{ THttpDownloader }

constructor THttpDownloader.Create(const AMovieID: string);
begin
  inherited;
  {$IFDEF MULTIDOWNLOADS}
  fNameList := TStringList.Create;
  fUrlList := TStringList.Create;
  {$ENDIF}
end;

destructor THttpDownloader.Destroy;
begin
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fNameList);
  FreeAndNil(fUrlList);
  {$ENDIF}
  inherited;
end;

function THttpDownloader.Prepare: boolean;
begin
  {$IFDEF MULTIDOWNLOADS}
  NameList.Clear;
  UrlList.Clear;
  DownloadIndex := 0;
  {$ENDIF}
  Result := inherited Prepare;
end;

function THttpDownloader.Download: boolean;
begin
  inherited Download;
  BytesTransferred := 0;
  Result := False;
  if MovieURL <> '' then
    begin
    VideoDownloader := CreateHttp;
    try
      if BeforeDownload(VideoDownloader) then
        begin
        try
          VideoDownloader.OutputStream := TFileStream.Create(FileName, fmCreate);
          try
            VideoDownloader.Sock.OnStatus := SockStatusMonitor;
            BytesTransferred := 0;
            if DownloadPage(VideoDownloader, MovieURL) then
              Result := VideoDownloader.OutputStream.Size > 0;
          finally
            VideoDownloader.Sock.OnStatus := nil;
            VideoDownloader.OutputStream.Free;
            VideoDownloader.OutputStream := nil;
            end;
        except
          if FileExists(FileName) then
            DeleteFile(FileName);
          Raise;
          end;
        end;
    finally
      VideoDownloader.Free;
      VideoDownloader := nil;
      end;
    end;
end;

{$IFDEF MULTIDOWNLOADS}
function THttpDownloader.First: boolean;
begin
  if Prepared then
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
  if Prepared then
    begin
    DownloadIndex := Succ(DownloadIndex);
    if (DownloadIndex >= 0) and (DownloadIndex < UrlList.Count) then
      begin
      SetName(NameList[DownloadIndex]);
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

function THttpDownloader.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := True;
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

procedure THttpDownloader.SetPrepared(Value: boolean);
begin
  inherited;
  BytesTransferred := 0;
end;

procedure THttpDownloader.SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string);
const Reasons : array[THookSocketReason] of string
              = ('Resolving began', 'Resolving ended', 'Socket created', 'Socket closed', 'Bound to IP/port', 'Connected.',
                 'Can read data', 'Can write data', 'Listening', 'Accepted connection', 'Read data', 'Wrote data',
                 'Waiting', 'Socket error');
begin
  SetLastErrorMsg(Reasons[Reason]);
  if (Reason = HR_ReadCount) then
    BytesTransferred := BytesTransferred + StrToInt64(Value);
  if not (Reason in [HR_SocketClose, HR_Error]) then
    DoProgress;
end;

end.
