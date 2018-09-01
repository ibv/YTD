unit uDownloader;

interface

uses
  SysUtils,
  HttpSend, blcksock, synautil;

type
  EDownloader = class(Exception);

  TDownloaderProgressEvent = procedure(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean) of object;

  TDownloaderClass = class of TDownloader;
  TDownloader = class
    private
      fPrepared: boolean;
      fName: string;
      fLastErrorMsg: string;
      fOnProgress: TDownloaderProgressEvent;
      fHttp: THttpSend;
      fBytesTransferred: int64;
      fVideoDownloader: THttpSend;
      fDestinationPath: string;
    protected
      procedure SetPrepared(Value: boolean); virtual;
      procedure SetLastErrorMsg(const Value: string); virtual;
      function GetName: string; virtual;
      procedure SetName(const Value: string); virtual;
      function GetFileName: string; virtual;
      function GetFileNameExt: string; virtual;
      function GetProvider: string; virtual; abstract;
      function GetTotalSize: int64; virtual;
      function GetDownloadedSize: int64; virtual;
      procedure DoProgress; virtual;
      function CreateHttp: THttpSend; virtual;
      function CheckRedirect(Http: THttpSend; var Url: string): boolean; virtual;
      function DownloadPage(Http: THttpSend; Url: string): boolean; overload; virtual;
      function DownloadPage(Http: THttpSend; Url: string; out Page: string): boolean; overload; virtual;
      procedure SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string); virtual;
      property BytesTransferred: int64 read fBytesTransferred write fBytesTransferred;
      property VideoDownloader: THttpSend read fVideoDownloader write fVideoDownloader;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      function Prepare: boolean; virtual; abstract;
      function Download: boolean; virtual;
      procedure AbortTransfer; virtual;
      property Prepared: boolean read fPrepared;
      property Name: string read GetName;
      property FileName: string read GetFileName;
      property LastErrorMsg: string read fLastErrorMsg;
      property Provider: string read GetProvider;
      property TotalSize: int64 read GetTotalSize;
      property DownloadedSize: int64 read GetDownloadedSize;
    published
      property DefaultHttp: THttpSend read fHttp;
      property DestinationPath: string read fDestinationPath write fDestinationPath;
      property OnProgress: TDownloaderProgressEvent read fOnProgress write fOnProgress;
    end;

implementation

uses
  uStringUtils;
  
{ TDownloader }

constructor TDownloader.Create;
begin
  inherited Create;
  SetLastErrorMsg('');
  SetPrepared(False);
  fHttp := THttpSend.Create;
end;

destructor TDownloader.Destroy;
begin
  SetPrepared(False);
  FreeAndNil(fHTTP);
  inherited;
end;

procedure TDownloader.SetPrepared(Value: boolean);
begin
  fPrepared := Value;
  BytesTransferred := 0;
end;

procedure TDownloader.SetLastErrorMsg(const Value: string);
begin
  fLastErrorMsg := Value;
end;

function TDownloader.GetName: string;
begin
  if Prepared then
    Result := fName
  else
    Raise EDownloader.Create('Downloader is not prepared!');
end;

procedure TDownloader.SetName(const Value: string);
begin
  fName := Value;
end;

function StrTr(const Kde, Co, Cim: string): string;
var i, j: integer;
begin
  Result := Kde;
  if Kde <> '' then
    for i := 1 to Length(Co) do
      repeat
        j := Pos(Co[i], Result);
        if j > 0 then
          Result[j] := Cim[i];
      until j <= 0;
end;

function TDownloader.GetFileName: string;
begin
  Result := {AnsiToOem}(StrTr(Name, '\/:*?"<>|', '--;..''--!') + GetFileNameExt);
  if DestinationPath <> '' then
    Result := IncludeTrailingBackslash(DestinationPath) + Result;
end;

function TDownloader.GetFileNameExt: string;
begin
  Result := '';
end;

function TDownloader.GetTotalSize: int64;
begin
  if VideoDownloader <> nil then
    Result := VideoDownloader.DownloadSize
  else
    Result := -1;
end;

function TDownloader.GetDownloadedSize: int64;
begin
  Result := BytesTransferred;
end;

procedure TDownloader.DoProgress;
var DoAbort: boolean;
begin
  if Assigned(OnProgress) then
    begin
    DoAbort := False;
    OnProgress(Self, TotalSize, DownloadedSize, DoAbort);
    if DoAbort then
      AbortTransfer;
    end;
end;

function TDownloader.Download: boolean;
begin
  SetLastErrorMsg('Can''t download this kind of content.');
  BytesTransferred := 0;
  if Prepared then
    Result := False
  else
    Raise EDownloader.Create('Downloader is not prepared!');
end;

function TDownloader.CreateHttp: THttpSend;
begin
  Result := THttpSend.Create;
  Result.UserAgent := DefaultHttp.UserAgent;
  Result.ProxyHost := DefaultHttp.ProxyHost;
  Result.ProxyPort := DefaultHttp.ProxyPort;
  Result.ProxyUser := DefaultHttp.ProxyUser;
  Result.ProxyPass := DefaultHttp.ProxyPass;
end;

function TDownloader.CheckRedirect(Http: THttpSend; var Url: string): boolean;
const Location = 'Location:';
var i: integer;
    Redirect: string;
    RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara: string;
    OldURL, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara: string;
begin
  Result := False;
  if (Http.ResultCode >= 300) and (Http.ResultCode < 400) then
    for i := 0 to Pred(Http.Headers.Count) do
      if AnsiCompareText(Location, Copy(Http.Headers[i], 1, Length(Location))) = 0 then
        begin
        Redirect := Trim(Copy(Http.Headers[i], Length(Location)+1, MaxInt));
        ParseUrl(Redirect, RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara);
        ParseUrl(Url, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara);
        if RedirProtocol = '' then
          RedirProtocol := UrlProtocol;
        if RedirUser = '' then
          RedirUser := UrlUser;
        if RedirPass = '' then
          RedirPass := UrlPass;
        if (RedirHost = '') or (AnsiCompareText(RedirHost, 'localhost') = 0) then
          RedirHost := UrlHost;
        if RedirPort = '' then
          RedirPort := UrlPort;
        OldUrl := Url;
        Url := RedirProtocol + '://';
        if RedirUser <> '' then
          begin
          Url := Url + RedirUser;
          if RedirPass <> '' then
            Url := Url + ':' + RedirPass;
          Url := Url + '@';
          end;
        Url := Url + RedirHost + ':' + RedirPort + RedirPath { + RedirPara} ;
        Result := Url <> OldUrl;
        Break;
        end;
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string): boolean;
begin
  repeat
    Http.Clear;
    Result := Http.HttpMethod('GET', Url);
  until (not Result) or (not CheckRedirect(Http, Url));
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string; out Page: string): boolean;
begin
  Page := '';
  Result := DownloadPage(Http, Url);
  if Result then
    begin
    SetLength(Page, Http.Document.Size);
    Http.Document.Seek(0, 0);
    Http.Document.ReadBuffer(Page[1], Http.Document.Size);
    end;
end;

procedure TDownloader.SockStatusMonitor(Sender: TObject; Reason: THookSocketReason; const Value: string);
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

procedure TDownloader.AbortTransfer;
begin
  if (VideoDownloader <> nil) and (VideoDownloader.Sock <> nil) then
    VideoDownloader.Sock.AbortSocket;
end;

end.
