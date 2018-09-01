unit uDownloader;

interface

uses
  SysUtils,
  HttpSend, blcksock;

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
      procedure DoProgress(TotalSize, DownloadedSize: int64); virtual;
      function CreateHttp: THttpSend; virtual;
      function CheckRedirect(Http: THttpSend; out Url: string): boolean; virtual;
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
      property DestinationPath: string read fDestinationPath write fDestinationPath;
      property Prepared: boolean read fPrepared;
      property Name: string read GetName;
      property FileName: string read GetFileName;
      property LastErrorMsg: string read fLastErrorMsg;
      property DefaultHttp: THttpSend read fHttp;
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

procedure TDownloader.DoProgress(TotalSize, DownloadedSize: int64);
var DoAbort: boolean;
begin
  if Assigned(OnProgress) then
    begin
    DoAbort := False;
    OnProgress(Self, TotalSize, DownloadedSize, DoAbort);
    if DoAbort then
      Abort;
    end;
end;

function TDownloader.Download: boolean;
begin
  SetLastErrorMsg('Can''t download this kind of content.');
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

function TDownloader.CheckRedirect(Http: THttpSend; out Url: string): boolean;
const Location = 'Location:';
var i: integer;
begin
  Result := False;
  if (Http.ResultCode >= 300) and (Http.ResultCode < 400) then
    for i := 0 to Pred(Http.Headers.Count) do
      if AnsiCompareText(Location, Copy(Http.Headers[i], 1, Length(Location))) = 0 then
        begin
        Url := Trim(Copy(Http.Headers[i], Length(Location)+1, MaxInt));
        Result := True;
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
    begin
    BytesTransferred := BytesTransferred + StrToInt64(Value);
    DoProgress(fVideoDownloader.DownloadSize, BytesTransferred);
    end
  else
    DoProgress(0, 0);
end;

end.
