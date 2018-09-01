unit uDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, SynaUtil, SynaCode,
  uOptions, uAMF;

type
  EDownloaderError = class(Exception);

  TDownloaderProgressEvent = procedure(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean) of object;
  TDownloaderFileNameValidateEvent = procedure(Sender: TObject; var FileName: string; var Valid: boolean) of object;

  TPageEncoding = (peUnknown, peANSI, peUTF8, peUTF16);
  THttpMethod = (hmGET, hmPOST, hmHEAD);

  TDownloaderClass = class of TDownloader;
  TDownloader = class
    private
      fPrepared: boolean;
      fName: string;
      fLastErrorMsg: string;
      fOnProgress: TDownloaderProgressEvent;
      fHttp: THttpSend;
      fDestinationPath: string;
      fMovieID: string;
      fFileName: string;
      fLastUrl: string;
      fOnFileNameValidate: TDownloaderFileNameValidateEvent;
      fOptions: TYTDOptions;
    protected
      function GetName: string; virtual;
      procedure SetName(const Value: string); virtual;
      procedure SetPrepared(Value: boolean); virtual;
      procedure SetLastErrorMsg(const Value: string); virtual;
      procedure SetMovieID(const Value: string); virtual;
      procedure SetLastUrl(const Value: string); virtual;
      procedure SetOptions(const Value: TYTDOptions); virtual;
      property LastURL: string read fLastUrl;
      property Options: TYTDOptions read fOptions write SetOptions;
    protected
      function GetDefaultFileName: string; virtual;
      function GetFileName: string; virtual;
      procedure SetFileName(const Value: string); virtual;
      function GetFileNameExt: string; virtual;
      function GetTotalSize: int64; virtual;
      function GetDownloadedSize: int64; virtual;
      procedure DoProgress; virtual;
      function CreateHttp: THttpSend; virtual;
      function CheckRedirect(Http: THttpSend; var Url: string): boolean; virtual;
      function DownloadPage(Http: THttpSend; Url: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; virtual;
      function DownloadPage(Http: THttpSend; Url: string; out Page: string; Encoding: TPageEncoding = peUnknown; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; virtual;
      function DownloadAMF(Http: THttpSend; Url: string; Request: TAMFPacket; out Response: TAMFPacket): boolean; virtual;
      function ValidateFileName(var FileName: string): boolean; overload; virtual;
      function ConvertString(const Text: string; Encoding: TPageEncoding): string; virtual;
      function HtmlDecode(const Text: string): string; virtual;
      function UrlDecode(const Text: string): string; virtual;
      function Base64Decode(const Text: string): string; virtual;
      function StripSlashes(const Text: string): string; virtual;
      {$IFDEF DEBUG}
      procedure Log(const Text: string; Overwrite: boolean = False); virtual;
      {$ENDIF}
      procedure NotPreparedError; virtual;
    public
      class function UltimateProvider: string; virtual;
      class function Provider: string; virtual; abstract;
      class function UrlRegExp: string; virtual; abstract;
      class function MovieIDParamName: string; virtual;
    public
      constructor Create(const AMovieID: string); virtual;
      destructor Destroy; override;
      procedure InitOptions(Options: TYTDOptions); virtual;
      function Prepare: boolean; virtual; abstract;
      function ValidateFileName: boolean; overload; virtual;
      function Download: boolean; virtual;
      procedure AbortTransfer; virtual;
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; virtual;
      function Next: boolean; virtual;
      {$ENDIF}
    public
      property Prepared: boolean read fPrepared;
      property Name: string read GetName;
      property FileName: string read GetFileName;
      property LastErrorMsg: string read fLastErrorMsg;
      property TotalSize: int64 read GetTotalSize;
      property DownloadedSize: int64 read GetDownloadedSize;
      property DefaultHttp: THttpSend read fHttp;
    published
      property MovieID: string read fMovieID write SetMovieID;
      property DestinationPath: string read fDestinationPath write fDestinationPath;
      property OnProgress: TDownloaderProgressEvent read fOnProgress write fOnProgress;
      property OnFileNameValidate: TDownloaderFileNameValidateEvent read fOnFileNameValidate write fOnFileNameValidate;
    end;

implementation

uses
  uStringUtils,
  uMessages;

resourcestring
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';

{ TDownloader }

class function TDownloader.UltimateProvider: string; 
begin
  Result := Provider;
end;

class function TDownloader.MovieIDParamName: string;
begin
  Result := ClassName;
end;

constructor TDownloader.Create(const AMovieID: string);
begin
  inherited Create;
  SetLastErrorMsg('');
  SetPrepared(False);
  fHttp := THttpSend.Create;
  fHttp.UserAgent := DEFAULT_USER_AGENT;
  MovieID := AMovieID;
end;

destructor TDownloader.Destroy;
begin
  SetPrepared(False);
  FreeAndNil(fHTTP);
  inherited;
end;

procedure TDownloader.NotPreparedError;
begin
  Raise EDownloaderError.Create(ERR_DOWNLOADER_IS_NOT_PREPARED);
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
    NotPreparedError;
end;

procedure TDownloader.SetName(const Value: string);
begin
  fName := Value;
end;

function TDownloader.GetDefaultFileName: string;
begin
  Result := {AnsiToOem}(StrTr(Name, '\/:*?"<>|', '--;..''--!') + GetFileNameExt);
  if DestinationPath <> '' then
    Result := IncludeTrailingBackslash(DestinationPath) + Result;
end;

function TDownloader.GetFileName: string;
begin
  if fFileName <> '' then
    Result := fFileName
  else
    Result := GetDefaultFileName;
end;

procedure TDownloader.SetFileName(const Value: string);
begin
  fFileName := Value;
end;

function TDownloader.GetFileNameExt: string;
begin
  Result := '';
end;

function TDownloader.GetTotalSize: int64;
begin
  Result := -1;
end;

function TDownloader.GetDownloadedSize: int64;
begin
  Result := 0;
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
  Result := False;
  SetLastErrorMsg(ERR_DOWNLOAD_NOT_IMPLEMENTED);
  if not Prepared then
    NotPreparedError;
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
const
  Location = 'Location:';
  Localhost = 'localhost';
var
  i: integer;
  Redirect: string;
  RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara: string;
  OldURL, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara: string;
begin
  Result := False;
  if (Http.ResultCode >= 300) and (Http.ResultCode < 400) then
    for i := 0 to Pred(Http.Headers.Count) do
      if AnsiCompareText(Location, Copy(Http.Headers[i], 1, Length(Location))) = 0 then
        begin
        OldUrl := Url;
        Redirect := Trim(Copy(Http.Headers[i], Length(Location)+1, MaxInt));
        ParseUrl(Redirect, RedirProtocol, RedirUser, RedirPass, RedirHost, RedirPort, RedirPath, RedirPara);
        if (RedirHost = '') or (AnsiCompareText(RedirHost, Localhost) = 0) then
          begin
          ParseUrl(Url, UrlProtocol, UrlUser, UrlPass, UrlHost, UrlPort, UrlPath, UrlPara);
          if RedirProtocol = '' then
            RedirProtocol := UrlProtocol;
          if RedirUser = '' then
            RedirUser := UrlUser;
          if RedirPass = '' then
            RedirPass := UrlPass;
          if (RedirHost = '') or (AnsiCompareText(RedirHost, Localhost) = 0) then
            RedirHost := UrlHost;
          if RedirPort = '' then
            RedirPort := UrlPort;
          Url := RedirProtocol + '://';
          if RedirUser <> '' then
            begin
            Url := Url + RedirUser;
            if RedirPass <> '' then
              Url := Url + ':' + RedirPass;
            Url := Url + '@';
            end;
          Url := Url + RedirHost + ':' + RedirPort + RedirPath;
          if RedirPara <> '' then
            Url := Url + '?' + RedirPara ;
          end
        else
          Url := Redirect;
        Result := Url <> OldUrl;
        Break;
        end;
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string; Method: THttpMethod; Clear: boolean): boolean;
var MethodStr: string;
begin
  repeat
    SetLastUrl(Url);
    if Clear then
      Http.Clear;
    case Method of
      hmGET:  MethodStr := 'GET';
      hmPOST: MethodStr := 'POST';
      hmHEAD: MethodStr := 'HEAD';
      else    MethodStr := 'GET';
      end;
    Result := Http.HttpMethod(MethodStr, Url);
  until (not Result) or (not CheckRedirect(Http, Url));
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string; out Page: string; Encoding: TPageEncoding; Method: THttpMethod; Clear: boolean): boolean;
begin
  Page := '';
  Result := DownloadPage(Http, Url, Method, Clear);
  if Result then
    begin
    SetLength(Page, Http.Document.Size);
    Http.Document.Seek(0, 0);
    Http.Document.ReadBuffer(Page[1], Http.Document.Size);
    Page := ConvertString(Page, Encoding);
    end;
end;

function TDownloader.DownloadAMF(Http: THttpSend; Url: string; Request: TAMFPacket; out Response: TAMFPacket): boolean;
var OldInputStr: TStream;
begin
  Result := False;
  Response := nil;
  Http.Clear;
  OldInputStr := Http.InputStream;
  try
    Http.InputStream := TMemoryStream.Create;
    try
      Http.MimeType := 'application/x-amf';
      Request.SaveToStream(Http.InputStream);
      Http.InputStream.Position := 0;
      if DownloadPage(Http, Url, hmPOST, False) then
        if (Http.ResultCode >= 200) and (Http.ResultCode < 300) then
          begin
          Response := TAMFPacket.Create;
          try
            Http.Document.Position := 0;
            Response.LoadFromStream(Http.Document);
            Result := True;
          except
            Response.Free;
            Response := nil;
            end;
          end;
    finally
      Http.OutputStream.Free;
      end;
  finally
    Http.InputStream := OldInputStr;
    end;
end;

procedure TDownloader.AbortTransfer;
begin
end;

{$IFDEF MULTIDOWNLOADS}
function TDownloader.First: boolean;
begin
  Result := Prepared;
end;

function TDownloader.Next: boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TDownloader.SetMovieID(const Value: string);
begin
  fMovieID := Value;
  SetPrepared(False);
end;

procedure TDownloader.SetLastUrl(const Value: string);
begin
  fLastURL := Value;
end;

procedure TDownloader.SetOptions(const Value: TYTDOptions);
begin
  fOptions := Value;
end;

function TDownloader.ValidateFileName(var FileName: string): boolean;
begin
  Result := (FileName <> '') and (not FileExists(FileName));
  if Assigned(OnFileNameValidate) then
    OnFileNameValidate(Self, FileName, Result);
end;

function TDownloader.ValidateFileName: boolean;
var FN: string;
begin
  Result := False;
  if Prepared then
    begin
    SetFileName('');
    FN := GetFileName;
    Result := ValidateFileName(FN);
    if Result then
      SetFileName(FN)
    else
      SetLastErrorMsg(Format(ERR_VALIDATE_FILENAME_FAILED, [FN]));
    end;
end;

{$IFDEF DEBUG}
procedure TDownloader.Log(const Text: string; Overwrite: boolean);
var T: TextFile;
    FileName: string;
begin
  try
    FileName := 'debug.' + ClassName + '.log';
    AssignFile(T, FileName);
    if Overwrite or (not FileExists(FileName)) then
      Rewrite(T)
    else
      Append(T);
    try
      Writeln(T, Text);
    finally
      CloseFile(T);
      end;
  except
    end;
end;
{$ENDIF}

function TDownloader.HtmlDecode(const Text: string): string;
begin
  Result := Text;
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function TDownloader.UrlDecode(const Text: string): string;
begin
  Result := DecodeUrl(StringReplace(Text, '+', ' ', [rfReplaceAll]));
end;

function TDownloader.Base64Decode(const Text: string): string;
begin
  Result := DecodeBase64(Text);
end;

function TDownloader.ConvertString(const Text: string; Encoding: TPageEncoding): string;
begin
  case Encoding of
    peUnknown:
      Result := Text;
    peANSI:
      Result := Text;
    peUTF8:
      Result := WideToAnsi(Utf8ToWide(Text));
    peUTF16:
      Result := WideToAnsi(Text);
    else
      Result := Text;
    end;
end;

function TDownloader.StripSlashes(const Text: string): string;
var i, n: integer;
begin
  Result := Text;
  i := 1;
  n := Length(Result);
  while i <= n do
    begin
    if Result[i] = '\' then
      begin
      Delete(Result, i, 1);
      Dec(n);
      end;
    Inc(i);
    end;
end;

procedure TDownloader.InitOptions(Options: TYTDOptions);
begin
  SetOptions(Options);
end;

end.
