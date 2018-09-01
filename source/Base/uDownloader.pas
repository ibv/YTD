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

unit uDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, 
  HttpSend, SynaUtil, SynaCode,
  uOptions, uXML, uAMF, uCompatibility;

type
  EDownloaderError = class(Exception);

  TDownloaderProgressEvent = procedure(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean) of object;
  TDownloaderFileNameValidateEvent = procedure(Sender: TObject; var FileName: string; var Valid: boolean) of object;

type
  TPageEncoding = (peNone, peUnknown, peANSI, peUTF8, peUTF16);

const
  peXml: TPageEncoding = peNone;

type
  THttpMethod = (hmGET, hmPOST, hmHEAD);

  TDownloaderClass = class of TDownloader;
  TDownloader = class
    private
      fPrepared: boolean;
      fName: string;
      fLastErrorMsg: string;
      fOnProgress: TDownloaderProgressEvent;
      fHttp: THttpSend;
      fMovieID: string;
      fFileName: string;
      fLastUrl: string;
      fOnFileNameValidate: TDownloaderFileNameValidateEvent;
      fOptions: TYTDOptions;
    protected
      function GetName: string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      procedure SetName(const Value: string); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      procedure SetPrepared(Value: boolean); virtual;
      procedure SetLastErrorMsg(const Value: string); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      procedure SetMovieID(const Value: string); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      procedure SetLastUrl(const Value: string); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); virtual;
      property UnpreparedName: string read fName;
      property LastURL: string read fLastUrl;
    protected
      function GetDefaultFileName: string; virtual;
      function GetFileName: string; virtual;
      procedure SetFileName(const Value: string); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetFileNameExt: string; virtual;
      function GetTotalSize: int64; virtual;
      function GetDownloadedSize: int64; virtual;
      procedure DoProgress; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function CreateHttp: THttpSend; virtual;
      function CheckRedirect(Http: THttpSend; var Url: string): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; Url: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; const Url: string; out Page: string; Encoding: TPageEncoding = peUnknown; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadBinary(Http: THttpSend; const Url: string; out Data: AnsiString; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadXml(Http: THttpSend; const Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadXml(Http: THttpSend; const Url: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function DownloadAMF(Http: THttpSend; Url: string; Request: TAMFPacket; out Response: TAMFPacket): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function ValidateFileName(var FileName: string): boolean; overload; virtual;
      function ConvertString(const Text: TStream; Encoding: TPageEncoding): string; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function ConvertString(Text: AnsiString; Encoding: TPageEncoding): string; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function HtmlDecode(const Text: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function UrlDecode(const Text: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function UrlEncode(const Text: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function Base64Decode(const Text: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function StripSlashes(const Text: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      {$IFDEF DEBUG}
      procedure Log(const Text: string; Overwrite: boolean = False); {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      {$ENDIF}
      procedure NotPreparedError; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
    public
      class function Provider: string; virtual; abstract;
      class function UrlRegExp: string; virtual; abstract;
      class function MovieIDParamName: string; virtual;
    public
      constructor Create(const AMovieID: string); virtual;
      destructor Destroy; override;
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
    public
      property MovieID: string read fMovieID write SetMovieID;
      property Options: TYTDOptions read fOptions write SetOptions;
      property OnProgress: TDownloaderProgressEvent read fOnProgress write fOnProgress;
      property OnFileNameValidate: TDownloaderFileNameValidateEvent read fOnFileNameValidate write fOnFileNameValidate;
    end;

implementation

uses
  uStringUtils,
  uMessages;

const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';

{ TDownloader }

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
  Raise EDownloaderError.Create(_(ERR_DOWNLOADER_IS_NOT_PREPARED));
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
  Result := {AnsiToOem}(StrTr(Trim(Name), '\/:*?"<>|', '--;..''--!') + GetFileNameExt);
  if Options.DestinationPath <> '' then
    Result := Options.DestinationPath + Result;
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
  SetLastErrorMsg(_(ERR_DOWNLOAD_NOT_IMPLEMENTED));
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
  Http.Document.Seek(0, 0);
end;

function TDownloader.DownloadPage(Http: THttpSend; const Url: string; out Page: string; Encoding: TPageEncoding; Method: THttpMethod; Clear: boolean): boolean;
begin
  Page := '';
  Result := DownloadPage(Http, Url, Method, Clear);
  if Result then
    begin
    Page := ConvertString(Http.Document, Encoding);
    Http.Document.Seek(0, 0);
    end;
end;

function TDownloader.DownloadBinary(Http: THttpSend; const Url: string; out Data: AnsiString; Method: THttpMethod; Clear: boolean): boolean;
begin
  Result := DownloadPage(Http, Url, Method, Clear);
  if Result and (Http.Document.Size > 0) then
    begin
    SetLength(Data, Http.Document.Size);
    Http.Document.ReadBuffer(Data[1], Http.Document.Size);
    Http.Document.Seek(0, 0);
    end
  else
    Data := '';
end;

function TDownloader.DownloadXml(Http: THttpSend; const Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean;
begin
  Xml := nil;
  Result := DownloadPage(Http, Url, Page, peXml, Method, Clear);
  if Result and (Page <> '') then
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.LoadFromStream(Http.Document);
      Http.Document.Seek(0, 0);
    except
      FreeAndNil(Xml);
      Result := False;
      end;
    end;
end;

function TDownloader.DownloadXml(Http: THttpSend; const Url: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean;
var Page: string;
begin
  Result := DownloadXml(Http, Url, Page, Xml, Method, Clear);
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
            Http.Document.Seek(0, 0);
            Response.LoadFromStream(Http.Document);
            Http.Document.Seek(0, 0);
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
  if Value.ProxyActive then
    begin
    DefaultHttp.ProxyHost := Value.ProxyHost;
    DefaultHttp.ProxyPort := Value.ProxyPort;
    DefaultHttp.ProxyUser := Value.ProxyUser;
    DefaultHttp.ProxyPass := Value.ProxyPassword;
    end
  else
    begin
    DefaultHttp.ProxyHost := '';
    DefaultHttp.ProxyPort := '';
    DefaultHttp.ProxyUser := '';
    DefaultHttp.ProxyPass := '';
    end;
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
      SetLastErrorMsg(Format(_(ERR_VALIDATE_FILENAME_FAILED), [FN]));
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
  Result := string(DecodeUrl(AnsiString(StringReplace(Text, '+', ' ', [rfReplaceAll]))));
end;

function TDownloader.UrlEncode(const Text: string): string;
begin
  Result := string(EncodeUrl(AnsiString(Text)));
end;

function TDownloader.Base64Decode(const Text: string): string;
begin
  Result := string(DecodeBase64(AnsiString(Text)));
end;

function TDownloader.ConvertString(const Text: TStream; Encoding: TPageEncoding): string;
var s: AnsiString;
begin
  SetLength(s, Text.Size);
  Text.Seek(0, 0);
  Text.ReadBuffer(s[1], Text.Size);
  Result := ConvertString(s, Encoding);
end;

function TDownloader.ConvertString(Text: AnsiString; Encoding: TPageEncoding): string;
var n: integer;
begin
  case Encoding of
    peNone:
      Result := string(Text);
    peUnknown:
      Result := string(Text);
    peANSI:
      Result := string(Text);
    peUTF8:
      Result := Utf8ToString(Utf8String(Text));
    peUTF16:
      begin
      n := Length(Text) shr 1;
      SetLength(Result, n);
      Move(Text[1], Result[1], n shl 1);
      {$IFNDEF UNICODE}
      Result := WideToAnsi(Result);
      {$ENDIF}
      end
    else
      Result := string(Text);
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

end.
