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

unit uDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, FileCtrl,
  HttpSend, SynaUtil, SynaCode,
  uOptions, uPCRE, uXML, uAMF, uFunctions,
  {$IFDEF GUI}
  guiDownloaderOptions,
  {$ENDIF}
  uCompatibility;

type
  EDownloaderError = class(Exception);

type
  TDownloaderProgressEvent = procedure(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean) of object;
  TDownloaderFileNameValidateEvent = procedure(Sender: TObject; var FileName: string; var Valid: boolean) of object;

type
  TStringArray = array of string;
  TPStringArray = array of PString;

type
  TPageEncoding = (peNone, peUnknown, peANSI, peUTF8, peUTF16);

type
  TDownloaderFeature = (dfDummy {$IFDEF SUBTITLES} , dfSubtitles, dfSubtitlesConvert {$ENDIF} , dfRtmpLiveStream, dfPreferRtmpLiveStream, dfRequireSecureToken );
  TDownloaderFeatures = set of TDownloaderFeature;

const
  peXml = peNone;

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
      function GetName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetName(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetPrepared(Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetLastErrorMsg: string; virtual;
      procedure SetLastErrorMsg(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMovieID(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetLastUrl(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); virtual;
      function GetFileName: string; virtual;
      function GetContentUrl: string; virtual;
      procedure SetFileName(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property UnpreparedName: string read fName;
      property LastURL: string read fLastUrl;
    protected
      function GetDefaultFileName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetFileNameExt: string; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function GetTotalSize: int64; virtual;
      function GetDownloadedSize: int64; virtual;
      procedure DoProgress; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ValidateFileName(var FileName: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function CreateHttp: THttpSend; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; Url: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; const Url: string; out Page: string; Encoding: TPageEncoding = peUnknown; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; Url: string; const PostData: AnsiString; const PostMimeType: string; const Headers: array of string; Clear: boolean): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; Url: string; const PostData: AnsiString; const PostMimeType: string; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Headers: array of string; out Page: string; Encoding: TPageEncoding = peUnknown; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadPage(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; out Page: string; Encoding: TPageEncoding = peUnknown; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadBinary(Http: THttpSend; const Url: string; out Data: AnsiString; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXml(Http: THttpSend; const Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXml(Http: THttpSend; const Url: string; out Xml: TXmlDoc; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXml(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; out Xml: TXmlDoc; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadSOAP(Http: THttpSend; const Url, SoapAction: string; const SoapHeader, SoapBody: TXmlNode; out Response: TXmlDoc; out ResponseHeader, ResponseBody: TXmlNode; Clear: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadAMF(Http: THttpSend; Url: string; Request: TAMFPacket; out Response: TAMFPacket): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function ConvertString(const Text: TStream; Encoding: TPageEncoding): string; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ConvertString(Text: AnsiString; Encoding: TPageEncoding): string; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function HtmlDecode(const Text: string; Unicode: boolean = False): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function HtmlEncode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function UrlDecode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function UrlEncode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Base64Decode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Base64Encode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function JSDecode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function StripSlashes(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function StripTags(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ContentTypeToExtension(const ContentType: string): string;
    protected
      function ExtractUrlRoot(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlFileName(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlPath(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlExt(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpAllVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: TStringArray): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True; const VarNameSubExprName: string = 'VARNAME'; const VarValueSubExprName: string = 'VARVALUE'): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlVar(Http: THttpSend; const Url, Path: string; out VarValue: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlVar(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path: string; out VarValue: string; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlAttr(Http: THttpSend; const Url, Path, Attribute: string; out VarValue: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlAttr(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path, Attribute: string; out VarValue: string; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      {$IFDEF DEBUG}
      procedure Log(const Text: string; Overwrite: boolean = False); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
      procedure NotPreparedError; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    public
      class function Provider: string; virtual; abstract;
      class function Features: TDownloaderFeatures; virtual;
      class function UrlRegExp: string; virtual; abstract;
      class function MovieIDParamName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; virtual;
      {$ENDIF}
    public
      constructor Create(const AMovieID: string); virtual;
      destructor Destroy; override;
      function Prepare: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF} abstract;
      function ValidateFileName: boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Download: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure AbortTransfer; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function Next: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      {$ENDIF}
    public
      property Prepared: boolean read fPrepared;
      property Name: string read GetName;
      property FileName: string read GetFileName;
      property ContentUrl: string read GetContentUrl;
      property LastErrorMsg: string read GetLastErrorMsg;
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
  uStringConsts,
  uStringUtils,
  uMessages;

const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';

{ TDownloader }

class function TDownloader.MovieIDParamName: string;
begin
  Result := ClassName;
end;

class function TDownloader.Features: TDownloaderFeatures;
begin
  Result := [];
end;

{$IFDEF GUI}
class function TDownloader.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := nil;
end;
{$ENDIF}

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

function TDownloader.GetLastErrorMsg: string;
begin
  Result := fLastErrorMsg;
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
var Ext: string;
{$IFDEF MAXFILENAMELENGTH}
var MaxLength, i, n: integer;
{$ENDIF}
begin
  Result := StrTr(Trim(Name), '\/:*?"<>|', '--;..''--!');
  Ext := GetFileNameExt;
  {$IFDEF MAXFILENAMELENGTH}
  MaxLength := MAX_PATH - 5 - Length(Ext);
  if Options.DestinationPath <> '' then
    MaxLength := MaxLength - Length(ExpandFileName(Options.DestinationPath));
  if Options.DownloadToProviderSubdirs then
    MaxLength := MaxLength - Succ(Length(Provider));
  if MaxLength > 0 then
    begin
    n := Length(Result);
    if n > MaxLength then
      begin
      i := (n - MaxLength) div 2;
      System.Delete(Result, i, n - MaxLength);
      System.Insert('...', Result, i);
      end;
    end;
  {$ENDIF}
  Result := {AnsiToOem}(Result + Ext);
  if Options.DownloadToProviderSubdirs then
    Result := IncludeTrailingPathDelimiter(Provider) + Result;
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

function TDownloader.GetContentUrl: string;
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

function TDownloader.DownloadPage(Http: THttpSend; Url: string; const PostData: AnsiString; const PostMimeType: string; const Headers: array of string; Clear: boolean): boolean;
var
  OldInputStream, InputStream: TStream;
  i: integer;
begin
  if Clear then
    Http.Clear;
  for i := 0 to Pred(Length(Headers)) do
    Http.Headers.Add(Headers[i]);
  InputStream := TMemoryStream.Create;
  try
    if PostData <> '' then
      begin
      InputStream.WriteBuffer(PostData[1], Length(PostData));
      InputStream.Position := 0;
      end;
    OldInputStream := Http.InputStream;
    try
      Http.InputStream := InputStream;
      Http.MimeType := PostMimeType;
      Result := DownloadPage(Http, Url, hmPOST, False);
    finally
      Http.InputStream := OldInputStream;
      end;
  finally
    FreeAndNil(InputStream);
    end;
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string; const PostData: AnsiString; const PostMimeType: string; Clear: boolean): boolean;
begin
  Result := DownloadPage(Http, Url, PostData, PostMimeType, [], Clear);
end;

function TDownloader.DownloadPage(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Headers: array of string; out Page: string; Encoding: TPageEncoding; Clear: boolean): boolean;
begin
  Page := '';
  Result := DownloadPage(Http, Url, PostData, PostMimeType, Headers, Clear);
  if Result then
    begin
    Page := ConvertString(Http.Document, Encoding);
    Http.Document.Seek(0, 0);
    end;
end;

function TDownloader.DownloadPage(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; out Page: string; Encoding: TPageEncoding; Clear: boolean): boolean;
begin
  Result := DownloadPage(Http, Url, PostData, PostMimeType, [], Page, Encoding, Clear);
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
  Result := False;
  if DownloadPage(Http, Url, Page, peXml, Method, Clear) then
    if Http.Document.Size > 0 then
      begin
      Xml := TXmlDoc.Create;
      try
        Xml.LoadFromStream(Http.Document);
        Http.Document.Seek(0, 0);
        Result := True;
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

function TDownloader.DownloadXml(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; out Xml: TXmlDoc; Clear: boolean): boolean;
begin
  Xml := nil;
  Result := False;
  if DownloadPage(Http, Url, PostData, PostMimeType, Clear) then
    if Http.Document.Size > 0 then
      begin
      Xml := TXmlDoc.Create;
      try
        Xml.LoadFromStream(Http.Document);
        Http.Document.Seek(0, 0);
        Result := True;
      except
        FreeAndNil(Xml);
        Result := False;
        end;
      end;
end;

function TDownloader.DownloadSOAP(Http: THttpSend; const Url,
  SoapAction: string; const SoapHeader, SoapBody: TXmlNode; out Response: TXmlDoc;
  out ResponseHeader, ResponseBody: TXmlNode; Clear: boolean): boolean;
var
  Request: TXmlDoc;
  Node: TXmlNode;
  RequestStr: AnsiString;
  Namespace: Utf8String;
begin
  Response := nil;
  ResponseHeader := nil;
  ResponseBody := nil;
  Request := TXmlDoc.CreateName('soap:Envelope');
  try
    Request.Root.AttributeByNameWide['xmlns:soap'] := 'http://schemas.xmlsoap.org/soap/envelope/';
    if SoapHeader <> nil then
      begin
      Node := Request.Root.NodeNew('soap:Header').NodeNew('');
      Node.Assign(SoapHeader);
      end;
    Node := Request.Root.NodeNew('soap:Body').NodeNew('');
    if SoapBody <> nil then
      Node.Assign(SoapBody);
    RequestStr := Request.SaveToBinaryString;
    if Clear then
      Http.Clear;
    Http.Headers.Add('SOAPAction: ' + SoapAction);
    Result := DownloadXml(Http, Url, RequestStr, HTTP_SOAP_ENCODING, Response, False);
    try
      if Result then
        begin
        Namespace := XmlGetNamespace(Response);
        if XmlNodeByPath(Response, Namespace + 'Fault', Node) then
          EDownloaderError.CreateFmt(ERR_SERVER_ERROR, [XmlValueByPath(Node, 'faultstring')])
        else
          begin
          if XmlNodeByPath(Response, Namespace + 'Header', Node) then
            ResponseHeader := Node;
          if XmlNodeByPath(Response, Namespace + 'Body', Node) then
            ResponseBody := Node;
          end;
        end;
    except
      FreeAndNil(Response);
      Raise;
      end;
  finally
    FreeAndNil(Request);
    end;
end;

function TDownloader.DownloadAMF(Http: THttpSend; Url: string; Request: TAMFPacket; out Response: TAMFPacket): boolean;
var RequestStr: AnsiString;
begin
  Result := False;
  Request.SaveToString(RequestStr);
  if DownloadPage(Http, Url, RequestStr, 'application/x-amf', True) then
    if (Http.ResultCode >= 200) and (Http.ResultCode < 300) then
      if Http.Document.Size > 0 then
        begin
        Response := TAMFPacket.Create;
        try
          Http.Document.Seek(0, 0);
          Response.LoadFromStream(Http.Document);
          Http.Document.Seek(0, 0);
          Result := True;
        except
          FreeAndNil(Response);
          end;
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
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
  if Dir <> '' then
    ForceDirectories(ExpandFileName(Dir));
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
      SetFileName(ExpandFileName(FN))
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

type
  THtmlDecodeItem = record
    Html, Txt: string;
    DecodeOnly: boolean;
    end;
const
  HtmlDecodeItems: array[0..5] of THtmlDecodeItem
    = (
        (Html: '&mdash;'; Txt: '--'; DecodeOnly: true),
        (Html: '&lt;'   ; Txt: '<' ; DecodeOnly: false),
        (Html: '&gt;'   ; Txt: '>' ; DecodeOnly: false),
        (Html: '&quot;' ; Txt: '"' ; DecodeOnly: false),
        (Html: '&apos;' ; Txt: ''''; DecodeOnly: false),
        (Html: '&amp;'  ; Txt: '&' ; DecodeOnly: false)
      );

function TDownloader.HtmlDecode(const Text: string; Unicode: boolean): string;
var
  i, Start, Code: integer;
  {$IFNDEF UNICODE}
  WC: WideChar;
  WS: WideString;
  s: string;
  {$ENDIF}
begin
  Result := Text;
  for i := 0 to Pred(Length(HtmlDecodeItems)) do
    Result := StringReplace(Result, HtmlDecodeItems[i].Html, HtmlDecodeItems[i].Txt, [rfReplaceAll]);
  i := 1;
  while i < Length(Result) do
    begin
    if Result[i] = '&' then
      if Result[Succ(i)] = '#' then
        begin
        Start := i;
        Inc(i, 2);
        while (i <= Length(Result)) and (Result[i] <> ';') do
          Inc(i);
        Code := StrToIntDef(Copy(Result, Start+2, i-Start-2), -1);
        System.Delete(Result, Start, i-Start+1);
        i := Start;
        {$IFDEF UNICODE}
        if (Code >= 0) and (Code <= 65535) then
        {$ELSE}
        if Unicode then
          begin
          WC := WideChar(Code);
          WS := WC;
          s := String(WS);
          Code := Ord(s[1]);
          end;
        if (Code >= 0) and (Code <= 255) then
        {$ENDIF}
          begin
          System.Insert(Chr(Code), Result, i);
          Inc(i);
          end;
        Continue;
        end;
    Inc(i);
    end;
end;

function TDownloader.HtmlEncode(const Text: string): string;
var
  i: integer;
begin
  Result := Text;
  for i := Pred(Length(HtmlDecodeItems)) downto 0 do
    if not HtmlDecodeItems[i].DecodeOnly then
      Result := StringReplace(Result, HtmlDecodeItems[i].Txt, HtmlDecodeItems[i].Html, [rfReplaceAll]);
end;

function TDownloader.UrlDecode(const Text: string): string;
begin
  Result := {$IFDEF UNICODE} string {$ENDIF} (DecodeUrl( {$IFDEF UNICODE} AnsiString {$ENDIF} (StringReplace(Text, '+', ' ', [rfReplaceAll]))));
end;

function TDownloader.UrlEncode(const Text: string): string;
begin
  Result := {$IFDEF UNICODE} string {$ENDIF} (EncodeUrl( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.Base64Decode(const Text: string): string;
begin
  Result :=  {$IFDEF UNICODE} string {$ENDIF} (DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.Base64Encode(const Text: string): string;
begin
  Result :=  {$IFDEF UNICODE} string {$ENDIF} (EncodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.JSDecode(const Text: string): string;
var
  i: integer;
begin
  Result := Text;
  i := 1;
  while i < Length(Result) do
    begin
    if Result[i] = '\' then
      if Result[i+1] = 'u' then
        begin
        System.Insert(WideChar(StrToInt('$' + Copy(Result, i+2, 4))), Result, i);
        System.Delete(Result, i+1, 6);
        end;
    Inc(i);
    end;
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

function TDownloader.StripTags(const Text: string): string;
var
  i, Start: integer;
  Quotes: Char;
begin
  Result := Text;
  i := 1;
  while i <= Length(Result) do
    if Result[i] = '<' then
      if Copy(Result, i, 4) = '<!--' then
        begin
        Start := i;
        while (i <= Length(Result)) and (Copy(Result, i, 3) <> '-->') do
          Inc(i);
        System.Delete(Result, Start, i-Start+3);
        i := Start;
        end
      else if Copy(Result, i, 9) = '<![CDATA[' then
        begin
        System.Delete(Result, i, 9);
        while (i <= Length(Result)) and (Copy(Result, i, 3) <> ']]>') do
          Inc(i);
        System.Delete(Result, i, 3);
        end
      else
        begin
        Start := i;
        Quotes := #0;
        while (i <= Length(Result)) do
          if Quotes <> #0 then
            if Result[i] = Quotes then
              begin
              Quotes := #0;
              Inc(i);
              end
            else
              Inc(i)
          else
            case Result[i] of
              '"', '''':
                begin
                Quotes := Result[i];
                Inc(i);
                end;
              '>':
                begin
                Inc(i);
                Break;
                end;
              else
                Inc(i);
              end;
        System.Delete(Result, Start, i-Start);
        i := Start;
        end
    else
      Inc(i);
end;

function TDownloader.ExtractUrlExt(const Url: string): string;
begin
  Result := ExtractFileExt(ExtractUrlFileName(Url));
end;

function TDownloader.ExtractUrlRoot(const Url: string): string;
var Protocol, User, Password, Host, Port, Path, Paragraph: string;
begin
  ParseUrl(Url, Protocol, User, Password, Host, Port, Path, Paragraph);
  Result := Protocol + '://';
  if User <> '' then
    if Password = '' then
      Result := Result + User + '@'
    else
      Result := Result + User + ':' + Password + '@';
  Result := Result + Host + ':' + Port;
end;

function TDownloader.ExtractUrlFileName(const Url: string): string;
var i: integer;
begin
  i := Pos('?', Url);
  if i <= 0 then
    Result := Url
  else
    Result := Copy(Url, 1, Pred(i));
  for i := Length(Result) downto 1 do
    if Result[i] = '/' then
      begin
      Result := Copy(Result, Succ(i), MaxInt);
      Break;
      end;
end;

function TDownloader.ExtractUrlPath(const Url: string): string;
var i: integer;
begin
  i := Pos('?', Url);
  if i <= 0 then
    Result := Url
  else
    Result := Copy(Url, 1, Pred(i));
  for i := Length(Result) downto 1 do
    if Result[i] = '/' then
      begin
      Result := Copy(Result, 1, i);
      Break;
      end;
end;

function TDownloader.GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean;
begin
  Result := RegExp.Match(Text) and RegExp.SubexpressionByName(VarName, VarValue);
end;

function TDownloader.GetRegExpAllVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: TStringArray): boolean;
var n: integer;
    b: boolean;
    Value: string;
begin
  SetLength(VarValue, 0);
  n := 0;
  b := RegExp.Match(Text);
  while b do
    begin
    if RegExp.SubexpressionByName(VarName, Value) then
      begin
      if Length(VarValue) <= n then
        SetLength(VarValue, Length(VarValue)+16);
      VarValue[n] := Value;
      Inc(n);
      end;
    b := RegExp.MatchAgain;
    end;
  SetLength(VarValue, n);
  Result := n > 0;
end;

function TDownloader.GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean): boolean;
var i: integer;
    VarValue: string;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  Result := RegExp.Match(Text);
  if Result then
    for i := 0 to High(VarNames) do
      if not RegExp.SubExpressionByName(VarNames[i], VarValue) then
        Result := False
      else
        VarValues[i]^ := VarValue;
end;

function TDownloader.GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean; const VarNameSubExprName, VarValueSubExprName: string): boolean;
var i, j: integer;
    VarName: string;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  Result := RegExp.Match(Text);
  if Result then
    repeat
      VarName := RegExp.SubexpressionByName(VarNameSubExprName);
      for j := 0 to High(VarNames) do
        if VarName = VarNames[j] then
          begin
          if j <= High(VarValues) then
            VarValues[j]^ := RegExp.SubexpressionByName(VarValueSubExprName);
          end;
    until not RegExp.MatchAgain;
end;

function TDownloader.GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean;
var Node: TXmlNode;
begin
  if XmlNodeByPath(Xml, Path, Node) then
    begin
    VarValue := XmlValueIncludingCData(Node);
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TDownloader.GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean;
var Node: TXmlNode;
begin
  if XmlNodeByPath(Xml, Path, Node) and Node.HasAttribute(Utf8String(Attribute)) then
    begin
    VarValue := Node.AttributeByNameWide[Utf8String(Attribute)];
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TDownloader.GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean;
begin
  Result := GetXmlVar(Xml.Root, Path, VarValue);
end;

function TDownloader.GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean;
begin
  Result := GetXmlAttr(Xml.Root, Path, Attribute, VarValue);
end;

function TDownloader.DownloadXmlVar(Http: THttpSend; const Url, Path: string; out VarValue: string; Method: THttpMethod; Clear: boolean): boolean;
var Xml: TXmlDoc;
begin
  Result := False;
  VarValue := '';
  if DownloadXml(Http, Url, Xml, Method, Clear) then
    try
      if GetXmlVar(Xml, Path, VarValue) then
        Result := True;
    finally
      FreeAndNil(Xml);
      end;
end;

function TDownloader.DownloadXmlVar(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path: string; out VarValue: string; Clear: boolean): boolean;
var Xml: TXmlDoc;
begin
  Result := False;
  VarValue := '';
  if DownloadXml(Http, Url, PostData, PostMimeType, Xml, Clear) then
    try
      if GetXmlVar(Xml, Path, VarValue) then
        Result := True;
    finally
      FreeAndNil(Xml);
      end;
end;

function TDownloader.DownloadXmlAttr(Http: THttpSend; const Url, Path, Attribute: string; out VarValue: string; Method: THttpMethod; Clear: boolean): boolean;
var Xml: TXmlDoc;
begin
  Result := False;
  VarValue := '';
  if DownloadXml(Http, Url, Xml, Method, Clear) then
    try
      if GetXmlAttr(Xml, Path, Attribute, VarValue) then
        Result := True;
    finally
      FreeAndNil(Xml);
      end;
end;

function TDownloader.DownloadXmlAttr(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path, Attribute: string; out VarValue: string; Clear: boolean): boolean;
var Xml: TXmlDoc;
begin
  Result := False;
  VarValue := '';
  if DownloadXml(Http, Url, PostData, PostMimeType, Xml, Clear) then
    try
      if GetXmlAttr(Xml, Path, Attribute, VarValue) then
        Result := True;
    finally
      FreeAndNil(Xml);
      end;
end;

function TDownloader.ContentTypeToExtension(const ContentType: string): string;
type
  TContentTypeExt = record
    ContentType: string;
    Extension: string;
    end;
const
  ContentTypes: array[0..3] of TContentTypeExt
    = (
        (ContentType: 'video/x-flv';                    Extension: '.flv'),
        (ContentType: 'video/x-mp4';                    Extension: '.mp4'),
        (ContentType: 'video/x-webm';                   Extension: '.webm'),
        (ContentType: 'application/x-shockwave-flash';  Extension: '.swf')
      );
var
  i: integer;
begin
  Result := '';
  for i := 0 to Pred(Length(ContentTypes)) do
    if AnsiCompareText(ContentType, ContentTypes[i].ContentType) = 0 then
      begin
      Result := ContentTypes[i].Extension;
      Break;
      end;
end;

end.
