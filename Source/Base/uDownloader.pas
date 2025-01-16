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
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  {$IFNDEF DELPHI7_UP} FileCtrl, {$ENDIF}
  HttpSend, SynaUtil, SynaCode,
  uOptions, uPCRE, uXML, uAMF, uFunctions, uLanguages,
  {$IFDEF GUI}
  guiDownloaderOptions;
  {$ENDIF}
  ///uCompatibility;

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
  TDownloaderFeature = (
    dfDummy
    {$IFDEF SUBTITLES} , dfSubtitles, dfSubtitlesConvert {$ENDIF}
    , dfRtmpLiveStream, dfPreferRtmpLiveStream, dfRtmpRealTime, dfPreferRtmpRealTime
    , dfAcceptSecureToken, dfRequireSecureToken, dfUserLogin
    );
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
      fReferer: string;
      {$IFDEF MULTIDOWNLOADS}
      fLastPrepareTime: TDateTime;
      fPrepareLifetime: Integer;
      {$ENDIF}
      fProviderName: string;
      fVideoResolution: integer;
      fVideoBitrate: integer;

    protected
      function GetName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetName(const Value: string); virtual;
      procedure SetPrepared(Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetLastErrorMsg: string; virtual;
      procedure SetLastErrorMsg(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMovieID(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetLastUrl(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); virtual;
      function GetFileName: string; virtual;
      function GetContentUrl: string; virtual;
      procedure SetFileName(const Value: string); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ApplyIndexToName(const Name: string; Index, Count: integer): string;
      property UnpreparedName: string read fName;
      property LastURL: string read fLastUrl;
      property Referer: string read fReferer write fReferer;
    protected
      function GetDefaultFileName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetFileNameExt: string; virtual;
      function GetTotalSize: int64; virtual;
      function GetDownloadedSize: int64; virtual;
      procedure DoProgress; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function InternalValidateFileName(var FileName: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function CreateHttp: THttpSend; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure ClearHttp(Http: THttpSend);
      procedure SetReferer(Http: THttpSend; const Referer: string);
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
      function UrlDecode(const Text: string; Encoding: TPageEncoding = peANSI): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function UrlEncode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Base64Decode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Base64Encode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function HexEncode(const Text: AnsiString): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function JSDecode(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function StripSlashes(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function StripTags(const Text: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ContentTypeToExtension(const ContentType: string): string;
    protected
      function ExtractUrlRoot(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlFileName(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlPath(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlExt(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRelativeUrl(CurrentUrl, Path: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpAllVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: TStringArray): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVarsAgain(RegExp: TRegExp; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True; const VarNameSubExprName: string = 'VARNAME'; const VarValueSubExprName: string = 'VARVALUE'): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlNode; const Path, AttributeName, AttributeValue, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlDoc; const Path, AttributeName, AttributeValue, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlVar(Http: THttpSend; const Url, Path: string; out VarValue: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlVar(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path: string; out VarValue: string; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlAttr(Http: THttpSend; const Url, Path, Attribute: string; out VarValue: string; Method: THttpMethod = hmGet; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadXmlAttr(Http: THttpSend; const Url: string; const PostData: AnsiString; const PostMimeType: string; const Path, Attribute: string; out VarValue: string; Clear: boolean = True): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      {$IFDEF DEBUG}
      procedure Log(const Text: string; Overwrite: boolean = False); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$ENDIF}
      procedure NotPreparedError; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    protected
      function RegExp_FindBestVideo(const List: string; InfoRegExp: TRegExp; const UrlID, QualityID: string; out BestUrl: string): boolean;
      function Smil_FindBestVideo(Container: TXmlNode; out Url: string; const MaxBitrate: integer = 0): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function UnixTimestamp(DT: TDateTime = 0): integer;
    public
      class function Provider: string; virtual; abstract;
      class function Features: TDownloaderFeatures; virtual;
      class function UrlRegExp: string; virtual; abstract;
      class function IsSupported_Url(const Url: string; out MovieID: string): boolean; virtual;
      class function IsSupportedUrl(const Url: string; out MovieID, Provid: string): boolean; virtual;
      class function MovieIDParamName: string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; virtual;
      {$ENDIF}
    public
      constructor Create(const AMovieID: string); virtual;
      destructor Destroy; override;
      function Prepare: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF} abstract;
      function ValidateFileName: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function Download: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure AbortTransfer; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function Next: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function ValidatePrepare: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      property LastPrepareTime: TDateTime read fLastPrepareTime write fLastPrepareTime;
      property PrepareLifetime: Integer read fPrepareLifetime write fPrepareLifetime; // seconds
      {$ENDIF}
    public
      property Prepared: boolean read fPrepared;
      property Name: string read GetName write SetName;
      property FileName: string read GetFileName;
      property FileNameExt: string read GetFileNameExt;
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

      property ProviderName: string read fProviderName write fProviderName;
      property MaxVResolution: integer read fVideoResolution write fVideoResolution;
      property MaxVBitrate: integer read fVideoBitrate write fVideoBitrate;

    end;

implementation

uses
  uStringConsts,
  uStrings,
  uMessages,
  math, strutils
  {$ifdef DEBUG}
  ,uLog
  {$endif}
  ;

const
  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';
  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:102.0) Gecko/20100101 Firefox/102.0';
  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36';
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/%d.0.%d.%d Safari/537.36';
  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36';



var
  UrlRegExps: TRegExpCache = nil;

{ TDownloader }

class function TDownloader.MovieIDParamName: string;
begin
  Result := ClassName;
end;

class function TDownloader.Features: TDownloaderFeatures;
begin
  Result := [];
end;

class function TDownloader.IsSupported_Url(const Url: string; out MovieID: string): boolean;
var
  RE: TRegExp;
begin
  Result := False;
  RE := UrlRegExps.GetRegExp(UrlRegExp);
  if RE.Match(Url) then
    if RE.SubexpressionByName(MovieIDParamName, MovieID) then
      Result := MovieID <> '';
end;

class function TDownloader.IsSupportedUrl(const Url: string; out MovieID,Provid : string): boolean;
begin
  Provid := Provider;
  Result := IsSupported_Url(Url,MovieID);
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
  {$IFDEF MULTIDOWNLOADS}
  PrepareLifetime := 60; // 60 seconds
  {$ENDIF}
  fHttp := THttpSend.Create;
  ///fHttp.UserAgent := DEFAULT_USER_AGENT;
  fHttp.UserAgent := format(DEFAULT_USER_AGENT, [RandomRange(100, 131),RandomRange(0, 120),RandomRange(0, 100)]);
  MovieID := AMovieID;
  ///fVideoBitRate := MaxInt;
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
  {$IFDEF MULTIDOWNLOADS}
  fLastPrepareTime := Now;
  {$ENDIF}
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
var
  Ext: string;
  MaxLength, i, n: integer;
begin
  Result := Trim(Name);
  Result := StrTr(Result, INVALID_FILENAME_CHARS, INVALID_FILENAME_CHARS_REPLACEMENTS);
  Ext := GetFileNameExt;
  Ext := StrTr(Ext, INVALID_FILENAME_CHARS, INVALID_FILENAME_CHARS_REPLACEMENTS);
  if AnsiCompareText(ExtractFileExt(Result), Ext) = 0 then
    Ext := '';
  // Limit the filename's length
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
  // Finalize the filename
  Result := Result + Ext;
  if Options.DownloadToProviderSubdirs then
    Result := IncludeTrailingPathDelimiter(Provider) + Result;
  if Options.DestinationPath <> '' then
    Result := IncludeTrailingPathDelimiter(Options.DestinationPath) + Result;
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

procedure TDownloader.ClearHttp(Http: THttpSend);
begin
  Http.Clear;
  if Referer <> '' then
    SetReferer(Http, Referer);
end;

procedure TDownloader.SetReferer(Http: THttpSend; const Referer: string);
const
  RefererHdr = 'Referer:';
var
  i: integer;
  Found: boolean;
begin
  if Referer <> '' then
    begin
    Found := False;
    for i := 0 to Pred(Http.Headers.Count) do
      if AnsiCompareText(RefererHdr, Copy(Http.Headers[i], 1, Length(RefererHdr))) = 0 then
        begin
        Http.Headers[i] := RefererHdr + ' ' + Referer;
        Found := True;
        Break;
        end;
    if not Found then
      Http.Headers.Add(RefererHdr + ' ' + Referer);
    end;
end;

function TDownloader.DownloadPage(Http: THttpSend; Url: string; Method: THttpMethod; Clear: boolean): boolean;
var MethodStr: string;
    Head: string;
begin
  repeat
    Url := Trim(Url);
    SetLastUrl(Url);
    if Clear then
      ClearHttp(Http);
    case Method of
      hmGET:  MethodStr := 'GET';
      hmPOST: MethodStr := 'POST';
      hmHEAD: MethodStr := 'HEAD';
    else      MethodStr := 'GET';
    end;
    {$ifdef debug}
    Head:=Http.Headers.Text;
    if Method=hmPost then
      Head:=ConvertString(Http.InputStream, peUTF8);
    {$endif}

    Result := Http.HttpMethod(MethodStr, Url);
    {$ifdef debug}
     if debug then
        uLog.Log('%s: %s'+EOLN+'UserAgent: %s'+EOLN+'Headers: %s', [MethodStr, Url, Http.UserAgent, Head]);
        uLog.Log('Response: %s'+EOLN+'Headers: %s', [copy(HtmlDecode(ConvertString(Http.Document, peUTF8)),1,1024),Http.Headers.Text]);
    {$endif}
    Method:=hmGET;
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
    ClearHttp(Http);
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
  Namespace: string;
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
      ClearHttp(Http);
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

function TDownloader.ValidatePrepare: boolean;
begin
  Result := False;
  if Prepared then
    if Trunc((Now - LastPrepareTime)*60*60*24) < PrepareLifeTime then
      Result := True
    else
      Result := Prepare;
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

function TDownloader.InternalValidateFileName(var FileName: string): boolean;
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
    Result := InternalValidateFileName(FN);
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
  HtmlDecodeItems: array[0..32] of THtmlDecodeItem
    = (
        (Html: '&mdash;'   ; Txt: '--'; DecodeOnly: true),
        (Html: '&lt;'      ; Txt: '<' ; DecodeOnly: false),
        (Html: '&gt;'      ; Txt: '>' ; DecodeOnly: false),
        (Html: '&quot;'    ; Txt: '"' ; DecodeOnly: false),
        (Html: '&apos;'    ; Txt: ''''; DecodeOnly: false),
        (Html: '&amp;'     ; Txt: '&' ; DecodeOnly: false),
        (Html: '&nbsp;'    ; Txt: ' ' ; DecodeOnly: false),
        (Html: '&Aacute;'  ; Txt: 'Á' ; DecodeOnly: true), 
        (Html: '&Egrave;'  ; Txt: 'È' ; DecodeOnly: true), 
        (Html: '&Iuml;'    ; Txt: 'Ï' ; DecodeOnly: true), 
        (Html: '&Eacute;'  ; Txt: 'É' ; DecodeOnly: true), 
        (Html: '&Igrave;'  ; Txt: 'Ì' ; DecodeOnly: true), 
        (Html: '&Iacute;'  ; Txt: 'Í' ; DecodeOnly: true), 
        (Html: '&Ograve;'  ; Txt: 'Ò' ; DecodeOnly: true), 
        (Html: '&Oacute;'  ; Txt: 'Ó' ; DecodeOnly: true), 
        (Html: '&Oslash;'  ; Txt: 'Ø' ; DecodeOnly: true), 
        (Html: '&Scaron;'  ; Txt: 'Š' ; DecodeOnly: true), 
        (Html: '&Uacute;'  ; Txt: 'Ú' ; DecodeOnly: true), 
        (Html: '&Ugrave;'  ; Txt: 'Ù' ; DecodeOnly: true), 
        (Html: '&Yacute;'  ; Txt: 'Ý' ; DecodeOnly: true), 
        (Html: '&aacute;'  ; Txt: 'á' ; DecodeOnly: true), 
        (Html: '&egrave;'  ; Txt: 'è' ; DecodeOnly: true), 
        (Html: '&iuml;'    ; Txt: 'ï' ; DecodeOnly: true), 
        (Html: '&eacute;'  ; Txt: 'é' ; DecodeOnly: true), 
        (Html: '&igrave;'  ; Txt: 'ì' ; DecodeOnly: true), 
        (Html: '&iacute;'  ; Txt: 'í' ; DecodeOnly: true), 
        (Html: '&ograve;'  ; Txt: 'ò' ; DecodeOnly: true), 
        (Html: '&oacute;'  ; Txt: 'ó' ; DecodeOnly: true), 
        (Html: '&oslash;'  ; Txt: 'ø' ; DecodeOnly: true), 
        (Html: '&scaron;'  ; Txt: 'š' ; DecodeOnly: true), 
        (Html: '&uacute;'  ; Txt: 'ú' ; DecodeOnly: true), 
        (Html: '&ugrave;'  ; Txt: 'ù' ; DecodeOnly: true), 
        (Html: '&yacute;'  ; Txt: 'ý' ; DecodeOnly: true)
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

function TDownloader.UrlDecode(const Text: string; Encoding: TPageEncoding): string;
var
  Decoded: AnsiString;
begin
  Decoded := DecodeUrl( {$IFDEF UNICODE} AnsiString {$ENDIF} (StringReplace(Text, '+', ' ', [rfReplaceAll])));
  Result := ConvertString(Decoded, Encoding);
end;

function TDownloader.UrlEncode(const Text: string): string;
begin
  Result := {$IFDEF UNICODE} string {$ENDIF} (EncodeUrl( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.ApplyIndexToName(const Name: string; Index, Count: integer): string;
const
  IndexOptFormats: array[TIndexForNames] of string = (
    '%0:s',
    '%1:s %0:s',
    '%0:s %1:s'
  );
var
  IndexOpt: TIndexForNames;
  n: integer;
begin
  IndexOpt := Options.AddIndexToNames;
  if IndexOpt <> ifnNone then
    if Count <= 0 then
      Result := Format(IndexOptFormats[IndexOpt], [Name, Format('[%03.3d]', [Index+1])])
    else
      begin
      n := Length(IntToStr(Count));
      Result := Format(IndexOptFormats[IndexOpt], [Name, Format(_('[%*.*d of %d]'), [n, n, Index+1, Count])]);
      end
  else
    Result := Name;
end;

function TDownloader.Base64Decode(const Text: string): string;
begin
  Result :=  {$IFDEF UNICODE} string {$ENDIF} (DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.Base64Encode(const Text: string): string;
begin
  Result :=  {$IFDEF UNICODE} string {$ENDIF} (EncodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Text)));
end;

function TDownloader.HexEncode(const Text: AnsiString): string;
const
  HexChar {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '0123456789abcdef';
var
  i, j, n: integer;
begin
  n := Length(Text);
  SetLength(Result, 2*n);
  j := 1;
  for i := 1 to n do
    begin
    Result[j] := HexChar[Succ(Byte(Text[i]) shr 4)];
    Result[Succ(j)] := HexChar[Succ(Byte(Text[i]) and $f)];
    Inc(j, 2);
    end;
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
      case Result[i+1] of
        'u':
          begin
            result:=AnsiReplaceStr(Result, '\u' + Copy(Result, i+2, 4), WideChar(StrToInt('$' + Copy(Result, i+2, 4))));
          end;
        'r':
          begin
            Result[i] := #13;
            System.Delete(Result, i+1, 1);
          end;
        'n':
          begin
            Result[i] := #10;
            System.Delete(Result, i+1, 1);
          end;
        't':
          begin
            Result[i] := #9;
            System.Delete(Result, i+1, 1);
          end;
        else
          System.Delete(Result, i, 1);
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
      Result := AnsiEncodedUtf8ToString(Text);
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
var
  Protocol, User, Password, Host, Port, Path, Paragraph, DefaultPort: string;
begin
  ParseUrl(Url, Protocol, User, Password, Host, Port, Path, Paragraph);
  Result := Protocol + '://';
  if User <> '' then
    if Password = '' then
      Result := Result + User + '@'
    else
      Result := Result + User + ':' + Password + '@';
  Result := Result + Host;
  ParseUrl(Result, Protocol, User, Password, Host, DefaultPort, Path, Paragraph);
  if Port <> DefaultPort then
    Result := Result + ':' + Port;
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
var
  Prot, User, Pass, Host, Port, Part, Para: string;
  i: integer;
begin
  ParseUrl(Url, Prot, User, Pass, Host, Port, Part, Para);
  i := Pos('?', Url);
  if i <= 0 then
    Result := Url
  else
    Result := Copy(Url, 1, Pred(i));
  for i := Length(Result) downto (Length(Prot)+Length(Host)+3) do
    if Result[i] = '/' then
      begin
      Result := Copy(Result, 1, i);
      Break;
      end;
end;

function TDownloader.GetRelativeUrl(CurrentUrl, Path: string): string;
var
  i: integer;
begin
  if Path = '' then
    Result := CurrentUrl
  else
    begin
    i := Pos('://', Path);
    if i > 1 then
      Result := Path
    else if Path[1] = '/' then
      if (Length(Path) >= 2) and (Path[2] = '/') then
        Result := Copy(CurrentUrl, 1, Pos('://', CurrentUrl)) + Path // two slashes are already at the beginning of Path
      else
        Result := ExtractUrlRoot(CurrentUrl) + Path
    else if Path[1] = '.' then
      begin
      CurrentUrl := ExtractUrlPath(CurrentUrl);
      while (Path <> '') and (Path[1] = '.') do
        begin
        Delete(Path, 1, 1);
        if Path <> '' then
          if Path[1] = '/' then
            Delete(Path, 1, 1)
          else if Path[1] = '.' then
            begin
            Delete(Path, 1, 1);
            if Path <> '' then
              if Path[1] = '/' then
                Delete(Path, 1, 1);
            if CurrentUrl[length(CurrentUrl)] = '/' then SetLength(CurrentUrl,Length(CurrentUrl)-1);
            CurrentUrl := ExtractUrlPath(CurrentUrl);
            end;
        end;
      Result := CurrentUrl + Path;
      end
    else
      Result := ExtractUrlPath(CurrentUrl) + Path;
    end;
end;

function TDownloader.GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean;
begin
  Result := RegExp.Match(Text) and RegExp.SubexpressionByNameEx(VarName, VarValue);
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
    if RegExp.SubexpressionByNameEx(VarName, Value) then
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
begin
  RegExp.Subject := {$IFDEF UNICODE} PCREString {$ENDIF} (Text);
  Result := GetRegExpVarsAgain(RegExp, VarNames, VarValues, InitValues);
end;

function TDownloader.GetRegExpVarsAgain(RegExp: TRegExp; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean): boolean;
var i: integer;
    VarValue: string;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  Result := RegExp.MatchAgain;
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
    Found: array of Boolean;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  SetLength(Found, Length(VarNames));
  for i := 0 to High(VarNames) do
    Found[i] := False;
  Result := RegExp.Match(Text);
  if Result then
    repeat
      VarName := RegExp.SubexpressionByName(VarNameSubExprName);
      for j := 0 to High(VarNames) do
        if VarName = VarNames[j] then
          if not Found[j] then
            begin
            Found[j] := True;
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
  if XmlNodeByPath(Xml, Path, Node) and XmlAttribute(Node, Attribute, VarValue) then
    begin
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TDownloader.GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean;
begin
  Result := GetXmlAttr(Xml.Root, Path, Attribute, VarValue);
end;

function TDownloader.GetXmlAttr(Xml: TXmlNode; const Path, AttributeName, AttributeValue, Attribute: string; out VarValue: string): boolean;
var Node: TXmlNode;
begin
  if XmlNodeByPathAndAttr(Xml, Path, AttributeName, AttributeValue, Node) and XmlAttribute(Node, Attribute, VarValue) then
    begin
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TDownloader.GetXmlAttr(Xml: TXmlDoc; const Path, AttributeName, AttributeValue, Attribute: string; out VarValue: string): boolean;
begin
  Result := GetXmlAttr(Xml.Root, Path, AttributeName, AttributeValue, Attribute, VarValue);
end;

function TDownloader.GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean;
begin
  Result := GetXmlVar(Xml.Root, Path, VarValue);
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

function TDownloader.Smil_FindBestVideo(Container: TXmlNode; out Url: string; const MaxBitrate: integer): boolean;
var
  sUrl, sBitrate: string;
  i, Bitrate, BestBitrate: integer;
begin
  Result := False;
  Url := '';
  BestBitrate := -1;
  for i := 0 to Pred(Container.NodeCount) do
    if Container.Nodes[i].Name = 'video' then
      if GetXmlAttr(Container.Nodes[i], '', 'src', sUrl) then
        begin
        if GetXmlAttr(Container.Nodes[i], '', 'system-bitrate', sBitrate) then
          Bitrate := StrToIntDef(sBitrate, 0)
        else
          Bitrate := 0;
        if (MaxBitrate <= 0) or (Bitrate <= MaxBitrate) then
          if Bitrate > BestBitrate then
            begin
            BestBitrate := Bitrate;
            Url := sUrl;
            Result := True;
            end;
        end;
  if not Result then
    if (MaxBitrate > 0) then
      Result := Smil_FindBestVideo(Container, Url, 0);
end;

function TDownloader.UnixTimestamp(DT: TDateTime): integer;
begin
  if DT = 0 then
    DT := Now;
  Result := Trunc((DT - 25569) * 24*60*60);
end;

function TDownloader.RegExp_FindBestVideo(const List: string; InfoRegExp: TRegExp; const UrlID, QualityID: string; out BestUrl: string): boolean;
var
  BestQuality, Quality: integer;
  Url, sQuality: string;
begin
  Result := False;
  BestQuality := -1;
  if GetRegExpVars(InfoRegExp, List, [UrlID, QualityID], [@Url, @sQuality]) then
    repeat
      if Url <> '' then
        begin
        Quality := StrToIntDef(sQuality, 0);
        if Quality > BestQuality then
          begin
          BestUrl := Url;
          BestQuality := Quality;
          Result := True;
          end;
        end;
    until not GetRegExpVarsAgain(InfoRegExp, [UrlID, QualityID], [@Url, @sQuality]);
end;

initialization
  UrlRegExps := TRegExpCache.Create;

finalization
  FreeAndNil(UrlRegExps);
  
end.
