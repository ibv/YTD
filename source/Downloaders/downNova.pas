(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit downNova;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_Nova,
    {$ELSE}
      guiOptionsVCL_Nova,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uNestedDownloader,
  uRtmpDownloader, uMSDownloader;

type
  TDownloader_Nova = class(TNestedDownloader)
    private
    protected
      LowQuality: boolean;
      SilverlightParamsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  TDownloader_Nova_RTMP = class(TRtmpDownloader)
    private
    protected
      MovieVariablesRegExp: TRegExp;
      LowQuality: boolean;
      FlvStream: string;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    protected
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

  TDownloader_Nova_MS = class(TMSDownloader)
    private
    protected
      LowQuality: boolean;
      SilverlightParamsRegExp: TRegExp;
      SilverlightVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    protected
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

const
  OPTION_NOVA_LOWQUALITY {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'low_quality';
  OPTION_NOVA_LOWQUALITY_DEFAULT = False;

implementation

uses
  uStringConsts,
  {$IFDEF DIRTYHACKS}
  uFiles,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://archiv.nova.cz/multimedia/ulice-1683-1684-dil.html
// http://voyo.nova.cz/home/plus-video/321-kriminalka-andel-podraz
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*(?<!tn\.)nova\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_H2_CLASS; // 'video_title'
  REGEXP_MS_INFO {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '<object\s[^>]*\btype="application/x-silverlight-2"(?:(?!</object\b).)*?<param\s+name=''initparams''\s+value=''(?P<INFO>identifier=.+?)''';
  REGEXP_MS_INFO_NAME {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'INFO';
  REGEXP_RTMP_VARIABLES = '\svar\s(?P<VARNAME>[a-z_][a-z0-9_]*)\s*=\s*(["'']?)(?P<VARVALUE>.*?)\2\s*;';
  REGEXP_MS_VARIABLES = '\s*(?P<VARNAME>[^=&]+)=(?P<VARVALUE>[^,]*)';

const
  NOVA_PROVIDER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'Nova.cz';
  NOVA_URLREGEXP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
  NOVA_MOVIE_INFO_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://voyo.nova.cz/%s';

{ TDownloader_Nova }

class function TDownloader_Nova.Provider: string;
begin
  Result := NOVA_PROVIDER;
end;

class function TDownloader_Nova.UrlRegExp: string;
begin
  Result := Format(NOVA_URLREGEXP, [MovieIDParamName]);
end;

{$IFDEF GUI}
class function TDownloader_Nova.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_Nova;
end;
{$ENDIF}

constructor TDownloader_Nova.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  SilverlightParamsRegExp := RegExCreate(REGEXP_MS_INFO);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
end;

destructor TDownloader_Nova.Destroy;
begin
  RegExFreeAndNil(SilverlightParamsRegExp);
  inherited;
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := Format(NOVA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_Nova.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
var
  Silverlight: string;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  if GetRegExpVar(SilverlightParamsRegExp, Page, REGEXP_MS_INFO_NAME, Silverlight) then
    Downloader := TDownloader_Nova_MS.Create(MovieID)
  else
    Downloader := TDownloader_Nova_RTMP.Create(MovieID);
  Result := True;
end;

procedure TDownloader_Nova.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  LowQuality := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT);
end;

{ TDownloader_Nova_RTMP }

class function TDownloader_Nova_RTMP.Provider: string;
begin
  Result := NOVA_PROVIDER;
end;

class function TDownloader_Nova_RTMP.UrlRegExp: string;
begin
  Result := Format(NOVA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_Nova_RTMP.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieVariablesRegExp := RegExCreate(REGEXP_RTMP_VARIABLES);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
end;

destructor TDownloader_Nova_RTMP.Destroy;
begin
  RegExFreeAndNil(MovieVariablesRegExp);
  inherited;
end;

function TDownloader_Nova_RTMP.GetFileNameExt: string;
begin
  if LowQuality then
    Result := '.flv'
  else
    Result := '.mp4';
end;

function TDownloader_Nova_RTMP.GetMovieInfoUrl: string;
begin
  Result := Format(NOVA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_Nova_RTMP.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
    Name, Value: string;
    MediaID, SiteID, SectionID, SessionID, UserAdID: string;
    ServersUrl, VideosUrl: string;
    FlvServer, FlvName: string;
    Servers, Videos: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  FlvStream := '';
  SessionID := '';
  UserAdID := '';
  GetRegExpVarPairs(MovieVariablesRegExp, Page, ['media_id', 'site_id', 'section_id'], [@MediaID, @SiteID, @SectionID]);
  for i := 0 to Pred(Http.Cookies.Count) do
    begin
    Name := Http.Cookies.Names[i];
    Value := Http.Cookies.Values[Name];
    if AnsiCompareText(Name, 'bit') = 0 then
      UserAdID := Value
    else if AnsiCompareText(Name, 'c4d') = 0 then
      SessionID := Value;
    end;
  Http.Cookies.Values['bit'] := UserAdID;
  if SiteID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['SiteID']))
  else if SectionID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['SectionID']))
  else if MediaID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['MediaID']))
  else if SessionID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['SessionID']))
  //else if UserAdID = '' then
  //  SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['UserAdID']))
  else
    begin
    ServersUrl := 'http://tn.nova.cz/bin/player/config.php?site_id=' + SiteID + '&';
    VideosUrl := 'http://tn.nova.cz/bin/player/serve.php' +
                 '?site_id=' + SiteID +
                 '&media_id=' + MediaID +
                 '&userad_id=' + UserAdID +
                 '&section_id=' + SectionID +
                 '&noad_count=0' +
                 '&fv=WIN 10,0,45,2' +
                 '&session_id=' + SessionID +
                 '&ad_file=noad';
    if not DownloadXml(Http, ServersUrl, Servers) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST)
    else
      try
        if not DownloadXml(Http, VideosUrl, Videos) then
          SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
        else
          try
            if Videos.Root.Name = 'error' then
              SetLastErrorMsg(ERR_MEDIA_REMOVED)
            else
              begin
              GetXmlAttr(Servers, 'flvserver', 'url', FlvServer);
              GetXmlAttr(Videos, 'item', 'src', FlvStream);
              GetXmlAttr(Videos, 'item', 'txt', FlvName);
              if FlvName = '' then
                FlvName := MediaID;
              if FlvServer = '' then
                SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
              else if FlvStream = '' then
                SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
              else
                begin
                SetName(FlvName);
                if LowQuality then
                  Self.PlayPath := 'flv:' + FlvStream
                else
                  Self.PlayPath := 'mp4:' + FlvStream;
                MovieUrl := FlvServer + '/' + PlayPath;
                Self.RtmpUrl := FlvServer;
                //AddRtmpDumpOption('f', FLASH_DEFAULT_VERSION);
                //AddRtmpDumpOption('W', 'http://voyo.nova.cz/static/shared/app/flowplayer/13-flowplayer.commercial-3.1.5-17-002.swf');
                //AddRtmpDumpOption('t', FlvServer);
                Result := True;
                SetPrepared(True);
                end;
              end;
          finally
            Videos.Free;
            end;
      finally
        Servers.Free;
        end;
    end;
end;

function TDownloader_Nova_RTMP.Download: boolean;
{$IFDEF DIRTYHACKS}
var
  TryLowQuality: boolean;
{$ENDIF}
begin
  Result := inherited Download;
  {$IFDEF DIRTYHACKS}
  TryLowQuality := False;
  if not LowQuality then
    begin
    if Result then
      if FileExists(FileName) then
        if FileGetSize(Filename) = 0 then
          TryLowQuality := True;
    if not Result then
      TryLowQuality := True;
    end;
  if TryLowQuality then
    begin
    SysUtils.DeleteFile(FileName);
    Self.Playpath := 'flv:' + FlvStream;
    Result := inherited Download;
    end;
  {$ENDIF}
end;

procedure TDownloader_Nova_RTMP.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  LowQuality := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT);
end;

{ TDownloader_Nova_MS }

class function TDownloader_Nova_MS.Provider: string;
begin
  Result := NOVA_PROVIDER;
end;

class function TDownloader_Nova_MS.UrlRegExp: string;
begin
  Result := Format(NOVA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_Nova_MS.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(Format(REGEXP_MOVIE_TITLE, ['video_title']));
  SilverlightParamsRegExp := RegExCreate(REGEXP_MS_INFO);
  SilverlightVarsRegExp := RegExCreate(REGEXP_MS_VARIABLES);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
end;

destructor TDownloader_Nova_MS.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(SilverlightParamsRegExp);
  RegExFreeAndNil(SilverlightVarsRegExp);
  inherited;
end;

function TDownloader_Nova_MS.GetMovieInfoUrl: string;
begin
  Result := Format(NOVA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_Nova_MS.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  NAMESPACE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://streaming.kitd.cz/cdn/nova';
  SOAP_ACTION = '"http://streaming.kitd.cz/cdn/nova/GetContentUrl"';
  QUALITY_STR: array[boolean] of WideString = ('LQ', 'HQ');
var
  Silverlight, ID, Token, Url: string;
  Request, Response: TXmlDoc;
  ResponseHeader, ResponseBody: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(SilverlightParamsRegExp, Page, REGEXP_MS_INFO_NAME, Silverlight) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetRegExpVarPairs(SilverlightVarsRegExp, Silverlight, ['identifier', 'token'], [@ID, @Token]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else
    begin
    Request := TXmlDoc.CreateName('GetContentUrl');
    try
      Request.Root.AttributeByName['xmlns'] := NAMESPACE;
      Request.Root.AttributeByName['xmlns:i'] := 'http://www.w3.org/2001/XMLSchema-instance';
      Request.Root.NodeNew('token').ValueAsUnicodeString := WideString(Token);
      Request.Root.NodeNew('id').ValueAsUnicodeString := WideString(ID);
      Request.Root.NodeNew('type').ValueAsUnicodeString := 'Archive';
      Request.Root.NodeNew('format').ValueAsUnicodeString := QUALITY_STR[not LowQuality];
      if not DownloadSoap(Http, 'http://fcdn-upload.visual.cz/Services.Test/Player.asmx', SOAP_ACTION, nil, Request.Root, Response, ResponseHeader, ResponseBody) then
        SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
      else
        try
          if ResponseBody.NodeCount <= 0 then
            SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
          else if not GetXmlVar(ResponseBody, 'GetContentUrlResponse/GetContentUrlResult', Url) then
            SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
          else
            begin
            MovieUrl := Url;
            SetPrepared(True);
            Result := True;
            end;
        finally
          FreeAndNil(Response);
          ResponseHeader := nil;
          ResponseBody := nil;
          end;
    finally
      Request.Free;
      end;
    end;
end;

procedure TDownloader_Nova_MS.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  LowQuality := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT);
end;

initialization
  RegisterDownloader(TDownloader_Nova);
  //RegisterDownloader(TDownloader_Nova_RTMP);
  //RegisterDownloader(TDownloader_Nova_MS);

end.
