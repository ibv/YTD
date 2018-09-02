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

unit downCT;
{$INCLUDE 'ytd.inc'}
{$DEFINE VER_OLD}
{$DEFINE VER_20111217}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_CT,
    {$ELSE}
      guiOptionsVCL_CT,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uNestedDownloader, uRtmpDownloader;

type
  TDownloader_CT = class(TNestedDownloader)
    private
    protected
      {$IFDEF VER_20111217}
      NewFlashVarsRegExp: TRegExp;
      NewFlashVarsItemsRegExp: TRegExp;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
      function PrepareToken: boolean; override;
    public
      class function Features: TDownloaderFeatures; override;
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  {$IFDEF VER_20111217}
  TDownloader_CT_20111217 = class(TRtmpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarsItemsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;
  {$ENDIF}

  {$IFDEF VER_OLD}
  TDownloader_CT_old = class(TRtmpDownloader)
    private
      {$IFDEF MULTIDOWNLOADS}
      fBaseUrls: TStringList;
      fStreams: TStringList;
      fDownloadIndex: integer;
      fBaseName: string;
      {$ENDIF}
    protected
      MovieObjectRegExp: TRegExp;
      EmbeddedFrameRegExp: TRegExp;
      JavascriptPlayerRegExp: TRegExp;
      VideoPlayerUrlRegExp: TRegExp;
      Extension: string;
      MaxBitrate: integer;
    protected
      procedure SetRtmpOptions(const BaseUrl, Stream: string); virtual;
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function GetMovieObject(Http: THttpSend; var Page: string; out MovieObject: string): boolean;
      function ConvertMovieObject(var Data: string): boolean;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$IFDEF MULTIDOWNLOADS}
      property BaseUrls: TStringList read fBaseUrls;
      property Streams: TStringList read fStreams;
      property DownloadIndex: integer read fDownloadIndex write fDownloadIndex;
      property BaseName: string read fBaseName write fBaseName;
      {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;
  {$ENDIF}

const
  OPTION_CT_MAXBITRATE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'max_bitrate';
  OPTION_CT_MAXBITRATE_DEFAULT = 0;

implementation

uses
  {$IFDEF VER_OLD}
  uStringConsts,
  uJSON, uLkJSON,
  SynaUtil,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        '^https?://(?:[a-z0-9-]+\.)*(?:ceskatelevize|ct24)\.cz/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)(?:\s*&mdash;\s*iVysílání)?(?:\s*&mdash;\s*Ceská televize)?\s*</title>';
  {$IFDEF VER_20111217}
  REGEXP_NEW_FLASHVARS = REGEXP_FLASHVARS;
  REGEXP_NEW_FLASHVARS_ITEMS = REGEXP_PARSER_HTMLVARS;
  VARNAME_NEW_SERVER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'streamer';
  VARNAME_NEW_STREAM {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'file';
  {$ENDIF}
  {$IFDEF VER_OLD}
  REGEXP_MOVIE_OBJECT = '\bcallSOAP\s*\(\s*(?P<OBJECT>\{.*?\})\s*\)\s*;';
  REGEXP_MOVIE_FRAME = '<iframe\s+[^>]*\bsrc="(?P<HOST>https?://[^"/]+)?(?P<PATH>/(?:ivysilani|embed)/.+?)"';
  REGEXP_JS_PLAYER = '<a\s+(?:\w+="[^"]*"\s+)*\bhref="javascript:void\s*\(\s*q\s*=\s*''(?P<PARAM>[^'']+)''\s*\)"\s+(?:id|target)="videoPlayer_';
  REGEXP_VIDEOPLAYERURL = '"videoPlayerUrl"\s*:\s*"(?P<URL>(?:https?:|\\?/).+?)"';
  CUSTOM_HEADERS: array[0..0] of string = (
    'X-addr: 127.0.0.1'
    );
  {$ENDIF}

class function TDownloader_CT.Provider: string;
begin
  Result := 'CeskaTelevize.cz';
end;

class function TDownloader_CT.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);
end;

class function TDownloader_CT.Features: TDownloaderFeatures;
begin
  Result := inherited Features
    {$IFDEF VER_OLD} + TDownloader_CT_old.Features {$ENDIF}
    {$IFDEF VER_20111217} + TDownloader_CT_20111217.Features {$ENDIF}
    ;
end;

constructor TDownloader_CT.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  {$IFDEF VER_20111217}
  NewFlashVarsRegExp := RegExCreate(REGEXP_NEW_FLASHVARS);
  NewFlashVarsItemsRegExp := RegExCreate(REGEXP_NEW_FLASHVARS_ITEMS);
  {$ENDIF}
end;

destructor TDownloader_CT.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  {$IFDEF VER_20111217}
  RegExFreeAndNil(NewFlashVarsRegExp);
  RegExFreeAndNil(NewFlashVarsItemsRegExp);
  {$ENDIF}
  inherited;
end;

{$IFDEF GUI}
class function TDownloader_CT.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_CT;
end;
{$ENDIF}

function TDownloader_CT.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CT.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
{$IFDEF VER_20111217}
var
  FlashVars, Server, Stream: string;
{$ENDIF}
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  Result := False;
  {$IFDEF VER_20111217}
  if not Result then
    if GetRegExpVar(NewFlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
      if GetRegExpVarPairs(NewFlashVarsItemsRegExp, FlashVars, [VARNAME_NEW_SERVER, VARNAME_NEW_STREAM], [@Server, @Stream]) then
        if (Server <> '') and (Stream <> '') then
          begin
          Downloader := TDownloader_CT_20111217.Create(MovieID);
          Result := True;
          end;
  {$ENDIF}
  {$IFDEF VER_OLD}
  if not Result then
    begin
    Downloader := TDownloader_CT_old.Create(MovieID);
    Result := True;
    end;
  {$ENDIF}
end;

{$IFDEF VER_OLD}

class function TDownloader_CT_old.Provider: string;
begin
  Result := TDownloader_CT.Provider;
end;

class function TDownloader_CT_old.UrlRegExp: string;
begin
  Result := TDownloader_CT.UrlRegExp;
end;

class function TDownloader_CT_old.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

constructor TDownloader_CT_old.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieObjectRegExp := RegExCreate(REGEXP_MOVIE_OBJECT);
  EmbeddedFrameRegExp := RegExCreate(REGEXP_MOVIE_FRAME);
  JavascriptPlayerRegExp := RegExCreate(REGEXP_JS_PLAYER);
  VideoPlayerUrlRegExp := RegExCreate(REGEXP_VIDEOPLAYERURL);
  {$IFDEF MULTIDOWNLOADS}
  fStreams := TStringList.Create;
  fBaseUrls := TStringList.Create;
  {$ENDIF}
end;

destructor TDownloader_CT_old.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieObjectRegExp);
  RegExFreeAndNil(EmbeddedFrameRegExp);
  RegExFreeAndNil(JavascriptPlayerRegExp);
  RegExFreeAndNil(VideoPlayerUrlRegExp);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fStreams);
  FreeAndNil(fBaseUrls);
  {$ENDIF}
  inherited;
end;

function TDownloader_CT_old.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_CT_old.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CT_old.GetMovieObject(Http: THttpSend; var Page: string; out MovieObject: string): boolean;
var Host, Path, Url, NewPage, Param: string;
begin
  Result := GetRegExpVar(MovieObjectRegExp, Page, 'OBJECT', MovieObject);
  if not Result then
    begin
    Url := '';
    if GetRegExpVars(EmbeddedFrameRegExp, Page, ['HOST', 'PATH'], [@Host, @Path]) then
      begin
      if Host = '' then
        Host := ExtractUrlRoot(MovieID);
      Path := HtmlDecode(Path);
      Path := StringReplace(Path, '&autoStart=false', '', [rfReplaceAll]);
      Path := StringReplace(Path, ' ', '%20', [rfReplaceAll]);
      Url := Host + Path;
        // Nepouzivat UrlEncode, cesty uz jsou obvykle UrlEncoded
      end
    else if GetRegExpVar(JavascriptPlayerRegExp, Page, 'PARAM', Param) then
      if DownloadPage(Http, 'http://www.ceskatelevize.cz/ct24/ajax/', 'cmd=getVideoPlayerUrl&q=' + {$IFDEF UNICODE} AnsiString {$ENDIF} (UrlEncode(PARAM)), HTTP_FORM_URLENCODING, CUSTOM_HEADERS, NewPage) then
        if GetRegExpVar(VideoPlayerUrlRegExp, NewPage, 'URL', Url) then
          Url := StripSlashes(Url);
    if Url <> '' then
      begin
      if Url[1] = '/' then
        Url := 'http://www.ceskatelevize.cz' + Url;
      if DownloadPage(Http, Url, NewPage, InfoPageEncoding) then
        if GetRegExpVar(MovieObjectRegExp, NewPage, 'OBJECT', MovieObject) then
          begin
          Page := NewPage;
          Result := True;
          end;
      end;
    end;
end;

function TDownloader_CT_old.ConvertMovieObject(var Data: string): boolean;

  function SaveJSON(JSON: TJSON; var Res: string; const Path: string): boolean;
    var
      Value, NewPath: string;
      i: integer;
    begin
      if JSON = nil then
        Result := False
      else
        begin
        Result := True;
        if JSON is TlkJSONobject then
          begin
          for i := 0 to Pred(JSON.Count) do
            if not SaveJSON(JSON.Child[i], Res, Path) then
              begin
              Result := False;
              Break;
              end;
          end
        else if JSON is TlkJSONobjectmethod then
          begin
          if Path = '' then
            NewPath := TlkJSONobjectmethod(JSON).Name
          else
            NewPath := Format('%s[%s]', [Path, TlkJSONobjectmethod(JSON).Name]);
          Result := SaveJSON(TlkJSONobjectmethod(JSON).ObjValue, Res, NewPath);
          end
        else if JSON is TlkJSONcustomlist then
          begin
          for i := 0 to Pred(JSON.Count) do
            if not SaveJSON(JSON.Child[i], Res, Format('%s[%d]', [Path, i])) then
              begin
              Result := False;
              Break;
              end;
          end
        else
          begin
          if JSON.Value = null then
            Value := ''
          else
            Value := JSON.Value;
          Value := UrlEncode(Path) + '=' + UrlEncode(Value);
          if Res = '' then
            Res := Value
          else
            Res := Res + '&' + Value;
          end;
        end;
    end;

var JSON: TJSON;
    Res: string;
begin
  Result := False;
  JSON := JSONCreate(Data);
  if JSON <> nil then
    begin
    Res := '';
    Result := SaveJSON(JSON, Res, '');
    if Result then
      Data := Res;
    end;
end;

function TDownloader_CT_old.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const REKLAMA = '-AD-';
      REKLAMA_LENGTH = Length(REKLAMA);
var MovieObject, Url, ID, BaseUrl, Stream: string;
    Xml: TXmlDoc;
    Body, Node: TXmlNode;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetMovieObject(Http, Page, MovieObject) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else if not ConvertMovieObject(MovieObject) then
    SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, 'http://www.ceskatelevize.cz/ajax/playlistURL.php', AnsiString(MovieObject), HTTP_FORM_URLENCODING_UTF8, CUSTOM_HEADERS, Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if Copy(Url, 1, 4) <> 'http' then
    SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Url]))
  else if not DownloadXml(Http, Url, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if Xml.NodeByPath('smilRoot/body', Body) then
        for i := 0 to Pred(Body.NodeCount) do
          if Body.Nodes[i].Name = 'switchItem' then
            begin
            Node := Body.Nodes[i];
            if GetXmlAttr(Node, '', 'id', ID) then
              if Pos(REKLAMA, ID) <= 0 then
                if GetXmlAttr(Node, '', 'base', BaseUrl) then
                  begin
                  if not Smil_FindBestVideo(Node, Stream, MaxBitrate) then
                    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
                  else
                    begin
                    SetRtmpOptions(BaseUrl, Stream);
                    {$IFDEF MULTIDOWNLOADS}
                    BaseUrls.Add(BaseUrl);
                    Streams.Add(Stream);
                    {$ENDIF}
                    SetPrepared(True);
                    Result := True;
                    end;
                  {$IFNDEF MULTIDOWNLOADS}
                  Break;
                  {$ENDIF}
                  end;
            end;
    finally
      Xml.Free;
      end;
end;

procedure TDownloader_CT_old.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  MaxBitrate := Value.ReadProviderOptionDef(Provider, OPTION_CT_MAXBITRATE, OPTION_CT_MAXBITRATE_DEFAULT);
end;

{$IFDEF MULTIDOWNLOADS}
function TDownloader_CT_old.First: boolean;
begin
  if ValidatePrepare then
    if BaseUrls.Count <= 0 then
      Result := MovieURL <> ''
    else
      begin
      DownloadIndex := -1;
      BaseName := Name;
      Result := Next;
      end
  else
    Result := False;
end;

function TDownloader_CT_old.Next: boolean;
begin
  Result := False;
  if ValidatePrepare then
    begin
    DownloadIndex := Succ(DownloadIndex);
    if (DownloadIndex >= 0) and (DownloadIndex < BaseUrls.Count) and (DownloadIndex < Streams.Count) then
      begin
      SetName(Format('%s (%d)', [BaseName, Succ(DownloadIndex)]));
      SetFileName('');
      SetRtmpOptions(BaseUrls[DownloadIndex], Streams[DownloadIndex]);
      Result := True;
      end;
    end;
end;
{$ENDIF}

procedure TDownloader_CT_old.SetRtmpOptions(const BaseUrl, Stream: string);
var Protocol, User, Password, Host, Port, Path, Para: string;
begin
  MovieURL := BaseUrl + '/' + Stream;
  Extension := ExtractUrlExt(Stream);
  ParseUrl(BaseUrl, Protocol, User, Password, Host, Port, Path, Para);
  Self.RtmpUrl := BaseUrl;
  Self.RtmpApp := Copy(Path, 2, MaxInt) + '?' + Para;
  Self.Playpath := Stream;
  //Self.FlashVer := FLASH_DEFAULT_VERSION;
  //Self.SwfUrl := 'http://img2.ceskatelevize.cz/libraries/player/flashPlayer.swf?version=1.4.23';
  //Self.TcUrl := BaseUrl;
  //Self.PageUrl := MovieID;
end;

{$ENDIF}

{ TDownloader_CT_20111217 }

class function TDownloader_CT_20111217.Provider: string;
begin
  Result := TDownloader_CT.Provider;
end;

class function TDownloader_CT_20111217.UrlRegExp: string;
begin
  Result := TDownloader_CT.UrlRegExp;
end;

class function TDownloader_CT_20111217.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream, dfRequireSecureToken];
end;

constructor TDownloader_CT_20111217.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_NEW_FLASHVARS);
  FlashVarsItemsRegExp := RegExCreate(REGEXP_NEW_FLASHVARS_ITEMS);
end;

destructor TDownloader_CT_20111217.Destroy;
begin
  inherited;
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarsItemsRegExp);
end;

function TDownloader_CT_20111217.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_CT_20111217.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  FlashVars, Server, Stream: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else if not GetRegExpVarPairs(FlashVarsItemsRegExp, FlashVars, [VARNAME_NEW_SERVER, VARNAME_NEW_STREAM], [@Server, @Stream]) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else if Server = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, [VARNAME_NEW_SERVER]))
  else if Stream = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, [VARNAME_NEW_STREAM]))
  else
    begin
    {$IFDEF DIRTYHACKS}
    // Due to a bug in HTTP implementation of RTMPDUMP, the HTTP tunnel does
    // not work. Details are here:
    // http://lists.mplayerhq.hu/pipermail/rtmpdump/2011-December/001783.html
    if UpperCase(Copy(Server, 1, 5)) = 'RTMPT' then
      System.Delete(Server, 5, 1);
    {$ENDIF}
    Self.RtmpUrl := Server;
    Self.PlayPath := 'mp4:' + Stream;
    //Self.FlashVer := FLASH_DEFAULT_VERSION;
    //Self.SwfUrl := 'http://img9.ceskatelevize.cz/libraries/JWPlayer/player.swf';
    //Self.SwfVfy := Self.SwfUrl;
    //Self.TcUrl := Server;
    //Self.PageUrl := GetMovieInfoUrl;
    MovieUrl := Self.RtmpUrl + '/' + Self.PlayPath;
    SetPrepared(True);
    Result := True;
    end;
end;

function TDownloader_CT.PrepareToken: boolean;
begin
  inherited PrepareToken;
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_CT);

end.
