(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2199 ibv
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

unit downPrima;
{$INCLUDE 'ytd.inc'}
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format


interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
//      guiOptionsWINAPI_CT,
    {$ELSE}
      guiOptionsLCL_Prima,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uHLSDownloader;

type
  TDownloader_Prima = class(THLSDownloader)
    private
      {$IFDEF MULTIDOWNLOADS}
      fNameList: TStringList;
      fUrlList: TStringList;
      fDownloadIndex: integer;
      {$ENDIF}
    protected
      StreamUrlRegExp: TRegExp;
      StreamTitleRegExp: TRegExp;
      ProductID1: TRegExp;
      ProductID2: TRegExp;
      StatusOK: TRegExp;
      Options: TRegExp;
      HLS : TRegExp;
      {$IFDEF MULTIDOWNLOADS}
      property NameList: TStringList read fNameList;
      property UrlList: TStringList read fUrlList;
      property DownloadIndex: integer read fDownloadIndex write fDownloadIndex;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetPlaylistInfo(Http: THttpSend; const Page: string; out Title: string): boolean;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF MULTIDOWNLOADS}
      function Prepare: boolean; override;
      function ValidatePrepare: boolean; override;
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;


const
  OPTION_Prima_MAXBITRATE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'max_video_width';
  OPTION_Prima_MAXBITRATE_DEFAULT = 0;

implementation

uses
  uStringConsts,
  uStrings,
  uDownloadClassifier,
  uFunctions,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?:iprima)\.cz/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PRODUCTID1 = 'prehravac/embedded\?id=(?P<ID>.+?)"';
  REGEXP_PRODUCTID2 = '_videoimport(?P<ID>\w+?)_';
  REGEXP_STATUS_CONTENT   = '^(?P<CONTENT>OK)$';
  REGEXP_OPTIONS_CONTENT  = '\bvar\s+\w*[pP]layerOptions\s*=\s*(?P<CONTENT>\{.*?\})\s*;';
  REGEXP_HLS_CONTENT      = 'tracks\s*:\s*\{\s*HLS\s*:\s*\[(?P<CONTENT>.+?)\]\s*,';
  REGEXP_STREAM_URL = 'src\s*:\s*''(?P<URL>https?://.+?)''';
  REGEXP_STREAM_TITLE = '<meta\s+(name|property)=(?P<QUOTE1>[''"])og:title(?P=QUOTE1)\s+(content|value)=(?P<QUOTE2>[''"])(?P<NAME>.*?)(?P=QUOTE2)';

class function TDownloader_Prima.Provider: string;
begin
  Result := 'iPrima.cz';
end;

class function TDownloader_Prima.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_Prima.Features: TDownloaderFeatures;
begin
  Result := inherited Features;
end;


{$IFDEF GUI}
class function TDownloader_Prima.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_Prima;
end;
{$ENDIF}


constructor TDownloader_Prima.Create(const AMovieID: string);
begin
  inherited;
  {$IFDEF MULTIDOWNLOADS}
  fNameList := TStringList.Create;
  fUrlList := TStringList.Create;
  {$ENDIF}
  StreamUrlRegExp := RegExCreate(REGEXP_STREAM_URL);
  StreamTitleRegExp := RegExCreate(REGEXP_STREAM_TITLE);

  ProductID1 := RegExCreate(REGEXP_PRODUCTID1);
  ProductID2 := RegExCreate(REGEXP_PRODUCTID2);
  StatusOK   := RegExCreate(REGEXP_STATUS_CONTENT);
  Options    := RegExCreate(REGEXP_OPTIONS_CONTENT);
  HLS        := RegExCreate(REGEXP_HLS_CONTENT);

  Referer := GetMovieInfoUrl;
end;

destructor TDownloader_Prima.Destroy;
begin
  RegExFreeAndNil(StreamUrlRegExp);
  RegExFreeAndNil(StreamTitleRegExp);
  RegExFreeAndNil(ProductID1);
  RegExFreeAndNil(ProductID2);
  RegExFreeAndNil(StatusOK);
  RegExFreeAndNil(Options);
  RegExFreeAndNil(HLS);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fNameList);
  FreeAndNil(fUrlList);
  {$ENDIF}
  inherited;
end;

function TDownloader_Prima.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Prima.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Prot, User, Pass, Host, Port, Part, Para: string;
  PlaylistUrl, Title, ID1,ID2, infoxml, opt: string;
  Xml: TXmlDoc;
  {$IFDEF MULTIDOWNLOADS}
  Urls: TStringArray;
  i: integer;
  {$ELSE}
  Url: string;
  {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;

  ParseUrl(GetMovieInfoUrl, Prot, User, Pass, Host, Port, Part, Para);
  if not GetPlaylistInfo(Http, Page, Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(ProductID1, Page, 'ID', ID1) then
    else if not GetRegExpVar(ProductID2, Page, 'ID', ID2) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE);
  if ID1='' then
    if ID2<>'' then ID1:=ID2
      else SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE);
  if not DownloadXML(Http,'http://play.iprima.cz/prehravac/init?_infuse=1&_ts=1467795020741&productId='+ID1,xml)
  then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE);
  infoxml:=Xml.ValueByPath('status');
  if infoxml<>'OK' then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE);
  infoxml:=Xml.ValueByPath('script');
  if not GetRegExpVar(Options, infoxml, 'CONTENT', opt) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(HLS, opt, 'CONTENT', PlayListUrl) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  {$IFDEF MULTIDOWNLOADS}
  else if not GetRegExpAllVar(StreamUrlRegExp, PlaylistUrl, 'URL', Urls) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  {$ELSE}
  else if not GetRegExpVar(StreamUrlRegExp, PlaylistUrl, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  {$ENDIF}
  else
    begin
    if Title = '' then
      if not GetRegExpVar(StreamTitleRegExp, Page, 'NAME', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE);
    Title := trim(AnsiEncodedUtf8ToString( {$IFDEF UNICODE} AnsiString {$ENDIF} (JSDecode(Title))));
    {$IFDEF MULTIDOWNLOADS}
    for i := 0 to Pred(Length(Urls)) do
      begin
      UrlList.Add(JSDecode(Urls[i]));
      if Length(Urls) > 1 then
        NameList.Add(Format('%s [%d]', [Title, Succ(i)]))
      else
        NameList.Add(Title);
      end;
    Name := NameList[0];
    {$ENDIF}
    Name := Title;
    MovieURL := {$IFDEF MULTIDOWNLOADS} JSDecode(Urls[0]) {$ELSE} Url {$ENDIF};
    SetPrepared(True);
    Result := True;
    end;
end;



function TDownloader_Prima.GetFileNameExt: string;
begin
  Result := '.ts';
end;

{function TDownloader_Prima.GetPlaylistInfo(Http: THttpSend; const Page: string; out PlaylistType, PlaylistID: string): boolean;
var
  Url, Page2: string;
begin
  Result := GetRegExpVars(PlaylistInfoRegExp, Page, ['TYP', 'ID'], [@PlaylistType, @PlaylistID]);
  if not Result then
    if GetRegExpVar(IFrameUrlRegExp, Page, 'URL', Url) then
      if DownloadPage(Http, GetRelativeUrl(GetMovieInfoUrl, Url), Page2, peUtf8) then
        Result := GetRegExpVars(PlaylistInfoRegExp, Page2, ['TYP', 'ID'], [@PlaylistType, @PlaylistID]);
end;}

function TDownloader_Prima.GetPlaylistInfo(Http: THttpSend; const Page: string; out Title: string): boolean;
var
  Url, Page2: string;
begin
  Result := GetRegExpVars(StreamTitleRegExp, Page, ['NAME'], [@Title]);
end;

{$IFDEF MULTIDOWNLOADS}

function TDownloader_Prima.Prepare: boolean;
begin
  NameList.Clear;
  UrlList.Clear;
  DownloadIndex := 0;
  Result := inherited Prepare;
end;

function TDownloader_Prima.ValidatePrepare: boolean;
var
  DownIndex: integer;
begin
  DownIndex := DownloadIndex;
  try
    Result := inherited ValidatePrepare;
  finally
    DownloadIndex := DownIndex;
    end;
end;

function TDownloader_Prima.First: boolean;
begin
  if ValidatePrepare then
    if UrlList.Count <= 0 then
      Result := MovieURL <> ''
    else
      begin
      DownloadIndex := -1;
      Result := Next;
      end
  else
    Result := False;
end;

function TDownloader_Prima.Next: boolean;
begin
  Result := False;
  if ValidatePrepare then
    begin
    DownloadIndex := Succ(DownloadIndex);
    if (DownloadIndex >= 0) and (DownloadIndex < UrlList.Count) then
      begin
      Name := NameList[DownloadIndex];
      SetFileName('');
      MovieURL := UrlList[DownloadIndex];
      Result := True;
      end;
    end;
end;

{$ENDIF}


procedure TDownloader_Prima.SetOptions(const Value: TYTDOptions);
var
  Bitrate: integer;
begin
  inherited;
  Bitrate := Value.ReadProviderOptionDef(Provider, OPTION_Prima_MAXBITRATE, OPTION_Prima_MAXBITRATE_DEFAULT);
  case Bitrate of
       512:  MaxVBitRate:=540672;
       640:  MaxVBitRate:=950272;
       768:  MaxVBitRate:=1155072;
      1024:  MaxVBitRate:=1667072;
      1280:  MaxVBitRate:=2310144;
      1920:  MaxVBitRate:=3334144;
      else   MaxVBitRate:=MaxInt;
  end;
end;


initialization
  RegisterDownloader(TDownloader_Prima);

end.

