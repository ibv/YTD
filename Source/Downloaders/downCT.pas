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
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format


interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
//      guiOptionsWINAPI_CT,
    {$ELSE}
      guiOptionsLCL_CT,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uHLSDownloader;

type
  TDownloader_CT = class(THLSDownloader)
    private
      {$IFDEF MULTIDOWNLOADS}
      fNameList: TStringList;
      fUrlList: TStringList;
      fDownloadIndex: integer;
      {$ENDIF}
    protected
      PlaylistInfoRegExp: TRegExp;
      PlaylistUrlRegExp: TRegExp;
      StreamUrlRegExp: TRegExp;
      StreamUrlRegExpNew: TRegExp;
      StreamUrlRegVodUrl: TRegExp;
      StreamUrlRegVodUrlMPD: TRegExp;
      StreamUrlRegVodTitle: TRegExp;
      StreamTitleRegExp: TRegExp;
      StreamTitle2RegExp: TRegExp;
      StreamTitleFromPageRegExp: TRegExp;
      IFrameUrlRegExp: TRegExp;
      UrlToken: TregExp;
      IDEC: TregExp;
      BID:  TRegExp;
      AID:  TRegExp;
      {$IFDEF MULTIDOWNLOADS}
      property NameList: TStringList read fNameList;
      property UrlList: TStringList read fUrlList;
      property DownloadIndex: integer read fDownloadIndex write fDownloadIndex;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetPlaylistInfo(Http: THttpSend; const Page: string; out PlaylistType, PlaylistID: string): boolean;
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
  OPTION_CT_MAX_VIDEO_WIDTH {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'max_video_width';
  OPTION_CT_MAX_VIDEO_WIDTH_DEFAULT = 0;
  OPTION_CT_DASH_SUPPORT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'dash_support';

implementation

uses
  uStringConsts,
  uStrings,
  uDownloadClassifier, uJson,
  uFunctions, {$ifdef DEBUG} uLog, {$endif}
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?:ceskatelevize|ct24)\.cz/(?:ivysilani|porady|inside)/.+';
  URLREGEXP_AFTER_ID =  '';

  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36';
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';
  ///DEFAULT_USER_AGENT = 'Dalvik/1.6.0 (Linux; U; Android 4.4.4; Nexus 7 Build/KTU84P)';

const
  REGEXP_PLAYLIST_INFO = 'getPlaylistUrl\(\[\{"type":"(?P<TYP>.+?)","id":"(?P<ID>.+?)"';
  REGEXP_PLAYLIST_URL = '"url"\s*:\s*"(?P<URL>https?:.+?)"';
  //REGEXP_PLAYLIST_URL_NEW = '<playlistURL><!\[CDATA\[(?P<URL>.+?)\]\]></playlistURL>';
  REGEXP_STREAM_URL = '{"type":"VOD",.*?"streamUrls"\s*:\s*\{\s*"main"\s*:\s*"(?P<URL>https?:.+?)"';
  REGEXP_STREAM_VOD_URL = 'PLAYLIST_VOD_URI":"(?P<URL>.+?)",';
  REGEXP_STREAM_VOD_URL_MPD = '"url":"(?P<URL>.+?)",';
  REGEXP_STREAM_VOD_TITLE = '"externalId":"\d+?","title":"(?P<TITLE>.+?)",';

  ///REGEXP_STREAM_URL_NEW = 'video src="(?P<URL>.+?)"';
  REGEXP_STREAM_URL_NEW = '<script type="application/javascript" src="(?P<URL>.+?)" defer></script>';
  REGEXP_STREAM_TITLE = '{"type":"VOD".*?,"title"\s*:\s*"(?P<TITLE>.*?)"';
  //REGEXP_STREAM_TITLE_NEW = '<title>(?P<TITLE>.*?)</title';
  REGEXP_STREAM_TITLE_BETTER = '{"type":"VOD".*?"gemius"\s*:\s*\{[^}]*"NAZEV"\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_IFRAME_URL = '<(?:iframe\b[^>]*\ssrc|a\b[^>]*\shref)="(?P<URL>(?:https?://[^/]+)?/ivysilani/.+?)"';
  REGEXP_STREAM_TITLEFROMPAGE = REGEXP_TITLE_TITLE;
  REGEXP_URL_TOKEN = '<token>(?P<TOKEN>.*?)</token>';
  REGEX_PLAYLIST_ID_IDEC = '"idec":"(?P<IDEC>.+?)",*?';
  REGEX_PLAYLIST_BONUS_ID = 'link href=".*?/bonus/(?P<BID>.+?)/" *?';
  REGEX_PLAYLIST_ART_ID = 'origin=artzona.+?;bonus=(?P<AID>.+?)"';

const

  PLAYLIST_URL = 'https://www.ceskatelevize.cz/services/ivysilani/xml/playlisturl/';
  TOKEN_URL    = 'https://www.ceskatelevize.cz/services/ivysilani/xml/token/';

class function TDownloader_CT.Provider: string;
begin
  Result := 'CeskaTelevize.cz';
end;

class function TDownloader_CT.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_CT.Features: TDownloaderFeatures;
begin
  Result := inherited Features;
end;


{$IFDEF GUI}
class function TDownloader_CT.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_CT;
end;
{$ENDIF}



constructor TDownloader_CT.Create(const AMovieID: string);
begin
  inherited;
  {$IFDEF MULTIDOWNLOADS}
  fNameList := TStringList.Create;
  fUrlList := TStringList.Create;
  {$ENDIF}
  InfoPageEncoding := peUTF8;
  PlaylistInfoRegExp := RegExCreate(REGEXP_PLAYLIST_INFO);
  PlaylistUrlRegExp := RegExCreate(REGEXP_PLAYLIST_URL);
  StreamUrlRegExp := RegExCreate(REGEXP_STREAM_URL);
  StreamUrlRegExpNew := RegExCreate(REGEXP_STREAM_URL_NEW);
  StreamUrlRegVodUrl := RegExCreate(REGEXP_STREAM_VOD_URL);
  StreamUrlRegVodUrlMPD := RegExCreate(REGEXP_STREAM_VOD_URL_MPD);
  StreamUrlRegVodTitle := RegExCreate(REGEXP_STREAM_VOD_TITLE);
  StreamTitleRegExp := RegExCreate(REGEXP_STREAM_TITLE);
  StreamTitle2RegExp := RegExCreate(REGEXP_STREAM_TITLE_BETTER);
  StreamTitleFromPageRegExp := RegExCreate(REGEXP_STREAM_TITLEFROMPAGE);
  IFrameUrlRegExp := RegExCreate(REGEXP_IFRAME_URL);
  UrlToken := RegExCreate(REGEXP_URL_TOKEN);
  IDEC := RegExCreate(REGEX_PLAYLIST_ID_IDEC);
  BID  := RegExCreate(REGEX_PLAYLIST_BONUS_ID);
  AID  := RegExCreate(REGEX_PLAYLIST_ART_ID);
  Referer := GetMovieInfoUrl;
end;

destructor TDownloader_CT.Destroy;
begin
  RegExFreeAndNil(PlaylistInfoRegExp);
  RegExFreeAndNil(PlaylistUrlRegExp);
  RegExFreeAndNil(StreamUrlRegExp);
  RegExFreeAndNil(StreamUrlRegExpNew);
  RegExFreeAndNil(StreamUrlRegVodUrl);
  RegExFreeAndNil(StreamUrlRegVodUrlMPD);
  RegExFreeAndNil(StreamUrlRegVodTitle);
  RegExFreeAndNil(StreamTitleRegExp);
  RegExFreeAndNil(StreamTitle2RegExp);
  RegExFreeAndNil(StreamTitleFromPageRegExp);
  RegExFreeAndNil(IFrameUrlRegExp);
  RegExFreeAndNil(UrlToken);
  RegExFreeAndNil(IDEC);
  RegExFreeAndNil(BID);
  RegExFreeAndNil(AID);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fNameList);
  FreeAndNil(fUrlList);
  {$ENDIF}
  inherited;
end;

function TDownloader_CT.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;


function TDownloader_CT.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Prot, User, Pass, Host, Port, Part, Para: string;
  PlayListType,PlaylistID, PlaylistUrlPage, PlaylistUrl, PlayListUrlMPD, Playlist, Title, Title2, URL_MPD: string;
  {$IFDEF MULTIDOWNLOADS}
  Urls: TStringArray;
  i: integer;
  {$ELSE}
  Url: string;
  {$ENDIF}
  PlayListIDEC, iframeHash: string;
  StreamType, RequestSource: string;
  json: TJson;

  function getRedirectUrl(Http: THttpSend; Url: string): string;
  var MethodStr: string;
      res: boolean;
  begin
    repeat
      Url := Trim(Url);
      SetLastUrl(Url);
      ClearHttp(Http);
      MethodStr := 'HEAD';
      res:=Http.HttpMethod(MethodStr, Url);
    until (not Res) or (not CheckRedirect(Http, Url));
    result:=url;
  end;


begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;

  PlayListType:='episode';
  //StreamType:='type=dash';
  StreamType:='streamType=dash';
  RequestSource:='iVysilani';

  if not GetRegExpVars(IDEC, Page, ['IDEC'], [@PlaylistIDEC]) then
     SetLastErrorMsg('Failed to locate media info page (idec).');
  if PlaylistIDEC = '' then
  begin
    if not GetRegExpVars(BID, Page, ['BID'], [@PlaylistIDEC]) then
      SetLastErrorMsg('Failed to locate bonus media info page (bid).');
    if PlayListIDEC = '' then
       if not GetRegExpVars(AID, Page, ['AID'], [@PlaylistIDEC]) then
         SetLastErrorMsg('Failed to locate art bonus media info page (aid).')
       else
         RequestSource:='Art';
    PlayListType:='bonus';
    StreamType:='type=html&streamingProtocol=dash';
  end;

  // pro DASH play list
  if not Options.ReadProviderOptionDef(Provider, OPTION_CT_DASH_SUPPORT, false) then
  begin
    Http.UserAgent:=DEFAULT_USER_AGENT;
    StreamType:='streamType=hls';
  end;

  {$ifdef debug}
   ///debug:=true;
  {$endif}

  // VOD stream

  if not DownloadPage(Http,'https://player.ceskatelevize.cz/?origin=iVysilani&IDEC='+PlaylistIDEC, PlaylistUrlPage) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
  begin
    if not GetRegExpVar(StreamUrlRegExpNew, PlayListUrlPage, 'URL', PlaylistUrl) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else  if not DownloadPage(Http,'https://player.ceskatelevize.cz'+PlaylistUrl, PlaylistUrlPage) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL) ;

    if not GetRegExpVar(StreamUrlRegVodUrl, PlayListUrlPage, 'URL', PlaylistUrl) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else if not DownloadPage(Http,PlaylistURL+'/stream-data/media/external/'+PlaylistIDEC+'?canPlayDrm=true&quality=web&'+StreamType, PlaylistUrlPage) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);


    json:=JSONCreate(PlayListUrlPage);
    if not JSONValue(json, 'title', Title) then
       SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE);
    if not JSONValue(json, 'streams[0]/url', PlayListUrl) then
         SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);
    JSONFreeAndNil(json);

    {if not GetRegExpVar(StreamUrlRegVodTitle, PlayListUrlPage, 'TITLE', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE);
    if not GetRegExpVar(StreamUrlRegVodUrlMPD, PlayListUrlPage, 'URL', PlaylistUrl) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL); }
    Title := StringReplace(Title, '|', '-', [rfReplaceAll]);

    Url_MPD:=getRedirectUrl(Http, JSDecode(PlaylistUrl));


        {$ifdef debug}
        if debug then
           uLog.Log('GET: %s', [Url_MPD]);

         debug:=false;
        {$endif}

    {$IFDEF MULTIDOWNLOADS}
      UrlList.Add(JSDecode(Url_MPD));
      if Length(Urls) > 1 then
        NameList.Add(Format('%s [%d]', [Title, 0]))
      else
        NameList.Add(Title);
    Name := NameList[0];
    {$ENDIF}
    Name := Title;
    ///MovieURL := {$IFDEF MULTIDOWNLOADS} JSDecode(Urls[0]) {$ELSE} Url {$ENDIF};
    MovieURL := {$IFDEF MULTIDOWNLOADS} JSDecode(Url_MPD) {$ELSE} Url {$ENDIF};
    SetPrepared(True);



    Result := True;


    {
    with TFileStream.create('playlisturl.html',fmCreate) do
    try
       writeBuffer(PlayListUrlPage[1],length(PlaylistUrlPage));
    finally
       free;
    end; }


  end;
end;


procedure TDownloader_CT.SetOptions(const Value: TYTDOptions);
var
  VWithRes: integer;
begin
  inherited;
  VWithRes := Value.ReadProviderOptionDef(Provider, OPTION_CT_MAX_VIDEO_WIDTH, OPTION_CT_MAX_VIDEO_WIDTH_DEFAULT);
  if not Options.ReadProviderOptionDef(Provider, OPTION_CT_DASH_SUPPORT, false) then
  case VWithRes of
       512:  MaxVBitRate:=628000;
       720:  MaxVBitRate:=1160000;
      1024:  MaxVBitRate:=2176000;
      1280:  MaxVBitRate:=3712000;
      1920:  MaxVBitRate:=6272000;
      else   MaxVBitRate:=MaxInt;
  end
  else
  case VWithRes of
       512:  MaxVBitRate:=500000;
       720:  MaxVBitRate:=1032000;
      1024:  MaxVBitRate:=2048000;
      1280:  MaxVBitRate:=3584000;
      1920:  MaxVBitRate:=6144000;
      else   MaxVBitRate:=MaxInt;
  end;
end;


function TDownloader_CT.GetFileNameExt: string;
begin
  Result:='.mpv';
  if not Options.ReadProviderOptionDef(Provider, OPTION_CT_DASH_SUPPORT, false) then
    Result := '.ts';
end;

function TDownloader_CT.GetPlaylistInfo(Http: THttpSend; const Page: string; out PlaylistType, PlaylistID: string): boolean;
var
  Url, Page2: string;
begin
  Result := GetRegExpVars(PlaylistInfoRegExp, Page, ['TYP', 'ID'], [@PlaylistType, @PlaylistID]);
  if not Result then
    if GetRegExpVar(IFrameUrlRegExp, Page, 'URL', Url) then
      if DownloadPage(Http, GetRelativeUrl(GetMovieInfoUrl, Url), Page2, peUtf8) then
        Result := GetRegExpVars(PlaylistInfoRegExp, Page2, ['TYP', 'ID'], [@PlaylistType, @PlaylistID]);
end;

{$IFDEF MULTIDOWNLOADS}

function TDownloader_CT.Prepare: boolean;
begin
  NameList.Clear;
  UrlList.Clear;
  DownloadIndex := 0;
  Result := inherited Prepare;
end;

function TDownloader_CT.ValidatePrepare: boolean;
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

function TDownloader_CT.First: boolean;
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

function TDownloader_CT.Next: boolean;
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

initialization
  RegisterDownloader(TDownloader_CT);

end.

