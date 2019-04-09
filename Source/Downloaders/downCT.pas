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
  SysUtils, Classes, Windows,
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_CT,
    {$ELSE}
      guiOptionsVCL_CT,
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
      StreamTitleRegExp: TRegExp;
      StreamTitle2RegExp: TRegExp;
      StreamTitleFromPageRegExp: TRegExp;
      IFrameUrlRegExp: TRegExp;
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
  uDownloadClassifier,
  uFunctions,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?:ceskatelevize|ct24)\.cz/ivysilani/.+';
  URLREGEXP_AFTER_ID =  '';

  ///DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36';
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)';

const
  REGEXP_PLAYLIST_INFO = '\bgetPlaylistUrl\s*\(\s*\[\s*\{\s*"type"\s*:\s*"(?P<TYP>.+?)"\s*,\s*"id"\s*:\s*"(?P<ID>\d+)"';
  REGEXP_PLAYLIST_URL = '"url"\s*:\s*"(?P<URL>https?:.+?)"';
  REGEXP_STREAM_URL = '"streamUrls"\s*:\s*\{\s*"main"\s*:\s*"(?P<URL>https?:.+?)"';
  REGEXP_STREAM_TITLE = '"playlist"\s*:\s*\[.*?"title"\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_STREAM_TITLE_BETTER = '"playlist"\s*:\s*\[.*?"gemius"\s*:\s*\{[^}]*"NAZEV"\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_IFRAME_URL = '<(?:iframe\b[^>]*\ssrc|a\b[^>]*\shref)="(?P<URL>(?:https?://[^/]+)?/ivysilani/.+?)"';
  REGEXP_STREAM_TITLEFROMPAGE = REGEXP_TITLE_TITLE;

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
  StreamTitleRegExp := RegExCreate(REGEXP_STREAM_TITLE);
  StreamTitle2RegExp := RegExCreate(REGEXP_STREAM_TITLE_BETTER);
  StreamTitleFromPageRegExp := RegExCreate(REGEXP_STREAM_TITLEFROMPAGE);
  IFrameUrlRegExp := RegExCreate(REGEXP_IFRAME_URL);
  Referer := GetMovieInfoUrl;
end;

destructor TDownloader_CT.Destroy;
begin
  RegExFreeAndNil(PlaylistInfoRegExp);
  RegExFreeAndNil(PlaylistUrlRegExp);
  RegExFreeAndNil(StreamUrlRegExp);
  RegExFreeAndNil(StreamTitleRegExp);
  RegExFreeAndNil(StreamTitle2RegExp);
  RegExFreeAndNil(StreamTitleFromPageRegExp);
  RegExFreeAndNil(IFrameUrlRegExp);
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
  PlaylistType, PlaylistID, PlaylistUrlPage, PlaylistUrl, Playlist, Title, Title2: string;
  {$IFDEF MULTIDOWNLOADS}
  Urls: TStringArray;
  i: integer;
  {$ELSE}
  Url: string;
  {$ENDIF}

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
  // pro DASH play list
  if not Options.ReadProviderOptionDef(Provider, OPTION_CT_DASH_SUPPORT, false) then
  begin
    Http.UserAgent:=DEFAULT_USER_AGENT;
  end;
  ParseUrl(GetMovieInfoUrl, Prot, User, Pass, Host, Port, Part, Para);
  if not GetPlaylistInfo(Http, Page, PlaylistType, PlaylistID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http,
                           'https://www.ceskatelevize.cz/ivysilani/ajax/get-client-playlist',
                           ///{$IFDEF UNICODE} AnsiString {$ENDIF} ('playlist%5B0%5D%5Btype%5D=' + PlaylistType + '&playlist%5B0%5D%5Bid%5D=' + PlaylistID + '&requestUrl=' + UrlEncode(Part) + '&requestSource=iVysilani&addCommercials=1&type=flash'),
                           {$IFDEF UNICODE} AnsiString {$ENDIF} ('playlist%5B0%5D%5Btype%5D=' + PlaylistType + '&playlist%5B0%5D%5Bid%5D=' + PlaylistID + '&requestUrl=' + UrlEncode(Part) + '&requestSource=iVysilani&addCommercials=1&type=dash'),
                           HTTP_FORM_URLENCODING_UTF8,
                           ['x-addr: 127.0.0.1', 'X-Requested-With: XMLHttpRequest'],
                           PlaylistUrlPage,
                           peUtf8
                          )
  then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(PlaylistUrlRegExp, PlaylistUrlPage, 'URL', PlaylistUrl) then
    if PlaylistUrlPage <> '' then
      SetLastErrorMsg(Format(ERR_SERVER_ERROR, [PlaylistUrlPage]))
    else
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, JSDecode(PlaylistUrl), Playlist) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  {$IFDEF MULTIDOWNLOADS}
  else if not GetRegExpAllVar(StreamUrlRegExp, Playlist, 'URL', Urls) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  {$ELSE}
  else if not GetRegExpVar(StreamUrlRegExp, Playlist, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  {$ENDIF}
  else if not GetRegExpVar(StreamTitleRegExp, Playlist, 'TITLE', Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else
    begin
      // novy redirect u ct.cz
      Urls[0]:=getRedirectUrl(Http, JSDecode(Urls[0]));

    Title := AnsiEncodedUtf8ToString( {$IFDEF UNICODE} AnsiString {$ENDIF} (JSDecode(Title)));
    if GetRegExpVar(StreamTitle2RegExp, Playlist, 'TITLE', Title2) and (Title2 <> '') then
      Title := AnsiEncodedUtf8ToString( {$IFDEF UNICODE} AnsiString {$ENDIF} (JSDecode(Title2)));
    if Title = '' then
      if not GetRegExpVar(StreamTitleFromPageRegExp, Page, 'TITLE', Title) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE);
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

procedure TDownloader_CT.SetOptions(const Value: TYTDOptions);
var
  Bitrate: integer;
begin
  inherited;
  Bitrate := Value.ReadProviderOptionDef(Provider, OPTION_CT_MAX_VIDEO_WIDTH, OPTION_CT_MAX_VIDEO_WIDTH_DEFAULT);
  ///if TDownloader_CT.classtype = THLSDownloader then
  if not Options.ReadProviderOptionDef(Provider, OPTION_CT_DASH_SUPPORT, false) then
  case Bitrate of
       512:  MaxBitRate:=628000;
       720:  MaxBitRate:=1160000;
      1024:  MaxBitRate:=2176000;
      1280:  MaxBitRate:=3712000;
      1920:  MaxBitRate:=6272000;
      else   MaxBitRate:=MaxInt;
  end
  else
  case Bitrate of
       512:  MaxBitRate:=500000;
       720:  MaxBitRate:=1032000;
      1024:  MaxBitRate:=2048000;
      1280:  MaxBitRate:=3584000;
      1920:  MaxBitRate:=6144000;
      else   MaxBitRate:=MaxInt;
  end
end;


function TDownloader_CT.GetFileNameExt: string;
begin
  Result:='.mpv';
  ///if TDownloader_CT.classtype = THLSDownloader then
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

