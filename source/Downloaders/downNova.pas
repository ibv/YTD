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

unit downNova;
{$INCLUDE 'ytd.inc'}

{
  Udaje pro ziskani playlistu pro RTMP verzi se daji ziskat dekompilovanim
  http://voyo.nova.cz/static/shared/app/flowplayer/13-flowplayer.nacevi-3.1.5-06-002.swf
  ve skriptech org.flowplayer.nacevi.Config (konfiguracni udaje, napr. token)
  a org.flowplayer.nacevi.Nacevi (sestaveni URL a ziskani a zpracovani playlistu -
  zejmena jde o metody getHashString a onGetTimeStamp).

  Nedari se mi sehnat funkcni RTMP i MS video soucasne. Zpravy by obecne mely
  byt na RTMP, ale o RTSP nevim.
  http://voyo.nova.cz/product/zpravy/31987-televizni-noviny-3-11-2012
  -> RTMP

}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaCode,
  uOptions, uCompatibility,
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
      MediaDataRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
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

  TDownloader_Nova_RTMP = class(TRtmpDownloader)
    private
    protected
      MediaIdRegExp: TRegExp;
      MediaDataRegExp: TRegExp;
      LowQuality: boolean;
      Secret: string;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    protected
    public
      class function Features: TDownloaderFeatures; override;
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  TDownloader_Nova_MS = class(TMSDownloader)
    private
    protected
      LowQuality: boolean;
      PlayerParamsRegExp: TRegExp;
      PlayerParamsItemRegExp: TRegExp;
      MediaDataRegExp: TRegExp;
      MovieIDRegExp: TRegExp;
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
  OPTION_NOVA_SECRET {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'secret';
  OPTION_NOVA_SECRET_DEFAULT = '';

implementation

uses
  uStringConsts,
  {$IFDEF DIRTYHACKS}
  uFiles,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://voyo.nova.cz/product/zpravy/30076-televizni-noviny-28-7-2012
// http://archiv.nova.cz/multimedia/ulice-1683-1684-dil.html
// http://voyo.nova.cz/home/plus-video/321-kriminalka-andel-podraz
// http://voyo.nova.cz/product/filmy/26894-testovaci-video-okresni-prebor-16
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?<!tn\.)nova\.cz/.+$';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_META_OGTITLE;
  REGEXP_MEDIADATA = '\bmainVideo\s*=\s*new\s+mediaData\s*\(\s*(?P<PROD_ID>\d+)\s*,\s*(?P<UNIT_ID>\d+)\s*,\s*(?P<MEDIA_ID>\d+)\s*,\s*(?P<FLAG>true|false)';

  REGEXP_RTMP_MOVIE_TITLE = REGEXP_TITLE_H1_CLASS;
  REGEXP_RTMP_MEDIAID = '(?:\bvar\s|\b)media_id\s*=\s*(?P<QUOTE>["'']?)(?P<ID>[0-9]+)(?P=QUOTE)';
  //REGEXP_RTMP_MEDIADATA = '\bmediadata\s*\(\s*[0-9]+\s*,\s*[0-9]+\s*,\s*(?P<ID>[0-9]+)';

  REGEXP_MS_TITLE = REGEXP_MOVIE_TITLE;
  REGEXP_MS_PLAYERPARAMS = 'voyoPlayer\.params\s*=\s*\{(?P<PARAMS>.*?)\}\s*;';
  REGEXP_MS_PLAYERPARAMS_ITEM = '\b(?P<VARNAME>[a-z0-9_]+)\s*:\s*(?P<QUOTE>["'']?)(?P<VARVALUE>.*?)(?P=QUOTE)\s*(?:,|$)';
  //REGEXP_MS_MEDIADATA = '\bnew\s+mediaData\s*\(\s*(?P<PROD_ID>\d+)\s*,\s*(?P<UNIT_ID>\d+)\s*,\s*(?P<MEDIA_ID>\d+)';
  REGEXP_MS_STREAMID = '<param\s+value=\\"(?:[^,]*,)*identifier=(?P<ID>(?P<YEAR>\d{4})-(?P<MONTH>\d{2})-[^",]+)';

const
  NOVA_PROVIDER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'Nova.cz';
  NOVA_URLREGEXP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;

{ TDownloader_Nova }

class function TDownloader_Nova.Features: TDownloaderFeatures;
begin
  Result := inherited Features + TDownloader_Nova_RTMP.Features + TDownloader_Nova_MS.Features;
end;

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
  MediaDataRegExp := RegExCreate(REGEXP_MEDIADATA);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
end;

destructor TDownloader_Nova.Destroy;
begin
  RegExFreeAndNil(MediaDataRegExp);
  inherited;
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Nova.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
//var
//  Flag: string;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
//  if GetRegExpVar(MediaDataRegExp, Page, 'FLAG', Flag) and (Flag = 'true') then
//    Downloader := TDownloader_Nova_MS.Create(MovieID)
//  else
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

class function TDownloader_Nova_RTMP.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

constructor TDownloader_Nova_RTMP.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(Format(REGEXP_RTMP_MOVIE_TITLE, ['original_title']));
  MediaIdRegExp := RegExCreate(REGEXP_RTMP_MEDIAID);
  MediaDataRegExp := RegExCreate(REGEXP_MEDIADATA);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
  Secret := OPTION_NOVA_SECRET_DEFAULT;
end;

destructor TDownloader_Nova_RTMP.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MediaIdRegExp);
  RegExFreeAndNil(MediaDataRegExp);
  inherited;
end;

function TDownloader_Nova_RTMP.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Nova_RTMP.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  NOVA_SERVICE_URL = 'http://master-ng.nacevi.cz/cdn.server/PlayerLink.ashx';
  NOVA_TIMESTAMP_URL = 'http://tn.nova.cz/lbin/time.php';
  NOVA_APP_ID = 'nova-vod';
var
  InfoXml: TXmlDoc;
  Node: TXmlNode;
  Media_ID, Timestamp, ID, Signature, Url, Status, BaseUrl, Quality: string;
  SignatureBytes: AnsiString;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MediaIdRegExp, Page, 'ID', Media_ID) then
    GetRegExpVar(MediaDataRegExp, Page, 'MEDIA_ID', Media_ID);
  if Media_ID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['media_id']))
  else if Secret = '' then
    SetLastErrorMsg(ERR_SECURE_TOKEN_NOT_SET)
  else if not DownloadPage(Http, NOVA_TIMESTAMP_URL, Timestamp) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['timestamp']))
  else
    begin
    Timestamp := Copy(Timestamp, 1, 14);
    ID := UrlEncode(NOVA_APP_ID + '|' + Media_ID);
    SignatureBytes := {$IFDEF UNICODE} AnsiString {$ENDIF} (NOVA_APP_ID + '|' + Media_ID + '|' + Timestamp + '|' + Secret);
    SignatureBytes := MD5(SignatureBytes);
    SignatureBytes := EncodeBase64(SignatureBytes);
    Signature := UrlEncode( {$IFDEF UNICODE} string {$ENDIF} (SignatureBytes));
    Url := Format(NOVA_SERVICE_URL + '?c=%s&h=0&t=%s&s=%s&tm=nova&d=1', [ID, Timestamp, Signature]);
    if not DownloadXml(Http, Url, InfoXml) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else
      try
        if (not GetXmlVar(InfoXml, 'status', Status)) or (Status <> 'Ok') then
          SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE + ' ' + Status)
        else if not GetXmlVar(InfoXml, 'baseUrl', BaseUrl) then
          SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
        else if not XmlNodeByPath(InfoXml, 'mediaList', Node) then
          SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
        else
          for i := 0 to Pred(Node.NodeCount) do
            if Node[i].Name = 'media' then
              if GetXmlVar(Node[i], 'url', Url) then
                if GetXmlVar(Node[i], 'quality', Quality) then
                  if (LowQuality and (Quality = 'lq')) or ((not LowQuality) and (Quality = 'hq')) then
                    begin
                    MovieUrl := Url;
                    Self.RtmpUrl := BaseUrl;
                    Self.Playpath := Url;
                    SetPrepared(True);
                    Result := True;
                    Break;
                    end;
      finally
        FreeAndNil(InfoXml);
        end;
    end;
end;

procedure TDownloader_Nova_RTMP.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  LowQuality := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT);
  Secret := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_SECRET, OPTION_NOVA_SECRET_DEFAULT);
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
  MovieTitleRegExp := RegExCreate(REGEXP_MS_TITLE);
  PlayerParamsRegExp := RegExCreate(REGEXP_MS_PLAYERPARAMS);
  PlayerParamsItemRegExp := RegExCreate(REGEXP_MS_PLAYERPARAMS_ITEM);
  MediaDataRegExp := RegExCreate(REGEXP_MEDIADATA);
  MovieIDRegExp := RegExCreate(REGEXP_MS_STREAMID);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
end;

destructor TDownloader_Nova_MS.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(PlayerParamsRegExp);
  RegExFreeAndNil(PlayerParamsItemRegExp);
  RegExFreeAndNil(MediaDataRegExp);
  RegExFreeAndNil(MediaDataRegExp);
  inherited;
end;

function TDownloader_Nova_MS.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Nova_MS.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  QualitySuffix: array[boolean] of string = ('-LQ', '-HQ');
var
  Params, SiteID, SectionID, Subsite, ProductID, UnitID, MediaID, StreamInfo, Year, Month, ID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(PlayerParamsRegExp, Page, 'PARAMS', Params) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not GetRegExpVarPairs(PlayerParamsItemRegExp, Params, ['siteId', 'sectionId', 'subsite'], [@SiteID, @SectionID, @Subsite]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if (SiteID = '') or (SectionID = '') or (Subsite = '') then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not GetRegExpVars(MediaDataRegExp, Page, ['PROD_ID', 'UNIT_ID', 'MEDIA_ID'], [@ProductID, @UnitID, @MediaID]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, Format('http://voyo.nova.cz/bin/eshop/ws/plusPlayer.php?x=playerFlash&prod=%s&unit=%s&media=%s&site=%s&section=%s&subsite=%s&embed=0&mute=0&size=&realSite=%s&width=704&height=441&hdEnabled=%d', [ProductID, UnitID, MediaID, SiteID, SectionID, Subsite, SiteID, Integer(LowQuality)]), StreamInfo) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVars(MovieIDRegExp, StreamInfo, ['YEAR', 'MONTH', 'ID'], [@Year, @Month, @ID]) then
    SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
  else
    begin
    MovieUrl := Format('http://cdn1003.nacevi.cz/nova-vod-wmv/%s/%s/%s%s.wmv', [Year, Month, ID, QualitySuffix[not LowQuality]]);
    SetPrepared(True);
    Result := True;
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
