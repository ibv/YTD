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
  Podweby Novy maji konfiguraci ulozenou v zasifrovanem konfiguracnim souboru,
  napr. "http://tn.nova.cz/bin/player/flowplayer/config.php?site=23000&realSite=77000&subsite=574&section=77300&media=752873&jsVar=flowConf1&mute=0&size=&pWidth=600&pHeight=383"
  Jeho desifrovani je v metode IdentifyDownloader, heslo se ziska dekompilaci
  13-flowplayer.swf a hledanim "AES". Toto se tyka napr. poker.nova.cz,
  poklicka.nova.cz a dalsich.

  Udaje pro ziskani playlistu pro RTMP verzi se daji ziskat dekompilovanim
  http://voyo.nova.cz/static/shared/app/flowplayer/13-flowplayer.nacevi-3.1.5-06-002.swf
  ve skriptu org.flowplayer.nacevi.Nacevi (sestaveni URL a ziskani a zpracovani
  playlistu - zejmena jde o metody getHashString a onGetTimeStamp). Potreba je
  pro to ResolverSecret, ktery se da najit v desifrovanem konfiguracnim souboru
  jako polozka "secret" (primo v SWF je jen falesna hodnota pro zmatelni nepritele).
}

{.$DEFINE VOYO_PLUS}
  // Zatim nefunguje

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, uCrypto, HttpSend, SynaCode,
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
  uRtmpDirectDownloader, uMSDirectDownloader;

type
  TDownloader_Nova = class(TNestedDownloader)
    private
    protected
      LowQuality: boolean;
      ResolverSecret: string;
      ConfigPassword: string;
      MediaDataRegExp: TRegExp;
      MovieIDRegExp: TRegExp;
      PlayerParamsRegExp: TRegExp;
      PlayerParamsItemRegExp: TRegExp;
      RegExpFlowPlayerConfigUrl: TRegExp;
      RegExpFlowPlayerConfig: TRegExp;
      JSONConfigRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
      function TryMSDownloader(Http: THttpSend; const SiteID, SectionID, Subsite, ProductID, UnitID, MediaID: string; out Downloader: TDownloader): boolean;
      function TryRTMPDownloader(Http: THttpSend; const SiteID, SectionID, Subsite, ProductID, UnitID, MediaID: string; out Downloader: TDownloader): boolean;
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

const
  OPTION_NOVA_LOWQUALITY {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'low_quality';
  OPTION_NOVA_LOWQUALITY_DEFAULT = False;
  OPTION_NOVA_SECRET {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'secret';
  OPTION_NOVA_SECRET_DEFAULT = '';
  OPTION_NOVA_CONFIG_PASSWORD {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'config_password';
  OPTION_NOVA_CONFIG_PASSWORD_DEFAULT = '';

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
  REGEXP_MEDIADATA = '\bmainVideo\s*=\s*new\s+mediaData\s*\(\s*(?P<PROD_ID>\d+)\s*,\s*(?P<UNIT_ID>\d+)\s*,\s*(?P<MEDIA_ID>\d+)'; // dalsi tri parametry jsou: Archivovane, Extra, Zive
  REGEXP_STREAMID = '<param\s+value=\\"(?:[^,]*,)*identifier=(?P<ID>(?P<YEAR>\d{4})-(?P<MONTH>\d{2})-[^",]+)';
  REGEXP_PLAYERPARAMS = 'voyoPlayer\.params\s*=\s*\{(?P<PARAMS>.*?)\}\s*;';
  REGEXP_PLAYERPARAMS_ITEM = '\b(?P<VARNAME>[a-z0-9_]+)\s*:\s*(?P<QUOTE>["'']?)(?P<VARVALUE>.*?)(?P=QUOTE)\s*(?:,|$)';
  REGEXP_CONFIG_URL = '<script\b[^>]*?\ssrc="(?P<URL>https?://[^"]+?/config\.php\?.+?)"';
  REGEXP_CONFIG = '''(?P<CONFIG>[a-zA-Z0-9+/=]+)''';
  REGEXP_JSON_CONFIG = '"(?P<VARNAME>[^"]+)"\s*:\s*(?P<VARVALUE>(?:"[^"]*"|\w+))';

const
  NOVA_PROVIDER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'Nova.cz';
  NOVA_URLREGEXP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;

resourcestring
  ERR_MISSING_CONFIG_PASSWORD = 'Invalid configuration: Config password not set.';

type
  TDownloader_Nova_MS = class(TMSDirectDownloader);

  TDownloader_Nova_RTMP = class(TRTMPDirectDownloader)
    public
      class function Features: TDownloaderFeatures; override;
    end;

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
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MediaDataRegExp := RegExCreate(REGEXP_MEDIADATA);
  MovieIDRegExp := RegExCreate(REGEXP_STREAMID);
  PlayerParamsRegExp := RegExCreate(REGEXP_PLAYERPARAMS);
  PlayerParamsItemRegExp := RegExCreate(REGEXP_PLAYERPARAMS_ITEM);
  RegExpFlowPlayerConfigUrl := RegExCreate(REGEXP_CONFIG_URL);
  RegExpFlowPlayerConfig := RegExCreate(REGEXP_CONFIG);
  JSONConfigRegExp := RegExCreate(REGEXP_JSON_CONFIG);
  LowQuality := OPTION_NOVA_LOWQUALITY_DEFAULT;
  ResolverSecret := OPTION_NOVA_SECRET_DEFAULT;
  ConfigPassword := OPTION_NOVA_CONFIG_PASSWORD_DEFAULT;
end;

destructor TDownloader_Nova.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MediaDataRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(PlayerParamsRegExp);
  RegExFreeAndNil(PlayerParamsItemRegExp);
  RegExFreeAndNil(RegExpFlowPlayerConfigUrl);
  RegExFreeAndNil(RegExpFlowPlayerConfig);
  RegExFreeAndNil(JSONConfigRegExp);
  inherited;
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

procedure TDownloader_Nova.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  LowQuality := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_LOWQUALITY, OPTION_NOVA_LOWQUALITY_DEFAULT);
  ResolverSecret := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_SECRET, OPTION_NOVA_SECRET_DEFAULT);
  ConfigPassword := Value.ReadProviderOptionDef(Provider, OPTION_NOVA_CONFIG_PASSWORD, OPTION_NOVA_CONFIG_PASSWORD_DEFAULT);
end;

function TDownloader_Nova.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
const
  AES_KEY_BITS = 128;
var
  Params, SiteID, SectionID, Subsite, ProductID, UnitID, MediaID: string;
  ConfigUrl, ConfigPage, Config, DecryptedConfig: string;
  HaveParams: boolean;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  Result := False;
  // Konfigurace prehravace muze mit nekolik zdroju
  HaveParams := False;
  SiteID := '';
  SectionID := '';
  Subsite := '';
  ProductID := '';
  UnitID := '';
  MediaID := '';
  // a) Mohou byt zasifrovane v javascriptove konfiguraci
  if not HaveParams then
    if GetRegExpVar(RegExpFlowPlayerConfigUrl, Page, 'URL', ConfigUrl) then
      if DownloadPage(Http, ConfigUrl, ConfigPage) then
        if GetRegExpVar(RegExpFlowPlayerConfig, ConfigPage, 'CONFIG', Config) then
          if ConfigPassword = '' then
            begin
            SetLastErrorMsg(ERR_MISSING_CONFIG_PASSWORD);
            Exit;
            end
          else
            begin
            DecryptedConfig :=  {$IFDEF UNICODE} string {$ENDIF} (AESCTR_Decrypt(DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Config)),  {$IFDEF UNICODE} AnsiString {$ENDIF} (ConfigPassword), AES_KEY_BITS));
            if GetRegExpVarPairs(JSONConfigRegExp, DecryptedConfig, ['mediaID', 'sectionID', 'siteID'], [@MediaID, @SectionID, @SiteID]) then
              HaveParams := True;
            end;
  // b)Mohou byt primo ve zdrojove strance
  if not HaveParams then
    if GetRegExpVar(PlayerParamsRegExp, Page, 'PARAMS', Params) then
      if GetRegExpVarPairs(PlayerParamsItemRegExp, Params, ['siteId', 'sectionId', 'subsite'], [@SiteID, @SectionID, @Subsite]) then
        if GetRegExpVars(MediaDataRegExp, Page, ['PROD_ID', 'UNIT_ID', 'MEDIA_ID'], [@ProductID, @UnitID, @MediaID]) then
          HaveParams := True;
  // Aspon nektere z tech parametru jsou povinne
  if not HaveParams then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if (SiteID = '') or (SectionID = '') or (MediaID = '') then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  // Ted postupne vyzkousim jednotlive downloadery
  else if TryMSDownloader(Http, SiteID, SectionID, Subsite, ProductID, UnitID, MediaID, Downloader) then
    Result := True
  else if TryRTMPDownloader(Http, SiteID, SectionID, Subsite, ProductID, UnitID, MediaID, Downloader) then
    Result := True
  else
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO);
end;

function TDownloader_Nova.TryMSDownloader(Http: THttpSend; const SiteID, SectionID, Subsite, ProductID, UnitID, MediaID: string; out Downloader: TDownloader): boolean;
const
  QualitySuffix: array[boolean] of string = ('-LQ', '-HQ');
  SoapQuality: array[0..2] of string = ('hd', 'hq', 'lq');
  SOAP_REQUEST = ''
    + '<GetSecuredUrl xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://streaming.kitd.cz/cdn/nova">'
    + '<token></token>'
    + '<mediaId>%0:s</mediaId>'
    + '<id>%1:s</id>'
    + '<type>Archive</type>'
    + '<format>%2:s</format>'
    + '</GetSecuredUrl>'
    ;
var
  InfoUrl, StreamInfo, Year, Month, ID, Url: string;
  MSDownloader: TMSDirectDownloader;
  RequestXml, ResponseXml: TXmlDoc;
  ResponseHeaderNode, ResponseBodyNode: TXmlNode;
  i: integer;
begin
  Result := False;
  if not Result then
    begin
    InfoUrl := Format('http://voyo.nova.cz/bin/eshop/ws/plusPlayer.php?x=playerFlash'
                      + '&prod=%0:s&unit=%1:s&media=%2:s&site=%3:s&section=%4:s&subsite=%5:s'
                      + '&embed=0&mute=0&size=&realSite=%3:s&width=704&height=441&hdEnabled=%6:d'
                      {$IFDEF VOYO_PLUS}
                      + '&hash=%7:s&dev=&8:s&wv=1&sts=%9:s&r=%10:d
                      {$ENDIF}
                      + '&finish=finishedPlayer', [
                      {0}ProductID, {1}UnitID, {2}MediaID, {3}SiteID, {4}SectionID, {5}Subsite
                      , {6}Integer(LowQuality)
                      {$IFDEF VOYO_PLUS}
                      , {7}Hash, {8}Device, {9}Timestamp, {10}Random(65535)
                      {$ENDIF}
                      ]);
    if DownloadPage(Http, InfoUrl, StreamInfo) then
      if GetRegExpVars(MovieIDRegExp, StreamInfo, ['YEAR', 'MONTH', 'ID'], [@Year, @Month, @ID]) then
        begin
        MovieUrl := Format('http://cdn1003.nacevi.cz/nova-vod-wmv/%s/%s/%s%s.wmv', [Year, Month, ID, QualitySuffix[not LowQuality]]);
        MSDownloader := TMSDirectDownloader.CreateWithName(MovieUrl, UnpreparedName);
        MSDownloader.Options := Options;
        Downloader := MSDownloader;
        Result := True;
        end;
    end;
  if not Result then
    begin
    if LowQuality then
      i := Pred(Length(SoapQuality))
    else
      i := 0;
    while (not Result) and (i >= 0) and (i < Length(SoapQuality)) do
      begin
      RequestXml := TXmlDoc.Create;
      try
        RequestXml.LoadFromBinaryString( {$IFDEF UNICODE} AnsiString {$ENDIF} (Format(SOAP_REQUEST, [MediaID, UnitID, SoapQuality[i]])));
        if DownloadSoap(Http, 'http://fcdn-dir.kitd.cz/Services/Player.asmx', 'http://streaming.kitd.cz/cdn/nova/GetSecuredUrl', nil, RequestXml.Root, ResponseXml, ResponseHeaderNode, ResponseBodyNode) then
          try
            if ResponseBodyNode <> nil then
              if GetXmlVar(ResponseBodyNode, 'GetSecuredUrlResponse/GetSecuredUrlResult', Url) then
                if Url <> '' then
                  if AnsiCompareText(Copy(Url, 1, 4), 'rtmp') <> 0 then
                    begin
                    MovieUrl := Url;
                    MSDownloader := TMSDirectDownloader.CreateWithName(MovieUrl, UnpreparedName);
                    MSDownloader.Options := Options;
                    Downloader := MSDownloader;
                    Result := True;
                    end;
          finally
            FreeAndNil(ResponseXml);
            end;
        if LowQuality then
          Dec(i)
        else
          Inc(i);
      finally
        FreeAndNil(RequestXml);
        end;
      end;
    end;
end;

function TDownloader_Nova.TryRTMPDownloader(Http: THttpSend; const SiteID, SectionID, Subsite, ProductID, UnitID, MediaID: string; out Downloader: TDownloader): boolean;
const
  NOVA_SERVICE_URL = 'http://master-ng.nacevi.cz/cdn.server/PlayerLink.ashx';
  NOVA_TIMESTAMP_URL = 'http://tn.nova.cz/lbin/time.php';
  NOVA_APP_ID = 'nova-vod';
var
  InfoXml: TXmlDoc;
  Node: TXmlNode;
  Timestamp, AppID, Signature, InfoUrl, Status, Url, BaseUrl, Quality: string;
  SignatureBytes: AnsiString;
  i: integer;
  RtmpDownloader: TDownloader_Nova_RTMP;
begin
  Result := False;
  if DownloadPage(Http, NOVA_TIMESTAMP_URL, Timestamp) then
    begin
    Timestamp := Copy(Timestamp, 1, 14);
    AppID := UrlEncode(NOVA_APP_ID + '|' + MediaID);
    SignatureBytes := {$IFDEF UNICODE} AnsiString {$ENDIF} (NOVA_APP_ID + '|' + MediaID + '|' + Timestamp + '|' + ResolverSecret);
    SignatureBytes := MD5(SignatureBytes);
    SignatureBytes := EncodeBase64(SignatureBytes);
    Signature := UrlEncode( {$IFDEF UNICODE} string {$ENDIF} (SignatureBytes));
    InfoUrl := Format(NOVA_SERVICE_URL + '?c=%s&h=0&t=%s&s=%s&tm=nova&d=1', [AppID, Timestamp, Signature]);
    if DownloadXml(Http, InfoUrl, InfoXml) then
      try
        if GetXmlVar(InfoXml, 'status', Status) then
          if Status = 'Ok' then
            if GetXmlVar(InfoXml, 'baseUrl', BaseUrl) then
              if XmlNodeByPath(InfoXml, 'mediaList', Node) then
                for i := 0 to Pred(Node.NodeCount) do
                  if Node[i].Name = 'media' then
                    if GetXmlVar(Node[i], 'url', Url) then
                      if GetXmlVar(Node[i], 'quality', Quality) then
                        if (LowQuality and (Quality = 'lq')) or ((not LowQuality) and (Quality = 'hq')) then
                          begin
                          MovieUrl := Url;
                          RtmpDownloader := TDownloader_Nova_RTMP.Create(Url);
                          RtmpDownloader.Options := Options;
                          RtmpDownloader.RtmpUrl := BaseUrl;
                          RtmpDownloader.Playpath := Url;
                          RtmpDownloader.SaveRtmpDumpOptions;
                          Downloader := RtmpDownloader;
                          Result := True;
                          Break;
                          end;
      finally
        FreeAndNil(InfoXml);
        end;
    end;
end;

{ TDownloader_Nova_RTMP }

class function TDownloader_Nova_RTMP.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

initialization
  RegisterDownloader(TDownloader_Nova);
  //RegisterDownloader(TDownloader_Nova_RTMP);

end.
