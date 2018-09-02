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

unit downIPrima;
{$INCLUDE 'ytd.inc'}
{$DEFINE PRIMA_LIVEBOX}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  {$IFDEF PRIMA_LIVEBOX}
  uRtmpDownloader,
  {$ENDIF}
  uHttpDownloader, downStream;

type
  TDownloader_iPrima = class(TNestedDownloader)
    private
    protected
      {$IFDEF PRIMA_LIVEBOX}
      LiveBoxRegExp: TRegExp;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  TDownloader_iPrima_Stream = class(TDownloader_Stream)
    private
    protected
      StreamIDRegExp: TRegExp;
      StreamCDNIDRegExp: TRegExp;
      TryingHighestQuality: boolean;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoUrlForID(const ID: string): string; override;
      function GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  {$IFDEF PRIMA_LIVEBOX}
  TDownloader_iPrima_LiveBox = class(TRtmpDownloader)
    private
    protected
      LiveBoxRegExp: TRegExp;
      LiveBoxAuthRegExp: TRegExp;
      LiveBoxUrlPrefix: TRegExp;
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

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://play.iprima.cz/all/233409
// http://play.iprima.cz/iprima/233409
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*iprima\.cz/';
  URLREGEXP_ID =        '(?!showjanakrause/).+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_META_OGTITLE;
  REGEXP_STREAM_ID = '<param\s+name="flashvars"\s+value="[^"]*&id=(?P<STREAMID>[0-9]+)';
  REGEXP_STREAM_CDNID = '<param\s+name="flashvars"\s+value="[^"]*&cdnID=(?P<STREAMID>[0-9]+)';
  {$IFDEF PRIMA_LIVEBOX}
  //REGEXP_LIVEBOX = '\bLiveboxPlayer\.init\s*\((?:\s*''[^'']*''\s*,)\s*width\s*,\s*height\s*,\s*''(?P<HQ>[^'']*)''\s*,\s*''(?P<LQ>[^'']*)''';
  REGEXP_LIVEBOX = '''hq_id''\s*:\s*''(?P<HQ>[^'']*)''\s*,\s*''lq_id''\s*:\s*''(?P<LQ>[^'']*)''(?:.*?''zoneGEO''\s*:\s*(?P<ZONEGEO>[0-9]*))?';
  REGEXP_LIVEBOX_AUTH = '''(?P<AUTH>\?auth=.*?)''';
  REGEXP_LIVEBOX_URLPREFIX = '\biph_full_url\s*=\s*''(?P<URL>https?://.+?)''';
  {$ENDIF}

const
  PRIMA_URLREGEXP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
  PRIMA_MOVIE_INFO_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://play.iprima.cz/%s';
  PRIMA_PROVIDER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'iPrima.cz';

{ TDownloader_iPrima }

class function TDownloader_iPrima.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

class function TDownloader_iPrima.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfRtmpLiveStream, dfPreferRtmpLiveStream];
end;

constructor TDownloader_iPrima.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
    MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  {$IFDEF PRIMA_LIVEBOX}
  LiveBoxRegExp := RegExCreate(REGEXP_LIVEBOX);
  {$ENDIF}
end;

destructor TDownloader_iPrima.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  {$IFDEF PRIMA_LIVEBOX}
  RegExFreeAndNil(LiveBoxRegExp);
  {$ENDIF}
  inherited;
end;

function TDownloader_iPrima.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  {$IFDEF PRIMA_LIVEBOX}
  if GetRegExpVars(LiveBoxRegExp, Page, [], []) then
    Downloader := TDownloader_iPrima_LiveBox.Create(MovieID)
  else
  {$ENDIF}
    Downloader := TDownloader_iPrima_Stream.Create(MovieID);
  Result := True;
end;

{ TDownloader_iPrima_Stream }

class function TDownloader_iPrima_Stream.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima_Stream.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_iPrima_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  StreamIDRegExp := RegExCreate(REGEXP_STREAM_ID);
  StreamCDNIDRegExp := RegExCreate(REGEXP_STREAM_CDNID);
end;

destructor TDownloader_iPrima_Stream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(StreamIDRegExp);
  RegExFreeAndNil(StreamCDNIDRegExp);
  inherited;
end;

function TDownloader_iPrima_Stream.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima_Stream.GetMovieInfoUrlForID(const ID: string): string;
begin
  if TryingHighestQuality then
    Result := 'http://prima.stream.cz/' + ID
  else
    Result := inherited GetMovieInfoUrlForID(ID);
end;

function TDownloader_iPrima_Stream.GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean;
begin
  Result := inherited GetFlashVarsIdStrings(ID, cdnLQ, cdnHQ, cdnHD, Title);
  if not TryingHighestQuality then
    begin
    cdnLQ := 'lqID';
    cdnHQ := 'hqID';
    cdnHD := 'hdID';
    end;
end;

function TDownloader_iPrima_Stream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, ID, EmbeddedPage: string;
    EmbeddedPageXml: TXmlDoc;
begin
  Result := False;
  if GetRegExpVar(StreamIDRegExp, Page, 'STREAMID', ID) then
    begin
    // Chci zkusit "Highest quality"
    if not Result then
      begin
      TryingHighestQuality := True;
      try
        Url := GetMovieInfoUrlForID(ID);
        if GetMovieInfoContent(Http, Url, EmbeddedPage, EmbeddedPageXml) then
          if inherited AfterPrepareFromPage(EmbeddedPage, EmbeddedPageXml, Http) then
            Result := Prepared;
      finally
        TryingHighestQuality := False;
        end;
      end;
    // Pokud se nepodari, zkusim normalni kvalitu, a to napred pres Primu
    if not Result then
      begin
      Url := GetMovieInfoUrlForID(ID);
      if GetMovieInfoContent(Http, Url, EmbeddedPage, EmbeddedPageXml) then
        if inherited AfterPrepareFromPage(EmbeddedPage, EmbeddedPageXml, Http) then
          Result := Prepared
    // a pak pres Stream samotny
        else if inherited AfterPrepareFromPage(Page, PageXml, Http) then
          Result := Prepared
      end;
    end;
  // Nakonec zkusim primo stahnout cdnID
  if not Result then
    if GetRegExpVar(StreamCDNIDRegExp, Page, 'STREAMID', ID) then
      begin
      ExternalCDNID := ID;
      Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
      end
end;

{$IFDEF PRIMA_LIVEBOX}

{ TDownloader_iPrima_LiveBox }

class function TDownloader_iPrima_LiveBox.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima_LiveBox.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

class function TDownloader_iPrima_LiveBox.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

constructor TDownloader_iPrima_LiveBox.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  LiveBoxRegExp := RegExCreate(REGEXP_LIVEBOX);
  LiveBoxAuthRegExp := RegExCreate(REGEXP_LIVEBOX_AUTH);
  LiveBoxUrlPrefix := RegExCreate(REGEXP_LIVEBOX_URLPREFIX);
end;

destructor TDownloader_iPrima_LiveBox.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(LiveBoxRegExp);
  RegExFreeAndNil(LiveBoxAuthRegExp);
  RegExFreeAndNil(LiveBoxUrlPrefix);
  inherited;
end;

function TDownloader_iPrima_LiveBox.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima_LiveBox.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  PlayerHttp: THttpSend;
  EmbedPlayer, Auth, HQStream, LQStream, ZoneGEO, Geo, Stream, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  PlayerHttp := CreateHttp;
  if not GetRegExpVars(LiveBoxRegExp, Page, ['HQ', 'LQ', 'ZONEGEO'], [@HQStream, @LQStream, @ZoneGEO]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    try
      PlayerHttp.Cookies.Assign(Http.Cookies);
      PlayerHttp.Headers.Add('Referer: ' + GetMovieInfoUrl);
      if not DownloadPage(PlayerHttp, Format('http://embed.livebox.cz/iprimaplay/player-embed-v2.js?__tok%d__=%d', [Random(1073741824), Random(1073741824)]), EmbedPlayer, peUnknown, hmGET, False) then
        SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
      else if not GetRegExpVar(LiveBoxAuthRegExp, EmbedPlayer, 'AUTH', Auth) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
      else if not GetRegExpVar(LiveBoxUrlPrefix, EmbedPlayer, 'URL', Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
      else
        begin
        if HQStream <> '' then
          Stream := HQStream
        else
          Stream := LQStream;
        if StrToIntDef(ZoneGeo, 0) > 0 then
          Geo := '_' + ZoneGeo
        else
          Geo := '';
        MovieUrl := Url {'rtmp://bcastlw.livebox.cz:80/iprima_token'} + Geo + Auth; // '?auth=_any_|1331380805|e0bdc430140646104fb9509b5c76791c78da9ce7';
        Self.RtmpUrl := MovieURL; // Prima Play
        if ExtractFileExt(Stream) = '.mp4' then
          Self.PlayPath := 'mp4:' + Stream
        else
          Self.PlayPath := ChangeFileExt(Stream, '');
        Self.FlashVer := 'WIN 11,1,102,63';
        Self.SwfUrl := 'http://embed.livebox.cz/iprimaplay/flash/LiveboxPlayer.swf'; // + '?nocache=' + UnixTime;
        Self.TcUrl := MovieUrl;
        Self.PageUrl := GetMovieInfoUrl;
        Result := True;
        SetPrepared(True);
        end;
    finally
      FreeAndNil(PlayerHttp);
      end;
end;

{$ENDIF}

initialization
  RegisterDownloader(TDownloader_iPrima);
  //RegisterDownloader(TDownloader_iPrima_Stream);
  //RegisterDownloader(TDownloader_iPrima_LiveBox);

end.
