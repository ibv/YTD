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

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader,
  uHttpDownloader;

type
  TDownloader_iPrima = class(TRtmpDownloader)
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

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://play.iprima.cz/all/233409
// http://play.iprima.cz/iprima/233409
const
  URLREGEXP_BEFORE_ID = 'iprima\.cz/';
  URLREGEXP_ID =        '(?!showjanakrause/).+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_META_OGTITLE;
  REGEXP_LIVEBOX = '"hq_id"\s*:\s*"(?P<HQ>[^"]*)"\s*,\s*"lq_id"\s*:\s*"(?P<LQ>[^"]*)"(?:.*?"zoneGEO"\s*:\s*(?P<ZONEGEO>[0-9]*))?';
  REGEXP_LIVEBOX_AUTH = '\?auth=(?:''\s*\+\s*(?P<QUOTE>[''"])(?P=QUOTE)\s*\+\s*'')?(?P<AUTH>.*?)''';
  REGEXP_LIVEBOX_URLPREFIX = '\biph_full_url\s*=\s*''(?P<URL>https?://.+?)''';

{ TDownloader_iPrima }

class function TDownloader_iPrima.Provider: string;
begin
  Result := 'iPrima.cz';
end;

class function TDownloader_iPrima.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_iPrima.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

constructor TDownloader_iPrima.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  LiveBoxRegExp := RegExCreate(REGEXP_LIVEBOX);
  LiveBoxAuthRegExp := RegExCreate(REGEXP_LIVEBOX_AUTH);
  LiveBoxUrlPrefix := RegExCreate(REGEXP_LIVEBOX_URLPREFIX);
end;

destructor TDownloader_iPrima.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(LiveBoxRegExp);
  RegExFreeAndNil(LiveBoxAuthRegExp);
  RegExFreeAndNil(LiveBoxUrlPrefix);
  inherited;
end;

function TDownloader_iPrima.GetMovieInfoUrl: string;
begin
  Result := Format('http://play.iprima.cz/%s', [MovieID]);
end;

function TDownloader_iPrima.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  PlayerHttp: THttpSend;
  EmbedPlayer, Auth, HQStream, LQStream, ZoneGEO, Geo, Stream, Url: string;
begin
  // Poznamka: Balutbj zjistil, ze krome LQ a HQ existuje jeste vyssi kvalita "HD".
  // Problem je v tom, ze se do ni neda dostat primo ze stranek, je potreba hacknout
  // HQStream - pokud normalni HQStream je 'Prima-1210210000-21275_1000.mp4', tak
  // odpovidajici HD stream je 'hq/Prima-1210210000-21275_1500.mp4' (tzn. prefix
  // 'hq/' a z 1000 je 1500. Jenze to funguje jen u nekterych videi a neda se to
  // zjistit jinak nez tak, ze to zkusim stahnout...
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
        MovieUrl := Url {'rtmp://bcastlw.livebox.cz:80/iprima_token'} + Geo + '?auth=' + Auth; // '?auth=_any_|1331380805|e0bdc430140646104fb9509b5c76791c78da9ce7';
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

initialization
  RegisterDownloader(TDownloader_iPrima);

end.
