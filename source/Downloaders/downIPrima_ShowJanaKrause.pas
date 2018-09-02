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

unit downiPrima_ShowJanaKrause;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_ShowJanaKrause = class(TRtmpDownloader)
    private
    protected
      MediaIDRegExp: TRegExp;
      RtmpUrlRegExp: TRegExp;
      {$IFDEF MULTIDOWNLOADS}
      IDList: TStringList;
      DownloadIndex: integer;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
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

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.iprima.cz/showjanakrause/videoarchiv-brezen-2011/
const
  URLREGEXP_BEFORE_ID = 'iprima\.cz/showjanakrause/videoarchiv';
  URLREGEXP_ID =        REGEXP_ANYTHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MEDIA_ID = '\bLiveboxPlayer\.init\s*\((?:\s*''[^'']*''\s*,){3}\s*''(?P<HQ>[^'']*)''\s*,\s*''(?P<LQ>[^'']*)''';
  REGEXP_RTMP_URL = '\bvar\s*flashvars\s*=\s*\{\s*stream\s*:\s*''(?P<RTMP>rtmpt?e?://.+?)''';

const
  PLAYER_URL = 'http://embed.livebox.cz/iprima/player.js';

{ TDownloader_ShowJanaKrause }

class function TDownloader_ShowJanaKrause.Provider: string;
begin
  Result := 'iPrima.cz/showjanakrause';
end;

class function TDownloader_ShowJanaKrause.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_ShowJanaKrause.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfPreferRtmpLiveStream];
end;

constructor TDownloader_ShowJanaKrause.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MediaIDRegExp := RegExCreate(REGEXP_MEDIA_ID);
  RtmpUrlRegExp := RegExCreate(REGEXP_RTMP_URL);
  {$IFDEF MULTIDOWNLOADS}
  IDList := TStringList.Create;
  {$ENDIF}
end;

destructor TDownloader_ShowJanaKrause.Destroy;
begin
  RegExFreeAndNil(MediaIDRegExp);
  RegExFreeAndNil(RtmpUrlRegExp);
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(IDList);
  {$ENDIF}
  inherited;
end;

function TDownloader_ShowJanaKrause.GetMovieInfoUrl: string;
begin
  Result := 'http://www.iprima.cz/showjanakrause/videoarchiv' + MovieID;
end;

function TDownloader_ShowJanaKrause.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Player, RtmpUrl, ID: string;
    PlayerHttp: THttpSend;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  {$IFDEF MULTIDOWNLOADS}
  IDList.Clear;
  {$ENDIF}
  PlayerHttp := CreateHttp;
  try
    PlayerHttp.Cookies.Assign(Http.Cookies);
    PlayerHttp.Headers.Add('Referer: ' + GetMovieInfoUrl);
    if not DownloadPage(PlayerHttp, PLAYER_URL, Player, peUnknown, hmGET, False) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else if not GetRegExpVar(RtmpUrlRegExp, Player, 'RTMP', RtmpUrl) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
    else if MediaIDRegExp.Match(Page) then
      repeat
        ID := MediaIDRegExp.SubexpressionByName('HQ');
        if ID = '' then
          ID := MediaIDRegExp.SubexpressionByName('LQ');
        if ID <> '' then
          begin
          MovieUrl := {RtmpUrl + ' ' +} ID;
          Self.RtmpUrl := RtmpUrl;
          Self.Playpath := ID;
          SetPrepared(True);
          Result := True;
          {$IFDEF MULTIDOWNLOADS}
          IDList.Add(ID);
          {$ELSE}
          Break;
          {$ENDIF}
          end;
      until not MediaIDRegExp.MatchAgain;
  finally
    FreeAndNil(PlayerHttp);
    end;
end;

{$IFDEF MULTIDOWNLOADS}
function TDownloader_ShowJanaKrause.First: boolean;
begin
  Result := False;
  if Prepared then
    if Self.RtmpUrl <> '' then
      if IDList.Count > 0 then
        begin
        DownloadIndex := -1;
        Result := Next;
        end;
end;

function TDownloader_ShowJanaKrause.Next: boolean;
begin
  Result := False;
  if Prepared then
    if Self.RtmpUrl <> '' then
      begin
      DownloadIndex := Succ(DownloadIndex);
      if (DownloadIndex >= 0) and (DownloadIndex < IDList.Count) then
        begin
        SetName(ChangeFileExt(IDList[DownloadIndex], ''));
        Self.Playpath := IDList[DownloadIndex];
        SetFileName('');
        Result := True;
        end;
      end;
end;
{$ENDIF}

initialization
  RegisterDownloader(TDownloader_ShowJanaKrause);

end.
