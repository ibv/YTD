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

unit downAnimacekTv;
{$INCLUDE 'ytd.inc'}

{
  Pouziva token, stejny jako Barrandov
}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_AnimacekTv = class(TRtmpDownloader)
    private
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

// http://www.animacek.tv/porady/pokemon/103-epizoda-52
const
  URLREGEXP_BEFORE_ID = 'animacek\.tv/porady/[^/]+/';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_AnimacekTv }

class function TDownloader_AnimacekTv.Provider: string;
begin
  Result := 'Animacek.tv';
end;

class function TDownloader_AnimacekTv.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_AnimacekTv.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  InfoPageIsXml := True;
end;

destructor TDownloader_AnimacekTv.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_AnimacekTv.GetMovieInfoUrl: string;
begin
  Result := 'http://www.animacek.tv/api/video/' + MovieID;
end;

function TDownloader_AnimacekTv.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Server, Stream, Title: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'mediainfo/file', Stream) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else if not GetXmlVar(PageXml, 'mediainfo/host', Server) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else
    begin
    if GetXmlAttr(PageXml, 'mediainfo', 'name', Title) then
      Name := Title;
    Server := 'rtmpe://' + Server;
    MovieUrl := Server + '/' + Stream;
    Self.RtmpUrl := Server;
    Self.Playpath := Stream;
    //Self.FlashVer := FLASH_DEFAULT_VERSION;
    //Self.SwfUrl := 'http://www.animacek.tv/Content/flash/uniplayer.swf?itemid=' + MovieID;
    //Self.TcUrl := Server;
    //Self.PageUrl := GetMovieInfoUrl; //'http://www.animacek.tv/porady/pokemon/103-epizoda-52';
    SetPrepared(True);
    Result := True;
    end;
end;

class function TDownloader_AnimacekTv.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfAcceptSecureToken, dfRequireSecureToken];
end;

initialization
  RegisterDownloader(TDownloader_AnimacekTv);

end.
