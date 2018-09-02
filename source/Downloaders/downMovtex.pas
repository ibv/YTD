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

unit downMovtex;
{$INCLUDE 'ytd.inc'}

{$IFDEF DELPHI6_UP}
{$MESSAGE WARN 'Nefunguje'}
{$ENDIF}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2007_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions, uCompatibility,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Movtex = class(TRtmpDownloader)
    private
    protected
      Username: string;
      Password: string;
      TitleRegExp: TRegExp;
      PlaypathRegExp: TRegExp;
      RtmpUrlRegExp: TRegExp;
      SwfUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function Features: TDownloaderFeatures; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uStrings,
  uMessages,
  uDownloadClassifier;

// https://movtex.com/video/295164/Nasa-snajka-Ep08
const
  URLREGEXP_BEFORE_ID = 'movtex\.com/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE    = REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_PLAYPATH = '<div\b[^>]*\sid="normal_player_cont"[^>]*>\s*<a\s+href="(?P<PLAYPATH>.+?)"';
  REGEXP_MOVIE_URL      = '\bnetConnectionUrl\s*:\s*''(?P<URL>rtmpt?e?://.+?)''';
  REGEXP_MOVIE_SWFURL   = '\bflowplayer\s*\(\s*"[^"]*"\s*,\s*"(?P<SWFURL>https?://.+?)"';

const
  FALLBACK_MOVIE_SWFURL = 'https://movtex.com/player/pak_player2/flowplayer.commercial-3.2.18.swf';

{ TDownloader_Movtex }

class function TDownloader_Movtex.Provider: string;
begin
  Result := 'Movtex.com';
end;

class function TDownloader_Movtex.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfRequireSecureToken, dfUserLogin];
end;

class function TDownloader_Movtex.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Movtex.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  TitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  PlaypathRegExp := RegExCreate(REGEXP_MOVIE_PLAYPATH);
  RtmpUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  SwfUrlRegExp := RegExCreate(REGEXP_MOVIE_SWFURL);
end;

destructor TDownloader_Movtex.Destroy;
begin
  FreeAndNil(TitleRegExp);
  FreeAndNil(PlaypathRegExp);
  FreeAndNil(RtmpUrlRegExp);
  FreeAndNil(SwfUrlRegExp);
  inherited;
end;

function TDownloader_Movtex.GetMovieInfoUrl: string;
begin
  Result := 'https://movtex.com/' + MovieID;
end;

function TDownloader_Movtex.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Result := False;
  if inherited GetMovieInfoContent(Http, Url, Page, Xml, Method) then
    begin
    FreeAndNil(Xml);
    if DownloadPage(Http, 'https://movtex.com/signup.php', 'username=' + {$IFDEF UNICODE} AnsiString {$ENDIF} (UrlEncode(Username)) + '&password=' + {$IFDEF UNICODE} AnsiString {$ENDIF} (UrlEncode(Password)) + '&login=Login', HTTP_FORM_URLENCODING, Page) then
      Result := inherited GetMovieInfoContent(Http, Url, Page, Xml, Method);
    end;
end;

function TDownloader_Movtex.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Playpath, Server, PlayerUrl: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(PlaypathRegExp, Page, 'PLAYPATH', Playpath) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else if not GetRegExpVar(RtmpUrlRegExp, Page, 'URL', Server) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else
    begin
    MovieUrl := Server + '/' + Playpath;
    Self.RTMPUrl := Server;
    Self.Playpath := Playpath;
    if GetRegExpVar(SwfUrlRegExp, Page, 'URL', PlayerUrl) then
      Self.SwfUrl := PlayerUrl
    else
      Self.SwfUrl := FALLBACK_MOVIE_SWFURL;
    Self.FlashVer := FLASH_DEFAULT_VERSION;
    Self.TcUrl := Server;
    Self.PageUrl := GetMovieInfoUrl;
    SetPrepared(True);
    Result := True;
    end;
end;

procedure TDownloader_Movtex.SetOptions(const Value: TYTDOptions);
begin
  inherited;
  Username := Value.ReadProviderOptionDef(Provider, OPTION_COMMONDOWNLOADER_USERNAME, '');
  Password := Value.ReadProviderOptionDef(Provider, OPTION_COMMONDOWNLOADER_PASSWORD, '');
end;

initialization
  RegisterDownloader(TDownloader_Movtex);

end.
