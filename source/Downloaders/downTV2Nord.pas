(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit downTV2Nord;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_TV2Nord = class(TRtmpDownloader)
    private
    protected
      ServerRegExp: TRegExp;
      StreamRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.tv2nord.dk/arkiv/2011/11/10?video_id=30106&autoplay=1
const
  URLREGEXP_BEFORE_ID = 'tv2nord\.dk/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="video-archive">.*?<h2\s+class="title">\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_MOVIE_SERVER = '\brtmpt?e?\s*:\s*\{.*?\bnetConnectionUrl\s*:\s*''(?P<SERVER>.+?)''';
  REGEXP_MOVIE_STREAM = '\.TV2RegionFlowplayer\s*\(*s*''(?P<STREAM>.+?)''';

{ TDownloader_TV2Nord }

class function TDownloader_TV2Nord.Provider: string;
begin
  Result := 'TV2Nord.dk';
end;

class function TDownloader_TV2Nord.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_TV2Nord.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peAnsi;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  ServerRegExp := RegExCreate(REGEXP_MOVIE_SERVER);
  StreamRegExp := RegExCreate(REGEXP_MOVIE_STREAM);
end;

destructor TDownloader_TV2Nord.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(ServerRegExp);
  RegExFreeAndNil(StreamRegExp);
  inherited;
end;

function TDownloader_TV2Nord.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tv2nord.dk/' + MovieID;
end;

function TDownloader_TV2Nord.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Settings, Server, Stream: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(StreamRegExp, Page, 'STREAM', Stream) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else if not DownloadPage(Http, 'http://www.tv2nord.dk/themes/tv2nord_theme/js/flowplayer-controls.js', Settings) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(ServerRegExp, Settings, 'SERVER', Server) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else
    begin
    Self.RtmpUrl := Server;
    Self.Playpath := 'mp4:' + Stream + '_2000.mp4';
    //Self.FlashVer := FLASH_DEFAULT_VERSION;
    //Self.SwfUrl := 'http://img2.ceskatelevize.cz/libraries/player/flashPlayer.swf?version=1.4.23';
    //Self.TcUrl := BaseUrl;
    //Self.PageUrl := MovieID;
    MovieUrl := Server + '/' + PlayPath;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TV2Nord);

end.
