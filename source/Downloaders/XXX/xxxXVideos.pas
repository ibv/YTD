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

unit xxxXVideos;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XVideos = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashMovieUrlRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xvideos\.com/video';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>\s*(?P<TITLE>.*?)\s*-\s*XVIDEOS.COM\s*</title>';
  REGEXP_FLASHVARS = '<embed\s(?:[^>]+\s)*flashvars="(?P<VARS>.*?)"';
  REGEXP_FLASHMOVIEURL = '[?&]flv_url=(?P<URL>[^&]+)';

{ TDownloader_XVideos }

class function TDownloader_XVideos.Provider: string;
begin
  Result := 'XVideos.com';
end;

class function TDownloader_XVideos.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_XVideos.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUnknown;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashMovieUrlRegExp := RegExCreate(REGEXP_FLASHMOVIEURL);
end;

destructor TDownloader_XVideos.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashMovieUrlRegExp);
  inherited;
end;

function TDownloader_XVideos.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xvideos.com/video' + MovieID + '/';
end;

function TDownloader_XVideos.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'VARS', FlashVars) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else
    begin
    FlashVars := HtmlDecode(FlashVars);
    if not GetRegExpVar(FlashMovieUrlRegExp, FlashVars, 'URL', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := UrlDecode(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XVideos);
  {$ENDIF}

end.
