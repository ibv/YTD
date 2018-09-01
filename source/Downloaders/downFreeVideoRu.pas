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

unit downFreeVideoRu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeVideoRu = class(THttpDownloader)
    private
    protected
      VideoContextRegExp: TRegExp;
      HQVidUrlRegExp: TRegExp;
      VidUrlRegExp: TRegExp;
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

// http://freevideo.ru/video/view/?id=v14445361101&highquality=1
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*freevideo\.ru/video/view/?\?id=';
  URLREGEXP_ID =        'v[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div class="Title">(?P<TITLE>.*?)</div>';
  REGEXP_VIDEO_CONTEXT = '\.addVariable\s*\(\s*''context''\s*,\s*"(?P<CONTEXT>.*?)"';
  REGEXP_VIDEO_URL_HQ = '[{,]\s*"_vidURL_hq"\s*:\s*"(?P<URL>https?:.*?)"';
  REGEXP_VIDEO_URL = '[{,]\s*"_vidURL"\s*:\s*"(?P<URL>https?:.*?)"';

{ TDownloader_FreeVideoRu }

class function TDownloader_FreeVideoRu.Provider: string;
begin
  Result := 'FreeVideo.ru';
end;

class function TDownloader_FreeVideoRu.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_FreeVideoRu.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  VideoContextRegExp := RegExCreate(REGEXP_VIDEO_CONTEXT);
  HQVidUrlRegExp := RegExCreate(REGEXP_VIDEO_URL_HQ);
  VidUrlRegExp := RegExCreate(REGEXP_VIDEO_URL);
end;

destructor TDownloader_FreeVideoRu.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoContextRegExp);
  RegExFreeAndNil(HQVidUrlRegExp);
  RegExFreeAndNil(VidUrlRegExp);
  inherited;
end;

function TDownloader_FreeVideoRu.GetMovieInfoUrl: string;
begin
  Result := 'http://freevideo.ru/video/view/?id=' + MovieID + '&highquality=1';
end;

function TDownloader_FreeVideoRu.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Context, Request, Info, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoContextRegExp, Page, 'CONTEXT', Context) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else
    begin
    Request := 'video_url=1&context=' + UrlEncode(UrlDecode(Context)) + '&p_id[1]=4&devid=LoadupFlashPlayer&begun=1&p_id[0]=2&ticket=' + MovieID;
    if not DownloadPage(Http, 'http://freevideo.ru/video/view/url/bot/?' + Request, Info) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else if not (GetRegExpVar(HQVidUrlRegExp, Info, 'URL', Url) or GetRegExpVar(VidUrlRegExp, Info, 'URL', Url)) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieUrl := StripSlashes(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_FreeVideoRu);

end.
