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

unit xxxPornHub;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader, xxxPornHubEmbed;

type
  TDownloader_PornHub = class(TDownloader_PornHubEmbed)
    private
    protected
      MovieIDRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornhub\.com/view_video\.php\?(?:.*&)?viewkey=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="video-title-nf"[^>]+>\s*<h1>(?P<TITLE>.+?)</h1>';
  REGEXP_MOVIE_ID = '<input\s+type="hidden"\s+id="video_[0-9+]"\s+value="(?P<ID>[0-9]+)"';

{ TDownloader_PornHub }

class function TDownloader_PornHub.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_PornHub.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := False;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
end;

destructor TDownloader_PornHub.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_PornHub.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhub.com/view_video.php?viewkey=' + MovieID;
end;

function TDownloader_PornHub.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var ID, Title, Info: string;
    InfoXml: TXmlDoc;
begin
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, 'http://www.pornhub.com/embed_player.php?id=' + ID, Info, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      Result := inherited AfterPrepareFromPage(Info, InfoXml, Http);
      if Result and GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', Title) then
        SetName(Title);
    finally
      FreeAndNil(InfoXml);
      end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornHub);
  {$ENDIF}

end.
