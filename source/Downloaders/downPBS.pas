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

unit downPBS;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_PBS = class(TNestedDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
      YouTubeIDRegExp: TRegExp;
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

// http://www.pbs.org/newshour/bb/world/jan-june11/libya_03-08.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pbs\.org/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_ID = '\bEmbedVideo\s*\(\s*''(?P<ID>[0-9]+)''';
  REGEXP_EXTRACT_YOUTUBE = '\bid_yt\s*=\s*"(?P<YOUTUBE>.+?)"';

{ TDownloader_PBS }

class function TDownloader_PBS.Provider: string;
begin
  Result := 'PBS.org';
end;

class function TDownloader_PBS.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_PBS.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_EXTRACT_ID);
  YouTubeIDRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE);
end;

destructor TDownloader_PBS.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(YouTubeIDRegExp);
  inherited;
end;

function TDownloader_PBS.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pbs.org/' + MovieID;
end;

function TDownloader_PBS.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var ID, MediaInfo, YouTubeID, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.pbs.org/newshour/scripts2/FlashPlayerDetectionKit/db_info.php?id=' + ID, MediaInfo, peAnsi) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(YouTubeIDRegExp, MediaInfo, 'YOUTUBE', YouTubeID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT))
  else
    begin
    Url := 'http://www.youtube.com/watch?v=' + YouTubeID;
    if CreateNestedDownloaderFromUrl(Url) then
      begin
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_PBS);

end.
