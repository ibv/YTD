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

unit downMpora;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Mpora = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uCompatibility,
  uDownloadClassifier,
  uMessages;

// http://video.mpora.com/watch/xfnGGmZDC/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*video\.mpora\.com/watch/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Mpora }

class function TDownloader_Mpora.Provider: string;
begin
  Result := 'Mpora.com';
end;

class function TDownloader_Mpora.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Mpora.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := True;
end;

destructor TDownloader_Mpora.Destroy;
begin
  inherited;
end;

function TDownloader_Mpora.GetMovieInfoUrl: string;
begin
  Result := 'http://api.mpora.com/tv/player/load/vid/' + MovieID + '/';
end;

function TDownloader_Mpora.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Title, Info: string;
    InfoXml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'configuration/title', Title) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
  else if not DownloadXml(Http, 'http://api.mpora.com/tv/player/playlist/vid/' + MovieID + '/', Info, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlAttr(InfoXml, 'channel/item/enclosure', 'url', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetName(Title);
        MovieUrl := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      FreeAndNil(InfoXml);
      end;
end;

function TDownloader_Mpora.GetFileNameExt: string;
begin
  // MovieURL: 'http://cdn2.video.mporatrons.com/play/video/xfnGGmZDC/mp4/'
  if Prepared then
    Result := '.' + ExtractFileName(ExcludeTrailingPathDelimiter(StringReplace(MovieURL, '/', '\', [rfReplaceAll])))
  else
    Result := inherited GetFileNameExt;
end;

initialization
  RegisterDownloader(TDownloader_Mpora);

end.
