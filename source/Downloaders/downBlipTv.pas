(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit downBlipTv;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BlipTv = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: TRegExp;
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

// http://blip.tv/play/hIVV4sNUAg
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*blip\.tv/play/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_URL = '[?&]file=http(?:%3A%2F%2F|://)(?:www\.)?blip\.tv(?:%2F|/)rss(?:%2F|/)flash(?:%2F|/)(?P<ID>[0-9]+)';

{ TDownloader_BlipTv }

class function TDownloader_BlipTv.Provider: string;
begin
  Result := 'Blip.tv';
end;

class function TDownloader_BlipTv.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BlipTv.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  InfoPageIsXml := True;
  MovieIdFromUrlRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTv.Destroy;
begin
  RegExFreeAndNil(MovieIdFromUrlRegExp);
  inherited;
end;

function TDownloader_BlipTv.GetMovieInfoUrl: string;
var Http: THttpSend;
    Url, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    Http.Document.Clear;
    if Http.HttpMethod('GET', 'http://blip.tv/play/' + MovieID) then
      if CheckRedirect(Http, Url) then
        if GetRegExpVar(MovieIdFromUrlRegExp, Url, 'ID', ID) then
          Result := 'http://blip.tv/rss/flash/' + ID;
  finally
    Http.Free;
    end;
end;

function TDownloader_BlipTv.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Title, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'channel/item/media:title', Title) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
  else if not GetXmlAttr(PageXml, 'channel/item/media:group/media:content', 'url', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    SetName(Title);
    MovieURL := Url;
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_BlipTV);

end.
