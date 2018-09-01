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

unit downUniMinnesota;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_UniMinnesota = class(TRtmpDownloader)
    private
    protected
      InstanceIdRegExp: TRegExp;
      VideoIdRegExp: TRegExp;
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

// http://www.ima.umn.edu/videos/?id=1187
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ima\.umn\.edu/videos/.*?[?&]id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+style=''color:#fff;''>\s*<b>(?P<TITLE>.*?)</b></div>';
  REGEXP_INSTANCE_ID = '<param\s+name="movie"\s+value="[^"]*[?&]instance=(?P<ID>[^"&]+)';
  REGEXP_VIDEO_ID = '<param\s+name="movie"\s+value="[^"]*[?&]video=(?P<ID>[^"&]+)';

{ TDownloader_UniMinnesota }

class function TDownloader_UniMinnesota.Provider: string;
begin
  Result := 'ima.umn.edu';
end;

class function TDownloader_UniMinnesota.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_UniMinnesota.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID, [rcoIgnoreCase, rcoSingleLine]);
  InstanceIdRegExp := RegExCreate(REGEXP_INSTANCE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_UniMinnesota.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoIdRegExp);
  RegExFreeAndNil(InstanceIdRegExp);
  inherited;
end;

function TDownloader_UniMinnesota.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ima.umn.edu/videos/?id=' + MovieID;
end;

function TDownloader_UniMinnesota.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var VideoID, InstanceID, BaseUrl, Path: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', VideoID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(InstanceIdRegExp, Page, 'ID', InstanceID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, 'http://www.ima.umn.edu/videos/xml/' + InstanceID + '/' + VideoID + '.xml', Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlAttr(Xml, 'head/meta', 'base', BaseUrl) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlAttr(Xml, 'body/video', 'src', Path) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := BaseUrl + Path;
        AddRtmpDumpOption('r', MovieURL);
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_UniMinnesota);

end.
