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

unit downMySpace;
{$INCLUDE 'ytd.inc'}
{.DEFINE MYSPACE_USES_RTMP}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, {$IFDEF MYSPACE_USES_RTMP} uRtmpDownloader {$ELSE} uHttpDownloader {$ENDIF} ;

type
  TDownloader_MySpace = class( {$IFDEF MYSPACE_USES_RTMP} TRtmpDownloader {$ELSE} THttpDownloader {$ENDIF} )
    private
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

// http://vids.myspace.com/index.cfm?fuseaction=vids.individual&videoid=63620005
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*vids\.myspace\.com/.*?[?&]videoid=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_MySpace }

class function TDownloader_MySpace.Provider: string;
begin
  Result := 'MySpace.com';
end;

class function TDownloader_MySpace.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MySpace.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := True;
end;

destructor TDownloader_MySpace.Destroy;
begin
  inherited;
end;

function TDownloader_MySpace.GetMovieInfoUrl: string;
begin
  Result := 'http://mediaservices.myspace.com/services/rss.ashx?type=video&videoID=' + MovieID;
end;

function TDownloader_MySpace.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Title, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'channel/item/title', Title) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
  else if not GetXmlAttr(PageXml, {$IFDEF MYSPACE_USES_RTMP} 'channel/item/myspace:RTMPE' {$ELSE} 'channel/item/media:content' {$ENDIF} , 'url', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    SetName(Title);
    {$IFDEF MYSPACE_USES_RTMP}
      // Note: Url is somewhat incorrect, MySpace uses protocol "rtmp" while in fact it should be "rtmpe"
      MovieURL := StringReplace(Url, 'rtmp://', 'rtmpe://', [rfIgnoreCase]);
      // Download
      AddRtmpDumpOption('r', MovieURL);
    {$ELSE}
      MovieURL := Url;
    {$ENDIF}
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_MySpace);

end.
