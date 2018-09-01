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

unit downBreakEmbedV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBreakEmbed;

type
  TDownloader_BreakEmbedV2 = class(TDownloader_BreakEmbed)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod = hmGET): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://embed.break.com/MzU5NDE4
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*embed\.break\.com/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '$';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';

{ TDownloader_BreakEmbedV2 }

class function TDownloader_BreakEmbedV2.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_BreakEmbedV2.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_BreakEmbedV2.Destroy;
begin
  inherited;
end;

function TDownloader_BreakEmbedV2.GetMovieInfoUrl: string;
begin
  Result := 'http://embed.break.com/' + MovieID;
end;

function TDownloader_BreakEmbedV2.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  // I don't need to download anything, but do I need to get the redirected URL
  // which contains all required info
  Xml := nil;
  Result := DownloadPage(Http, Url, hmHEAD);
  if Result then
    Page := LastUrl;
end;

initialization
  RegisterDownloader(TDownloader_BreakEmbedV2);

end.
