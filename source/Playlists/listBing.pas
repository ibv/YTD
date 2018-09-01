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

unit listBing;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader, listHTML;

type
  TPlaylist_Bing = class(TPlaylist_HTML)
    private
    protected
      function GetUrlRegExp: string; override;
      function GetMovieInfoUrl: string; override;
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

// http://www.bing.com/videos/watch/video/surfer-mounts-hd-camera-to-board/1abwgqhhl
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bing\.com/videos/watch/video/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_EXTRACT_URL = '\.writeFlash\s*\(\s*''[^'']+''\s*,\s*''(?P<URL>https?://.*?)''';

{ TPlaylist_Bing }

class function TPlaylist_Bing.Provider: string;
begin
  Result := 'Bing.com';
end;

class function TPlaylist_Bing.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + ClassName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylist_Bing.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
end;

destructor TPlaylist_Bing.Destroy;
begin
  inherited;
end;

function TPlaylist_Bing.GetUrlRegExp: string;
begin
  Result := REGEXP_EXTRACT_URL;
end;

function TPlaylist_Bing.GetMovieInfoUrl: string;
begin
  Result := 'http://www.bing.com/videos/watch/video/dummy/' + MovieID;
end;

initialization
  RegisterDownloader(TPlaylist_Bing);

end.
