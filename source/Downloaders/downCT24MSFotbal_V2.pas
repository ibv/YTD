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

unit downCT24MSFotbal_V2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT24MSFotbal;

type
  TDownloader_CT24MSFotbal_V2 = class(TDownloader_CT24MSFotbal)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://msfotbal.ct24.cz/video.asp?video_id=95
// http://msfotbal.ct24.cz/video.asp
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*msfotbal\.ct24\.cz/';
  URLREGEXP_ID =        'video\.asp(?:\?video_id=[0-9]+)?';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';

const
  SOAP_URL = 'http://ctdir.visual.cz/ivysilani/services/streaming/SLP.asmx';
  SOAP_ACTION = 'http://ivysilani.visual.cz/services/GetPlaylistUrl';
  SOAP_REQUEST = '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
                   '<s:Body>' +
                     '<GetPlaylistUrl xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ivysilani.visual.cz/services">' +
                       '<request>' +
                         '<Format>%s</Format>' +
                         '<ClientAddress>%s</ClientAddress>' +
                         '<Expiration>%s</Expiration>' +
                         '<Playlist>' +
                           '<PlaylistItem>' +
                             '<Type>Archive</Type>' +
                             '<Identifier>%s</Identifier>' +
                             '<Begin>0</Begin>' +
                             '<Duration i:nil="true" /> ' +
                             '<NoSkip i:nil="true" /> ' +
                           '</PlaylistItem>' +
                         '</Playlist>' +
                       '</request>' +
                     '</GetPlaylistUrl>' +
                   '</s:Body>' +
                 '</s:Envelope>';

{ TDownloader_CT24MSFotbal_V2 }

class function TDownloader_CT24MSFotbal_V2.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_CT24MSFotbal_V2.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
end;

destructor TDownloader_CT24MSFotbal_V2.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_CT24MSFotbal_V2.GetMovieInfoUrl: string;
begin
  Result := 'http://msfotbal.ct24.cz/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_CT24MSFotbal_V2);

end.
