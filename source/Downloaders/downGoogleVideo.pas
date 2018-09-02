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

unit downGoogleVideo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downGoogleVideo_Embed;

type
  TDownloader_GoogleVideo = class(TNestedDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetNestedID(out ID: string): boolean; override;
      function CreateNestedDownloaderFromID(const MovieID: string): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://video.google.com/videoplay?docid=-9077214414651731007&ei=y-8dS-n9JZ6q2wK7o-CRCA
const
  URLREGEXP_BEFORE_ID = 'video\.google\.com/videoplay\?';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = REGEXP_TITLE_TITLE;

{ TDownloader_GoogleVideo }

class function TDownloader_GoogleVideo.Provider: string;
begin
  Result := TDownloader_GoogleVideo_Embed.Provider;
end;

class function TDownloader_GoogleVideo.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_GoogleVideo.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
end;

destructor TDownloader_GoogleVideo.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_GoogleVideo.GetMovieInfoUrl: string;
begin
  Result := 'http://video.google.com/videoplay?' + MovieID;
end;

function TDownloader_GoogleVideo.CreateNestedDownloaderFromID(const MovieID: string): boolean;
begin
  Result := CreateNestedDownloaderFromDownloader(TDownloader_GoogleVideo_Embed.Create(MovieID));
end;

function TDownloader_GoogleVideo.GetNestedID(out ID: string): boolean;
begin
  ID := MovieID;
  Result := True;
end;

initialization
  RegisterDownloader(TDownloader_GoogleVideo);

end.

