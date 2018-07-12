(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
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

unit downLibimSeTi;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LibimSeTi = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
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

// http://video.libimseti.cz/basket-trosku-jinak/f1ea1c7fcc03?uid=
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*libimseti\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h3\s+class="big">(?P<TITLE>.*?)</h3>';
  REGEXP_MOVIE_URL = '\.addParam\s*\(\s*''file''\s*,\s*''(?P<URL>https?://videostream\.libimseti\.cz/[^'']+)''';

{ TDownloader_LibimSeTi }

class function TDownloader_LibimSeTi.Provider: string;
begin
  Result := 'LibimSeTi.cz';
end;

class function TDownloader_LibimSeTi.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_LibimSeTi.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_LibimSeTi.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_LibimSeTi.GetMovieInfoUrl: string;
begin
  Result := 'http://video.libimseti.cz/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_LibimSeTi);

end.
