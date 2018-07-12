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

unit downClipFishV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downClipfish;

type
  TDownloader_ClipfishV2 = class(TDownloader_Clipfish)
    private
    protected
      MovieIDFromPageRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.clipfish.de/special/lets-dance/home/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*clipfish\.de/special/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_PAGE = '/devxml/videoinfo/(?P<MOVIEID>[0-9]+)';

{ TDownloader_ClipfishV2 }

class function TDownloader_ClipfishV2.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_ClipfishV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PAGE);
end;

destructor TDownloader_ClipfishV2.Destroy;
begin
  RegExFreeAndNil(MovieIDFromPageRegExp);
  inherited;
end;

function TDownloader_ClipfishV2.GetMovieInfoUrl: string;
var Http: THttpSend;
    Page, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://www.clipfish.de/special/' + MovieID, Page) then
      if GetRegExpVar(MovieIDFromPageRegExp, Page, 'MOVIEID', ID) then
        begin
        MovieID := ID;
        Result := inherited GetMovieInfoUrl;
        end;
  finally
    Http.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_ClipfishV2);

end.
