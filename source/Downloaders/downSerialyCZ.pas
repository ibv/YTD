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

unit downSerialyCZ;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downZkoukniTo;

type
  TDownloader_SerialyCZ = class(TDownloader_ZkoukniTo)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieID(const Page: string; out ID: string): boolean; override;
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

// http://www.serialycz.cz/2011/01/chuck-04x12/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*serialycz\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_ID = '<param\s+name="movie"\s+value="[^"]*[?&]vid=(?P<ID>[0-9]+)';

{ TDownloader_SerialyCZ }

class function TDownloader_SerialyCZ.Provider: string;
begin
  Result := 'SerialyCZ.cz';
end;

class function TDownloader_SerialyCZ.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_SerialyCZ.Create(const AMovieID: string);
begin
  inherited;
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  RegExFreeAndNil(MovieIDRegExp);
  MovieIDRegExp := RegExCreate(REGEXP_EXTRACT_ID);
end;

destructor TDownloader_SerialyCZ.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_SerialyCZ.GetMovieInfoUrl: string;
begin
  Result := 'http://www.serialycz.cz/' + MovieID;
end;

function TDownloader_SerialyCZ.GetMovieID(const Page: string; out ID: string): boolean;
begin
  Result := GetRegExpVar(MovieIDRegExp, Page, 'ID', ID);
end;

initialization
  RegisterDownloader(TDownloader_SerialyCZ);

end.
