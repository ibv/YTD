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

unit downFishki;
{$INCLUDE 'ytd.inc'}
{$DEFINE DONT_READ_TITLE}
  // Problem: Title is in russian and would need to be transliterated somehow

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Fishki = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      {$IFDEF DONT_READ_TITLE}
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
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

// http://fishki.net/comment.php?id=79682
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*fishki\.net/comment\.php\?(?:.*?&)*id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  {$IFNDEF DONT_READ_TITLE}
  REGEXP_EXTRACT_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  {$ENDIF}
  REGEXP_EXTRACT_URL = '<param\s+name="movie"\s+value="[^"]*[?&]file=(?P<URL>https?://[^"&]+)';

{ TDownloader_Fishki }

class function TDownloader_Fishki.Provider: string;
begin
  Result := 'Fishki.net';
end;

class function TDownloader_Fishki.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Fishki.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peAnsi;
  {$IFNDEF DONT_READ_TITLE}
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Fishki.Destroy;
begin
  {$IFNDEF DONT_READ_TITLE}
  RegExFreeAndNil(MovieTitleRegExp);
  {$ENDIF}
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Fishki.GetMovieInfoUrl: string;
begin
  Result := 'http://fishki.net/comment.php?id=' + MovieID;
end;

{$IFDEF DONT_READ_TITLE}
function TDownloader_Fishki.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  SetName(ChangeFileExt(ExtractUrlFileName(MovieUrl), ''));
  Result := Prepared;
end;
{$ENDIF}

initialization
  RegisterDownloader(TDownloader_Fishki);

end.
