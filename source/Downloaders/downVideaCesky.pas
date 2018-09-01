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

unit downVideaCesky;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_VideaCesky = class(TNestedDownloader)
    private
    protected
      NestedUrlRegExps: array of TRegExp;
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

// http://www.videacesky.cz/serialy/upoutavka-na-treti-radu-the-guild
// http://www.videacesky.cz/autori/Jandis/videa/BeerNation.flv
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videacesky\.cz/(?!autori/[^/?&]*/videa/)[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>[^<]*?)\s*-\s*Videa\s*Èesky';
  REGEXP_EXTRACT_NESTED_URLS: array[0..4] of string
    = ('\sflashvars="(?:[^"]*&amp;)?file=(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?file=(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<param\s+name="movie"\s+value="(?P<URL>https?://.+?)"',
       '<embed\s+[^>]*\sflashvars="(?:[^"]*&amp;)?file=(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<embed\s+[^>]*\ssrc="(?P<URL>https?[^"]+?)"');
  {$IFDEF SUBTITLES}
  REGEXP_EXTRACT_SUBTITLE_URLS: array[0..2] of string
    = ('\sflashvars="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)',
       '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)',
       '<embed\s+[^>]*\sflashvars="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)');
  {$ENDIF}

{ TDownloader_VideaCesky }

class function TDownloader_VideaCesky.Provider: string;
begin
  Result := 'VideaCesky.cz';
end;

class function TDownloader_VideaCesky.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_VideaCesky.Create(const AMovieID: string);
var i: integer;
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  SetLength(NestedUrlRegExps, Length(REGEXP_EXTRACT_NESTED_URLS));
  for i := 0 to Pred(Length(REGEXP_EXTRACT_NESTED_URLS)) do
    NestedUrlRegExps[i] := RegExCreate(REGEXP_EXTRACT_NESTED_URLS[i]);
  {$IFDEF SUBTITLES}
  SetLength(fSubtitleUrlRegExps, Length(REGEXP_EXTRACT_SUBTITLE_URLS));
  for i := 0 to Pred(Length(REGEXP_EXTRACT_SUBTITLE_URLS)) do
    fSubtitleUrlRegExps[i] := RegExCreate(REGEXP_EXTRACT_SUBTITLE_URLS[i]);
  {$ENDIF}
end;

destructor TDownloader_VideaCesky.Destroy;
var i: integer;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  for i := 0 to Pred(Length(NestedUrlRegExps)) do
    RegExFreeAndNil(NestedUrlRegExps[i]);
  {$IFDEF SUBTITLES}
  for i := 0 to Pred(Length(fSubtitleUrlRegExps)) do
    RegExFreeAndNil(fSubtitleUrlRegExps[i]);
  SetLength(fSubtitleUrlRegExps, 0);
  {$ENDIF}
  inherited;
end;

function TDownloader_VideaCesky.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videacesky.cz/dummy/' + MovieID;
end;

function TDownloader_VideaCesky.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
begin
  Result := False;
  try
    for i := 0 to Pred(Length(NestedUrlRegExps)) do
      begin
      NestedUrlRegExp := NestedUrlRegExps[i];
      if inherited AfterPrepareFromPage(Page, PageXml, Http) then
        begin
        Result := True;
        Break;
        end;
      end;
  finally
    NestedUrlRegExp := nil;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VideaCesky);

end.
