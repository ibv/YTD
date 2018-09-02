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

unit downStream;
{$INCLUDE 'ytd.inc'}
{.DEFINE XMLINFO}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uHttpDirectDownloader;

type
  TDownloader_Stream = class(THttpDownloader)
    private
    protected
      StreamsRegexp: TRegExp;
    protected
      function GetMovieInfoUrlForID(const ID: string): string; virtual;
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
  uStringConsts,
  {$IFDEF XMLINFO}
  uXML,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://www.stream.cz/reklamozrouti/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/video/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/object/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/profil/seznam.cz/safranjan?video_id=604024
const
  URLREGEXP_BEFORE_ID = '(?<!old\.)stream\.cz/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_STREAMS = '\{\s*"source"\s*:\s*"(?P<URL>https?://[^"]+)"\s*,\s*"type"\s*:\s*"[^"]*"\s*,\s*"quality_label"\s*:\s*"[^"]*"\s*,\s*"quality"\s*:\s*"(?P<QUALITY>\d+)[^"]*"\s*\}';

{ TDownloader_Stream }

class function TDownloader_Stream.Provider: string;
begin
  Result := 'Stream.cz';
end;

class function TDownloader_Stream.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  StreamsRegexp := RegExCreate(REGEXP_MOVIE_STREAMS);
end;

destructor TDownloader_Stream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(StreamsRegexp);
  inherited;
end;

function TDownloader_Stream.GetMovieInfoUrl: string;
begin
  Result := GetMovieInfoUrlForID(MovieID);
end;

function TDownloader_Stream.GetMovieInfoUrlForID(const ID: string): string;
begin
  Result := 'http://www.stream.cz/' + ID;
end;

function TDownloader_Stream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  BestUrl, Url, sQuality: string;
  BestQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BestUrl := '';
  BestQuality := -1;
  if GetRegExpVars(StreamsRegexp, Page, ['URL', 'QUALITY'], [@Url, @sQuality]) then
    repeat
      Quality := StrToIntDef(sQuality, 0);
      if Quality > BestQuality then
        begin
        BestUrl := Url;
        BestQuality := Quality;
        end;
    until not GetRegExpVarsAgain(StreamsRegexp, ['URL', 'QUALITY'], [@Url, @sQuality]);
  if BestUrl <> '' then
    begin
    MovieUrl := JSDecode(BestUrl);
    SetName(UrlDecode(UnpreparedName));
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Stream);

end.
