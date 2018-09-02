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

unit downMusicStreamCz;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, downStream;

type
  TDownloader_MusicStreamCz = class(TDownloader_Stream)
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
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://music.stream.cz/klip/268709-avril-lavigne-what-the-hell
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*music\.stream\.cz/klip/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_PARAMS = '\bwriteSWF\s*\((?P<PARAM>.+?)\)\s*;';
  REGEXP_FLASHVARS_PARSER = '[''&](?P<VARNAME>.+?)=(?P<VARVALUE>.*?)(?=[''&])';

{ TDownloader_MusicStreamCz }

class function TDownloader_MusicStreamCz.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_MusicStreamCz.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(FlashVarsParserRegExp);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS);
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER);
end;

destructor TDownloader_MusicStreamCz.Destroy;
begin
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(FlashVarsParserRegExp);
  inherited;
end;

function TDownloader_MusicStreamCz.GetMovieInfoUrl: string;
begin
  Result := 'http://music.stream.cz/klip/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_MusicStreamCz);

end.
