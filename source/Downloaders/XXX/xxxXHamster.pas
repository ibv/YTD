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

unit xxxXHamster;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XHamster = class(THttpDownloader)
    private
    protected
      MovieServerRegExp: TRegExp;
      MovieFileNameRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xhamster\.com/movies/';
  URLREGEXP_ID =        '[0-9]+/.*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_MOVIE_SERVER = '''srv''\s*:\s*''(?P<SERVER>https?://[^'']+)''';
  REGEXP_MOVIE_FILENAME = '''file''\s*:\s*''(?P<FILENAME>[0-9]+:[^'']+)''';

{ TDownloader_XHamster }

class function TDownloader_XHamster.Provider: string;
begin
  Result := 'XHamster.com';
end;

class function TDownloader_XHamster.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_XHamster.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieServerRegExp := RegExCreate(REGEXP_MOVIE_SERVER, [rcoIgnoreCase, rcoSingleLine]);
  MovieFileNameRegExp := RegExCreate(REGEXP_MOVIE_FILENAME, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_XHamster.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieServerRegExp);
  RegExFreeAndNil(MovieFileNameRegExp);
  inherited;
end;

function TDownloader_XHamster.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xhamster.com/movies/' + MovieID;
end;

function TDownloader_XHamster.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Server, FileName: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieFileNameRegExp, Page, 'FILENAME', FileName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['file']))
  else if not GetRegExpVar(MovieServerRegExp, Page, 'SERVER', Server) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['srv']))
  else
    begin
    MovieUrl := Server + '/flv2/' + FileName;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XHamster);
  {$ENDIF}

end.
