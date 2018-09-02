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

unit downTed;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Ted = class(TRtmpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

// http://www.ted.com/talks/lang/cze/beau_lotto_optical_illusions_show_how_we_see.html
const
  URLREGEXP_BEFORE_ID = 'ted\.com/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<h1>\s*<span[^>]*>\s*(?P<TITLE>.*?)\s*</span>\s*</h1>';
  REGEXP_FLASHVARS =    '\bflashVars\s*=\s*\{\s*(?P<FLASHVARS>.*?)\s*\}\s*;';
  REGEXP_FLASHVAR =     '\b(?P<VARNAME>[a-z_][a-z0-9_]*)\s*:\s*"(?P<VARVALUE>[^"]*)"';

{ TDownloader_Ted }

class function TDownloader_Ted.Provider: string;
begin
  Result := 'Ted.com';
end;

class function TDownloader_Ted.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Ted.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashVarRegExp := RegExCreate(REGEXP_FLASHVAR);
end;

destructor TDownloader_Ted.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarRegExp);
  inherited;
end;

function TDownloader_Ted.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ted.com/' + MovieID;
end;

function TDownloader_Ted.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, Server, StreamHQ, StreamMQ, StreamSQ, Stream: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else if not GetRegExpVarPairs(FlashVarRegExp, FlashVars, ['fms', 'hs', 'ms', 'ls'], [@Server, @StreamHQ, @StreamMQ, @StreamSQ]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else
    begin
    Stream := '';
    if StreamHQ <> '' then
      Stream := StreamHQ
    else if StreamMQ <> '' then
      Stream := StreamMQ
    else if StreamSQ <> '' then
      Stream := StreamSQ;
    if StreamSQ = '' then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      if Server = '' then
        Server := 'streaming.ted.com';
      Server := 'rtmp://' + Server + '/ondemand?_fcs_vhost=' + Server + '&akmfv=1.7';
      for i := Length(Stream) downto 1 do
        if Stream[i] = '.' then
          begin
          Stream := Copy(Stream, 1, Pred(i));
          Break;
          end;
      MovieUrl := Server + '/' + Stream;
      Self.RtmpUrl := Server;
      Self.Playpath := Stream;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Ted);

end.
