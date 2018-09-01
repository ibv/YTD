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

unit downTyzdenSk;
{$INCLUDE 'ytd.inc'}
{.DEFINE ALLOW_MDY_DATE} // Allow switching of day and month. Not recommended!

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_TyzdenSk = class(TRtmpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarsItemsRegExp: TRegExp;
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

// http://www.tyzden.sk/lampa/lampa-z-16-12-2010.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tyzden\.sk/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>(?:[^|]*\|)?[^|]*)[|<]';
  REGEXP_FLASHVARS = '\.addParam\s*\(\s*"FlashVars"\s*,\s*"(?P<FLASHVARS>.*?)"';
  REGEXP_FLASHVARS_ITEMS = '(?P<VARNAME>[^="]+)=(?P<VARVALUE>.*?)(?:&amp;|$)';

{ TDownloader_TyzdenSk }

class function TDownloader_TyzdenSk.Provider: string;
begin
  Result := 'Tyzden.sk';
end;

class function TDownloader_TyzdenSk.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_TyzdenSk.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashVarsItemsRegExp := RegExCreate(REGEXP_FLASHVARS_ITEMS);
end;

destructor TDownloader_TyzdenSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarsItemsRegExp);
  inherited;
end;

function TDownloader_TyzdenSk.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tyzden.sk/' + MovieID;
end;

function TDownloader_TyzdenSk.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashVars, Node, V: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not GetRegExpVarPairs(FlashVarsItemsRegExp, FlashVars, ['node', 'v'], [@Node, @V]) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if (Node = '') or (V = '') then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else
    begin
    Node := UrlDecode(Node);
    repeat
      i := Pos(';', Node);
      if i <= 0 then
        Break
      else if i > 1 then
        begin
        SetLength(Node, Pred(i));
        Break;
        end
      else
        System.Delete(Node, 1, 1);
    until Node <> '';
    V := UrlDecode(V);
    if Node = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
    else
      begin
      MovieUrl := Node + V;
      AddRtmpDumpOption('r', Node);
      AddRtmpDumpOption('y', V); 
      AddRtmpDumpOption('f', 'WIN 10,1,82,76');
      AddRtmpDumpOption('W', 'http://www.tyzden.sk/fileadmin/template/swf/tyzden_player.swf?v=4');
      AddRtmpDumpOption('p', GetMovieInfoUrl);
      AddRtmpDumpOption('t', Node);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TyzdenSk);

end.
