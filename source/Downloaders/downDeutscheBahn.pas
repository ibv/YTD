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

unit downDeutscheBahn;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DeutscheBahn = class(THttpDownloader)
    private
    protected
      BaseNameRegExp: TRegExp;
      SwfObjectRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

// http://bewegtbild.deutschebahn.com/btvo/site/index.php?s=5600&ids=143306
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bewegtbild\.deutschebahn\.com/btvo/site/(?:index\.php)\?(?:[^&]+&)*ids=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<td\s+class="ueberschrift2">\s*(?P<TITLE>.*?)\s*</td>';
  REGEXP_BASENAME = '\bbasename\s*:\s*"(?P<BASENAME>.+?)"';
  REGEXP_SWFOBJECT = '\bswfobject\.embedSWF\s*\(\s*"(?P<BASEURL>https?://[^"]+?/)Player/player\.swf"';

{ TDownloader_DeutscheBahn }

class function TDownloader_DeutscheBahn.Provider: string;
begin
  Result := 'DeutscheBahn.com';
end;

class function TDownloader_DeutscheBahn.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_DeutscheBahn.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  BaseNameRegExp := RegExCreate(REGEXP_BASENAME, [rcoIgnoreCase, rcoSingleLine]);
  SwfObjectRegExp := RegExCreate(REGEXP_SWFOBJECT, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_DeutscheBahn.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(BaseNameRegExp);
  RegExFreeAndNil(SwfObjectRegExp);
  inherited;
end;

function TDownloader_DeutscheBahn.GetMovieInfoUrl: string;
begin
  Result := 'http://bewegtbild.deutschebahn.com/btvo/site/index.php?s=5600&ids=' + MovieID;
end;

function TDownloader_DeutscheBahn.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var BaseName, BaseUrl: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(BaseNameRegExp, Page, 'BASENAME', BaseName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['basename']))
  else if not GetRegExpVar(SwfObjectRegExp, Page, 'BASEURL', BaseUrl) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['swfobject']))
  else
    begin
    MovieUrl := BaseUrl + BaseName + '_700k.mp4';
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DeutscheBahn);

end.
