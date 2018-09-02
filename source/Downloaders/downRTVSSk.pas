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

unit downRTVSSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, downSTV;

type
  TDownloader_RTVSSk = class(TDownloader_STV)
    private
    protected
      ServerInfoScriptRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetAuthPageUrl(const Page: string; out Url: string): boolean; override;
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

// http://www.rtvs.sk/televizia/program/detail/4686/milujem-slovensko/archiv?date=17.05.2013
// http://www.rtvs.sk/tv.programmes.detail/archive/4686?date=14.06.2013
// http://www.rtvs.sk/radio/relacie/detail/pohoda-fm/archiv?date=28.05.2013&station=fm
const
  URLREGEXP_BEFORE_ID = 'rtvs\.sk/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  SERVER_INFO_SCRIPT = '<script\b[^>]*\ssrc="(?P<URL>https?://[^"]+-arch.js)"';

{ TDownloader_RTVSSk }

class function TDownloader_RTVSSk.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_RTVSSk.Create(const AMovieID: string);
begin
  inherited;
  ServerInfoScriptRegExp := RegExCreate(SERVER_INFO_SCRIPT);
end;

destructor TDownloader_RTVSSk.Destroy;
begin
  RegExFreeAndNil(ServerInfoScriptRegExp);
  inherited;
end;

function TDownloader_RTVSSk.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rtvs.sk/' + MovieID;
end;

function TDownloader_RTVSSk.GetAuthPageUrl(const Page: string; out Url: string): boolean;
begin
  Result := GetRegExpVar(ServerInfoScriptRegExp, Page, 'URL', Url);
end;

initialization
  RegisterDownloader(TDownloader_RTVSSk);

end.
