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

unit downCT_Program;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT_Program = class(TDownloader_CT)
    private
    protected
      PortToIVysilaniRegExp: TRegExp;
      PortTitleRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.ceskatelevize.cz/program/10252893246-07.08.2010-08:32-24-prizma.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ceskatelevize\.cz/program/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '[^/?&]*\.html?';

const
  REGEXP_PORT_TO_IVYSILANI = '<iframe\s+[^>]*\bsrc="(?P<URL>https?://(?:[a-z0-9-]+\.)*ceskatelevize\.cz/ivysilani/embed/.*?)"';
  REGEXP_PORT_TITLE = '<h1\s+id="nazev">\s*<a\s+[^>]*>\s*(?P<TITLE>.*?)\s*</a>';

{ TDownloader_CT_Program }

class function TDownloader_CT_Program.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT_Program.Create(const AMovieID: string);
begin
  inherited;
  PortToIVysilaniRegExp := RegExCreate(REGEXP_PORT_TO_IVYSILANI, [rcoIgnoreCase, rcoSingleLine]);
  PortTitleRegExp := RegExCreate(REGEXP_PORT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT_Program.Destroy;
begin
  RegExFreeAndNil(PortToIVysilaniRegExp);
  RegExFreeAndNil(PortTitleRegExp);
  inherited;
end;

function TDownloader_CT_Program.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ceskatelevize.cz/program/' + MovieID + '?online=1';
end;

function TDownloader_CT_Program.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, EmbeddedPlayer, Title: string;
begin
  Result := False;
  if not GetRegExpVar(PortToIVysilaniRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else
    begin
    Url := EncodeUrl(HtmlDecode(Url));
    if not DownloadPage(Http, Url, EmbeddedPlayer, peXml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      begin
      Result := inherited AfterPrepareFromPage(EmbeddedPlayer, nil, Http);
      if Result then
        if GetRegExpVar(PortTitleRegExp, Page, 'TITLE', Title) then
          SetName(Title);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CT_Program);

end.
