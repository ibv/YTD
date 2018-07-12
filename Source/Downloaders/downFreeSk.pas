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

unit downFreeSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeSk = class(THttpDownloader)
    private
    protected
      FlashVideoRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://free.zoznam.sk/video/Splhajuci-buldozer
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*free\.zoznam\.sk/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<input\s+type="hidden"\s+name="module_name"\s+value="(?P<TITLE>[^"]+)"';
  REGEXP_EXTRACT_URL = '<param\s+name="flashvars"\s+value="(?:[^">]*&)?file=(?P<URL>https?://.+?)[&"]';
  REGEXP_FLASH_VIDEO = '^(?P<URL>https?://[^/]+/viewflv/.+)$';

{ TDownloader_FreeSk }

class function TDownloader_FreeSk.Provider: string;
begin
  Result := 'Free.zoznam.sk';
end;

class function TDownloader_FreeSk.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_FreeSk.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
  FlashVideoRegExp := RegExCreate(REGEXP_FLASH_VIDEO);
end;

destructor TDownloader_FreeSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(FlashVideoRegExp);
  inherited;
end;

function TDownloader_FreeSk.GetMovieInfoUrl: string;
begin
  Result := 'http://free.zoznam.sk/video/' + MovieID;
end;

function TDownloader_FreeSk.GetFileNameExt: string;
begin
  Result := inherited GetFileNameExt;
  if Result = '' then
    Result := '.flv';
end;

function TDownloader_FreeSk.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Dummy: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := Prepared;
  if Result then
    begin
    if not GetRegExpVar(FlashVideoRegExp, MovieUrl, 'URL', Dummy) then
      MovieUrl := MovieUrl + '.mp4';
    end;
end;

initialization
  RegisterDownloader(TDownloader_FreeSk);

end.
