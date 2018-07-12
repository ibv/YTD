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

unit downLiveLeak;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LiveLeak = class(THttpDownloader)
    private
    protected
      InfoUrlRegExp: TRegExp;
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

// http://www.liveleak.com/view?i=6f4_1272904024
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*liveleak\.com/view\?i=';
  URLREGEXP_ID =        '[^?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>\s*(?:LiveLeak\.com\s*-\s*)?(?P<TITLE>.*?)\s*</title>';
  REGEXP_INFO_URL = '\bconfig\s*:\s*"(?P<URL>https?://.+?)"';

{ TDownloader_LiveLeak }

class function TDownloader_LiveLeak.Provider: string;
begin
  Result := 'LiveLeak.com';
end;

class function TDownloader_LiveLeak.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_LiveLeak.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL);
end;

destructor TDownloader_LiveLeak.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_LiveLeak.GetMovieInfoUrl: string;
begin
  Result := 'http://www.liveleak.com/view?i=' + MovieID;
end;

function TDownloader_LiveLeak.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var InfoUrl, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', InfoUrl) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXmlVar(Http, InfoUrl, 'file', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    begin
    MovieURL := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_LiveLeak);

end.
