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

unit downKiwi;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHTTPDownloader;

type
  TDownloader_Kiwi = class(THTTPDownloader)
    private
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

// http://kiwi.kz/watch/jiaon54nfybz/
const
  URLREGEXP_BEFORE_ID = 'kiwi\.kz/watch/';
  URLREGEXP_ID =        REGEXP_PATH_COMPONENT;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<h1\s+class="page-h1">[^<]*</h1>\s*<div>[^<]*<b>(?P<TITLE>.*?)</b>';
  REGEXP_MOVIE_URL =    '"resources"\s*:\s*\{\s*"url"\s*:\s*"(?P<URL>https?:.+?)"';

{ TDownloader_Kiwi }

class function TDownloader_Kiwi.Provider: string;
begin
  Result := 'Kiwi.kz';
end;

class function TDownloader_Kiwi.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Kiwi.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_Kiwi.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Kiwi.GetMovieInfoUrl: string;
begin
  Result := 'http://kiwi.kz/watch/' + MovieID + '/download/';
end;

function TDownloader_Kiwi.GetFileNameExt: string;
begin
  Result := '.mp4';
end;

function TDownloader_Kiwi.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Info, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadPage(Http, 'http://kiwi.kz/services/watch/download', {$IFDEF UNICODE} AnsiString {$ENDIF} ('hash=' + MovieID + '&download-btn=%D0%9F%D0%BE%D0%BB%D1%83%D1%87%D0%B8%D1%82%D1%8C%20%D1%81%D1%81%D1%8B%D0%BB%D0%BA%D1%83%20%D0%BD%D0%B0%20%D1%81%D0%BA%D0%B0%D1%87%D0%B8%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5'), HTTP_FORM_URLENCODING, Info) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(MovieUrlRegExp, Info, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := StripSlashes(Url);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Kiwi);

end.
