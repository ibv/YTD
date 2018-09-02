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

unit downMTV;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader, downMTVEmbed;

type
  TDownloader_MTV = class(TDownloader_MTVEmbed)
    private
    protected
      MovieParamsRegExp: TRegExp;
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
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.mtv.com/videos/movie-trailers/602324/mega-shark-vs-crocosaurus.jhtml
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*mtv\.com/videos/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_PARAMS = '<meta\s+name="mtvn_uri"\s+content="(?P<PARAMS>.+?)"';

{ TDownloader_MTV }

class function TDownloader_MTV.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_MTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peAnsi;
  InfoPageIsXml := False;
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS)
end;

destructor TDownloader_MTV.Destroy;
begin
  RegExFreeAndNil(MovieParamsRegExp);
  inherited;
end;

function TDownloader_MTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.mtv.com/videos/' + MovieID;
end;

function TDownloader_MTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Params: string;
    EmbeddedPage: string;
    EmbeddedXml: TXmlDoc;
begin
  Result := False;
  if not GetRegExpVar(MovieParamsRegExp, Page, 'PARAMS', Params) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, GetMovieInfoUrlForID(Params), EmbeddedPage, EmbeddedXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      Result := inherited AfterPrepareFromPage(EmbeddedPage, EmbeddedXml, Http);
    finally
      EmbeddedXml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_MTV);

end.
