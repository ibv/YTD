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

unit downSmeSK;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SmeSK = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
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

// http://www.sme.sk/c/5796529/policia-mozno-bankomatovemu-gangu-zachranila-zivot.html
// http://tv.sme.sk/v/19916/spojenci-ostreluju-kaddafiho-tanky.html
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*sme\.sk/.*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+property="og:title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_MOVIE_INFO = '\.addVariable\s*\(\s*"file"\s*,\s*escape\s*\(\s*"(?P<INFOURL>https?://.+?)"';

{ TDownloader_SmeSK }

class function TDownloader_SmeSK.Provider: string;
begin
  Result := 'Sme.sk';
end;

class function TDownloader_SmeSK.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);
end;

constructor TDownloader_SmeSK.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peAnsi;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_SmeSK.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_SmeSK.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_SmeSK.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var InfoUrl, Url: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoRegExp, Page, 'INFOURL', InfoUrl) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, InfoUrl, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(Xml, 'trackList/track/location', Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        MovieURL := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_SmeSK);

end.
