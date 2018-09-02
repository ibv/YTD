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

unit downMultimediaVseCz;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_MultimediaVseCz = class(TMSDownloader)
    private
    protected
      MovieObjectRegExp: TRegExp;
      MovieStreamRegExp: TRegExp;
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

// http://multimedia.vse.cz/media/Viewer/?peid=e847bd00891e47389492f06651a0a7761d
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*multimedia\.vse\.cz/media/Viewer/\?peid=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE_PATTERN = '<title>\s*(?P<TITLE>.*?)\s*</title>';
  REGEXP_MOVIE_OBJECT = '<script\s+src="(?P<URL>https?://[^"]+/media/FileServer/Presentation/.+?)"';
  REGEXP_MOVIE_STREAM = '\.VideoUrl\s*=\s*"(?P<URL>mmsh?://.+?)"';

{ TDownloader_MultimediaVseCz }

class function TDownloader_MultimediaVseCz.Provider: string;
begin
  Result := 'MultimediaVseCz.cz';
end;

class function TDownloader_MultimediaVseCz.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_MultimediaVseCz.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE_PATTERN);
  MovieObjectRegExp := RegExCreate(REGEXP_MOVIE_OBJECT);
  MovieStreamRegExp := RegExCreate(REGEXP_MOVIE_STREAM);
end;

destructor TDownloader_MultimediaVseCz.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieObjectRegExp);
  RegExFreeAndNil(MovieStreamRegExp);
  inherited;
end;

function TDownloader_MultimediaVseCz.GetMovieInfoUrl: string;
begin
  Result := 'http://multimedia.vse.cz/media/Viewer/?peid=' + MovieID;
end;

function TDownloader_MultimediaVseCz.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Info: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieObjectRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, Url, Info) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(MovieStreamRegExp, Info, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_MultimediaVseCz);

end.
