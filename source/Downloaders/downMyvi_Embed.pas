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

unit downMyvi_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Myvi_Embed = class(THttpDownloader)
    private
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

// http://myvi.ru/ru/flash/player/pre/oQ_uJCGiADipN8mzgriJzJrAJsecK5PYnBZErn47vhimBnv5c3zTqNrrV3chb6TFH0
const
  URLREGEXP_BEFORE_ID = 'myvi\.ru/(?:ru/flash/)?player/(?:pre/)?';
  URLREGEXP_ID =        REGEXP_PATH_COMPONENT;
  URLREGEXP_AFTER_ID =  '';

const                                                           
  REGEXP_MOVIE_URL =    '(?P<URL>http:\/\/fs\d{0,2}\.myvi\.ru\/.+?\.((?>flv|mp4)).+)"\stitle=';

const
  MYVI_GET_VIDEO_XML = 'http://api.myvi.ru/public/player/getvideoinfo?globalization=&video=';
  MYVI_GET_VIDEO_API = 'http://www.myvi.ru/watch/GetVideoEmbedCodes';  //http://myvi.ru/ru/flash/player/oxB8JtpmyxUUbNVPSNqe1utpI-eGhy9NUxeDaKOLuARPSOr8j7oXbONcfcOiMIJgI0

{ TDownloader_Myvi_Embed }

class function TDownloader_Myvi_Embed.Provider: string;
begin
  Result := 'Myvi.ru';
end;

class function TDownloader_Myvi_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Myvi_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := True;
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_Myvi_Embed.Destroy;
begin
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_Myvi_Embed.GetMovieInfoUrl: string;
begin
  Result := MYVI_GET_VIDEO_XML + MovieID;
end;

function TDownloader_Myvi_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  VideoID, Title, Url, Info: string;
  PostData: AnsiString;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'Video/Info/Logical/Detail/public_video_id', VideoID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not GetXmlVar(PageXml, 'Video/Info/Logical/Detail/title', Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else
    begin
    Insert('-', VideoID, 21);
    Insert('-', VideoID, 17);
    Insert('-', VideoID, 13);
    Insert('-', VideoID,  9);
    PostData := {$IFDEF UNICODE} AnsiString {$ENDIF} (Format('public_video_or_ticket_id=%s&rnd=%d.%d', [VideoID, 500000 + Random(500000), 1000000000 + Random(1000000000)]));
    if not DownloadPage(Http, MYVI_GET_VIDEO_API, PostData, HTTP_FORM_URLENCODING_UTF8, Info) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else if not GetRegExpVar(MovieUrlRegExp, Info, 'URL', Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      Cookies.Assign(Http.Cookies);
      Name := HtmlDecode(UrlDecode(Title));
      MovieUrl := HtmlDecode(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Myvi_Embed);

end.
