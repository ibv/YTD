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

unit xxxXTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_XTube = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod = hmGET): boolean; override;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*xtube\.com/.*[?&]v=';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="content_d">\s*<div[^>]*>\s*(?P<TITLE>.*?)</div>';
  REGEXP_FLASHVARS = '\.addVariable\s*\(\s*"(?P<VARNAME>[^"]+)"\s*,\s*"(?P<VARVALUE>[^"]+)"';

{ TDownloader_XTube }

class function TDownloader_XTube.Provider: string;
begin
  Result := 'XTube.com';
end;

class function TDownloader_XTube.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_XTube.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
end;

destructor TDownloader_XTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  inherited;
end;

function TDownloader_XTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.xtube.com/watch.php?v=' + MovieID;
end;

function TDownloader_XTube.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Http.Cookies.Add('cookie_warning=S');
  Result := inherited GetMovieInfoContent(Http, Url, Page, Xml, Method);
end;

function TDownloader_XTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  PREFIX_FILENAME = '&filename=';
  PREFIX_FILENAME_LENGTH = Length(PREFIX_FILENAME);
var
  SwfUrl, UserID, VideoID, ClipID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  GetRegExpVarPairs(FlashVarsRegExp, Page, ['swfURL', 'user_id', 'video_id', 'clip_id'], [@SwfUrl, @UserID, @VideoID, @ClipID]);
  if VideoID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['video_id']))
  else if not DownloadPage(Http, 'http://www.xtube.com/find_video.php', AnsiString('user_id=' + UrlEncode(UserID) + '&clip_id=' + UrlEncode(ClipID) + '&video_id=' + UrlEncode(VideoID)), HTTP_FORM_URLENCODING, Page) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if Copy(Page, 1, PREFIX_FILENAME_LENGTH) <> PREFIX_FILENAME then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := SwfUrl + UrlDecode(Copy(Page, Succ(PREFIX_FILENAME_LENGTH), MaxInt));
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_XTube);
  {$ENDIF}

end.
