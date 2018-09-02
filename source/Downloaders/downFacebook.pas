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

unit downFacebook;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Facebook = class(THttpDownloader)
    private
    protected
      MovieListRegExp: TRegExp;
      MovieHDUrlRegExp: TRegExp;
      MovieSDUrlRegExp: TRegExp;
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

// http://www.facebook.com/video/video.php?v=1131482863478
// http://www.facebook.com/media/set/?set=vb.33966109512&type=2#!/photo.php?v=137514062921&set=vb.33966109512&type=3&theater
// http://www.facebook.com/photo.php?v=137514062921&set=vb.33966109512&type=3&theater
const
  URLREGEXP_BEFORE_ID = 'facebook\.com/.*?[?&]v=';
  //URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*facebook\.com/video/.*?[?&]v=';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h3\s+class="video_title\s+datawrap">(?P<TITLE>.*?)</h3>';
  REGEXP_MOVIE_LIST = '\[\s*"video"\s*,\s*"(?P<INFO>.+?)"';
  REGEXP_SD_MOVIE_URL = '"sd_src"\s*:\s*"(?P<URL>.+?)"';
  REGEXP_HD_MOVIE_URL = '"hd_src"\s*:\s*"(?P<URL>.+?)"';

{ TDownloader_Facebook }

class function TDownloader_Facebook.Provider: string;
begin
  Result := 'Facebook.com';
end;

class function TDownloader_Facebook.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Facebook.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieListRegExp := RegExCreate(REGEXP_MOVIE_LIST);
  MovieHDUrlRegExp := RegExCreate(REGEXP_HD_MOVIE_URL);
  MovieSDUrlRegExp := RegExCreate(REGEXP_SD_MOVIE_URL);
end;

destructor TDownloader_Facebook.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieListRegExp);
  RegExFreeAndNil(MovieHDUrlRegExp);
  RegExFreeAndNil(MovieSDUrlRegExp);
  inherited;
end;

function TDownloader_Facebook.GetMovieInfoUrl: string;
begin
  Result := 'http://www.facebook.com/video/video.php?v=' + MovieID;
end;

function TDownloader_Facebook.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Info, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieListRegExp, Page, 'INFO', Info) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    Info := UrlDecode(JSDecode(Info));
    if not (GetRegExpVar(MovieHDUrlRegExp, Info, 'URL', Url) or GetRegExpVar(MovieSDUrlRegExp, Info, 'URL', Url)) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieURL := JSDecode(Url);
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Facebook);

end.
