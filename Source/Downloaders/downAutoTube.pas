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

unit downAutoTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_AutoTube = class(THttpDownloader)
    private
    protected
      VideoIdRegExp: TRegExp;
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

// http://www.autotube.cz/roadtube/612-jak-na-dopravni-zacpu.html
const
  URLREGEXP_BEFORE_ID = 'autotube\.cz/';
  URLREGEXP_ID =        '[^/?&]+/[0-9]+.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_TITLE;
  REGEXP_VIDEO_ID =     '[^/]+/(?P<ID>[0-9]+)';

{ TDownloader_AutoTube }

class function TDownloader_AutoTube.Provider: string;
begin
  Result := 'AutoTube.cz';
end;

class function TDownloader_AutoTube.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_AutoTube.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID);
end;

destructor TDownloader_AutoTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoIdRegExp);
  inherited;
end;

function TDownloader_AutoTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.autotube.cz/' + MovieID;
end;

function TDownloader_AutoTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var ID, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, MovieID, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXmlVar(Http, 'http://www.autotube.cz/ui/player/video.php?id=' + ID, 'file', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieURL := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_AutoTube);

end.

