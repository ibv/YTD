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

unit downOwn3D_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Own3D_Embed = class(THttpDownloader)
    private
    protected
      BasePathRegExp: TRegExp;
      HDPathRegExp: TRegExp;
      HQPathRegExp: TRegExp;
      SDPathRegExp: TRegExp;
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

// http://www.own3d.tv/stream/927952
const
  URLREGEXP_BEFORE_ID = 'own3d\.tv/stream/';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '''title''\s*:\s*\{.*?''title''\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_MOVIE_BASE =   '''baseUrl''\s*:\s*''(?P<BASE>https?://.+?)''';
  REGEXP_MOVIE_HD =     '''HDUrl''\s*:\s*''(?P<PATH>videos/.+?\.mp4)''';
  REGEXP_MOVIE_HQ =     '''HQUrl''\s*:\s*''(?P<PATH>videos/.+?\.mp4)''';
  REGEXP_MOVIE_SD =     '''url''\s*:\s*''(?P<PATH>videos/.+?\.mp4)''';

{ TDownloader_Own3D_Embed }

class function TDownloader_Own3D_Embed.Provider: string;
begin
  Result := 'Own3D.tv';
end;

class function TDownloader_Own3D_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Own3D_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUnknown;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  BasePathRegExp := RegExCreate(REGEXP_MOVIE_BASE);
  HDPathRegExp := RegExCreate(REGEXP_MOVIE_HD);
  HQPathRegExp := RegExCreate(REGEXP_MOVIE_HQ);
  SDPathRegExp := RegExCreate(REGEXP_MOVIE_SD);
end;

destructor TDownloader_Own3D_Embed.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(BasePathRegExp);
  RegExFreeAndNil(HDPathRegExp);
  RegExFreeAndNil(HQPathRegExp);
  RegExFreeAndNil(SDPathRegExp);
  inherited;
end;

function TDownloader_Own3D_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.own3d.tv/inc/flowplayer/embed/json_cfg.php?video_id=' + MovieID;
end;

function TDownloader_Own3D_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  BaseUrl, Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(BasePathRegExp, Page, 'BASE', BaseUrl) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else if not (GetRegExpVar(HDPathRegExp, Page, 'PATH', Path) or GetRegExpVar(HQPathRegExp, Page, 'PATH', Path) or GetRegExpVar(SDPathRegExp, Page, 'PATH', Path)) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else
    begin
    MovieUrl := BaseUrl + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Own3D_Embed);

end.
