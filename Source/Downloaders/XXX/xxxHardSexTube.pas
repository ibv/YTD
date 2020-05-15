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

unit xxxHardSexTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_HardSexTube = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = 'hardsextube\.com/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<h1[^>]*>(?P<TITLE>.*?)</h1>';
  REGEXP_MOVIE_INFO =    '\bmakesecurl\s*:\s*"(?P<SECURL>[^"]+)"\s*,\s*flvserver\s*:\s*"(?P<SERVER>https?://[^"]+?)"\s*,\s*flv\s*:\s*"(?P<PATH>/[^"]+?)"';

{ TDownloader_HardSexTube }

class function TDownloader_HardSexTube.Provider: string;
begin
  Result := 'HardSexTube.com';
end;

class function TDownloader_HardSexTube.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_HardSexTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peAnsi;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_HardSexTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_HardSexTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.hardsextube.com/video/' + MovieID;
end;

function TDownloader_HardSexTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Server, Path, SecUrl: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(MovieInfoRegExp, Page, ['SERVER', 'PATH', 'SECURL'], [@Server, @Path, @SecUrl]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if (Server = '') or (Path = '') then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if AnsiCompareText(SecUrl, 'true') = 0 then
    SetLastErrorMsg(ERR_ENCRYPTION_NOT_SUPPORTED)
  else
    begin
    MovieUrl := Server + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_HardSexTube);
  {$ENDIF}

end.
