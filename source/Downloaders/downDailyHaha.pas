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

unit downDailyHaha;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_DailyHaha = class(THttpDownloader)
    private
    protected
      FlashObjectRegExp: TRegExp;
      FlashVarsRegExp: TRegExp;
      FlashVarSrcRegExp: TRegExp;
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

// http://www.dailyhaha.com/_vids/dog-chasing-shadow.htm
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*dailyhaha\.com/_vids/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_FLASHOBJECT = '\bAC_AX_RunContent\s*\((?P<OBJECT>.*?)\);';
  REGEXP_FLASHVARS = '\s*''(?P<VARNAME>.*?)''\s*,\s*''(?P<VARVALUE>.*?)''\s*,?';
  REGEXP_FLASHVARSRC = '(?:^|&)Vid=(?P<SRC>[^&]+)';

{ TDownloader_DailyHaha }

class function TDownloader_DailyHaha.Provider: string;
begin
  Result := 'DailyHaha.com';
end;

class function TDownloader_DailyHaha.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_DailyHaha.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  FlashObjectRegExp := RegExCreate(REGEXP_FLASHOBJECT);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashVarSrcRegExp := RegExCreate(REGEXP_FLASHVARSRC);
end;

destructor TDownloader_DailyHaha.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashObjectRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarSrcRegExp);
  inherited;
end;

function TDownloader_DailyHaha.GetMovieInfoUrl: string;
begin
  Result := 'http://www.dailyhaha.com/_vids/' + MovieID;
end;

function TDownloader_DailyHaha.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var FlashObject, FlashVars, UrlBase, FileName: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashObjectRegExp, Page, 'OBJECT', FlashObject) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else
    begin
    GetRegExpVarPairs(FlashVarsRegExp, FlashObject, ['base', 'FlashVars'], [@UrlBase, @FlashVars]);
    GetRegExpVar(FlashVarSrcRegExp, FlashVars, 'SRC', FileName);
    if (UrlBase = '') or (FileName = '') then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieURL := UrlBase + FileName;
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_DailyHaha);

end.
