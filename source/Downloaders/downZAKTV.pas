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

unit downZAKTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ZAKTV = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://www.zaktv.cz/cz/videos/index
const
  URLREGEXP_BEFORE_ID = 'zaktv\.cz';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_INFO = '\sonclick="run_player\s*\(\s*''(?P<TITLE>[^'']*)''\s*,\s*''(?P<PATH>/[^'']+)''';

{ TDownloader_ZAKTV }

class function TDownloader_ZAKTV.Provider: string;
begin
  Result := 'ZAKTV.cz';
end;

class function TDownloader_ZAKTV.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_ZAKTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_ZAKTV.Destroy;
begin
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_ZAKTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.zaktv.cz/' + MovieID;
end;

function TDownloader_ZAKTV.GetFileNameExt: string;
begin
  Result := inherited GetFileNameExt;
  if Result = '.f4v' then
    Result := '.flv';
end;

function TDownloader_ZAKTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Title, Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(MovieInfoRegExp, Page, ['TITLE', 'PATH'], [@Title, @Path]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else
    repeat
      if Path <> '' then
        begin
        SetName(Title);
        MovieUrl := GetRelativeUrl(GetMovieInfoUrl, Path);
        {$IFDEF MULTIDOWNLOADS}
        NameList.Add(Title);
        UrlList.Add(MovieUrl);
        {$ENDIF}
        SetPrepared(True);
        Result := True;
        end;
    until not GetRegExpVarsAgain(MovieInfoRegExp, ['TITLE', 'PATH'], [@Title, @Path]);
end;

initialization
  RegisterDownloader(TDownloader_ZAKTV);

end.
