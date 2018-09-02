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

unit downAutoSalonTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_AutoSalonTV = class(TMSDownloader)
    private
    protected
      MovieIdVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetYearWeek(out Year, Week: string): boolean;
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

// http://autosalontv.cz/?year=2010&week=50
const
  URLREGEXP_BEFORE_ID = 'autosalontv\.cz/.*\?';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE_PATTERN = '<h2>\s*<a\s+href="/\?year=%s&week=0*%s">(?P<TITLE>.*?)</a>';
  REGEXP_MOVIEID_VARS = '(?:^|&amp;|&)(?P<VARNAME>[^=&]+)=(?P<VARVALUE>[^&]*)';

{ TDownloader_AutoSalonTV }

class function TDownloader_AutoSalonTV.Provider: string;
begin
  Result := 'AutoSalonTV.cz';
end;

class function TDownloader_AutoSalonTV.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_AutoSalonTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := nil;
  MovieIdVarsRegExp := RegExCreate(REGEXP_MOVIEID_VARS);
end;

destructor TDownloader_AutoSalonTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdVarsRegExp);
  inherited;
end;

function TDownloader_AutoSalonTV.GetMovieInfoUrl: string;
begin
  Result := 'http://autosalontv.cz/?' + MovieID;
end;

function TDownloader_AutoSalonTV.GetYearWeek(out Year, Week: string): boolean;
begin
  Result := GetRegExpVarPairs(MovieIdVarsRegExp, MovieID, ['year', 'week'], [@Year, @Week]);
end;

function TDownloader_AutoSalonTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Year, Week, Title: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetYearWeek(Year, Week) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    MovieTitleRegExp := RegExCreate(Format(REGEXP_MOVIE_TITLE_PATTERN, [Year, Week]));
    try
      if GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', Title) then
        SetName(Title);
    finally
      RegExFreeAndNil(MovieTitleRegExp);
      end;
    MovieUrl := Format('http://bcastd.livebox.cz/up/as/%s/%s%s.wmv', [Year, Week, Year]);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_AutoSalonTV);

end.
