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

unit downILikeBike;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_ILikeBike = class(TMSDownloader)
    private
    protected
      YearMonthRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function BuildMovieUrl(out Url: string): boolean; override;
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

// http://www.ilikebike.cz/?year=2011&week=11
const
  URLREGEXP_BEFORE_ID = 'ilikebike\.cz/\?(?:.*?&)*';
  URLREGEXP_ID =        'year=[0-9]{4}&week=[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_YEAR_MONTH = 'year=(?P<YEAR>[0-9]{4})&week=(?P<MONTH>[0-9]+)';

{ TDownloader_ILikeBike }

class function TDownloader_ILikeBike.Provider: string;
begin
  Result := 'ILikeBike.cz';
end;

class function TDownloader_ILikeBike.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_ILikeBike.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peNone;
  YearMonthRegExp := RegExCreate(REGEXP_YEAR_MONTH);
end;

destructor TDownloader_ILikeBike.Destroy;
begin
  RegExFreeAndNil(YearMonthRegExp);
  inherited;
end;

function TDownloader_ILikeBike.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ilikebike.cz/?' + MovieID;
end;

function TDownloader_ILikeBike.BuildMovieUrl(out Url: string): boolean;
var
  Year, Month: string;
begin
  Result := GetRegExpVars(YearMonthRegExp, MovieID, ['YEAR', 'MONTH'], [@Year, @Month]);
  if Result then
    begin
    Name := Format('I Like Bike %s-%s', [Month, Year]);
    Url := Format('http://bcastb.livebox.cz/up/ilikebike/%s/_%s%s.wmv', [Year, Month, Year]);
    end;
end;

initialization
  RegisterDownloader(TDownloader_ILikeBike);

end.
