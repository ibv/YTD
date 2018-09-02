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

unit downNovaMov;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downNovaMov_Embed;

type
  TDownloader_NovaMov = class(TNestedDownloader)
    private
    protected
      UrlComponentsRegexp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetTransformedUrl(out Url: string): boolean; override;
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

// http://www.novamov.com/video/i0d0yi9eqw2yj
// http://www.novamov.me/video/980511a4b44e1
const
  REGEXP_SERVER_PART = 'novamov\.(?:com|me)';
  REGEXP_PATH_PART = '/(?:video/|embed\.php\b.*?[?&]v=)';
  REGEXP_ID_PART = '[A-Za-z0-9]+';

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + REGEXP_SERVER_PART + REGEXP_PATH_PART + REGEXP_ID_PART;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_H3;
  REGEXP_URL_COMPONENTS = REGEXP_COMMON_URL_PREFIX + '(?P<SERVER>' + REGEXP_SERVER_PART + ')' + REGEXP_PATH_PART + '(?P<ID>' + REGEXP_ID_PART + ')';

{ TDownloader_NovaMov }

class function TDownloader_NovaMov.Provider: string;
begin
  Result := 'NovaMov.com';
end;

class function TDownloader_NovaMov.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_NovaMov.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  UrlComponentsRegexp := RegExCreate(REGEXP_URL_COMPONENTS);
end;

destructor TDownloader_NovaMov.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(UrlComponentsRegexp);
  inherited;
end;

function TDownloader_NovaMov.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_NovaMov.GetTransformedUrl(out Url: string): boolean;
var
  Server, ID: string;
begin
  Result := False;
  if GetRegExpVars(UrlComponentsRegexp, MovieID, ['SERVER', 'ID'], [@Server, @ID]) then
    if Server <> '' then
      if ID <> '' then
        begin
        Url := 'http://www.' + Server + '/api/player.api.php?file=' + ID;
        Result := True;
        end;
end;

initialization
  RegisterDownloader(TDownloader_NovaMov);

end.
