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

unit downVideoNurKz;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VideoNurKz = class(THttpDownloader)
    private
    protected
      EncryptedUrlRegExp: TRegExp;
      Extension: string;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function Decrypt(var Str: string): boolean;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uCrypto,
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://video.nur.kz/view=8irrnc87
const
  URLREGEXP_BEFORE_ID = 'video\.nur\.kz/view=';
  URLREGEXP_ID =        REGEXP_PATH_COMPONENT;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_H1;
  REGEXP_MOVIE_URL =    '[&"]file=(?P<URL>[0-9a-zA-Z/=\+]+)[&"]';

{ TDownloader_VideoNurKz }

class function TDownloader_VideoNurKz.Provider: string;
begin
  Result := 'Video.Nur.kz';
end;

class function TDownloader_VideoNurKz.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_VideoNurKz.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  EncryptedUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_VideoNurKz.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(EncryptedUrlRegExp);
  inherited;
end;

function TDownloader_VideoNurKz.GetMovieInfoUrl: string;
begin
  Result := 'http://video.nur.kz/view=' + MovieID;
end;

function TDownloader_VideoNurKz.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(EncryptedUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not Decrypt(Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := Url;
    if DownloadPage(Http, Url, hmHead) then
      Extension := ExtractUrlExt(LastUrl);
    SetPrepared(True);
    Result := True;
    end;
end;

function TDownloader_VideoNurKz.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_VideoNurKz.Decrypt(var Str: string): boolean;
const
  Key: WideString = 'RA$7xPZR';
var
  Enc, Dec: AnsiString;
  n: integer;
begin
  Enc := DecodeBase64(Str);
  n := Length(Enc);
  if n <= 0 then
    begin
    Dec := '';
    Result := True;
    end
  else
    begin
    SetLength(Dec, n);
    Result := RC4_Decrypt(@Enc[1], @Dec[1], @Key[1], Length(Key)*Sizeof(Key[1]), n);
    Str := EncodeBase64(Dec);
    end;
end;

initialization
  RegisterDownloader(TDownloader_VideoNurKz);

end.
