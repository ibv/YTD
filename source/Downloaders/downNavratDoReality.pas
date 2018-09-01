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

unit downNavratDoReality;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NavratDoReality = class(THttpDownloader)
    private
    protected
      MoviePathRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://navratdoreality.cz/?p=view&id=5766
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*navratdoreality\.cz/.*[?&]id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h2>(?P<TITLE>[^<]*)</h2>\s*<div id="media_content">';
  REGEXP_EXTRACT_PATH = '\bso\.addVariable\s*\(\s*"file"\s*,\s*"(?P<PATH>.*?)"';

{ TDownloader_NavratDoReality }

class function TDownloader_NavratDoReality.Provider: string;
begin
  Result := 'NavratDoReality.cz';
end;

class function TDownloader_NavratDoReality.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_NavratDoReality.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MoviePathRegExp := RegExCreate(REGEXP_EXTRACT_PATH, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_NavratDoReality.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MoviePathRegExp);
  inherited;
end;

function TDownloader_NavratDoReality.GetMovieInfoUrl: string;
begin
  Result := 'http://navratdoreality.cz/?p=view&id=' + MovieID;
end;

function TDownloader_NavratDoReality.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean;
var FormData: AnsiString;
    Buf: TMemoryStream;
begin
  Page := '';
  Xml := nil;
  FormData := 'ACTION=check_adult&check=18plus';
  Buf := TMemoryStream.Create;
  try
    Buf.WriteBuffer(FormData[1], Length(FormData));
    Buf.Position := 0;
    Http.MimeType := 'application/x-www-form-urlencoded';
    Http.InputStream := Buf;
    try
      Result := DownloadPage(Http, Url, Page, InfoPageEncoding, hmPOST, False);
    finally
      Http.InputStream := nil;
      end;
  finally
    Buf.Free;
    end;
end;

function TDownloader_NavratDoReality.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MoviePathRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    MovieUrl := 'http://navratdoreality.cz/' + Path;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_NavratDoReality);

end.
