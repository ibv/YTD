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

unit downQipRu_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_QipRu_Embed = class(THttpDownloader)
    private
    protected
      RealUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
      function BuildMovieUrl(out Url: string): boolean; override;
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

// http://file.qip.ru/embed/141785477/17228276
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*qip\.ru/embed/';
  URLREGEXP_ID =        '[0-9]+/[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  EXTRACT_URL_REGEXP = '[?&]file=(?P<URL>https?://[^&]+)';

{ TDownloader_QipRu_Embed }

class function TDownloader_QipRu_Embed.Provider: string;
begin
  Result := 'Qip.ru';
end;

class function TDownloader_QipRu_Embed.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_QipRu_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUnknown;
  RealUrlRegExp := RegExCreate(EXTRACT_URL_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_QipRu_Embed.Destroy;
begin
  RegExFreeAndNil(RealUrlRegExp);
  inherited;
end;

function TDownloader_QipRu_Embed.GetMovieInfoUrl: string;
begin
  Result := 'dummy';
end;

function TDownloader_QipRu_Embed.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  // Nothing to do
  Page := '';
  Xml := nil;
  Result := True;
end;

function TDownloader_QipRu_Embed.BuildMovieUrl(out Url: string): boolean;
var Http: THttpSend;
begin
  Result := False;
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://file.qip.ru/embed/' + MovieID, hmHEAD) then
      if GetRegExpVar(RealUrlRegExp, LastUrl, 'URL', Url) then
        begin
        SetName('Qip-movie-' + MovieID); // no name available
        Url := UrlDecode(Url);
        Result := True;
        end;
  finally
    Http.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_QipRu_Embed);

end.
