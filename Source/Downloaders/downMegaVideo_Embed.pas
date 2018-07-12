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

unit downMegaVideo_Embed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downMegaVideo;

type
  TDownloader_MegaVideo_Embed = class(TNestedDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
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

// http://www.megavideo.com/v/HF2OV2PPf800ca3bd438e6d30c2490a75932d6331
const
  URLREGEXP_BEFORE_ID = 'megavideo\.com/v/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_MegaVideo_Embed }

class function TDownloader_MegaVideo_Embed.Provider: string;
begin
  Result := TDownloader_MegaVideo.Provider;
end;

class function TDownloader_MegaVideo_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_MegaVideo_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peNone;
end;

destructor TDownloader_MegaVideo_Embed.Destroy;
begin
  inherited;
end;

function TDownloader_MegaVideo_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.megavideo.com/v/' + MovieID;
end;

function TDownloader_MegaVideo_Embed.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Page := '';
  Xml := nil;
  Result := True;
end;

function TDownloader_MegaVideo_Embed.GetTransformedUrl(out Url: string): boolean;
var
  Http: THttpSend;
begin
  Result := False;
  Http := CreateHttp;
  try
    if DownloadPage(Http, GetMovieInfoUrl, hmHead) then
      begin
      Url := LastUrl;
      Result := True;
      end;
  finally
    FreeAndNil(Http);
    end;
end;

initialization
  RegisterDownloader(TDownloader_MegaVideo_Embed);

end.
