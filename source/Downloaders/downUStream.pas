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

unit downUStream;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_UStream = class(THttpDownloader)
    private
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
  uAMF,
  uDownloadClassifier,
  uMessages;

// http://www.ustream.tv/recorded/7022540
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ustream\.tv/recorded/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2\s+id="videoTitle">\s*(?P<TITLE>.*?)\s*</h2>';

const
  AMF_REQUEST_PACKET =
    'AAAAAAABAA9WaWV3ZXIuZ2V0VmlkZW8AAi8xAAAAmAoAAAABAwAGbG9jYWxlAgAFZW5fVVMA' +
    'B3BhZ2VVcmwCACZodHRwOi8vd3d3LnVzdHJlYW0udHYvcmVjb3JkZWQvNzAyMjU0MAAIYXV0' +
    'b3BsYXkBAQAHYnJhbmRJZAIAATEABHJwaW4CABhycGluLjAuMDg0MTM5NzMzODk3MDAzMjcA' +
    'B3ZpZGVvSWQCAAc3MDIyNTQwAAAJ';

{ TDownloader_UStream }

class function TDownloader_UStream.Provider: string;
begin
  Result := 'UStream.tv';
end;

class function TDownloader_UStream.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_UStream.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
end;

destructor TDownloader_UStream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_UStream.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ustream.tv/recorded/' + MovieID;
end;

function TDownloader_UStream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var AMFRequest, AMFResponse: TAMFPacket;
    Url, ErrorMsg: TAMFValue;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  AMFRequest := TAMFPacket.Create;
  try
    AMFRequest.LoadFromString(AnsiString(Base64Decode(AMF_REQUEST_PACKET)));
    // Note: I don't need to check types (or make sure pointers are not null)
    // because I use a pre-made packet which has all required properties. That
    // is not true while parsing response packets!
    with TAMFCommonArray(AMFRequest.Body[0].Content).Items[0] do
      begin
      SetValueByPath('videoId', MovieID);
      SetValueByPath('pageUrl', GetMovieInfoUrl);
      end;
    if DownloadAMF(Http, 'http://216.52.240.138/gateway.php', AMFRequest, AMFResponse) then
      try
        if AMFResponse.HasBody(0) then
          if AMFResponse.Body[0].Content.FindValueByPath('error/message', ErrorMsg, TAMFString) then
            SetLastErrorMsg(Format(ERR_SERVER_ERROR, [string(ErrorMsg)]))
          else if AMFResponse.Body[0].Content.FindValueByPath('flv', Url, TAMFString) then
            begin
            MovieURL := string(Url);
            SetPrepared(True);
            Result := True;
            end;
      finally
        AMFResponse.Free;
        end;
  finally
    AMFRequest.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_UStream);

end.
