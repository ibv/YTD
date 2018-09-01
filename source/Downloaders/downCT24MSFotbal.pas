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

unit downCT24MSFotbal;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT24MSFotbal = class(TDownloader_CT)
    private
    protected
      MovieParamsRegExp: TRegExp;
      MovieVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://msfotbal.ct24.cz/article.asp?id=339
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*msfotbal\.ct24\.cz/article\.asp\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_MOVIE_PARAMS = '<param\s+name="initParams"\s+value="(?P<PARAMS>.*?)"';
  REGEXP_MOVIE_VARS = '(?P<VARNAME>[a-z_][a-z0-9_]*)=(?P<VARVALUE>[^,]*),?';

const
  SOAP_URL = 'http://ctdir.visual.cz/ivysilani/services/streaming/SLP.asmx';
  SOAP_ACTION = 'http://ivysilani.visual.cz/services/GetPlaylistUrl';
  SOAP_REQUEST = '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
                   '<s:Body>' +
                     '<GetPlaylistUrl xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ivysilani.visual.cz/services">' +
                       '<request>' +
                         '<Format>%s</Format>' +
                         '<ClientAddress>%s</ClientAddress>' +
                         '<Expiration>%s</Expiration>' +
                         '<Playlist>' +
                           '<PlaylistItem>' +
                             '<Type>Archive</Type>' +
                             '<Identifier>%s</Identifier>' +
                             '<Begin>0</Begin>' +
                             '<Duration i:nil="true" /> ' +
                             '<NoSkip i:nil="true" /> ' +
                           '</PlaylistItem>' +
                         '</Playlist>' +
                       '</request>' +
                     '</GetPlaylistUrl>' +
                   '</s:Body>' +
                 '</s:Envelope>';

{ TDownloader_CT24MSFotbal }

class function TDownloader_CT24MSFotbal.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT24MSFotbal.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  MovieVarsRegExp := RegExCreate(REGEXP_MOVIE_VARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT24MSFotbal.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(MovieVarsRegExp);
  inherited;
end;

function TDownloader_CT24MSFotbal.GetMovieInfoUrl: string;
begin
  Result := 'http://msfotbal.ct24.cz/article.asp?id=' + MovieID;
end;

function TDownloader_CT24MSFotbal.GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean;
var Request: string;
    RequestStream: TStringStream;
    Params, Token, VideoID, Quality: string;
    Xml: TXmlDoc;
begin
  Result := False;
  if GetRegExpVar(MovieParamsRegExp, Page, 'PARAMS', Params) then
    if GetRegExpVarPairs(MovieParamsRegExp, Params, ['token', 'videoId', 'quality'], [@Token, @VideoID, @Quality]) then
      if (Token <> '') and (VideoID <> '') and (Quality <> '') then
        begin
        Request := Format(SOAP_REQUEST, [Quality, Token, FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss".00000+02:00"', Now + 1), VideoID]);
        RequestStream := TStringStream.Create(Request);
        try
          Http.Clear;
          Http.Headers.Add('SOAPAction: "' + SOAP_ACTION + '"');
          Http.MimeType := 'text/xml; charset=utf-8';
          Http.InputStream := RequestStream;
          if DownloadXml(Http, SOAP_URL, Xml, hmPOST, False) then
            try
              Result := GetXmlVar(Xml, 'soap:Body/GetPlaylistUrlResponse/GetPlaylistUrlResult', Url);
            finally
              Xml.Free;
              end;
        finally
          Http.InputStream := nil;
          RequestStream.Free;
          end;
        end;
end;

initialization
  RegisterDownloader(TDownloader_CT24MSFotbal);

end.
