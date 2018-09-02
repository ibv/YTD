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

unit downOverStream;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_OverStream = class(TNestedDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      {$IFDEF SUBTITLES}
    public
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    public
      class function Provider: string; override;
      class function Features: TDownloaderFeatures; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  {$IFDEF SUBTITLES}
  uStringUtils,
  uAMF,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://www.overstream.net/view.php?oid=uinsn9n7uomx
// http://www.overstream.net/swf/player/oplx?oid=m8t4s4uzibl0
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*overstream\.net/(?:view\.php\?oid=|swf/player/oplx\?oid=)';
  URLREGEXP_ID =        '[^&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>[^<'']*?''(?P<TITLE>.*?)''[^<'']*?</title>';
  REGEXP_EXTRACT_URL = '<td>\s*Original\s+video\s*:\s*<a\s+href="(?P<URL>https?://.+?)"';

{$IFDEF SUBTITLES}
// http://www.overstream.net/view.php?oid=nxioixihszlz
const
  AMF_REQUEST_SUBTITLES_PACKET =
    'AAAAAAABAB1QbGF5ZXJTdWJ0aXRsZXNTZXJ2aWNlLmdldE9NTAACLzEAAAAUCgAAAAECAAxu' +
    'eGlvaXhpaHN6bHo=';
{$ENDIF}

{ TDownloader_OverStream }

class function TDownloader_OverStream.Provider: string;
begin
  Result := 'OverStream.net';
end;

class function TDownloader_OverStream.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [
    {$IFDEF SUBTITLES} dfSubtitles {$ENDIF}
    ];
end;

class function TDownloader_OverStream.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_OverStream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
end;

destructor TDownloader_OverStream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  inherited;
end;

function TDownloader_OverStream.GetMovieInfoUrl: string;
begin
  Result := 'http://www.overstream.net/view.php?oid=' + MovieID;
end;

{$IFDEF SUBTITLES}
function TDownloader_OverStream.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var AMFRequest, AMFResponse: TAMFPacket;
    Subtitles: TAMFValue;
begin
  Result := False;
  AMFRequest := TAMFPacket.Create;
  try
    AMFRequest.LoadFromString(AnsiString(Base64Decode(AMF_REQUEST_SUBTITLES_PACKET)));
    TAMFCommonArray(AMFRequest.Body[0].Content).Items[0].Value := MovieID;
    if DownloadAMF(Http, 'http://www.overstream.net/services/gateway.php', AMFRequest, AMFResponse) then
      try
        if AMFResponse.HasBody(0) then
          if AMFResponse.Body[0].Content.FindValueByPath('res/oml', Subtitles) then
            begin
            fSubtitles := AnsiString(StringToUtf8(string(Subtitles)));
            fSubtitlesExt := '.oml';
            Result := True;
            end;
      finally
        AMFResponse.Free;
        end;
  finally
    AMFRequest.Free;
    end;
end;
{$ENDIF}

initialization
  RegisterDownloader(TDownloader_OverStream);

end.
