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

unit downCrunchyRoll;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_CrunchyRoll = class(TRtmpDownloader)
    private
    protected
      InfoUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetXmlParams(Http: THttpSend; const Request: string; const XmlNames: array of string; const XmlValues: array of PString): boolean;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
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
  uDownloadClassifier,
  uMessages;

// http://www.crunchyroll.com/naruto/episode-193-the-man-who-died-twice-567104
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*crunchyroll\.com/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?:Watch\s+)?(?P<TITLE>.*?)</title>';
  REGEXP_INFO_URL = '"config_url"\s*:\s*"(?P<URL>.*?)"';

{ TDownloader_CrunchyRoll }

class function TDownloader_CrunchyRoll.Provider: string;
begin
  Result := 'CrunchyRoll.com';
end;

class function TDownloader_CrunchyRoll.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [
    {$IFDEF SUBTITLES} dfSubtitles {$ENDIF}
    ];
end;

class function TDownloader_CrunchyRoll.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_CrunchyRoll.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL);
end;

destructor TDownloader_CrunchyRoll.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_CrunchyRoll.GetMovieInfoUrl: string;
begin
  Result := 'http://www.crunchyroll.com/' + MovieID;
end;

function TDownloader_CrunchyRoll.GetXmlParams(Http: THttpSend; const Request: string; const XmlNames: array of string; const XmlValues: array of PString): boolean;
var Xml: TXmlDoc;
    i: integer;
begin
  Result := False;
  for i := 0 to Pred(Length(XmlValues)) do
    XmlValues[i]^ := '';
  if DownloadXml(Http, 'http://www.crunchyroll.com/xml/', AnsiString(UrlEncode(Request)), 'application/x-www-form-urlencoded', Xml) then
    try
      Result := True;
      for i := 0 to Pred(Length(XmlNames)) do
        if not GetXmlVar(Xml, XmlNames[i], XmlValues[i]^) then
          begin
          Result := False;
          Break;
          end;
    finally
      Xml.Free;
      end;
end;

function TDownloader_CrunchyRoll.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, FlvHost, FlvStream, ID: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, UrlDecode(Url), Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(Xml, 'default:preload/media_metadata/media_id', ID) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
      else if not GetXmlParams(Http, 'media_id=' + ID + {'&video_format=102&video_encode_quality=20' +} '&req=RpcApiVideoEncode_GetStreamInfo', ['host', 'file'], [@FlvHost, @FlvStream]) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        {$IFDEF SUBTITLES}
        //if GetXmlParams(Http, 'req=RpcApiSubtitle_GetListing&media_id=' + ID, [], []) then
        {$ENDIF}
        MovieURL := FlvHost + FlvStream;
        AddRtmpDumpOption('r', FlvHost);
        AddRtmpDumpOption('y', FlvStream);
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_CrunchyRoll);

end.
