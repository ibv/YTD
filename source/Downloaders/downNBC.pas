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

unit downNBC;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_NBC = class(TRtmpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function ReadMediaInfo(Http: THttpSend; const MovieID: string; out Title, SmilUrl: string): boolean; virtual;
      function ReadVideoPath(Http: THttpSend; const SmilUrl: string; out VideoPath: string): boolean; virtual;
      function ReadVideoServer(Http: THttpSend; out VideoHost, VideoServer, VideoApp: string): boolean; virtual;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uAMF,
  uDownloadClassifier,
  uMessages;

// http://www.nbc.com/classic-tv/miami-vice/video/pilot/213020/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*nbc\.com/(?:[^/]+/)*video/[^/]+/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_MOVIE_ID = '\bvar\s+assetId\s*=\s*(?P<ID>[0-9]+)';

const
  AMF_REQUEST_PACKET =
    'AAAAAAABABZnZXRDbGlwSW5mby5nZXRDbGlwQWxsAAIvMQAAAB4KAAAABAIABjIxMzUwNAIA' +
    'AlVTAgADNjMyAgACLTE=';

{ TDownloader_NBC }

class function TDownloader_NBC.Provider: string;
begin
  Result := 'NBC.com';
end;

class function TDownloader_NBC.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_NBC.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
end;

destructor TDownloader_NBC.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_NBC.GetMovieInfoUrl: string;
begin
  Result := 'DUMMY';
end;

function TDownloader_NBC.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Page := '';
  Xml := nil;
  Result := True;
end;

function TDownloader_NBC.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Title, SmilUrl, VideoPath, VideoHost, VideoServer, VideoApp: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if ReadMediaInfo(Http, MovieID, Title, SmilUrl) then
    if ReadVideoPath(Http, SmilUrl, VideoPath) then
      if ReadVideoServer(Http, VideoHost, VideoServer, VideoApp) then
          begin
          SetName(Title);
          MovieUrl := 'rtmp://' + VideoServer + '/' + VideoApp + '/' + VideoPath;
          AddRtmpDumpOption('r', MovieUrl);
          AddRtmpDumpOption('s', 'http://www.nbc.com/[[IMPORT]]/video.nbcuni.com/outlet/extensions/inext_video_player/video_player_extension.swf?4.5.3');
          AddRtmpDumpOption('t', 'rtmp://' + VideoServer + '/' + VideoApp + '?_fcs_vhost=' + VideoHost);
          SetPrepared(True);
          Result := True;
          end;
end;

function TDownloader_NBC.ReadMediaInfo(Http: THttpSend; const MovieID: string; out Title, SmilUrl: string): boolean;
var AMFRequest, AMFResponse: TAMFPacket;
    Error, VideoTitle, VideoSubtitle, Url: TAMFValue;
begin
  Result := False;
  AMFRequest := TAMFPacket.Create;
  try
    AMFRequest.LoadFromString(AnsiString(Base64Decode(AMF_REQUEST_PACKET)));
    // Note: I don't need to check types (or make sure pointers are not null)
    // because I use a pre-made packet which has all required properties. That
    // is not true while parsing response packets!
    TAMFCommonArray(AMFRequest.Body[0].Content).Items[0].Value := MovieID;
    if DownloadAMF(Http, 'http://video.nbcuni.com/amfphp/gateway.php', AMFRequest, AMFResponse) then
      try
        if AMFResponse.HasBody(0) then
          if AMFResponse.Body[0].Content.FindValueByPath('error', Error) and (Error <> 'none') then
            SetLastErrorMsg(Format(_(ERR_SERVER_ERROR), [string(Error)]))
          else if not AMFResponse.Body[0].Content.FindValueByPath('metadata/title', VideoTitle, TAMFString) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
          else if not AMFResponse.Body[0].Content.FindValueByPath('clipurl', Url, TAMFString) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
          else
            begin
            AMFResponse.Body[0].Content.FindValueByPath('metadata/subtitle', VideoSubtitle, TAMFString);
            if VideoSubtitle <> '' then
              Title := Format('%s (%s)', [string(VideoTitle), string(VideoSubtitle)])
            else
              Title := string(VideoTitle);
            SmilUrl := 'http://video.nbcuni.com/' + string(Url);
            Result := True;
            end;
      finally
        AMFResponse.Free;
        end;
  finally
    AMFRequest.Free;
    end;
end;

function TDownloader_NBC.ReadVideoPath(Http: THttpSend; const SmilUrl: string; out VideoPath: string): boolean;
var SmilXml: TXmlDoc;
begin
  Result := False;
  if not DownloadXml(Http, SmilUrl, SmilXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlAttr(SmilXml, 'body/switch/ref', 'src', VideoPath) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        Result := True;
    finally
      SmilXml.Free;
      end;
end;

function TDownloader_NBC.ReadVideoServer(Http: THttpSend; out VideoHost, VideoServer, VideoApp: string): boolean;
var ConfigXml, IpXml: TXmlDoc;
begin
  Result := False;
  if not DownloadXml(Http, 'http://videoservices.nbcuni.com/player/config?configId=17010&clear=true', ConfigXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST))
  else
    try
      if not GetXmlVar(ConfigXml, 'akamaiAppName', VideoApp) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
      else if not GetXmlVar(ConfigXml, 'akamaiHostName', VideoHost) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
      else if not DownloadXml(Http, 'http://' + VideoHost + '/fcs/ident', IpXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
      else
        try
          if not GetXmlVar(IpXml, 'ip', VideoServer) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
          else
            begin
            VideoServer := VideoServer + ':1935';
            Result := True;
            end;
        finally
          IpXml.Free;
          end;
    finally
      ConfigXml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_NBC);

end.
