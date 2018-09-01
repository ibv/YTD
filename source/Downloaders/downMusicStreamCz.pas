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

unit downMusicStreamCz;
{$INCLUDE 'ytd.inc'}
{.DEFINE USE_RTMP}
  // It is possible to download using both HTTP and RTMP protocols

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader,
  {$IFDEF USE_RTMP} uRtmpDownloader {$ELSE} uHttpDownloader {$ENDIF} ;

type
  TDownloader_MusicStreamCz = class( {$IFDEF USE_RTMP} TRtmpDownloader {$ELSE} THttpDownloader {$ENDIF} )
    private
    protected
      StreamIDRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

// http://music.stream.cz/klip/268709-avril-lavigne-what-the-hell
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*music\.stream\.cz/klip/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_STREAM_ID = '''id=(?P<ID>[0-9]+)';

{ TDownloader_MusicStreamCz }

class function TDownloader_MusicStreamCz.Provider: string;
begin
  Result := 'Stream.cz';
end;

class function TDownloader_MusicStreamCz.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_MusicStreamCz.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  StreamIDRegExp := RegExCreate(REGEXP_STREAM_ID);
end;

destructor TDownloader_MusicStreamCz.Destroy;
begin
  RegExFreeAndNil(StreamIDRegExp);
  inherited;
end;

function TDownloader_MusicStreamCz.GetMovieInfoUrl: string;
begin
  Result := 'http://music.stream.cz/klip/' + MovieID;
end;

function TDownloader_MusicStreamCz.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const DISPATCHER_URL = 'http://cdn-dispatcher.stream.cz/getSource?id=%s&proto=' + {$IFDEF USE_RTMP} 'rtmp,rtmpe' {$ELSE} 'http' {$ENDIF} ;
var ID, CdnID, Title, BaseUrl, Service, Path: string;
    Xml, Dispatcher: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(StreamIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not DownloadXml(Http, 'http://flash.stream.cz/get_info/' + ID, Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not Xml.NodeByPath('video', Node) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not (GetXmlAttr(Node, '', 'hdID', CdnID) or GetXmlAttr(Node, '', 'cdnID', CdnID)) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlVar(Node, 'title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else if not DownloadXml(Http, Format(DISPATCHER_URL, [CdnID]), Dispatcher) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST))
      else
        try
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL));
          if Dispatcher.NodeByPath('server', Node) then
            if GetXmlAttr(Node, '', 'baseUrl', BaseUrl) then
              if GetXmlAttr(Node, '', 'service', Service) then
                if GetXmlAttr(Node, '', 'path', Path) then
                  begin
                  SetName(Title);
                  {$IFDEF USE_RTMP}
                  MovieUrl := BaseUrl + '/' + Service + 'mp4:' + Path;
                  AddRtmpDumpOption('r', BaseUrl + '/' + Service);
                  AddRtmpDumpOption('y', 'mp4:' + Path);
                  AddRtmpDumpOption('f', 'WIN 10,1,82,76');
                  {$ELSE}
                  MovieUrl := BaseUrl + '/' + Path;
                  {$ENDIF}
                  SetPrepared(True);
                  Result := True;
                  end;
        finally
          Dispatcher.Free;
          end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_MusicStreamCz);

end.
