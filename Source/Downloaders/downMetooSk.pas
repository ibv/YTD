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

unit downMetooSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaUtil,
  uDownloader, uCommonDownloader, uNestedDownloader, uMSDownloader, uRTMPDownloader;

type
  TDownloader_MetooSk = class(TNestedDownloader)
    private
    protected
      RtmpServerRegExp: TRegExp;
      RtmpStreamRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
    public
      class function Features: TDownloaderFeatures; override;
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;
    
  TDownloader_MetooSk_MS = class(TMSDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  TDownloader_MetooSk_RTMP = class(TRTMPDownloader)
    private
    protected
      RtmpServerRegExp: TRegExp;
      RtmpStreamRegExp: TRegExp;
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
  uDownloadClassifier,
  uMessages;

// http://pohoda.metoo.sk/pohoda2011-fibernet/?relacia=cluster
// http://www.metoo.sk/voicestv?relacia=vl26-sto-much-stratasoul-bene-sewologylab
// http://www.metoo.sk/vychodna2012?relacia=vrchovska

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + 'metoo\.sk/' + REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '<title>(?P<TITLE>.*?)\s*(?:\||</title>)';
  REGEXP_MOVIE_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = REGEXP_URL_EMBED_SRC;
  REGEXP_RTMP_SERVER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '\.addVariable\s*\(\s*''streamer''\s*,\s*''(?P<SERVER>rtmpt?e?://.+?)''';
  REGEXP_RTMP_STREAM {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '\.addVariable\s*\(\s*''file''\s*,\s*''(?P<STREAM>.+?)''';

{ TDownloader_MetooSk }

class function TDownloader_MetooSk.Features: TDownloaderFeatures;
begin
  Result := inherited Features
    + TDownloader_MetooSk_MS.Features
    + TDownloader_MetooSk_RTMP.Features;
end;

class function TDownloader_MetooSk.Provider: string;
begin
  Result := 'Metoo.sk';
end;

class function TDownloader_MetooSk.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_MetooSk.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  RtmpServerRegExp := RegExCreate(REGEXP_RTMP_SERVER);
  RtmpStreamRegExp := RegExCreate(REGEXP_RTMP_STREAM);
end;

destructor TDownloader_MetooSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(RtmpServerRegExp);
  RegExFreeAndNil(RtmpStreamRegExp);
  inherited;
end;

function TDownloader_MetooSk.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_MetooSk.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
var
  Url, Server, Stream: string;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  Result := False;
  if not Result then
    if GetRegExpVar(MovieUrlRegExp, Page, 'URL', Url) then
      begin
      Downloader := TDownloader_MetooSk_MS.Create(MovieID);
      Result := True;
      end;
  if not Result then
    if GetRegExpVar(RtmpServerRegExp, Page, 'SERVER', Server) then
      if GetRegExpVar(RtmpStreamRegExp, Page, 'STREAM', Stream) then
        begin
        Downloader := TDownloader_MetooSk_RTMP.Create(MovieID);
        Result := True;
        end;
end;

{ TDownloader_MetooSk_MS }

class function TDownloader_MetooSk_MS.Provider: string;
begin
  Result := TDownloader_MetooSk.Provider;
end;

class function TDownloader_MetooSk_MS.UrlRegExp: string;
begin
  Result := TDownloader_MetooSk.UrlRegExp;
end;

constructor TDownloader_MetooSk_MS.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
end;

destructor TDownloader_MetooSk_MS.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_MetooSk_MS.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

{ TDownloader_MetooSk_RTMP }

class function TDownloader_MetooSk_RTMP.Provider: string;
begin
  Result := TDownloader_MetooSk.Provider;
end;

class function TDownloader_MetooSk_RTMP.UrlRegExp: string;
begin
  Result := TDownloader_MetooSk.UrlRegExp;
end;

constructor TDownloader_MetooSk_RTMP.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  RtmpServerRegExp := RegExCreate(REGEXP_RTMP_SERVER);
  RtmpStreamRegExp := RegExCreate(REGEXP_RTMP_STREAM);
end;

destructor TDownloader_MetooSk_RTMP.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(RtmpServerRegExp);
  RegExFreeAndNil(RtmpStreamRegExp);
  inherited;
end;

function TDownloader_MetooSk_RTMP.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_MetooSk_RTMP.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Server, Stream: string;
  Protocol, User, Password, Host, Port, Path, Paragraph: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(RtmpServerRegExp, Page, 'SERVER', Server) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
  else if not GetRegExpVar(RtmpStreamRegExp, Page, 'STREAM', Stream) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else
    begin
    ParseUrl(Server, Protocol, User, Password, Host, Port, Path, Paragraph);
    MovieUrl := Server + '/' + Stream;
    Self.RtmpUrl := Server;
    Self.RtmpApp := Copy(Path, 2, MaxInt);
    Self.Playpath := 'mp4:' + Stream;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_MetooSk);

end.
