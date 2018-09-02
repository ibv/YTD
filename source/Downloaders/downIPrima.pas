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

unit downIPrima;
{$INCLUDE 'ytd.inc'}
{$DEFINE PRIMA_LIVEBOX}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  {$IFDEF PRIMA_LIVEBOX}
  uRtmpDownloader,
  {$ENDIF}
  uHttpDownloader, downStream;

type
  TDownloader_iPrima = class(TNestedDownloader)
    private
    protected
      {$IFDEF PRIMA_LIVEBOX}
      LiveBoxRegExp: TRegExp;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  TDownloader_iPrima_Stream = class(TDownloader_Stream)
    private
    protected
      StreamIDRegExp: TRegExp;
      StreamCDNIDRegExp: TRegExp;
      TryingHighestQuality: boolean;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoUrlForID(const ID: string): string; override;
      function GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

  {$IFDEF PRIMA_LIVEBOX}
  TDownloader_iPrima_LiveBox = class(TRtmpDownloader)
    private
    protected
      LiveBoxRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;
  {$ENDIF}

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.iprima.cz/videoarchiv/44524/all/all
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*iprima\.cz/';
  URLREGEXP_ID =        '(?:videoarchiv|videoplayer)/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = '<h3\s+id="videoTitle">(?P<TITLE>.*?)</h3>';
  REGEXP_STREAM_ID = '<param\s+name="flashvars"\s+value="[^"]*&id=(?P<STREAMID>[0-9]+)';
  REGEXP_STREAM_CDNID = '<param\s+name="flashvars"\s+value="[^"]*&cdnID=(?P<STREAMID>[0-9]+)';
  {$IFDEF PRIMA_LIVEBOX}
  REGEXP_LIVEBOX = '\bLiveboxPlayer\.init\s*\((?:\s*''[^'']*''\s*,)\s*width\s*,\s*height\s*,\s*''(?P<HQ>[^'']*)''\s*,\s*''(?P<LQ>[^'']*)''';
  {$ENDIF}

const
  PRIMA_PROVIDER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'iPrima.cz';
  PRIMA_URLREGEXP {$IFDEF MINIMIZESIZE} : string {$ENDIF} = URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
  PRIMA_MOVIE_INFO_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://www.iprima.cz/%s';

{ TDownloader_iPrima }

class function TDownloader_iPrima.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_iPrima.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  {$IFDEF PRIMA_LIVEBOX}
  LiveBoxRegExp := RegExCreate(REGEXP_LIVEBOX);
  {$ENDIF}
end;

destructor TDownloader_iPrima.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  {$IFDEF PRIMA_LIVEBOX}
  RegExFreeAndNil(LiveBoxRegExp);
  {$ENDIF}
  inherited;
end;

function TDownloader_iPrima.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  {$IFDEF PRIMA_LIVEBOX}
  if GetRegExpVars(LiveBoxRegExp, Page, [], []) then
    Downloader := TDownloader_iPrima_LiveBox.Create(MovieID)
  else
  {$ENDIF}
    Downloader := TDownloader_iPrima_Stream.Create(MovieID);
  Result := True;
end;

{ TDownloader_iPrima_Stream }

class function TDownloader_iPrima_Stream.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima_Stream.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_iPrima_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  StreamIDRegExp := RegExCreate(REGEXP_STREAM_ID);
  StreamCDNIDRegExp := RegExCreate(REGEXP_STREAM_CDNID);
end;

destructor TDownloader_iPrima_Stream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(StreamIDRegExp);
  RegExFreeAndNil(StreamCDNIDRegExp);
  inherited;
end;

function TDownloader_iPrima_Stream.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima_Stream.GetMovieInfoUrlForID(const ID: string): string;
begin
  if TryingHighestQuality then
    Result := 'http://prima.stream.cz/' + ID
  else
    Result := inherited GetMovieInfoUrlForID(ID);
end;

function TDownloader_iPrima_Stream.GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean;
begin
  Result := inherited GetFlashVarsIdStrings(ID, cdnLQ, cdnHQ, cdnHD, Title);
  if not TryingHighestQuality then
    begin
    cdnLQ := 'lqID';
    cdnHQ := 'hqID';
    cdnHD := 'hdID';
    end;
end;

function TDownloader_iPrima_Stream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, ID, EmbeddedPage: string;
    EmbeddedPageXml: TXmlDoc;
begin
  Result := False;
  if GetRegExpVar(StreamIDRegExp, Page, 'STREAMID', ID) then
    begin
    // Chci zkusit "Highest quality"
    if not Result then
      begin
      TryingHighestQuality := True;
      try
        Url := GetMovieInfoUrlForID(ID);
        if GetMovieInfoContent(Http, Url, EmbeddedPage, EmbeddedPageXml) then
          if inherited AfterPrepareFromPage(EmbeddedPage, EmbeddedPageXml, Http) then
            Result := Prepared;
      finally
        TryingHighestQuality := False;
        end;
      end;
    // Pokud se nepodari, zkusim normalni kvalitu, a to napred pres Primu
    if not Result then
      begin
      Url := GetMovieInfoUrlForID(ID);
      if GetMovieInfoContent(Http, Url, EmbeddedPage, EmbeddedPageXml) then
        if inherited AfterPrepareFromPage(EmbeddedPage, EmbeddedPageXml, Http) then
          Result := Prepared
    // a pak pres Stream samotny
        else if inherited AfterPrepareFromPage(Page, PageXml, Http) then
          Result := Prepared
      end;
    end;
  // Nakonec zkusim primo stahnout cdnID
  if not Result then
    if GetRegExpVar(StreamCDNIDRegExp, Page, 'STREAMID', ID) then
      begin
      ExternalCDNID := ID;
      Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
      end
end;

{$IFDEF PRIMA_LIVEBOX}

{ TDownloader_iPrima_LiveBox }

class function TDownloader_iPrima_LiveBox.Provider: string;
begin
  Result := PRIMA_PROVIDER;
end;

class function TDownloader_iPrima_LiveBox.UrlRegExp: string;
begin
  Result := Format(PRIMA_URLREGEXP, [MovieIDParamName]);
end;

constructor TDownloader_iPrima_LiveBox.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  LiveBoxRegExp := RegExCreate(REGEXP_LIVEBOX);
end;

destructor TDownloader_iPrima_LiveBox.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(LiveBoxRegExp);
  inherited;
end;

function TDownloader_iPrima_LiveBox.GetMovieInfoUrl: string;
begin
  Result := Format(PRIMA_MOVIE_INFO_URL, [MovieID]);
end;

function TDownloader_iPrima_LiveBox.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  HQStream, LQStream, Stream: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(LiveBoxRegExp, Page, ['HQ', 'LQ'], [@HQStream, @LQStream]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    if HQStream <> '' then
      Stream := HQStream
    else
      Stream := LQStream;
    MovieUrl := 'rtmp://iprima.livebox.cz/iprima/' + Stream;
    Self.RtmpUrl := MovieURL;
    Result := True;
    SetPrepared(True);
    end;
end;

{$ENDIF}

function TDownloader_iPrima_LiveBox.GetFileNameExt: string;
begin
  Result := '.flv';
end;

initialization
  RegisterDownloader(TDownloader_iPrima);
  //RegisterDownloader(TDownloader_iPrima_Stream);
  //RegisterDownloader(TDownloader_iPrima_LiveBox);

end.
