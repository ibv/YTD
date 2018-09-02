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

unit downStream;
{$INCLUDE 'ytd.inc'}

// Token se da ziskat debugovanim stranky prehravace v Chrome,
// dat breakpoint na funkci MD5 v /static/js/stream.all.js?751835d
// a podivat se, co je pred lomitkem.

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, uJSON, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uHttpDownloader, uHttpDirectDownloader;

type
  TDownloader_Stream = class(THttpDownloader)
    private
    protected
      function GetApiPath: string;
      function GetApiPassword: string;
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean; override;
      procedure ProcessMediaList(const ElName: string; Elem: TJSONNode; data: pointer);
      procedure ProcessMediaFormatList(const ElName: string; Elem: TJSONNode; data: pointer);
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
  uCompatibility,
  uStrings,
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// https://www.stream.cz/chlapidarium/10008864-zeny-ktere-byste-mit-doma-nechteli
const
  URLREGEXP_BEFORE_ID = 'stream\.cz/[^/]+/';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

const
  STREAM_API_URL = 'https://www.stream.cz/API';
  STREAM_TOKEN = 'fb5f58a820353bd7095de526253c14fd';

{ TDownloader_Stream }

class function TDownloader_Stream.Provider: string;
begin
  Result := 'Stream.cz';
end;

class function TDownloader_Stream.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfRequireSecureToken];
end;

class function TDownloader_Stream.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
end;

destructor TDownloader_Stream.Destroy;
begin
  inherited;
end;

function TDownloader_Stream.GetApiPath: string;
begin
  Result := '/episode/' + MovieID;
end;

function TDownloader_Stream.GetApiPassword: string;
var
  Data: AnsiString;
begin
  Data := {$IFDEF UNICODE} AnsiString {$ENDIF} (STREAM_TOKEN + GetApiPath + IntToStr(UnixTimestamp div (3600*24)));
  Result := HexEncode(MD5(Data));
end;

function TDownloader_Stream.GetMovieInfoUrl: string;
begin
  Result := STREAM_API_URL + GetApiPath;
end;

function TDownloader_Stream.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean;
begin
  ClearHttp(Http);
  Http.Headers.Add('Api-Password: '+ GetApiPassword);
  Result := DownloadPage(Http, Url, Page, InfoPageEncoding, hmGET, False);
  Xml := nil;
end;

type
  PBestUrlInfo = ^TBestUrlInfo;
  TBestUrlInfo = record
    Url: string;
    Quality: integer;
    Found: boolean;
  end;

procedure TDownloader_Stream.ProcessMediaFormatList(const ElName: string; Elem: TJSONNode; data: pointer);
var
  BestUrl: PBestUrlInfo;
  Url, sQuality: string;
  i, Quality: integer;
begin
  BestUrl := data;
  //Writeln(TlkJSON.GenerateText(Elem));
  if JSONValue(Elem, 'source', Url) then
    begin
    sQuality := JSONValue(Elem, 'quality');
    Quality := 0;
    for i := 1 to Length(sQuality) do
      if CharInSet(sQuality[i], ['0'..'9']) then
        Quality := 10 * Quality + Ord(sQuality[i]) - Ord('0')
      else
        Break;
    if Quality > BestUrl.Quality then
      begin
      BestUrl.Url := Url;
      BestUrl.Quality := Quality;
      BestUrl.Found := True;
      end;
    end;
end;

procedure TDownloader_Stream.ProcessMediaList(const ElName: string; Elem: TJSONNode; data: pointer);
var
  Items: TJSONnode;
begin
  //Writeln(TlkJSON.GenerateText(Elem));
  if JSONNodeByPath(Elem, 'formats', Items) then
    JSONForEach(Items, ProcessMediaFormatList, data);
end;

function TDownloader_Stream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Info, UrlList: TJSON;
  BestUrl: TBestUrlInfo;
  ErrorMsg, Title: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Info := JSONCreate(Page);
  try
    if JSONValue(Info, 'hint', ErrorMsg) then
      SetLastErrorMsg(Format(ERR_SERVER_ERROR, [ErrorMsg]))
    else if not JSONValue(Info, 'name', Title) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
    else if not JSONNodeByPath(Info, 'video_qualities', UrlList) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
    else
      begin
      BestUrl.Url := '';
      BestUrl.Quality := -1;
      BestUrl.Found := False;
      JSONForEach(UrlList, ProcessMediaList, @BestUrl);
      if not BestUrl.Found then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        MovieUrl := BestUrl.Url;
        Name := {$IFNDEF UNICODE} AnsiEncodedUtf8ToString {$ENDIF} (Title);
        SetPrepared(True);
        Result := True;
        end;
      end;
  finally
    JSONFreeAndNil(Info);
    end;
end;

initialization
  RegisterDownloader(TDownloader_Stream);

end.
