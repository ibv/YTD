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

unit downStream;
{$INCLUDE 'ytd.inc'}
{.DEFINE XMLINFO}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Stream = class(THttpDownloader)
    private
    protected
      MovieParamsRegExp: TRegExp;
      FlashVarsParserRegExp: TRegExp;
      {
      MovieIdFromParamsRegExp: TRegExp;
      MovieHDIdFromParamsRegExp: TRegExp;
      MovieCdnIdFromParamsRegExp: TRegExp;
      }
      function GetMovieInfoUrlForID(const ID: string): string; virtual;
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
  {$IFDEF XMLINFO}
  uXML,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://www.stream.cz/reklamozrouti/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/video/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/object/410282-reklamozrouti-medvedi-reklama
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stream\.cz/(?:[^/]+/)*';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)\s+-[^<-]+</title>';
  REGEXP_MOVIE_PARAMS = '<param\s+name="flashvars"\s+value="(?P<PARAM>.*?)"|\swriteSWF\s*\((?P<PARAM2>.*?)\)\s*;';
  REGEXP_FLASHVARS_PARSER = '(?:^|&)(?P<VARNAME>[^&]+?)=(?P<VARVALUE>[^&]+)';
  {
  REGEXP_MOVIE_ID_FROM_PARAMS = '[&'']id=(?P<ID>[0-9]+)';
  REGEXP_MOVIE_HDID_FROM_PARAMS = '[&'']hdID=(?P<ID>[0-9]+)';
  REGEXP_MOVIE_CDNID_FROM_PARAMS = '[&'']cdnID=(?P<ID>[0-9]+)';
  }
  
{ TDownloader_Stream }

class function TDownloader_Stream.Provider: string;
begin
  Result := 'Stream.cz';
end;

class function TDownloader_Stream.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsParserRegExp := RegExCreate(REGEXP_FLASHVARS_PARSER, [rcoIgnoreCase, rcoSingleLine]);
  {
  MovieIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PARAMS, [rcoIgnoreCase]);
  MovieHDIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_HDID_FROM_PARAMS, [rcoIgnoreCase]);
  MovieCdnIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_CDNID_FROM_PARAMS, [rcoIgnoreCase]);
  }
end;

destructor TDownloader_Stream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(FlashVarsParserRegExp);
  {
  RegExFreeAndNil(MovieIdFromParamsRegExp);
  RegExFreeAndNil(MovieHDIdFromParamsRegExp);
  RegExFreeAndNil(MovieCdnIdFromParamsRegExp);
  }
  inherited;
end;

function TDownloader_Stream.GetMovieInfoUrl: string;
begin
  Result := GetMovieInfoUrlForID(MovieID);
end;

function TDownloader_Stream.GetMovieInfoUrlForID(const ID: string): string;
begin
  Result := 'http://www.stream.cz/video/' + ID;
end;

function TDownloader_Stream.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var {$IFDEF XMLINFO}
    Title: string;
    Xml: TXmlDoc;
    TitleNode, ContentNode: TjanXmlNode2;
    {$ENDIF}
    Params, CdnID, CdnLQ, CdnHQ, CdnHD, ID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Params := '';
  if MovieParamsRegExp.Match(Page) then
    if not MovieParamsRegExp.SubexpressionByName('PARAM', Params) then
      Params := MovieParamsRegExp.SubexpressionByName('PARAM2');
  if Params = '' then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not GetRegExpVarPairs(FlashVarsParserRegExp, Params,
                 ['id', 'cdnLQ', 'cdnHQ', 'cdnHD'],
                 [@ID,  @CdnLQ,  @CdnHQ,  @CdnHD ])
  then
  //else if not GetRegExpVar(MovieCdnIdFromParamsRegExp, Params, 'ID', CdnID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    if CdnHD <> '' then
      CdnID := CdnHD
    else if CdnHQ <> '' then
      CdnID := CdnHQ
    else if CdnLQ <> '' then
      CdnID := CdnLQ
    else
      CdnID := '';
    if CdnID = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
    {$IFDEF XMLINFO}
    if GetRegExpVar(MovieIdFromParamsRegExp, Params, 'ID', ID) then
      try
        if DownloadXml(Http, 'http://flash.stream.cz/get_info/' + ID, Xml) then
          try
            if GetXmlVar(Xml, 'video/title', Title) then
              SetName(Title);
          finally
            Xml.Free;
            end;
      except
        ;
        end;
    {$ENDIF}
    if DownloadPage(Http, 'http://cdn-dispatcher.stream.cz/?id=' + CdnID, hmHEAD) then
      begin
      MovieURL := LastUrl;
      Result := True;
      SetPrepared(True);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Stream);

end.
