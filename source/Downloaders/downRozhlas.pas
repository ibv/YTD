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

unit downRozhlas;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Rozhlas = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: TRegExp;
      TableRowsRegExp: TRegExp;
      StreamIdRegExp: TRegExp;
      StreamTitleRegExp: TRegExp;
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

// http://www.rozhlas.cz/vltava/porady/_zprava/676996
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rozhlas\.cz/vltava/porady/_zprava/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_TABLE_ROWS = '<tr>(?P<ROW>.*?)</tr>';
  //REGEXP_STREAM_ID = '"https?://(?:www\.)?rozhlas\.cz/default/default/rnp-player\.php\?id=(?P<ID>[0-9]+)"';
  REGEXP_STREAM_ID = 'https?://(?:[a-z0-9]+\.)*rozhlas.cz/(?:_audio|stream)/(?P<ID>[0-9]+)\.mp3';
  REGEXP_STREAM_TITLE = '<strong>(?P<TITLE>.*?)</strong>';

{ TDownloader_Rozhlas }

class function TDownloader_Rozhlas.Provider: string;
begin
  Result := 'Rozhlas.cz';
end;

class function TDownloader_Rozhlas.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Rozhlas.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUnknown;
  TableRowsRegExp := RegExCreate(REGEXP_TABLE_ROWS);
  StreamIdRegExp := RegExCreate(REGEXP_STREAM_ID);
  StreamTitleRegExp := RegExCreate(REGEXP_STREAM_TITLE);
end;

destructor TDownloader_Rozhlas.Destroy;
begin
  RegExFreeAndNil(TableRowsRegExp);
  RegExFreeAndNil(StreamIdRegExp);
  RegExFreeAndNil(StreamTitleRegExp);
  inherited;
end;

function TDownloader_Rozhlas.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rozhlas.cz/vltava/porady/_zprava/' + MovieID;
end;

function TDownloader_Rozhlas.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  MediaUrl = 'http://media.rozhlas.cz/_audio/%s.mp3';
var Row, Title, ID: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if TableRowsRegExp.Match(Page) then
    begin
    repeat
      Row := TableRowsRegExp.SubexpressionByName('ROW');
      if GetRegExpVar(StreamIdRegExp, Row, 'ID', ID) and GetRegExpVar(StreamTitleRegExp, Row, 'TITLE', Title) then
        begin
        {$IFDEF MULTIDOWNLOADS}
        NameList.Add(Title);
        UrlList.Add(Format(MediaUrl, [ID]));
        {$ELSE}
        SetName(Title);
        MovieUrl := Format(MediaUrl, [ID]);
        Result := True;
        SetPrepared(True);
        Exit;
        {$ENDIF}
        end;
    until not TableRowsRegExp.MatchAgain;
    {$IFDEF MULTIDOWNLOADS}
    if UrlList.Count <= 0 then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      SetPrepared(True);
      Result := First;
      end;
    {$ELSE}
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL));
    {$ENDIF}
    end;
end;

initialization
  RegisterDownloader(TDownloader_Rozhlas);

end.
