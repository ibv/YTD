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

unit downTipovani;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_Tipovani = class(TMSDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
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

// http://www.tipovani.cz/online-live-stream-TV/sla-vs-ceb-24-10-2010.aspx
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tipovani\.cz/online-live-stream-TV/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID = '<param\s+name="initParams"\s+value="(?:[^",]*,)*token=(?P<ID>.*?)[",]';

{ TDownloader_Tipovani }

class function TDownloader_Tipovani.Provider: string;
begin
  Result := 'Tipovani.cz';
end;

class function TDownloader_Tipovani.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Tipovani.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
end;

destructor TDownloader_Tipovani.Destroy;
begin
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_Tipovani.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tipovani.cz/online-live-stream-TV/' + MovieID;
end;

function TDownloader_Tipovani.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var ID, Url, Title: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  // Pozn.: To ID by se melo nejak upravit. Na vstupu jsem mel 'OTQ4NTgxMXw2MzQyNDExODA0ODY5MjY4OTU='
  // a do playlistu se posilalo 'OTQ4NTgyN3w2MzQyNDExODA2NTExNjQ0MzA=' (tj. po BASE64 decode to bylo
  // '9485811|634241180486926895' a '9485827|634241180651164430'.
  // Na druhy pokus to bylo 'OTQ4Njk4OHw2MzQyNDEyMTA1NzE0NDY2NzU=' a 'OTQ4Njk5NHw2MzQyNDEyMTA3NDc1NDg2ODU=',
  // tj. '9486988|634241210571446675' a '9486994|634241210747548685'.
  else if not DownloadXml(Http, 'http://sazkadir.kitd.cz/Streaming/Services/ClientPlaylist.aspx?id=' + ID , Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlAttr(Xml, 'ENTRY/REF', 'HREF', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Xml, 'ENTRY/TITLE', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else
        begin
        SetName(Title);
        MovieURL := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_Tipovani);

end.
