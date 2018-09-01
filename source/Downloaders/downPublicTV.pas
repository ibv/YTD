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

unit downPublicTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PublicTV = class(THttpDownloader)
    private
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

// http://www.publictv.cz/cz/menu/3/videoarchiv/clanek-535-re-play/1718/
// http://www.publictv.cz/videoarchiv/535/1718/
// http://www.publictv.cz/cz/menu/3/videoarchiv/clanek-22184-jukebox-s-lenny/1935/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*publictv\.cz/';
  URLREGEXP_ID =        '(?:[^/?&]+/)*videoarchiv/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<td><p\s+style="text-align:\s*justify;">(?P<TITLE>.*?)</p></td>';
  REGEXP_EXTRACT_URL = '\bvar\s+cfg\s*=\s*\{\s*file\s*:\s*''(?P<URL>.+?)''';

{ TDownloader_PublicTV }

class function TDownloader_PublicTV.Provider: string;
begin
  Result := 'PublicTV.com';
end;

class function TDownloader_PublicTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PublicTV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PublicTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_PublicTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.publictv.cz/' + MovieID;
end;

function TDownloader_PublicTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    i: integer;
    HREF, Title: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  SetPrepared(False);
  MovieUrl := 'http://www.publictv.cz' + MovieUrl;
  if AnsiCompareText(ExtractFileExt(MovieUrl), '.asx') = 0 then
    begin
    if not DownloadXml(Http, MovieUrl, Xml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      try
        for i := 0 to Pred(Xml.Root.NodeCount) do
          if Xml.Root.Nodes[i].Name = 'ENTRY' then
            if GetXmlAttr(Xml.Root.Nodes[i], 'REF', 'HREF', HREF) then
              if GetXmlVar(Xml.Root.Nodes[i], 'TITLE', Title) then
                begin
                {$IFDEF MULTIDOWNLOADS}
                NameList.Add(Title);
                UrlList.Add('http://www.publictv.cz' + HREF);
                {$ELSE}
                SetName(Title);
                MovieUrl := 'http://www.publictv.cz' + HREF;
                Result := True;
                SetPrepared(True);
                Exit;
                {$ENDIF}
                end;
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
      finally
        Xml.Free;
        end;
    end
  else
    begin
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_PublicTV);

end.
