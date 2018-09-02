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

unit downStudioPlus;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_StudioPlus = class(THttpDownloader)
    private
    protected
      TitlePartsRegExp: TRegExp;
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

// http://studio-plus.tv/?video_id=270
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*studio-plus\.tv/.*[?&]video_id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h2>(?P<TITLE>.*?)</h2>';
  REGEXP_EXTRACT_URL = '<param\s+name="URL"\s+value="(?P<URL>https?://.+?)"';
  REGEXP_TITLE_PARTS = '<a\s[^>]*>(?P<PART>.*?)</a>';

{ TDownloader_StudioPlus }

class function TDownloader_StudioPlus.Provider: string;
begin
  Result := 'Studio-Plus.tv';
end;

class function TDownloader_StudioPlus.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_StudioPlus.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL);
  TitlePartsRegExp := RegExCreate(REGEXP_TITLE_PARTS);
end;

destructor TDownloader_StudioPlus.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  RegExFreeAndNil(TitlePartsRegExp);
  inherited;
end;

function TDownloader_StudioPlus.GetMovieInfoUrl: string;
begin
  Result := 'http://studio-plus.tv/?video_id=' + MovieID;
end;

function TDownloader_StudioPlus.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Title: string;
  Part: TStringArray;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := Prepared;
  if Result then
    if GetRegExpAllVar(TitlePartsRegExp, Name, 'PART', Part) then
      begin
      Title := '';
      for i := 0 to Pred(Length(Part)) do
        if Part[i] <> '' then
          if Title = '' then
            if Part[i] = 'Hlavní stránka' then
              begin
              // Skip it
              end
            else
              Title := Part[i]
          else
            Title := Title + ' - ' + Part[i];
      if Title <> '' then
        SetName(Title);
      end;
end;

initialization
  RegisterDownloader(TDownloader_StudioPlus);

end.
