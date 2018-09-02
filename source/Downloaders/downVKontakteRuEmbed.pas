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

unit downVKontakteRuEmbed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VKontakteRuEmbed = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
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

// http://vkontakte.ru/video_ext.php?oid=98777833&id=159674868&hash=c4cd1179fb4e52d1&hd=1
// http://vk.com/video_ext.php?oid=106919938&id=161961696&hash=bb3e2d1a73bdb262
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + '(?:vkontakte\.ru|vk\.com)/video_ext\.php\?.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_FLASHVARS = '\bvar\s+video_(?P<VARNAME>[a-z0-9_]+)\s*=\s*''(?P<VARVALUE>.*?)''';

{ TDownloader_VKontakteRuEmbed }

class function TDownloader_VKontakteRuEmbed.Provider: string;
begin
  Result := 'VKontakte.ru';
end;

class function TDownloader_VKontakteRuEmbed.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_VKontakteRuEmbed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peAnsi;
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
end;

destructor TDownloader_VKontakteRuEmbed.Destroy;
begin
  RegExFreeAndNil(FlashVarsRegExp);
  inherited;
end;

function TDownloader_VKontakteRuEmbed.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_VKontakteRuEmbed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  Quality: array[0..3] of integer = (720, 480, 360, 240);
var
  Host, UID, VTag, Url, Title: string;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  GetRegExpVarPairs(FlashVarsRegExp, Page, ['host', 'uid', 'vtag', 'title'], [@Host, @UID, @VTag, @Title]);
  if Host = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['Host']))
  else if UID = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['UID']))
  else if VTag = '' then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['VTag']))
  else
    begin
    for i := 0 to Pred(Length(Quality)) do
      begin
      Url := Format('%su%s/video/%s.%d.mp4', [Host, UID, VTag, Quality[i]]);
      if DownloadPage(Http, Url, hmHEAD) then
        begin
        SetName(HtmlDecode(UrlDecode(Title)));
        MovieUrl := Url;
        SetPrepared(True);
        Result := True;
        end;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VKontakteRuEmbed);

end.
