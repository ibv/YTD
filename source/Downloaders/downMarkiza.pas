(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit downMarkiza;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Markiza = class(THttpDownloader)
    private
    protected
      {$IFDEF JSON}
      JSONSourceRegExp: TRegExp;
      {$ELSE}
      PlaylistItemRegExp: TRegExp;
      NotAnAdRegExp: TRegExp;
      DescriptionRegExp: TRegExp;
      {$ENDIF}
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
  {$IFDEF JSON}
  uJSON, uLkJSON,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://video.markiza.sk/archiv-tv-markiza/112/8723
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*markiza\.sk/archiv-tv-markiza/[^/]+/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{$IFDEF JSON}
const
  JSON_SOURCE_REGEXP = '(?P<JSON>\{.*\})';
{$ELSE}
const
  PLAYLIST_ITEM_REGEXP = '\{\s*"provider"\s*:(?P<INSIDE>.*?)"url"\s*:\s*"(?P<URL>https?://[^"]+)"';
  NOTANAD_REGEXP = '"notanadd"\s*:\s*(?P<TRUE>true)\b';
  DESCRIPTION_REGEXP = '"description"\s*:\s*"(?P<DESC>[^"]*)"';
{$ENDIF}

{ TDownloader_Markiza }

class function TDownloader_Markiza.Provider: string;
begin
  Result := 'Markiza.sk';
end;

class function TDownloader_Markiza.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Markiza.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  {$IFDEF JSON}
  JSONSourceRegExp := RegExCreate(JSON_SOURCE_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  {$ELSE}
  PlaylistItemRegExp := RegExCreate(PLAYLIST_ITEM_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  NotAnAdRegExp := RegExCreate(NOTANAD_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  DescriptionRegExp := RegExCreate(DESCRIPTION_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
end;

destructor TDownloader_Markiza.Destroy;
begin
  {$IFDEF JSON}
  RegExFreeAndNil(JSONSourceRegExp);
  {$ELSE}
  RegExFreeAndNil(PlaylistItemRegExp);
  RegExFreeAndNil(NotAnAdRegExp);
  RegExFreeAndNil(DescriptionRegExp);
  {$ENDIF}
  inherited;
end;

function TDownloader_Markiza.GetMovieInfoUrl: string;
begin
  Result := 'http://www.markiza.sk/js/flowplayer/config.js?&media=' + MovieID;
end;

function TDownloader_Markiza.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
{$IFDEF JSON_SOURCE_REGEXP}
var JSON, Playlist, NotAnAd, UrlNode, TitleNode: TJSON;
    JSONsrc: string;
    i: integer;
{$ELSE}
var Url, Inside, NotAnAd, Title: string;
{$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  {$IFDEF JSON}
    {$IFDEF FPC}
      {$MESSAGE WARN 'LkJSON fails on Markiza.sk'}
    {$ELSE}
      {$IFDEF DELPHI6_UP}
        {$MESSAGE WARN 'LkJSON fails on Markiza.sk'}
      {$ENDIF}
    {$ENDIF}
    SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE));
    if GetRegExpVar(JSONSourceRegExp, Page, 'JSON', JSONsrc) then
      begin
      JSON := JSONCreate(JSONsrc);
      if JSON <> nil then
        try
          if JSONNodeByPath(JSON, 'playlist', Playlist) then
            for i := 0 to Pred(Playlist.Count) do
              if Playlist.Child[i] <> nil then
                if Playlist.Child[i] is TlkJSONobject then
                  if JSONNodeByPath(Playlist.Child[i], 'customProperties/notanadd', NotAnAd) then
                    if (NotAnAd is TlkJSONboolean) and TlkJSONboolean(NotAnAd).Value then
                      begin
                      if JSONNodeByPath(Playlist.Child[i], 'url', UrlNode) then
                        begin
                        MovieUrl := UrlNode.Value;
                        SetPrepared(True);
                        Result := True;
                        end;
                      if JSONNodeByPath(Playlist.Child[i], 'customProperties/description', TitleNode) then
                        SetName(TitleNode.Value);
                      Exit;
                      end;

        finally
          JSONFreeAndNil(JSON);
          end;
      end;
  {$ELSE ~JSON}
    if PlaylistItemRegExp.Match(Page) then
      repeat
        if PlaylistItemRegExp.SubexpressionByName('URL', Url) then
          if PlaylistItemRegExp.SubexpressionByName('INSIDE', Inside) then
            if GetRegExpVar(NotAnAdRegExp, Inside, 'TRUE', NotAnAd) then
              begin
              MovieUrl := Url;
              if GetRegExpVar(DescriptionRegExp, Inside, 'DESC', Title) then
                SetName(Trim(Title));
              SetPrepared(True);
              Result := True;
              Break;
              end;
      until not PlaylistItemRegExp.MatchAgain;
  {$ENDIF ~JSON}
end;

initialization
  RegisterDownloader(TDownloader_Markiza);

end.
