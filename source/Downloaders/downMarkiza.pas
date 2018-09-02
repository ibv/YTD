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
      TitleFixRegExp: TRegExp;
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
  uStringConsts,
  {$IFDEF JSON}
  uJSON, uLkJSON,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://video.markiza.sk/archiv-tv-markiza/112/8723
// http://doma.markiza.sk/archiv-doma/rebeli-na-strednej/60667
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*(?<!particka\.)markiza\.sk/archiv-(?:[^/]*)/[^/]+/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{$IFDEF JSON}
const
  JSON_SOURCE_REGEXP = '(?P<JSON>\{.*\})';
  JSON_TITLE_FIX_REGEXP = '(?P<TITLE>.*?)[ .-]*$';
{$ELSE}
const
  PLAYLIST_ITEM_REGEXP = '\{\s*"provider"\s*:(?P<INSIDE>.*?)"url"\s*:\s*"(?P<URL>https?://[^"]+)"';
  NOTANAD_REGEXP = '"notanadd"\s*:\s*(?P<TRUE>true)\b';
  DESCRIPTION_REGEXP = '"description"\s*:\s*"(?P<DESC>[^"]*?)[ .-]*"';
{$ENDIF}

{ TDownloader_Markiza }

class function TDownloader_Markiza.Provider: string;
begin
  Result := 'Markiza.sk';
end;

class function TDownloader_Markiza.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Markiza.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  {$IFDEF JSON}
  JSONSourceRegExp := RegExCreate(JSON_SOURCE_REGEXP);
  TitleFixRegExp := RegExCreate(JSON_TITLE_FIX_REGEXP);
  {$ELSE}
  PlaylistItemRegExp := RegExCreate(PLAYLIST_ITEM_REGEXP);
  NotAnAdRegExp := RegExCreate(NOTANAD_REGEXP);
  DescriptionRegExp := RegExCreate(DESCRIPTION_REGEXP);
  {$ENDIF}
end;

destructor TDownloader_Markiza.Destroy;
begin
  {$IFDEF JSON}
  RegExFreeAndNil(JSONSourceRegExp);
  RegExFreeAndNil(TitleFixRegExp);
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
{$IFDEF JSON}
var JSON, Playlist, NotAnAd, UrlNode, TitleNode: TJSON;
    JSONsrc, Title: string;
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
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE);
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
                        if GetRegExpVar(TitleFixRegExp, TitleNode.Value, 'TITLE', Title) then
                          SetName(Title)
                        else
                          SetName(TitleNode.Value);
                      Exit;
                      end;

        finally
          JSONFreeAndNil(JSON);
          end;
      end;
  {$ELSE ~JSON}
    if PlaylistItemRegExp.Match(Page) then
      begin
      repeat
        if PlaylistItemRegExp.SubexpressionByName('URL', Url) then
          if PlaylistItemRegExp.SubexpressionByName('INSIDE', Inside) then
            if GetRegExpVar(NotAnAdRegExp, Inside, 'TRUE', NotAnAd) then
              begin
              if GetRegExpVar(DescriptionRegExp, Inside, 'DESC', Title) then
              {$IFDEF MULTIDOWNLOADS}
                NameList.Add(Title);
              UrlList.Add(Url);
              {$ELSE}
                SetName(Trim(Title));
              MovieUrl := Url;
              SetPrepared(True);
              Result := True;
              Break;
              {$ENDIF}
              end;
      until not PlaylistItemRegExp.MatchAgain;
      {$IFDEF MULTIDOWNLOADS}
      if UrlList.Count > 0 then
        begin
        SetPrepared(True);
        Result := True;
        end;
      {$ENDIF}
      end;
  {$ENDIF ~JSON}
end;

initialization
  RegisterDownloader(TDownloader_Markiza);

end.
