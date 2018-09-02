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

unit xxxPornTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uCompatibility,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornTube = class(THttpDownloader)
    private
    protected
      PlaylistRegExp: TRegExp;
      PlaylistItemRegExp: TRegExp;
      StreamsRegExp: TRegExp;
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

const
  URLREGEXP_BEFORE_ID = 'porntube\.com/videos/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_PLAYLIST = 'var\s+playerConfigPlaylist\s*=\s*\[(?P<INFO>.*?)\]\s*;';
  REGEXP_MOVIE_PLAYLIST_ITEM = '\bsources\s*:\s*\[\s*(?P<ITEM>.*?)\]\s*,\s*title\s*:\s*"(?P<TITLE>.*?)"';
  REGEXP_MOVIE_STREAMS = '\{\s*"file"\s*:\s*"(?P<URL>https?:[^"]+)"\s*,\s*"label"\s*:\s*"(?P<QUALITY>\d+)[^"]*".*?\}';

{ TDownloader_PornTube }

class function TDownloader_PornTube.Provider: string;
begin
  Result := 'PornTube.com';
end;

class function TDownloader_PornTube.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_PornTube.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  PlaylistRegExp := RegExCreate(REGEXP_MOVIE_PLAYLIST);
  PlaylistItemRegExp := RegExCreate(REGEXP_MOVIE_PLAYLIST_ITEM);
  StreamsRegExp := RegExCreate(REGEXP_MOVIE_STREAMS);
end;

destructor TDownloader_PornTube.Destroy;
begin
  RegExFreeAndNil(PlaylistRegExp);
  RegExFreeAndNil(PlaylistItemRegExp);
  RegExFreeAndNil(StreamsRegExp);
  inherited;
end;

function TDownloader_PornTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.porntube.com/videos/' + MovieID;
end;

function TDownloader_PornTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Playlist, PlaylistItem, Title, BestUrl, Url, sQuality: string;
  Quality, BestQuality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if GetRegExpVar(PlaylistRegExp, Page, 'INFO', Playlist) then
    if GetRegExpVars(PlaylistItemRegExp, Playlist, ['ITEM', 'TITLE'], [@PlaylistItem, @Title]) then
      begin
      {$IFDEF MULTIDOWNLOADS}
      repeat
      {$ENDIF}
        BestUrl := '';
        BestQuality := -1;
        if GetRegExpVars(StreamsRegExp, PlaylistItem, ['URL', 'QUALITY'], [@Url, @sQuality]) then
          repeat
            Quality := StrToIntDef(sQuality, 0);
            if (Url <> '') and (Quality > BestQuality) then
              begin
              BestUrl := Url;
              BestQuality := Quality;
              end;
          until not GetRegExpVarsAgain(StreamsRegExp, ['URL', 'QUALITY'], [@Url, @sQuality]);
        if BestUrl <> '' then
          begin
          SetName(JSDecode(Title));
          MovieUrl := JSDecode(BestUrl);
          {$IFDEF MULTIDOWNLOADS}
          NameList.Add(Title);
          UrlList.Add(MovieUrl);
          {$ENDIF}
          SetPrepared(True);
          Result := True;
          end;
      {$IFDEF MULTIDOWNLOADS}
      until not GetRegExpVarsAgain(PlaylistItemRegExp, ['ITEM', 'TITLE'], [@PlaylistItem, @Title]);
      {$ENDIF}
      end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornTube);
  {$ENDIF}

end.
