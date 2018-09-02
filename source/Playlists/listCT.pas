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

unit listCT;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, uPlaylistDownloader,
  downCT;

type
  TPlaylist_CT = class(TPlaylistDownloader)
    private
    protected
      EmbeddedFrameRegExp: TRegExp;
      JavascriptPlayerRegExp: TRegExp;
      VideoPlayerUrlRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      procedure FindPlaylistItems(var Page: string; PageXml: TXmlDoc; Http: THttpSend); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier;

// http://www.ceskatelevize.cz/ct24/regiony/153063-silnice-namrzaji-auta-klouzou-do-hromadnych-nehod/
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        '^https?://(?:[a-z0-9-]+\.)*(?:ceskatelevize|ct24)\.cz/ct24/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_FRAME = '<iframe\s+[^>]*\bsrc="(?P<HOST>https?://[^"/]+)?(?P<PATH>/(?:ivysilani|embed)/.+?)"';
  REGEXP_JS_PLAYER = '<a\s[^>]*\bhref="javascript:void\s*\(\s*q\s*=\s*''(?P<PARAM>[^'']+)''\s*\)"\s+(?:id|target)="videoPlayer_';
  REGEXP_VIDEOPLAYERURL = '"videoPlayerUrl"\s*:\s*"(?P<URL>https?:.+?)"';

{ TPlaylist_CT }

class function TPlaylist_CT.Provider: string;
begin
  Result := TDownloader_CT.Provider;
end;

class function TPlaylist_CT.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TPlaylist_CT.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  EmbeddedFrameRegExp := RegExCreate(REGEXP_MOVIE_FRAME);
  JavascriptPlayerRegExp := RegExCreate(REGEXP_JS_PLAYER);
  VideoPlayerUrlRegExp := RegExCreate(REGEXP_VIDEOPLAYERURL);
end;

destructor TPlaylist_CT.Destroy;
begin
  RegExFreeAndNil(EmbeddedFrameRegExp);
  RegExFreeAndNil(JavascriptPlayerRegExp);
  RegExFreeAndNil(VideoPlayerUrlRegExp);
  inherited;
end;

function TPlaylist_CT.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

procedure TPlaylist_CT.FindPlaylistItems(var Page: string; PageXml: TXmlDoc; Http: THttpSend);
var
  Host, Path, Url, Param, NewPage: string;
  JSIDList: TStringList;
  AjaxHttp: THttpSend;
begin
  //inherited;
  if EmbeddedFrameRegExp.Match(Page) then
    repeat
      if EmbeddedFrameRegExp.SubExpressionByName('HOST', Host) then
        if EmbeddedFrameRegExp.SubExpressionByName('PATH', Path) then
          begin
          if Host = '' then
            Host := ExtractUrlRoot(MovieID);
          Path := HtmlDecode(Path);
          Path := StringReplace(Path, '&autoStart=false', '', [rfReplaceAll]);
          Path := StringReplace(Path, ' ', '%20', [rfReplaceAll]);
          Url := Host + Path;
            // Nepouzivat UrlEncode, cesty uz jsou obvykle UrlEncoded
          UrlList.Add(Url);
          NameList.Add(GetPlayListItemName(EmbeddedFrameRegExp, UrlList.Count));
          end;
    until not EmbeddedFrameRegExp.MatchAgain;
  AjaxHttp := CreateHttp;
  try
    JSIDList := TStringList.Create;
    try
      if JavascriptPlayerRegExp.Match(Page) then
        repeat
          if JavascriptPlayerRegExp.SubExpressionByName('PARAM', Param) then
            if JSIDList.IndexOf(Param) < 0 then
              begin
              JSIDList.Add(Param);
              AjaxHttp.Clear;
              AjaxHttp.Cookies.Assign(Http.Cookies);
              //AjaxHttp.Headers.Add('X-Requested-With: XMLHttpRequest');
              AjaxHttp.Headers.Add('X-Client: 127.0.0.1');
              if DownloadPage(AjaxHttp, 'http://www.ceskatelevize.cz/ct24/ajax/', 'cmd=getVideoPlayerUrl&q=' + UrlEncode(PARAM), HTTP_FORM_URLENCODING, NewPage, peUtf8, False) then
                if GetRegExpVar(VideoPlayerUrlRegExp, NewPage, 'URL', Url) then
                  begin
                  Url := StripSlashes(Url);
                  UrlList.Add(Url);
                  NameList.Add(GetPlayListItemName(JavascriptPlayerRegExp, UrlList.Count));
                  end;
              end;
        until not JavascriptPlayerRegExp.MatchAgain;
    finally
      FreeAndNil(JSIDList);
      end;
  finally
    FreeAndNil(AjaxHttp);
    end;
end;

initialization
  RegisterDownloader(TPlaylist_CT);

end.
