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

unit downNovaTN;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DIRTYHACKS} WinSock, {$ENDIF}
  uPCRE, uXml, uCompatibility, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_NovaTN = class(THttpDownloader)
    private
    protected
      VideoListRegExp: TRegExp;
      VideoParamsRegExp: TRegExp;
      ArticleDateRegExp: TRegExp;
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
  uStringUtils,
  uDownloadClassifier,
  uMessages;

// http://tn.nova.cz/magazin/hi-tech/veda/dnes-ve-13-14-mine-zemi-planetka-mozna-je-umela.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tn\.nova\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+property="og:title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_VIDEO_LIST = '<a\s+href="https?://(?:[a-z0-9-]+\.)*blog\.cz/insert\?(?P<VIDEO>.*?)"';
  REGEXP_VIDEO_PARAMS = '&amp;(?P<VARNAME>[^&=]+)(?:=(?P<VARVALUE>[^&]*))?';
  REGEXP_ARTICLE_DATE = '\bvar\s+article_date\s*=\s*"(?P<YEAR>[0-9]{4})(?P<MONTH>[0-9]{2})(?P<DAY>[0-9]{2})"';

{$IFDEF DIRTYHACKS}
const
  SERVER_COUNT = 30;
{$ENDIF}

{ TDownloader_NovaTN }

class function TDownloader_NovaTN.Provider: string;
begin
  Result := 'Nova.cz';
end;

class function TDownloader_NovaTN.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_NovaTN.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoListRegExp := RegExCreate(REGEXP_VIDEO_LIST, [rcoIgnoreCase, rcoSingleLine]);
  VideoParamsRegExp := RegExCreate(REGEXP_VIDEO_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  ArticleDateRegExp := RegExCreate(REGEXP_ARTICLE_DATE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_NovaTN.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoListRegExp);
  RegExFreeAndNil(VideoParamsRegExp);
  RegExFreeAndNil(ArticleDateRegExp);
  inherited;
end;

function TDownloader_NovaTN.GetMovieInfoUrl: string;
begin
  Result := 'http://tn.nova.cz/' + MovieID;
end;

function TDownloader_NovaTN.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Year, Month, Day: string;
    Video, Content, ID, Title, Url: string;
    {$IFDEF DIRTYHACKS}
    UrlTester: THttpSend;
    Host: string;
    i: integer;
    Found: boolean;
    {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVars(ArticleDateRegExp, Page, ['YEAR', 'MONTH', 'DAY'], [@Year, @Month, @Day]) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['article_date']))
  else if VideoListRegExp.Match(Page) then
    begin
    repeat
      Video := VideoListRegExp.SubexpressionByName('VIDEO');
      if GetRegExpVarPairs(VideoParamsRegExp, Video, ['content', 'id', 'title'], [@Content, @ID, @Title]) then
        if (Content = 'video') and (StrToIntDef(ID, -1) > 0) then
          begin
          Title := Utf8ToString(Utf8String(UrlDecode(Title)));
          {$IFDEF DIRTYHACKS}
            // The problem is, I don't know how to get the subdomain for a particular video.
            // So I try all of them.
            {$IFDEF DELPHI6_UP}
              {$MESSAGE WARN 'This is a dirty hack!'}
            {$ENDIF}
            Url := '';
            Found := False;
            i := 1;
            repeat
              Host := Format('vid%d.tn.cz', [i]);
              Inc(i);
              if GetHostByName(PChar(Host)) = nil then
                Break
              else
                begin
                Url := Format('http://%s/%s/%s/%s/%s-hq.mp4', [Host, Year, Month, Day, ID]);
                UrlTester := CreateHttp;
                try
                  if UrlTester.HTTPMethod('HEAD', Url) and (UrlTester.ResultCode >= 200) and (UrlTester.ResultCode < 300) then
                    begin
                    Found := True;
                    Break;
                    end;
                finally
                  FreeAndNil(UrlTester);
                  end;
                end;
            until i > SERVER_COUNT;
            if not Found then
              Continue;
          {$ELSE}
            Url := '';
            'tn.nova.cz is not supported without dirty hacks!';
          {$ENDIF}
          {$IFDEF MULTIDOWNLOADS}
          NameList.Add(Title);
          UrlList.Add(Url);
          {$ELSE}
          SetName(Title);
          MovieUrl := Url;
          Result := True;
          SetPrepared(True);
          Exit;
          {$ENDIF}
          end;
    until not VideoListRegExp.MatchAgain;
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
  RegisterDownloader(TDownloader_NovaTN);

end.
