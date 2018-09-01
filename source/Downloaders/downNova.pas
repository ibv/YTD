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

unit downNova;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Nova = class(TRtmpDownloader)
    private
    protected
      MovieVariablesRegExp: TRegExp;
    protected
      function GetFileNameExt: string; override;
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

// http://archiv.nova.cz/multimedia/ulice-1683-1684-dil.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*archiv\.nova\.cz/+multimedia/';
  URLREGEXP_ID =        '.+?';
  URLREGEXP_AFTER_ID =  '\.html?';

const
  REGEXP_MOVIE_VARIABLES = '\svar\s(?P<VARNAME>[a-z_][a-z0-9_]*)\s*=\s*(["'']?)(?P<VARVALUE>.*?)\2\s*;';

{ TDownloader_Nova }

class function TDownloader_Nova.Provider: string;
begin
  Result := 'Nova.cz';
end;

class function TDownloader_Nova.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Nova.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieVariablesRegExp := RegExCreate(REGEXP_MOVIE_VARIABLES, [rcoIgnoreCase, rcoSingleLine])
end;

destructor TDownloader_Nova.Destroy;
begin
  RegExFreeAndNil(MovieVariablesRegExp);
  inherited;
end;

function TDownloader_Nova.GetFileNameExt: string;
begin
  Result := {$IFDEF LOW_QUALITY} '.flv' {$ELSE} '.mp4' {$ENDIF};
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := 'http://archiv.nova.cz/multimedia/' + MovieID + '.html';
end;

function TDownloader_Nova.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
    Name, Value: string;
    MediaID, SiteID, SectionID, SessionID, UserAdID: string;
    ServersUrl, VideosUrl: string;
    FlvServer, FlvStream, FlvName: string;
    Servers, Videos: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  SessionID := '';
  UserAdID := '';
  GetRegExpVarPairs(MovieVariablesRegExp, Page, ['media_id', 'site_id', 'section_id'], [@MediaID, @SiteID, @SectionID]);
  for i := 0 to Pred(Http.Cookies.Count) do
    begin
    Name := Http.Cookies.Names[i];
    Value := Http.Cookies.Values[Name];
    if AnsiCompareText(Name, 'bit') = 0 then
      UserAdID := Value
    else if AnsiCompareText(Name, 'c4d') = 0 then
      SessionID := Value;
    end;
  Http.Cookies.Values['bit'] := UserAdID;
  if SiteID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SiteID']))
  else if SectionID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SectionID']))
  else if MediaID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['MediaID']))
  else if SessionID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SessionID']))
  //else if UserAdID = '' then
  //  SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['UserAdID']))
  else
    begin
    ServersUrl := 'http://tn.nova.cz/bin/player/config.php?site_id=' + SiteID + '&';
    VideosUrl := 'http://tn.nova.cz/bin/player/serve.php' +
                 '?site_id=' + SiteID +
                 '&media_id=' + MediaID +
                 '&userad_id=' + UserAdID +
                 '&section_id=' + SectionID +
                 '&noad_count=0' +
                 '&fv=WIN 10,0,45,2' +
                 '&session_id=' + SessionID +
                 '&ad_file=noad';
    if not DownloadXml(Http, ServersUrl, Servers) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST))
    else
      try
        if not DownloadXml(Http, VideosUrl, Videos) then
          SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
        else
          try
            if Videos.Root.Name = 'error' then
              SetLastErrorMsg(_(ERR_MEDIA_REMOVED))
            else
              begin
              GetXmlAttr(Servers, 'flvserver', 'url', FlvServer);
              GetXmlAttr(Videos, 'item', 'src', FlvStream);
              GetXmlAttr(Videos, 'item', 'txt', FlvName);
              if FlvName = '' then
                FlvName := MediaID;
              if FlvServer = '' then
                SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
              else if FlvStream = '' then
                SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_STREAM))
              else
                begin
                SetName(FlvName);
                {$IFNDEF LOW_QUALITY}
                FlvStream := 'mp4:' + FlvStream;
                {$ENDIF}
                MovieUrl := FlvServer + '/' + FlvStream;
                AddRtmpDumpOption('r', MovieURL);
                AddRtmpDumpOption('y', FlvStream);
                Result := True;
                SetPrepared(True);
                end;
              end;
          finally
            Videos.Free;
            end;
      finally
        Servers.Free;
        end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Nova);

end.
