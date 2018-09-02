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

unit downRuTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_RuTube = class(TNestedDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
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
  uHttpDirectDownloader,
  uRtmpDownloader,
  uDownloadClassifier,
  uMessages;

// RTMP videos:
// http://rutube.ru/video/54477e8b005020d309ef371dff690eb3/
// http://bl.rutube.ru/c327d57926a628644fad3bc36d81ad08
// http://video.rutube.ru/c327d57926a628644fad3bc36d81ad08
// http://rutube.ru/trackinfo/c327d57926a628644fad3bc36d81ad08.html

// HDS videos (not yet supported):
// http://rutube.ru/video/fe3300d02a3c2058cbffece783c43997/
// http://rutube.ru/video/f364037be6e405ee18783cceebf25d61/

// RTMPS videos:
// http://rutube.ru/video/958d1a1eeb77c811a166e480dc98c6ec/
// http://rutube.ru/video/95375a912bf7a48b74de49688627eef2/
const
  URLREGEXP_BEFORE_ID = 'rutube\.ru/(?:video/|trackinfo/)?';
  URLREGEXP_ID =        '[0-9a-f]{32}';
  URLREGEXP_AFTER_ID =  '(?:[.?/]|$)';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_INFO =   '<param\s+name=\\"movie\\"\s+value=\\"(?P<URL>https?://.+?)\\"';

type
  TDownloader_RuTube_HTTP = class(THttpDirectDownloader);

  TDownloader_RuTube_RTMP = class(TRTMPDownloader)
    private
      fName: string;
      fServer: string;
      fStream: string;
      fMovieInfoUrl: string;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor CreateWithParams(const AName, AServer, AStream, AMovieInfoUrl: string);
      function Prepare: boolean; override;
    end;

{ TDownloader_RuTube }

class function TDownloader_RuTube.Provider: string;
begin
  Result := 'RuTube.ru';
end;

class function TDownloader_RuTube.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_RuTube.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_RuTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_RuTube.GetMovieInfoUrl: string;
begin
  Result := 'http://rutube.ru/video/' + MovieID + '/';
end;

function TDownloader_RuTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
type
  TUrlType = (utNone, utHttp, utRTMP);
var
  Info, Url, Response, ResponseStr, Server: string;
  BestStream, Stream, Bitrate: string;
  i, BestQuality, Quality: integer;
  Xml: TXmlDoc;
  UrlType, BestUrlType: TUrlType;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadPage(Http, 'http://rutube.ru/api/video/' + MovieID, Info) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else if not GetRegExpVar(MovieInfoRegExp, Info, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
  else
    begin
    Xml := nil;
    Url := Url + '.f4m?referer=' + UrlEncode(GetMovieInfoUrl);
    if not DownloadXml(Http, StringReplace(Url, 'http://video.', 'http://bl.', []), Xml) then
      if not DownloadXml(Http, Url, Xml) then
        FreeAndNil(Xml);
    if Xml = nil then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST)
    else
      try
        //Xml.SaveToFile('rutube.xml');
        if not GetXmlVar(Xml, 'responseCode', Response) then
          SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
        else if StrToIntDef(Response, 0) <> 200 then
          begin
          if GetXmlVar(Xml, 'responseDsc', ResponseStr) then
            Response := ResponseStr;
          SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Response]));
          end
        else if not GetXmlVar(Xml, 'baseURL', Server) then
          SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
        else
          begin
          BestStream := '';
          BestQuality := -1;
          BestUrlType := utNone;
          for i := 0 to Pred(Xml.Root.NodeCount) do
            if Xml.Root.Nodes[i].Name = 'media' then
              begin
              UrlType := utNone;
              if GetXmlAttr(Xml.Root.Nodes[i], '','href', Stream) then
                UrlType := utHTTP
              else if GetXmlAttr(Xml.Root.Nodes[i], '','url', Stream) then
                UrlType := utRTMP;
              if UrlType <> utNone then
                begin
                if GetXmlAttr(Xml.Root.Nodes[i], '', 'bitrate', Bitrate) then
                  Quality := StrToIntDef(Bitrate, 0)
                else
                  Quality := 0;
                if Quality > BestQuality then
                  begin
                  BestStream := Stream;
                  BestQuality := Quality;
                  BestUrlType := UrlType;
                  end;
                end;
              end;
          if BestStream = '' then
            SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
          else
            begin
            MovieUrl := Server + BestStream;
            case BestUrlType of
              utHttp:
                begin
                SetPrepared(True);
                Result := CreateNestedDownloaderFromDownloader(TDownloader_RuTube_HTTP.CreateWithName(MovieUrl, Name, Http.Cookies));
                end;
              utRTMP:
                begin
                SetPrepared(True);
                Result := CreateNestedDownloaderFromDownloader(TDownloader_RuTube_RTMP.CreateWithParams(Name, Server, BestStream, GetMovieInfoUrl));
                end;
              end;
            end;
          end;
      finally
        FreeAndNil(Xml);
        end;
    end;
end;

{ TDownloader_RuTube_RTMP }

class function TDownloader_RuTube_RTMP.Provider: string;
begin
  Result := TDownloader_RuTube.Provider;
end;

class function TDownloader_RuTube_RTMP.UrlRegExp: string;
begin
  Raise EDownloaderError.Create('TDownloader_RuTube_RTMP.UrlRegExp is not supported.');
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
end;

constructor TDownloader_RuTube_RTMP.CreateWithParams(const AName, AServer, AStream, AMovieInfoUrl: string);
begin
  Create('');
  fName := AName;
  fServer := AServer;
  fStream := AStream;
  fMovieInfoUrl := AMovieInfoUrl;
end;

function TDownloader_RuTube_RTMP.GetMovieInfoUrl: string;
begin
  Result := '';
end;

function TDownloader_RuTube_RTMP.Prepare: boolean;
var
  Protocol, User, Password, Host, Port, Path, Para: string;
begin
  inherited Prepare;
  Result := False;
  if (fServer <> '') and (fStream <> '') then
    begin
    SetName(fName);
    MovieUrl := fServer + fStream;
    Self.RtmpUrl := MovieUrl;
    Self.Playpath := Copy(fStream, 2, MaxInt);
    Self.PageUrl := fMovieInfoUrl;
    Self.SwfUrl := 'http://rutube.ru/player.swf';
    Self.TcUrl := fServer;
    ParseUrl(fServer, Protocol, User, Password, Host, Port, Path, Para);
    if Path = '/vod' then
      Self.Live := True;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_RuTube);

end.
