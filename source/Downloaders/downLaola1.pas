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

unit downLaola1;
{$INCLUDE 'ytd.inc'}
{.$DEFINE LAOLA1_HTTP} // Balutbj tvrdi, ze mu to funguje, ale me ne

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, {$IFDEF LAOLA1_HTTP} uHttpDownloader {$ELSE} uRtmpDownloader {$ENDIF} ;

type
  TDownloader_Laola1 = class( {$IFDEF LAOLA1_HTTP} THttpDownloader {$ELSE} TRtmpDownloader {$ENDIF} )
    private
    protected
      MovieIDRegExp: TRegExp;
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

// http://www.laola1.tv/en/int/volleyball/cev-mens-olympic-qualification/cev-oq-men-sofia-italy-germany/video/353-2021-84517.html
const
  URLREGEXP_BEFORE_ID = 'laola1\.tv/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_TITLE;
  REGEXP_MOVIE_ID =     '"flashvars"\s*,\s*"playkey=(?P<ID>' + {$IFDEF LAOLA1_HTTP} '[^"&]+' {$ELSE} '[0-9]+' {$ENDIF} + ')';

{ TDownloader_Laola1 }

class function TDownloader_Laola1.Provider: string;
begin
  Result := 'Laola1.tv';
end;

class function TDownloader_Laola1.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Laola1.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
end;

destructor TDownloader_Laola1.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_Laola1.GetMovieInfoUrl: string;
begin
  Result := 'http://www.laola1.tv/' + MovieID;
end;

function TDownloader_Laola1.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Server, Stream: string;
  {$IFNDEF LAOLA1_HTTP}
  Status, Auth: string;
  {$ENDIF}
  Info: TXmlDoc;
  Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, {$IFDEF LAOLA1_HTTP} 'http://www.laola1.tv/server/ondemand_xml_esi.php?playkey=' + ID {$ELSE} Format('http://streamaccess.laola1.tv/flash/vod/22/%s_high.xml?partnerid=22&streamid=%s', [ID, ID]) {$ENDIF}, Info) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      {$IFDEF LAOLA1_HTTP}
      if not (XmlNodeByPath(Info, 'video/high', Node) or XmlNodeByPath(Info, 'video/low', Node)) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else if not GetXmlAttr(Node, '', 'server', Server) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
      else if not GetXmlAttr(Node, '', 'pfad', Stream) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
      else
        begin
        MovieUrl := Format('http://%s/%s.flv', [Server, Stream]);
        SetPrepared(True);
        Result := True;
        end;
      {$ELSE}
      if not XmlNodeByPath(Info, 'token', Node) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else if (not GetXmlAttr(Node, '', 'status', Status)) or (StrToIntDef(Status, -1) <> 0) then
        if GetXmlAttr(Node, '', 'statustext', Status) then
          SetLastErrorMsg(Format(ERR_SERVER_ERROR, [Status]))
        else
          SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else if not GetXmlAttr(Node, '', 'auth', Auth) then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['auth']))
      else if not GetXmlAttr(Node, '', 'url', Server) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
      else if not GetXmlAttr(Node, '', 'stream', Stream) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
      else
        begin
        Self.RtmpUrl := Format('rtmp://%s?_fcs_vhost=%s&auth=%s&aifp=v001&slist=%s', [Server, Copy(Server, 1, Pred(Pos('/', Server))), Auth, Stream]);
        //Self.FlashVer := FLASH_DEFAULT_VERSION;
        //Self.SwfUrl := 'http://www.laola1.tv/swf/player.v12.4.swf';
        //Self.TcUrl := Self.RtmpUrl;
        //Self.PageUrl := GetMovieInfoUrl;
        Self.Playpath := 'mp4:' + Stream;
        MovieUrl := Self.RtmpUrl + '   ' + Self.Playpath;
        SetPrepared(True);
        Result := True;
        end;
      {$ENDIF}
    finally
      FreeAndNil(Info);
      end;
end;

initialization
  RegisterDownloader(TDownloader_Laola1);

end.
