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

unit downGodTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_GodTube = class(THttpDownloader)
    private
    protected
      PlaylistRegExp: TRegExp;
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

// http://www.godtube.com/featured/video/jesus-your-co-pilot-or-leader
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*godtube\.com/featured/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PLAYLIST = '\bflashvars\.playlistPath\s*=\s*''(?P<URL>https?://.+?)''';

{ TDownloader_GodTube }

class function TDownloader_GodTube.Provider: string;
begin
  Result := 'GodTube.com';
end;

class function TDownloader_GodTube.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_GodTube.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  PlaylistRegExp := RegExCreate(REGEXP_PLAYLIST);
end;

destructor TDownloader_GodTube.Destroy;
begin
  RegExFreeAndNil(PlaylistRegExp);
  inherited;
end;

function TDownloader_GodTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.godtube.com/featured/video/' + MovieID;
end;

function TDownloader_GodTube.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Title: string;
    Xml: TXmlDoc;
    Node: TXmlNode;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(PlayListRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, Url, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not Xml.NodeByPath('playlist', Node) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else
        begin
        for i := 0 to Pred(Node.NodeCount) do
          if Node.Nodes[i].Name = 'item' then
            if GetXmlVar(Node.Nodes[i], 'filelocation', Url) and GetXmlVar(Node.Nodes[i], 'title', Title) then
              begin
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
        {$IFDEF MULTIDOWNLOADS}
        if UrlList.Count <= 0 then
          SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
        else
          begin
          SetPrepared(True);
          Result := First;
          end;
        {$ELSE}
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);
        {$ENDIF}
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_GodTube);

end.
