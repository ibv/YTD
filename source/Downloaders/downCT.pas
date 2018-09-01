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

unit downCT;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_CT = class(TRtmpDownloader)
    private
    protected
      MovieObjectRegExp: TRegExp;
      IFrameRegExp: TRegExp;
      LiveStream: boolean;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean; virtual;
      procedure SetOptions(const Value: TYTDOptions); override;
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

// http://www.ceskatelevize.cz/ivysilani/309292320520025-den-d-ii-rada/
// http://www.ceskatelevize.cz/porady/873537-hledani-ztraceneho-casu/207522161510013-filmy-z-vaclavaku/?online=1
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ceskatelevize\.cz/';
  URLREGEXP_ID =        '(?:ivysilani|porady).+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>\s*(?P<TITLE>.*?)\s*</title>';
  //REGEXP_MOVIE_OBJECT = '<object\s+id="(?:programmeObject|WMP)"(?:\s+data|.*?<param\s+name="(?:url|src)"\s+value)="(?P<OBJURL>[^"]+)"';
  REGEXP_MOVIE_OBJECT = '\bflashvars\.playlistURL\s*=\s*"(?P<OBJURL>https?://.+?)"';
  REGEXP_IFRAME_TO_IVYSILANI = '<iframe\s+src="(?P<PATH>/ivysilani/embed/.*?)"';

{ TDownloader_CT }

class function TDownloader_CT.Provider: string;
begin
  Result := 'CeskaTelevize.cz';
end;

class function TDownloader_CT.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieObjectRegExp := RegExCreate(REGEXP_MOVIE_OBJECT, [rcoIgnoreCase, rcoSingleLine]);
  IFrameRegExp := RegExCreate(REGEXP_IFRAME_TO_IVYSILANI, [rcoIgnoreCase, rcoSingleLine]);
  LiveStream := True;
end;

destructor TDownloader_CT.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieObjectRegExp);
  RegExFreeAndNil(IFrameRegExp);
  inherited;
end;

function TDownloader_CT.GetMovieInfoUrl: string;
begin
  // http://www.ceskatelevize.cz/porady/1095946610-diagnoza/84-alzheimerova-choroba/video/?pridat=84
  Result := 'http://www.ceskatelevize.cz/' + MovieID;
end;

function TDownloader_CT.GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean;
var Path, Frame: string;
begin
  Result := GetRegExpVar(MovieObjectRegExp, Page, 'OBJURL', Url);
  if not Result then
    if GetRegExpVar(IFrameRegExp, Page, 'PATH', Path) then
      if DownloadPage(Http, 'http://www.ceskatelevize.cz' + UrlEncode(HtmlDecode(Path)), Frame, InfoPageEncoding) then
        Result := GetRegExpVar(MovieObjectRegExp, Frame, 'OBJURL', Url);
end;

function TDownloader_CT.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const REKLAMA = '-AD-';
      REKLAMA_LENGTH = Length(REKLAMA);
var Url, ID, BaseUrl, BestStream, Stream, sBitrate: string;
    Xml: TXmlDoc;
    Body, Node: TXmlNode;
    i, j, Bitrate, BestBitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetMovieObjectUrl(Http, Page, Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, URL, Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if Xml.NodeByPath('smilRoot/body', Body) then
        for i := 0 to Pred(Body.NodeCount) do
          if Body.Nodes[i].Name = 'switchItem' then
            begin
            Node := Body.Nodes[i];
            if GetXmlAttr(Node, '', 'id', ID) then
              if Pos(REKLAMA, ID) <= 0 then
                if GetXmlAttr(Node, '', 'base', BaseUrl) then
                  begin
                  BestStream := '';
                  BestBitrate := -1;
                  for j := 0 to Pred(Node.NodeCount) do
                    if Node.Nodes[j].Name = 'video' then
                      if GetXmlAttr(Node.Nodes[j], '', 'src', Stream) then
                        begin
                        if GetXmlAttr(Node.Nodes[j], '', 'system-bitrate', sBitrate) then
                          Bitrate := StrToIntDef(sBitrate, 0)
                        else
                          Bitrate := 0;
                        if Bitrate > BestBitrate then
                          begin
                          BestStream := Stream;
                          BestBitrate := Bitrate;
                          end;
                        end;
                  if BestStream = '' then
                    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
                  else
                    begin
                    MovieURL := BestStream;
                    AddRtmpDumpOption('r', BaseUrl);
                    AddRtmpDumpOption('y', Stream);
                    if LiveStream then
                      AddRtmpDumpOption('v', '');
                    SetPrepared(True);
                    Result := True;
                    end;
                  end;
            end;
    finally
      Xml.Free;
      end;
end;

procedure TDownloader_CT.SetOptions(const Value: TYTDOptions);
var s: string;
begin
  inherited;
  if Value.ReadProviderOption(Provider, 'live_stream', s) then
    LiveStream := StrToIntDef(s, 0) <> 0;
end;

initialization
  RegisterDownloader(TDownloader_CT);

end.
