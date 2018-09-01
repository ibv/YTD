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

unit downFreeCaster;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeCaster = class(THttpDownloader)
    private
    protected
      StreamIdRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://freecaster.tv/freeski/1012253
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*freecaster\.tv/';
  URLREGEXP_ID =        '[^/]+/[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_STREAM_ID = '\.addVariable\s*\(\s*"stream"\s*,\s*"(?P<ID>.*?)"';

{ TDownloader_FreeCaster }

class function TDownloader_FreeCaster.Provider: string;
begin
  Result := 'FreeCaster.tv';
end;

class function TDownloader_FreeCaster.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FreeCaster.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  StreamIdRegExp := RegExCreate(REGEXP_STREAM_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_FreeCaster.Destroy;
begin
  RegExFreeAndNil(StreamIdRegExp);
  inherited;
end;

function TDownloader_FreeCaster.GetMovieInfoUrl: string;
begin
  Result := 'http://freecaster.tv/' + MovieID;
end;

function TDownloader_FreeCaster.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var StreamID, MaxUrl, Url, UrlBase, Title, Bitrate: string;
    MaxBitRate, i: integer;
    Xml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(StreamIdRegExp, Page, 'ID', StreamID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadXml(Http, 'http://freecaster.tv/player/info/' + StreamID, Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not Xml.NodeByPath('streams', Node) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlAttr(Node, '', 'server', UrlBase) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Xml, 'video/title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else
        begin
        MaxUrl := '';
        MaxBitRate := 0;
        for i := 0 to Pred(Node.NodeCount) do
          if Node.Nodes[i].Name = 'stream' then
            if GetXmlAttr(Node.Nodes[i], '', 'bitrate', Bitrate) then
              if StrToIntDef(Bitrate, 0) > MaxBitRate then
                begin
                Url := Trim(XmlValueIncludingCData(Node.Nodes[i]));
                if Url <> '' then
                  begin
                  MaxUrl := Url;
                  MaxBitRate := StrToInt(BitRate);
                  end;
                end;
        if MaxUrl = '' then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieUrl := UrlBase + MaxUrl;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_FreeCaster);

end.
