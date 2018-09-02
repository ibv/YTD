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

unit downHokejCz;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_HokejCz = class(TRtmpDownloader)
    private
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
  uMessages,
  uDownloadClassifier;

// http://online.hokej.cz/?date=2012-01-20&video=69421
const
  URLREGEXP_BEFORE_ID = 'online\.hokej\.cz/.*[?&]video=';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_HokejCz }

class function TDownloader_HokejCz.Provider: string;
begin
  Result := 'Online.Hokej.cz';
end;

class function TDownloader_HokejCz.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_HokejCz.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
end;

destructor TDownloader_HokejCz.Destroy;
begin
  inherited;
end;

function TDownloader_HokejCz.GetMovieInfoUrl: string;
begin
  Result := 'http://online.hokej.cz/?video=' + MovieID;
end;

function TDownloader_HokejCz.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Timestamp, Signature, BaseUrl, Stream: string;
  Xml: TXmlDoc;
  Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Timestamp := FormatDateTime('yyyymmddhhnnss', Now);
  if not DownloadPage(Http, 'http://online.hokej.cz/archive_token.php?match=' + MovieID + '&time=' + Timestamp, Signature) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['archive_token']))
  else if not DownloadXml(Http, 'http://master-ng.nacevi.cz/cdn.server/PlayerLink.ashx?c=Tipsport-VOD|' + MovieID + '&t=' + Timestamp + '&s=' + Signature + '&h=1&tm=smil', Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not XmlNodeByPath(Xml, 'smilRoot/body/switchItem', Node) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else if not GetXMlAttr(Node, '', 'base', BaseUrl) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_SERVER)
      else if not Smil_FindBestVideo(Node, Stream) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
      else
        begin
        MovieUrl := {BaseUrl + '/' + } Stream;
        Self.RtmpUrl := BaseUrl;
        Self.Playpath := Stream;
        SetPrepared(True);
        Result := True;
        end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_HokejCz);

end.
