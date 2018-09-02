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

unit downPlaywire;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Playwire = class(TRtmpDownloader)
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

// http://cdn.playwire.com/343/embed/28047.html
const
  URLREGEXP_BEFORE_ID = 'cdn\.playwire\.com/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<title>\s*(?:Playwire\s+Video\s*-\s*)?(?P<TITLE>.*?)\s*</title>';

{ TDownloader_Playwire }

class function TDownloader_Playwire.Provider: string;
begin
  Result := 'Playwire.com';
end;

class function TDownloader_Playwire.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Playwire.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUnknown;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
end;

destructor TDownloader_Playwire.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_Playwire.GetMovieInfoUrl: string;
begin
  Result := 'http://cdn.playwire.com/' + MovieID;
end;

function TDownloader_Playwire.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  InfoUrl, BaseUrl: string;
  Xml: TXmlDoc;
  Node: TXmlNode;
  i, BestQuality, Quality: integer;
  BestStream, Stream, sHeight, sBitrate: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not DownloadXmlVar(Http, ChangeFileExt(GetMovieInfoUrl, '.xml'), 'src', InfoUrl) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, InfoUrl, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if not GetXmlVar(Xml, 'baseURL', BaseUrl) then
        SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['baseURL']))
      else if not XmlNodeByPath(Xml, '', Node) then
        SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
      else
        begin
        BestStream := '';
        BestQuality := -1;
        for i := 0 to Pred(Node.NodeCount) do
          if Node[i].Name = 'media' then
            if GetXmlAttr(Node[i], '', 'url', Stream) then
              if GetXmlAttr(Node[i], '', 'height', sHeight) then
                if GetXmlAttr(Node[i], '', 'bitrate', sBitrate) then
                  begin
                  Quality := StrToIntDef(sHeight, 1) * StrToIntDef(sBitrate, 1);
                  if Quality > BestQuality then
                    begin
                    BestStream := Stream;
                    BestQuality := Quality;
                    end;
                  end;
        if BestStream <> '' then
          begin
          Self.RtmpUrl := StringReplace(BaseUrl, '_definst_', '', [rfReplaceAll]);
          Self.SwfVfy := 'http://cdn.playwire.com/bolt.swf';
          Self.PageUrl := GetMovieInfoUrl;
          Self.Playpath := BestStream;
          Self.FlashVer := FLASH_DEFAULT_VERSION;
          MovieUrl := Self.RtmpUrl + '/' + Self.PlayPath;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_Playwire);

end.
