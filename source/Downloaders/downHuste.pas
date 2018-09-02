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

unit downHuste;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, uCompatibility,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Huste = class(THttpDownloader)
    private
    protected
      VideoIdRegExp: TRegExp;
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

// http://hudba.huste.sk/exkluzivne/kat:videoklipy/klip:2010-05-26-videoklipy-get-low-skener.html
// http://hudba.huste.tv/grimaso-feat-s-a-s-suvereno/2010-05-26-videoklipy-get-low-skener.html
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*huste\.(?:sk|tv)/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<title>(?P<TITLE>.*?)(?:\s*-HUSTE\.(?:SK|TV))?</title>';
  REGEXP_VIDEO_ID =     '\bvideoId=(?P<ID>[0-9]+)';

{ TDownloader_Huste }

class function TDownloader_Huste.Provider: string;
begin
  Result := 'Huste.sk';
end;

class function TDownloader_Huste.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Huste.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID);
end;

destructor TDownloader_Huste.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoIdRegExp);
  inherited;
end;

function TDownloader_Huste.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Huste.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; 
var
  InfoXml: TXmlDoc;
  Node: TXmlNode;
  ID, BestUrl, sQuality, Url: string;
  i, j, BestQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadXml(Http, 'http://www.huste.tv/services/Video.php?clip=' + ID, InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if XmlNodeByPath(InfoXml, 'files', Node) then
        begin
        BestQuality := -1;
        BestUrl := '';
        for i := 0 to Pred(Node.NodeCount) do
          if Node[i].Name = 'file' then
            if GetXmlAttr(Node[i], '', 'url', Url) then
              begin
              Quality := 0;
              if GetXmlAttr(Node[i], '', 'label', sQuality) then
                begin
                for j := 1 to Length(sQuality) do
                  if CharInSet(sQuality[j], ['0'..'9']) then
                    Quality := 10 * Quality + Ord(sQuality[j]) - Ord('0')
                  else
                    Break;
                end;
              if Quality > BestQuality then
                begin
                BestQuality := Quality;
                BestUrl := Url;
                end;
              end;
        if BestUrl <> '' then
          begin
          SetName(UnpreparedName);
          i := Pos(':', BestUrl);
          MovieUrl := 'http' + Copy(BestUrl, i, MaxInt);
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      FreeAndNil(InfoXml);
      end;
end;

initialization
  RegisterDownloader(TDownloader_Huste);

end.
