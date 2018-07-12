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

unit downG4TV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_G4TV = class(THttpDownloader)
    private
    protected
      MovieUrlFixRegExp: TRegExp;
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

// http://www.g4tv.com/videos/53181/Duke-Nukem-Forever-Launch-Trailer/
const
  URLREGEXP_BEFORE_ID = 'g4tv\.com/videos/';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '/';

const
  REGEXP_MOVIE_URL_FIX = '[?&]r=(?P<URL>https?[^&]+)';

{ TDownloader_G4TV }

class function TDownloader_G4TV.Provider: string;
begin
  Result := 'G4TV.com';
end;

class function TDownloader_G4TV.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_G4TV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  InfoPageIsXml := True;
  MovieUrlFixRegExp := RegExCreate(REGEXP_MOVIE_URL_FIX);
end;

destructor TDownloader_G4TV.Destroy;
begin
  RegExFreeAndNil(MovieUrlFixRegExp);
  inherited;
end;

function TDownloader_G4TV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.g4tv.com/xml/BroadbandPlayerService.asmx/GetEmbeddedVideo?videoKey=' + MovieID + '&strPlaylist=&playLargeVideo=true&excludedVideoKeys=&playlistType=normal&maxPlaylistSize=10&cb=7657737';
end;

function TDownloader_G4TV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Node: TXmlNode;
    Title, FilePath, Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not XmlNodeByPathAndAttr(PageXml, 'EmbeddedVideos/Video', 'Key', MovieID, Node) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetXmlVar(Node, 'Title', Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else if not GetXmlVar(Node, 'FilePath', FilePath) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else if not GetRegExpVar(MovieUrlFixRegExp, FilePath, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    Name := Title;
    MovieUrl := UrlDecode(Url);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_G4TV);

end.
