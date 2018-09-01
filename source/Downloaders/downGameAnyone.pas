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

unit downGameAnyone;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downYouTube;

type
  TDownloader_GameAnyone = class(TNestedDownloader)
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
  uDownloadClassifier,
  uMessages;

// http://www.gameanyone.com/video/88319
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*gameanyone\.com/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div\s+style="font-weight:bold;padding-left:25px;width:910px;text-align:left;font-size:15px;padding-bottom:3px;float:left;">\s*(?P<TITLE>.*?)\s*</div>';

{ TDownloader_GameAnyone }

class function TDownloader_GameAnyone.Provider: string;
begin
  Result := 'GameAnyone.com';
end;

class function TDownloader_GameAnyone.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_GameAnyone.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peANSI;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
end;

destructor TDownloader_GameAnyone.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_GameAnyone.GetMovieInfoUrl: string;
begin
  Result := 'http://www.gameanyone.com/video/' + MovieID;
end;

function TDownloader_GameAnyone.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Tracklist: TXmlNode;
    ID, Url: string;
    i: integer;
begin
  Result := False;
  if not DownloadXml(Http, 'http://www.gameanyone.com/pl.php?id=' + MovieID + '&l=1' + MovieID, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);
      if XmlNodeByPath(Xml, 'tracklist', Tracklist) then
        for i := 0 to Pred(Tracklist.NodeCount) do
          if Tracklist.Nodes[i].Name = 'track' then
            if GetXmlVar(Tracklist.Nodes[i], 'jwplayer:mediaid', ID) then
              if ID = MovieID then
                begin
                if GetXmlVar(Tracklist.Nodes[i], 'location', Url) then
                  if CreateNestedDownloaderFromURL(Url) then
                    begin
                    SetPrepared(True);
                    Result := True;
                    end;
                Break;
                end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_GameAnyone);

end.
