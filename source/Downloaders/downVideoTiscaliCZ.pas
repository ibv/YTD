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

unit downVideoTiscaliCZ;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VideoTiscaliCZ = class(THttpDownloader)
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

// http://video.tiscali.cz/the-duty-calls-trailer-283
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*video\.tiscali\.cz/[^/]*?-';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '(?:[/?]|$)';

{ TDownloader_VideoTiscaliCZ }

class function TDownloader_VideoTiscaliCZ.Provider: string;
begin
  Result := 'Tiscali.cz';
end;

class function TDownloader_VideoTiscaliCZ.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_VideoTiscaliCZ.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peXml;
  InfoPageIsXml := True;
end;

destructor TDownloader_VideoTiscaliCZ.Destroy;
begin
  inherited;
end;

function TDownloader_VideoTiscaliCZ.GetMovieInfoUrl: string;
begin
  Result := 'http://video.tiscali.cz/player/playlist-' + MovieID + '.xml';
end;

function TDownloader_VideoTiscaliCZ.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Tracks: TXmlNode;
    Creator, Url, Title: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if PageXml <> nil then
    if PageXml.NodeByPath('trackList', Tracks) then
      for i := 0 to Pred(Tracks.NodeCount) do
        if Tracks[i].Name = 'track' then
          if GetXmlVar(Tracks[i], 'creator', Creator) then
            if Creator <> 'commercial' then
              if GetXmlVar(Tracks[i], 'location', Url) then
                if GetXmlVar(Tracks[i], 'title', Title) then
                  if Title <> 'Reklama' then
                    begin
                    {$IFDEF MULTIDOWNLOADS}
                    NameList.Add(Title);
                    UrlList.Add(Url);
                    {$ELSE}
                    SetName(Title);
                    MovieUrl := Url;
                    SetPrepared(True);
                    Result := True;
                    Exit;
                    {$ENDIF}
                    end;
  if UrlList.Count > 0 then
    begin
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VideoTiscaliCZ);

end.
