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

unit downLiveVideo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LiveVideo = class(THttpDownloader)
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
  uDownloadClassifier,
  uMessages;

// http://www.livevideo.com/liveshow/StarBuck16
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*livevideo\.com/liveshow/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_LiveVideo }

class function TDownloader_LiveVideo.Provider: string;
begin
  Result := 'LiveVideo.com';
end;

class function TDownloader_LiveVideo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_LiveVideo.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  InfoPageIsXml := True;
end;

destructor TDownloader_LiveVideo.Destroy;
begin
  inherited;
end;

function TDownloader_LiveVideo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.livevideo.com/livetv/schedule.ashx?uname=' + MovieID;
end;

function TDownloader_LiveVideo.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Archives: TXmlNode;
    Title, Url: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if PageXml.NodeByPath('archives', Archives) then
    for i := 0 to Pred(Archives.NodeCount) do
      if Archives.Nodes[i].Name = 'archive' then
        if GetXmlVar(Archives.Nodes[i], 'url', Url) and GetXmlVar(Archives.Nodes[i], 'title', Title) then
          begin
          Title := MovieID + ' - ' + Title;
          Url := HtmlDecode(Url);
          if DownloadPage(Http, Url, hmHEAD) then
            Url := LastUrl;
          {$IFDEF MULTIDOWNLOADS}
          NameList.Add(Title);
          UrlList.Add(Url);
          {$ELSE}
          SetName(Title);
          MovieURL := Url;
          Result := True;
          SetPrepared(True);
          Exit;
          {$ENDIF}
          end;
  {$IFDEF MULTIDOWNLOADS}
  if UrlList.Count <= 0 then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    SetPrepared(True);
    Result := First;
    end;
  {$ELSE}
  SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL));
  {$ENDIF}
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_LiveVideo);
  {$ENDIF}

end.
