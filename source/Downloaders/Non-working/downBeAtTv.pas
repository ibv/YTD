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

unit downBeAtTv;
{$INCLUDE 'ytd.inc'}

// Basically, it's working, but the server streams video and audio as separate
// files, which would need to be combined. Plus I can't get the audio to play.

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BeAtTv_Embed = class(THttpDownloader)
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

// http://www.be-at.tv/embed.swf?p=342526&ap=1
const
  URLREGEXP_BEFORE_ID = 'be-at\.tv/embed\.swf\?p=';
  URLREGEXP_ID =        REGEXP_NUMBERS;
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_BeAtTv }

class function TDownloader_BeAtTv_Embed.Provider: string;
begin
  Result := 'Be-at.tv';
end;

class function TDownloader_BeAtTv_Embed.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_BeAtTv_Embed.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageIsXml := True;
  InfoPageEncoding := peXml;
end;

destructor TDownloader_BeAtTv_Embed.Destroy;
begin
  inherited;
end;

function TDownloader_BeAtTv_Embed.GetMovieInfoUrl: string;
begin
  Result := 'http://old.be-at.tv/CMS/Feeds/Playlist.ashx?page=' + MovieID;
end;

function TDownloader_BeAtTv_Embed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Title, SessionID, VideoID: string;
  {$IFDEF MULTIDOWNLOADS}
  AudioID, AudioPath, AudioURL: string;
  {$ENDIF}
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlAttr(PageXml, '', 'id', SessionID) then
    SetLastErrorMsg(ERR_INVALID_MEDIA_INFO_PAGE)
  else if not GetXmlAttr(PageXml, '', 'title', Title) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else if not GetXmlAttr(PageXml, 'part/video', 'id', VideoID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  {$IFDEF MULTIDOWNLOADS}
  else if not GetXmlAttr(PageXml, 'part/video', 'audio', AudioID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  else if not GetXmlAttr(PageXml, 'part/audio', 'id', AudioID, 'url', AudioPath) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
  {$ENDIF}
  else
    begin
    Name := Title;
    MovieUrl := Format('http://video.aws.be-at.tv/%s/%s/1000/0.flv', [SessionID, VideoID]);
    {$IFDEF MULTIDOWNLOADS}
{
    if DownloadPage(Http, MovieUrl, hmHEAD) then
      begin
      AudioUrl := GetRelativeUrl('http://audio.cdn.be-at.tv/', AudioPath);
      if DownloadPage(Http, AudioUrl, hmHEAD) then
        begin
        UrlList.Add(AudioUrl);
        NameList.Add(Title + '(audio)');
        UrlList.Add(MovieUrl);
        NameList.Add(Title);
        end;
      end;
}
    {$ENDIF}
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BeAtTv_Embed);

end.
