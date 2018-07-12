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

unit downHasici150;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  uRTMPDirectDownloader, uMSDirectDownloader;

type
  TDownloader_Hasici150 = class(TNestedDownloader)
    private
    protected
      RtmpServerRegExp: TRegExp;
      RtmpStreamRegExp: TRegExp;
      MSConfigRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; override;
      function TryRTMPDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; 
      function TryMSDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean; 
    public
      class function Features: TDownloaderFeatures; override;
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

// RTMP: http://www.hasici150.tv/cz/Media-galerie/Video/Predavani-cen-RWE-_____312/Predavani-cen-RWE-_____1004/
// MS: http://www.hasici150.tv/cz/Media-galerie/Video/Plzenska-stovka-2012---serial-CPVK_____210/Plzenska-stovka-2012_____776/
const
  URLREGEXP_BEFORE_ID = 'hasici150\.tv/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?:.*?\|\s*)(?P<TITLE>[^|]*?)\s*</title>';
  REGEXP_RTMP_SERVER = '\bstreamer\s*:\s*''(?P<SERVER>.+?)''';
  REGEXP_RTMP_STREAM = '\bfile\s*:\s*''(?P<STREAM>.+?)''';
  REGEXP_MS_CONFIG = '<iframe\s[^>]*[?&]configxml=(?P<URL>https?://[^&"]+)';

type
  TDownloader_Hasici150_MS = class(TMSDirectDownloader);
  TDownloader_Hasici150_RTMP = class(TRTMPDirectDownloader);

{ TDownloader_Hasici150 }

class function TDownloader_Hasici150.Features: TDownloaderFeatures;
begin
  Result := inherited Features + TDownloader_Hasici150_RTMP.Features + TDownloader_Hasici150_MS.Features;
end;

class function TDownloader_Hasici150.Provider: string;
begin
  Result := 'Hasici150.tv';
end;

class function TDownloader_Hasici150.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Hasici150.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  RtmpServerRegExp := RegExCreate(REGEXP_RTMP_SERVER);
  RtmpStreamRegExp := RegExCreate(REGEXP_RTMP_STREAM);
  MSConfigRegExp := RegExCreate(REGEXP_MS_CONFIG);
end;

destructor TDownloader_Hasici150.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(RtmpServerRegExp);
  RegExFreeAndNil(RtmpStreamRegExp);
  RegExFreeAndNil(MSConfigRegExp);
  inherited;
end;

function TDownloader_Hasici150.GetMovieInfoUrl: string;
begin
  Result := 'http://www.hasici150.tv/' + MovieID;
end;

function TDownloader_Hasici150.IdentifyDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
begin
  inherited IdentifyDownloader(Page, PageXml, Http, Downloader);
  Result := False;
  if TryRTMPDownloader(Page, PageXml, Http, Downloader) then
    Result := True
  else if TryMSDownloader(Page, PageXml, Http, Downloader) then
    Result := True
  else
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO);
end;

function TDownloader_Hasici150.TryRTMPDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
var
  Server, Stream: string;
  RtmpDownloader: TDownloader_Hasici150_RTMP;
begin
  Result := False;
  if GetRegExpVar(RtmpServerRegExp, Page, 'SERVER', Server) then
    if GetRegExpVar(RtmpStreamRegExp, Page, 'STREAM', Stream) then
      begin
      MovieUrl := Server + '/' + Stream;
      RtmpDownloader := TDownloader_Hasici150_RTMP.Create(MovieUrl);
      RtmpDownloader.Options := Options;
      RtmpDownloader.RtmpUrl := Server;
      RtmpDownloader.Playpath := ChangeFileExt(Stream, '');
      RtmpDownloader.SaveRtmpDumpOptions;
      Downloader := RtmpDownloader;
      Result := True;
      end;
end;

function TDownloader_Hasici150.TryMSDownloader(var Page: string; PageXml: TXmlDoc; Http: THttpSend; out Downloader: TDownloader): boolean;
var
  MSDownloader: TMSDirectDownloader;
  InfoUrl, Url, Role, Title: string;
  Info: TXmlDoc;
  i: integer;
begin
  Result := False;
  if GetRegExpVar(MSConfigRegExp, Page, 'URL', InfoUrl) then
    if DownloadXml(Http, StringReplace(InfoUrl, '.php;params:', '.php?params=', []), Info) then
      try
        for i := 0 to Pred(Info.Root.NodeCount) do
          if Info.Root.Nodes[i].Name = 'media' then
            if GetXmlAttr(Info.Root.Nodes[i], '', 'src', Url) then
              if (not GetXmlAttr(Info.Root.Nodes[i], '', 'role', Role)) or (Role <> 'advertisement') then
                begin
                if GetXmlAttr(Info.Root.Nodes[i], '', 'title', Title) then
                  Name := Title;
                MovieUrl := Url;
                MSDownloader := TDownloader_Hasici150_MS.CreateWithName(MovieUrl, UnpreparedName);
                MSDownloader.Options := Options;
                Downloader := MSDownloader;
                Result := True;
                Break;
                end;
      finally
        FreeAndNil(Info);
        end;
end;

initialization
  RegisterDownloader(TDownloader_Hasici150);

end.
