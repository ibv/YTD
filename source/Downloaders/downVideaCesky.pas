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

unit downVideaCesky;
{$INCLUDE 'ytd.inc'}
{$DEFINE SUBTITLES}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  uHttpDirectDownloader;

type
  TDownloader_VideaCesky = class(TNestedDownloader)
    private
    protected
      DirectUrlRegExp: TRegExp;
      YouTubeUrlRegexp1, YouTubeUrlRegExp2: TRegExp;
      {$IFDEF SUBTITLES}
      SubtitlesRegExp1, SubtitlesRegExp2: TRegExp;
      Subtitles: string;
      SubtitlesName: string;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function CreateNestedDownloaderFromURL(var Url: string): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.videacesky.cz/serialy/upoutavka-na-treti-radu-the-guild
// http://www.videacesky.cz/autori/Jandis/videa/BeerNation.flv
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videacesky\.cz/(?!autori/[^/?&]*/videa/)[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>[^<]*?)\s*-\s*Videa\s*Èesky';
  REGEXP_EXTRACT_DIRECTURL = '^(?P<URL>https?://[^?&]+\.flv).*$';
  REGEXP_EXTRACT_YOUTUBE_URL = '\sflashvars="(?:[^"]*&amp;)?file=(?P<URL>https?[^"]+?)(?:&amp;|")';
  REGEXP_EXTRACT_YOUTUBE_URL2 = '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?file=(?P<URL>https?[^"]+?)(?:&amp;|")';
  {$IFDEF SUBTITLES}
  REGEXP_EXTRACT_SUBTITLES = '\sflashvars="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)';
  REGEXP_EXTRACT_SUBTITLES2 = '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)';
  {$ENDIF}

{ TDownloader_VideaCesky }

class function TDownloader_VideaCesky.Provider: string;
begin
  Result := 'VideaCesky.cz';
end;

class function TDownloader_VideaCesky.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_VideaCesky.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  DirectUrlRegExp := RegExCreate(REGEXP_EXTRACT_DIRECTURL, [rcoIgnoreCase, rcoSingleLine]);
  //NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE_ID, [rcoIgnoreCase, rcoSingleLine]);
  YouTubeUrlRegExp1 := RegExCreate(REGEXP_EXTRACT_YOUTUBE_URL, [rcoIgnoreCase, rcoSingleLine]);
  YouTubeUrlRegExp2 := RegExCreate(REGEXP_EXTRACT_YOUTUBE_URL2, [rcoIgnoreCase, rcoSingleLine]);
  {$IFDEF SUBTITLES}
  SubtitlesRegExp1 := RegExCreate(REGEXP_EXTRACT_SUBTITLES, [rcoIgnoreCase, rcoSingleLine]);
  SubtitlesRegExp2 := RegExCreate(REGEXP_EXTRACT_SUBTITLES2, [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
end;

destructor TDownloader_VideaCesky.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(DirectUrlRegExp);
  //RegExFreeAndNil(NestedUrlRegExp);
  RegExFreeAndNil(YouTubeUrlRegExp1);
  RegExFreeAndNil(YouTubeUrlRegExp2);
  {$IFDEF SUBTITLES}
  RegExFreeAndNil(SubtitlesRegExp1);
  RegExFreeAndNil(SubtitlesRegExp2);
  {$ENDIF}
  inherited;
end;

function TDownloader_VideaCesky.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videacesky.cz/dummy/' + MovieID;
end;

function TDownloader_VideaCesky.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
{$IFDEF SUBTITLES}
var Url: string;
{$ENDIF}
begin
  try
    NestedUrlRegExp := YouTubeUrlRegExp1;
    Result := inherited AfterPrepareFromPage(Page, Http);
    if not Result then
      begin
      NestedUrlRegExp := YouTubeUrlRegExp2;
      Result := inherited AfterPrepareFromPage(Page, Http);
      end;
    {$IFDEF SUBTITLES}
    Subtitles := '';
    SubtitlesName := '';
    if Result then
      if GetRegExpVar(SubtitlesRegExp1, Page, 'SUBTITLES', Url) or GetRegExpVar(SubtitlesRegExp2, Page, 'SUBTITLES', Url) then
        if not DownloadPage(Http, Url, Subtitles, peUTF8) then
          Subtitles := ''
        else
          SubtitlesName := ChangeFileExt(GetThisFileName, ExtractFileExt(Url));
    {$ENDIF}
  finally
    NestedUrlRegExp := nil;
    end;
end;

function TDownloader_VideaCesky.Download: boolean;
{$IFDEF SUBTITLES}
var Overwrite: boolean;
{$ENDIF}
begin
  Result := inherited Download;
  {$IFDEF SUBTITLES}
  if (Subtitles <> '') and (SubtitlesName <> '') then
    begin
    Overwrite := True;
    if FileExists(SubtitlesName) then
      if Assigned(OnFileNameValidate) then
        OnFileNameValidate(Self, SubtitlesName, Overwrite);
    if Overwrite then
      with TFileStream.Create(SubtitlesName, fmCreate) do
        try
          WriteBuffer(Subtitles[1], Length(Subtitles));
        finally
          Free;
          end;
    end;
  {$ENDIF}
end;

function TDownloader_VideaCesky.CreateNestedDownloaderFromURL(var Url: string): boolean;
var Downloader: THttpDirectDownloader;
    Dummy: string;
begin
  Url := UrlDecode(Url);
  Result := inherited CreateNestedDownloaderFromURL(Url);
  if not Result then
    // "Native" URL
    if GetRegExpVar(DirectUrlRegExp, Url, 'URL', Dummy) then
      begin
      Downloader := THttpDirectDownloader.Create(Url, UnpreparedName);
      Result := CreateNestedDownloaderFromDownloader(Downloader);
      if Result then
        MovieURL := Url
      else
        Downloader.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_VideaCesky);

end.
