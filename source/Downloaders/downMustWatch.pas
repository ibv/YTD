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

unit downMustWatch;
{$INCLUDE 'ytd.inc'}
{$DEFINE SUBTITLES}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  uHttpDirectDownloader;

type
  TDownloader_MustWatch = class(TNestedDownloader)
    private
    protected
      NestedUrlRegExps: array of TRegExp;
      DirectUrlRegExp: TRegExp;
      {$IFDEF SUBTITLES}
      SubtitleUrlRegExps: array of TRegExp;
      Subtitles: string;
      SubtitlesName: string;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
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

// http://mustwatch.hztz.cz/film/lecba-rakoviny-zevnitr-healing-cancer-from-inside-out/
// http://mustwatch.hztz.cz/film/swirled-order-kruhy-v-obili/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*mustwatch\.hztz\.cz/film/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<a\s[^>]*\bclass="title"[^>]*>(?P<TITLE>.*?)</a>';
  REGEXP_EXTRACT_DIRECTURL = '^(?P<URL>https?://[^?&]+\.(?:flv|mp4)).*$';
  REGEXP_EXTRACT_NESTED_URLS: array[0..1] of string
    = ('<div\s+class="video">\s*<a\s+href="(?P<URL>https?://.+?)"',
       '<div\s+class="video">.*<param\s+name="movie"\s+value="(?P<URL>https?://.+?)"');
  {$IFDEF SUBTITLES}
  REGEXP_EXTRACT_SUBTITLE_URLS: array[0..0] of string
    = ('<strong>Titulky:</strong>[^\n]*<a\s+href\s*=\s*"(?P<SUBTITLES>https?://.+?)"');
  {$ENDIF}

{ TDownloader_MustWatch }

class function TDownloader_MustWatch.Provider: string;
begin
  Result := 'MustWatch.hztz.cz';
end;

class function TDownloader_MustWatch.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MustWatch.Create(const AMovieID: string);
var i: integer;
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  DirectUrlRegExp := RegExCreate(REGEXP_EXTRACT_DIRECTURL, [rcoIgnoreCase, rcoSingleLine]);
  SetLength(NestedUrlRegExps, Length(REGEXP_EXTRACT_NESTED_URLS));
  for i := 0 to Pred(Length(REGEXP_EXTRACT_NESTED_URLS)) do
    NestedUrlRegExps[i] := RegExCreate(REGEXP_EXTRACT_NESTED_URLS[i], [rcoIgnoreCase, rcoSingleLine]);
  {$IFDEF SUBTITLES}
  SetLength(SubtitleUrlRegExps, Length(REGEXP_EXTRACT_SUBTITLE_URLS));
  for i := 0 to Pred(Length(REGEXP_EXTRACT_SUBTITLE_URLS)) do
    SubtitleUrlRegExps[i] := RegExCreate(REGEXP_EXTRACT_SUBTITLE_URLS[i], [rcoIgnoreCase, rcoSingleLine]);
  {$ENDIF}
end;

destructor TDownloader_MustWatch.Destroy;
var i: integer;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(DirectUrlRegExp);
  for i := 0 to Pred(Length(NestedUrlRegExps)) do
    RegExFreeAndNil(NestedUrlRegExps[i]);
  {$IFDEF SUBTITLES}
  for i := 0 to Pred(Length(SubtitleUrlRegExps)) do
    RegExFreeAndNil(SubtitleUrlRegExps[i]);
  {$ENDIF}
  inherited;
end;

function TDownloader_MustWatch.GetMovieInfoUrl: string;
begin
  Result := 'http://mustwatch.hztz.cz/film/' + MovieID;
end;

function TDownloader_MustWatch.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
    {$IFDEF SUBTITLES}
    Url: string;
    {$ENDIF}
begin
  Result := False;
  try
    for i := 0 to Pred(Length(NestedUrlRegExps)) do
      begin
      NestedUrlRegExp := NestedUrlRegExps[i];
      if inherited AfterPrepareFromPage(Page, PageXml, Http) then
        begin
        Result := True;
        Break;
        end;
      end;
    {$IFDEF SUBTITLES}
    Subtitles := '';
    SubtitlesName := '';
    if Result then
      for i := 0 to Pred(Length(SubtitleUrlRegExps)) do
        if GetRegExpVar(SubtitleUrlRegExps[i], Page, 'SUBTITLES', Url) then
          if DownloadPage(Http, Url, Subtitles, peUTF8) then
            begin
            SubtitlesName := ChangeFileExt(GetThisFileName, ExtractFileExt(Url));
            Break;
            end;
    {$ENDIF}
  finally
    NestedUrlRegExp := nil;
    end;
end;

function TDownloader_MustWatch.Download: boolean;
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

function TDownloader_MustWatch.CreateNestedDownloaderFromURL(var Url: string): boolean;
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
  RegisterDownloader(TDownloader_MustWatch);

end.
