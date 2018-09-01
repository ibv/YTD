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

unit downEHow;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI2009_UP} Windows, {$ENDIF}
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_EHow = class(THttpDownloader)
    private
    protected
      SwitchVideoRegExp: TRegExp;
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

// http://www.ehow.com/video_4871930_clean-computer-monitor-glass.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ehow\.com/video_';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_SWITCH_VIDEO_FUNCTION = '\sfunction\s+switchVideoTo(?P<VERSION>.*?)\s*\([^)]*\)\s\{.*?\sshowPlayer\s*\(\s*\{[^}]*\bid\s*:\s*''(?P<URL>https?://.*?)''';

{ TDownloader_EHow }

class function TDownloader_EHow.Provider: string;
begin
  Result := 'EHow.com';
end;

class function TDownloader_EHow.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_EHow.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  SwitchVideoRegExp := RegExCreate(REGEXP_SWITCH_VIDEO_FUNCTION);
end;

destructor TDownloader_EHow.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(SwitchVideoRegExp);
  inherited;
end;

function TDownloader_EHow.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ehow.com/video_' + MovieID;
end;

function TDownloader_EHow.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, s: string;
    UrlQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  Url := '';
  UrlQuality := 0;
  if SwitchVideoRegExp.Match(Page) then
    repeat
      s := SwitchVideoRegExp.SubexpressionByName('VERSION');
      if AnsiCompareText(s, 'SD') = 0 then
        Quality := 1
      else if AnsiCompareText(s, 'HD') = 0 then
        Quality := 2
      else
        Quality := 0;
      if Quality > UrlQuality then
        begin
        UrlQuality := Quality;
        Url := SwitchVideoRegExp.SubexpressionByName('URL');
        end;
    until not SwitchVideoRegExp.MatchAgain;
  if Url <> '' then
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_EHow);

end.
