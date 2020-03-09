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

unit downEuroSeptik;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uOptions, HttpSend,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_EuroSeptik,
    {$ELSE}
      {$IFNDEF GUI_LCL}
        guiOptionsVCL_EuroSeptik,
      {$ELSE}
        guiOptionsLCL_EuroSeptik,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uNestedDownloader;

type
  TDownloader_EuroSeptik = class(TNestedDownloader)
    private
    protected
      {$IFDEF SUBTITLES}
      SubtitlesRegExp: TRegExp;
      SubtitleHeader: string;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF SUBTITLES}
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    end;

{$IFDEF SUBTITLES}
const
  OPTION_EUROSEPTIK_SUBTITLELANGUAGE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'subtitle_language';
{$ENDIF}

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://euroseptik.cz/nigel-farage-vs-barroso-brok-kapo-schulz-tohle-je-zvracena-demokracie/
const
  URLREGEXP_BEFORE_ID = 'euroseptik\.cz/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_URL =    REGEXP_URL_IFRAME_SRC;
  {$IFDEF SUBTITLES}
  REGEXP_SUBTITLES =    '<p><strong>\s*(?P<LANGUAGE>[^<]+?)\s*</strong><br\s*/>[\r\n]+(?P<SUBTITLES>\d+<br\s*/>.*?</p>[\r\n]+(<p>\d+<br\s*/>.*?</p>[\r\n]+)+)';
  {$ENDIF}

resourcestring
  EUROSEPTIK_SUBTITLES_LANGUAGE = 'Pøepis, angliètina';

{ TDownloader_EuroSeptik }

class function TDownloader_EuroSeptik.Provider: string;
begin
  Result := 'EuroSeptik.cz';
end;

class function TDownloader_EuroSeptik.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_EuroSeptik.Features: TDownloaderFeatures;
begin
  Result := inherited Features {$IFDEF SUBTITLES} + [dfSubtitles] {$ENDIF} ; 
end;

{$IFDEF GUI}
class function TDownloader_EuroSeptik.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_EuroSeptik;
end;
{$ENDIF}

constructor TDownloader_EuroSeptik.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  NestedUrlRegExp := RegExCreate(REGEXP_MOVIE_URL);
  {$IFDEF SUBTITLES}
  SubtitlesRegExp := RegExCreate(REGEXP_SUBTITLES);
  SubtitleHeader := EUROSEPTIK_SUBTITLES_LANGUAGE;
  {$ENDIF}
end;

destructor TDownloader_EuroSeptik.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  {$IFDEF SUBTITLES}
  RegExFreeAndNil(SubtitlesRegExp);
  {$ENDIF}
  inherited;
end;

function TDownloader_EuroSeptik.GetMovieInfoUrl: string;
begin
  Result := 'http://www.euroseptik.cz/' + MovieID;
end;

function TDownloader_EuroSeptik.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  UrlList: TStringArray;
  i: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if GetRegExpAllVar(NestedUrlRegExp, Page, 'URL', UrlList) then
    for i := 0 to Pred(Length(UrlList)) do
      if CreateNestedDownloaderFromURL(UrlList[i]) then
        begin
        MovieUrl := UrlList[i];
        SetPrepared(True);
        Result := True;
        Break;
        end;
end;

{$IFDEF SUBTITLES}
function TDownloader_EuroSeptik.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  s, Language: string;
begin
  Result := False;
  if GetRegExpVars(SubtitlesRegExp, Page, ['LANGUAGE', 'SUBTITLES'], [@Language, @s]) then
    repeat
      if s <> '' then
        if Pos(UpperCase(SubtitleHeader), UpperCase(Language)) > 0 then
          begin
          s := StringReplace(s, #13, '', [rfReplaceAll]);
          s := StringReplace(s, #10, '', [rfReplaceAll]);
          s := StringReplace(s, '<p', #13#10#13#10'<p', [rfReplaceAll, rfIgnoreCase]);
          s := StringReplace(s, '<br', #13#10'<br', [rfReplaceAll, rfIgnoreCase]);
          s := StringReplace(s, '&#8211;', '---', [rfReplaceAll]);
          s := StripTags(s);
          s := HtmlDecode(s);
          fSubtitles := {$IFDEF UNICODE} AnsiString {$ENDIF} (s);
          fSubtitlesExt := '.srt';
          Result := True;
          Break;
          end;
    until not GetRegExpVarsAgain(SubtitlesRegExp, ['LANGUAGE', 'SUBTITLES'], [@Language, @s]);
end;
{$ENDIF}

procedure TDownloader_EuroSeptik.SetOptions(const Value: TYTDOptions);
{$IFDEF SUBTITLES}
var
  s: string;
{$ENDIF}
begin
  inherited;
  {$IFDEF SUBTITLES}
  s := Value.ReadProviderOptionDef(Provider, OPTION_EUROSEPTIK_SUBTITLELANGUAGE, '');
  if s <> '' then
    SubtitleHeader := s;
  {$ENDIF}
end;

initialization
  RegisterDownloader(TDownloader_EuroSeptik);

end.
