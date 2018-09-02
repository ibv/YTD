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

unit downVideaCesky;
{$INCLUDE 'ytd.inc'}
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uVarNestedDownloader;

type
  TDownloader_VideaCesky = class(TVarNestedDownloader)
    private
    protected
      MovieUrlAreaRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function CreateNestedDownloaderFromURL(var Url: string): boolean; override;
    public
      class function Provider: string; override;
      class function Features: TDownloaderFeatures; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF SUBTITLES}
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  {$IFDEF SUBTITLES}
  uSubtitles,
  {$ENDIF}
  uMessages;

// http://www.videacesky.cz/serialy/upoutavka-na-treti-radu-the-guild
// http://www.videacesky.cz/autori/Jandis/videa/BeerNation.flv
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videacesky\.cz/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>[^<]*?)\s*-\s*Videa\s*Èesky';
  REGEXP_MOVIE_AREA = '(?P<AREA><div\s[^>]*\bid="contentArea".*)';
  REGEXP_EXTRACT_NESTED_URLS: array[0..6] of string
    = ('\sflashvars="(?:[^"]*&amp;)?file=\s*(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?file=\s*(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<param\s+name="movie"\s+value="\s*(?P<URL>https?://.+?)"',
       '<embed\s+[^>]*\sflashvars="(?:[^"]*&amp;)?file=\s*(?P<URL>https?[^"]+?)(?:&amp;|")',
       '<embed\s+[^>]*\ssrc="\s*(?P<URL>https?[^"]+?)"',
       '<iframe\s+[^>]*\ssrc="\s*(?P<URL>https?[^"]+?)"',
       '\.setup\s*\(\s*\{.*?\bfile\s*:\s*"(?P<URL>https?://[^"]+)"'
       );
  {$IFDEF SUBTITLES}
  REGEXP_EXTRACT_SUBTITLE_URLS: array[0..3] of string
    = ('\sflashvars="(?:[^"]*&amp;)?captions\.file=\s*(?P<SUBTITLES>https?://[^&"]+)',
       '<param\s+name="flashvars"\s+value="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)',
       '<embed\s+[^>]*\sflashvars="(?:[^"]*&amp;)?captions\.file=(?P<SUBTITLES>https?://[^&"]+)',
       '\.setup\s*\(\s*\{.*?\btracks\s*:\s*\[\s*\{.*?\bfile\s*:\s*"(?P<SUBTITLES>https?://[^"]+\.srt)"'
       );
  {$ENDIF}

{ TDownloader_VideaCesky }

class function TDownloader_VideaCesky.Provider: string;
begin
  Result := 'VideaCesky.cz';
end;

class function TDownloader_VideaCesky.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [
    {$IFDEF SUBTITLES} dfSubtitles {$IFDEF CONVERTSUBTITLES} , dfSubtitlesConvert {$ENDIF} {$ENDIF}
    ];
end;

class function TDownloader_VideaCesky.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_VideaCesky.Create(const AMovieID: string);
{$IFDEF SUBTITLES}
var i: integer;
{$ENDIF}
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  MovieUrlAreaRegExp := RegExCreate(REGEXP_MOVIE_AREA);
  AddNestedUrlRegExps(REGEXP_EXTRACT_NESTED_URLS);
  {$IFDEF SUBTITLES}
  SetLength(fSubtitleUrlRegExps, Length(REGEXP_EXTRACT_SUBTITLE_URLS));
  for i := 0 to Pred(Length(REGEXP_EXTRACT_SUBTITLE_URLS)) do
    fSubtitleUrlRegExps[i] := RegExCreate(REGEXP_EXTRACT_SUBTITLE_URLS[i]);
  {$ENDIF}
end;

destructor TDownloader_VideaCesky.Destroy;
{$IFDEF SUBTITLES}
var i: integer;
{$ENDIF}
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlAreaRegExp);
  ClearNestedUrlRegExps;
  {$IFDEF SUBTITLES}
  for i := 0 to Pred(Length(fSubtitleUrlRegExps)) do
    RegExFreeAndNil(fSubtitleUrlRegExps[i]);
  SetLength(fSubtitleUrlRegExps, 0);
  {$ENDIF}
  inherited;
end;

function TDownloader_VideaCesky.GetMovieInfoUrl: string;
begin
  Result := 'http://www.videacesky.cz/' + MovieID;
end;

{$IFDEF SUBTITLES}
function TDownloader_VideaCesky.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
{$IFDEF CONVERTSUBTITLES}
var Xml: TXmlDoc;
    Node: TXmlNode;
    NewSubs, SubStart, SubEnd, Sub: string;
    i, n: integer;
{$ENDIF}
begin
  Result := inherited ReadSubtitles(Page, PageXml, Http);
  {$IFDEF CONVERTSUBTITLES}
  if Result then
    if fSubtitles <> '' then
      if ConvertSubtitles then
        if AnsiCompareText(Trim(fSubtitlesExt), '.xml') = 0 then
          try
            Xml := TXmlDoc.Create;
            try
              Xml.LoadFromBinaryString(fSubtitles);
              if Xml.Root.Name = 'tt' then
                if XmlNodeByPathAndAttr(Xml, 'body/div', 'xml:id', 'captions', Node) then
                  begin
                  NewSubs := '';
                  n := 0;
                  for i := 0 to Pred(Node.NodeCount) do
                    if Node.Nodes[i].Name = 'p' then
                      if GetXmlAttr(Node.Nodes[i], '', 'begin', SubStart) then
                        if GetXmlAttr(Node.Nodes[i], '', 'end', SubEnd) then
                          if GetXmlVar(Node.Nodes[i], '', Sub) then
                            NewSubs := NewSubs + SubtitlesToSrt(n, StringReplace(SubStart, '.', ',', [rfReplaceAll]), StringReplace(SubEnd, '.', ',', [rfReplaceAll]), StringReplace(Sub, '<br />', #13#10, [rfIgnoreCase, rfReplaceAll]));
                  if NewSubs <> '' then
                    begin
                    fSubtitles := AnsiString(NewSubs);
                    fSubtitlesExt := '.srt';
                    end;
                  end;
            finally
              Xml.Free;
              end;
          except
            end;
  {$ENDIF}
end;
{$ENDIF}

function TDownloader_VideaCesky.CreateNestedDownloaderFromURL(var Url: string): boolean;
begin
  Url := UrlDecode(Url);
  Result := inherited CreateNestedDownloaderFromURL(Url);
end;

function TDownloader_VideaCesky.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Area: string;
begin
  if GetRegExpVar(MovieUrlAreaRegExp, Page, 'AREA', Area) then
    Page := Area;
  Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
end;

initialization
  RegisterDownloader(TDownloader_VideaCesky);

end.
