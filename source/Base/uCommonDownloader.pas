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

unit uCommonDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXML, HttpSend, blcksock,
  uDownloader, uOptions;

type
  TStringArray = array of string;

  TCommonDownloader = class(TDownloader)
    private
      fMovieURL: string;
      fInfoPageEncoding: TPageEncoding;
      fInfoPageIsXml: boolean;
    protected
      function GetMovieInfoUrl: string; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF} abstract;
    protected
      MovieTitleRegExp: TRegExp;
      MovieUrlRegExp: TRegExp;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean; overload; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; overload; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function ExtractUrlFileName(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ExtractUrlExt(const Url: string): string; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property MovieUrl: string read fMovieUrl write fMovieUrl;
      property InfoPageEncoding: TPageEncoding read fInfoPageEncoding write fInfoPageEncoding;
      property InfoPageIsXml: boolean read fInfoPageIsXml write fInfoPageIsXml;
    protected
      function GetFileNameExt: string; override;
      function BuildMovieUrl(out Url: string): boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function BeforePrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      procedure SetOptions(const Value: TYTDOptions); override;
      {$IFDEF SUBTITLES}
    protected
      fSubtitleUrlRegExps: array of TRegExp;
      fSubtitleRegExps: array of TRegExp;
      fSubtitlesEnabled: boolean;
      fSubtitles: AnsiString;
      fSubtitlesExt: string;
    public
      property SubtitlesEnabled: boolean read fSubtitlesEnabled {write fSubtitlesEnabled};
      property Subtitles: AnsiString read fSubtitles {write fSubtitles};
      property SubtitlesExt: string read fSubtitlesExt {write fSubtitlesExt};
      function GetSubtitlesFileName: string; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      function WriteSubtitles: boolean; {$IFDEF MINIMIZESIZE} dynamic; {$ELSE} virtual; {$ENDIF}
      {$ENDIF}
    protected
      function GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpAllVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: TStringArray): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True; const VarNameSubExprName: string = 'VARNAME'; const VarValueSubExprName: string = 'VARVALUE'): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      property ContentUrl: string read fMovieUrl;
    end;

implementation

uses
  uCompatibility, uMessages, uStringUtils;

{ TCommonDownloader }

constructor TCommonDownloader.Create(const AMovieID: string);
begin
  inherited;
  fMovieUrl := '';
  fInfoPageEncoding := peUnknown;
  fInfoPageIsXml := False;
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  {$IFDEF SUBTITLES}
  SetLength(fSubtitleUrlRegExps, 0);
  SetLength(fSubtitleRegExps, 0);
  fSubtitlesEnabled := True;
  {$ENDIF}
end;

destructor TCommonDownloader.Destroy;
{$IFDEF SUBTITLES}
var i: integer;
{$ENDIF}
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  {$IFDEF SUBTITLES}
  for i := 0 to Pred(Length(fSubtitleUrlRegExps)) do
    RegExFreeAndNil(fSubtitleUrlRegExps[i]);
  SetLength(fSubtitleUrlRegExps, 0);
  for i := 0 to Pred(Length(fSubtitleRegExps)) do
    RegExFreeAndNil(fSubtitleRegExps[i]);
  SetLength(fSubtitleRegExps, 0);
  {$ENDIF}
  inherited;
end;

function TCommonDownloader.GetFileNameExt: string;
begin
  Result := ExtractUrlExt(MovieURL);
end;

function TCommonDownloader.ExtractUrlExt(const Url: string): string;
begin
  Result := ExtractFileExt(ExtractUrlFileName(Url));
end;

function TCommonDownloader.ExtractUrlFileName(const Url: string): string;
var i: integer;
begin
  i := Pos('?', Url);
  if i <= 0 then
    Result := Url
  else
    Result := Copy(Url, 1, Pred(i));
  for i := Length(Result) downto 1 do
    if Result[i] = '/' then
      begin
      Result := Copy(Result, Succ(i), MaxInt);
      Break;
      end;
end;

function TCommonDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean;
begin
  Result := GetMovieInfoContent(Http, Url, Page, Xml, hmGET);
end;

function TCommonDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Xml := nil;
  Result := DownloadPage(Http, Url, Page, InfoPageEncoding, Method);
  if Result and InfoPageIsXml then
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.LoadFromStream(Http.Document);
      Http.Document.Seek(0, 0);
    except
      FreeAndNil(Xml);
      end;
    end;
end;

{$IFDEF SUBTITLES}
function TCommonDownloader.GetSubtitlesFileName: string;
begin
  Result := ChangeFileExt(GetFileName, fSubtitlesExt);
end;

function TCommonDownloader.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
    Url, Subs: string;
    s: AnsiString;
begin
  Result := False;
  fSubtitles := '';
  fSubtitlesExt := '';
  if SubtitlesEnabled then
    begin
    for i := 0 to Pred(Length(fSubtitleUrlRegExps)) do
      if GetRegExpVar(fSubtitleUrlRegExps[i], Page, 'SUBTITLES', Url) then
        if DownloadBinary(Http, Url, s) then
          begin
          fSubtitles := s;
          fSubtitlesExt := ExtractFileExt(Url);
          Result := True;
          Break;
          end;
    if not Result then
      for i := 0 to Pred(Length(fSubtitleRegExps)) do
        if GetRegExpVar(fSubtitleRegExps[i], Page, 'SUBTITLES', Subs) then
          begin
          fSubtitles := AnsiString(StringToUtf8(Subs));
          //fSubtitlesExt := '.txt';
          Result := True;
          Break;
          end;
    end;
end;

function TCommonDownloader.WriteSubtitles: boolean;
var Overwrite: boolean;
    SubtitlesFileName: string;
begin
  Result := False;
  if SubtitlesEnabled then
    if fSubtitles <> '' then
      begin
      Overwrite := True;
      SubtitlesFileName := GetSubtitlesFileName;
      if FileExists(SubtitlesFileName) then
        if Assigned(OnFileNameValidate) then
          OnFileNameValidate(Self, SubtitlesFileName, Overwrite);
      if Overwrite then
        with TFileStream.Create(SubtitlesFileName, fmCreate) do
          try
            WriteBuffer(fSubtitles[1], Length(fSubtitles) * Sizeof(fSubtitles[1]));
            Result := True;
          finally
            Free;
            end;
      end;
end;

{$ENDIF}

function TCommonDownloader.Prepare: boolean;
var Info: THttpSend;
    URL, Page, s: string;
    PageXml: TXmlDoc;
begin
  SetLastErrorMsg('');
  Result := False;
  SetPrepared(False);
  PageXml := nil;
  Info := CreateHttp;
  try
    // Download the media info page.
    URL := GetMovieInfoUrl;
    if URL = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
    else if not GetMovieInfoContent(Info, URL, Page, PageXml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      try
        if InfoPageIsXml and (PageXml = nil) then
          SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
        else if not BeforePrepareFromPage(Page, PageXml, Info) then
          SetLastErrorMsg(_(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE))
        else
          begin
          SetName('');
          MovieURL := '';
          {$IFDEF SUBTITLES}
          fSubtitles := '';
          fSubtitlesExt := '';
          {$ENDIF}
          // If regular expression for TITLE is set, use it to get title.
          if MovieTitleRegExp <> nil then
            if GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', s) then
              SetName(s);
          // If a function for building URL is provided, use it.
          if BuildMovieURL(s) then
            MovieURL := s
          // Otherwise if regular expression for URL is set, use it.
          else
            if MovieUrlRegExp <> nil then
              if GetRegExpVar(MovieURLRegExp, Page, 'URL', s) then
                MovieURL := s;
          // If URL was set, Prepare was successful.
          if MovieUrl <> '' then
            SetPrepared(True);
          // Try additional processing of page data
          if not AfterPrepareFromPage(Page, PageXml, Info) then
            SetPrepared(False);
          if (not Prepared) and (LastErrorMsg = '') then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO));
          {$IFDEF SUBTITLES}
          if Prepared then
            ReadSubtitles(Page, PageXml, Info);
          {$ENDIF}
          Result := Prepared;
          end;
      finally
        FreeAndNil(PageXml);
        end;
  finally
    Info.Free;
    end;
end;

function TCommonDownloader.Download: boolean;
begin
  {$IFDEF SUBTITLES}
  WriteSubtitles;
  {$ENDIF}
  Result := inherited Download;
end;

function TCommonDownloader.BuildMovieUrl(out Url: string): boolean;
begin
  Result := False;
end;

function TCommonDownloader.BeforePrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean;
begin
  Result := RegExp.Match(Text) and RegExp.SubexpressionByName(VarName, VarValue);
end;

function TCommonDownloader.GetRegExpAllVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: TStringArray): boolean;
var n: integer;
    b: boolean;
    Value: string;
begin
  SetLength(VarValue, 0);
  n := 0;
  b := RegExp.Match(Text);
  while b do
    begin
    if RegExp.SubexpressionByName(VarName, Value) then
      begin
      if Length(VarValue) <= n then
        SetLength(VarValue, Length(VarValue)+16);
      VarValue[n] := Value;
      Inc(n);
      end;
    b := RegExp.MatchAgain;
    end;
  SetLength(VarValue, n);
  Result := n > 0;
end;

function TCommonDownloader.GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean;
var i: integer;
    VarValue: string;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  Result := RegExp.Match(Text);
  if Result then
    for i := 0 to High(VarNames) do
      if not RegExp.SubExpressionByName(VarNames[i], VarValue) then
        Result := False
      else
        VarValues[i]^ := VarValue;
end;

function TCommonDownloader.GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean; const VarNameSubExprName, VarValueSubExprName: string): boolean;
var i, j: integer;
    VarName: string;
begin
  if InitValues then
    for i := 0 to High(VarValues) do
      VarValues[i]^ := '';
  Result := RegExp.Match(Text);
  if Result then
    repeat
      VarName := RegExp.SubexpressionByName(VarNameSubExprName);
      for j := 0 to High(VarNames) do
        if VarName = VarNames[j] then
          begin
          if j <= High(VarValues) then
            VarValues[j]^ := RegExp.SubexpressionByName(VarValueSubExprName);
          end;
    until not RegExp.MatchAgain;
end;

function TCommonDownloader.GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean;
var Node: TXmlNode;
begin
  if XmlNodeByPath(Xml, Path, Node) then
    begin
    VarValue := XmlValueIncludingCData(Node);
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TCommonDownloader.GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean;
var Node: TXmlNode;
begin
  if XmlNodeByPath(Xml, Path, Node) and Node.HasAttribute(Utf8String(Attribute)) then
    begin
    VarValue := Node.AttributeByNameWide[Utf8String(Attribute)];
    Result := True;
    end
  else
    begin
    VarValue := '';
    Result := False;
    end;
end;

function TCommonDownloader.GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean;
begin
  Result := GetXmlVar(Xml.Root, Path, VarValue);
end;

function TCommonDownloader.GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean;
begin
  Result := GetXmlAttr(Xml.Root, Path, Attribute, VarValue);
end;

procedure TCommonDownloader.SetOptions(const Value: TYTDOptions);
{$IFDEF SUBTITLES}
var s: string;
{$ENDIF}
begin
  inherited;
  {$IFDEF SUBTITLES}
  fSubtitlesEnabled := Value.SubtitlesEnabled;
  if fSubtitlesEnabled then
    if Value.ReadProviderOption(Provider, 'subtitles_enabled', s) then
      fSubtitlesEnabled := StrToIntDef(s, 0) <> 0;
  {$ENDIF}
end;

end.
