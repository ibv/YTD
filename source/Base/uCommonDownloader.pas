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

unit uCommonDownloader;
{$INCLUDE 'ytd.inc'}
{$DEFINE SUBTITLES}

interface

uses
  SysUtils, Classes,
  uPCRE, uXML, HttpSend, blcksock,
  uDownloader;

type
  TCommonDownloader = class(TDownloader)
    private
      fMovieURL: string;
      fInfoPageEncoding: TPageEncoding;
      fInfoPageIsXml: boolean;
    protected
      function GetMovieInfoUrl: string; virtual; abstract;
    protected
      MovieTitleRegExp: TRegExp;
      MovieUrlRegExp: TRegExp;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean; overload; virtual;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; overload; virtual;
      function ExtractUrlExt(const Url: string): string; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      property MovieUrl: string read fMovieUrl write fMovieUrl;
      property InfoPageEncoding: TPageEncoding read fInfoPageEncoding write fInfoPageEncoding;
      property InfoPageIsXml: boolean read fInfoPageIsXml write fInfoPageIsXml;
    protected
      function GetFileNameExt: string; override;
      function BuildMovieUrl(out Url: string): boolean; virtual;
      function BeforePrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; virtual;
      {$IFDEF SUBTITLES}
    protected
      SubtitleUrlRegExps: array of TRegExp;
      SubtitleRegExps: array of TRegExp;
      Subtitles: string;
      SubtitlesExt: string;
      function GetSubtitlesFileName: string; virtual;
      function ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; virtual;
      function WriteSubtitles: boolean; virtual;
      {$ENDIF}
    protected
      function GetRegExpVar(RegExp: TRegExp; const Text, VarName: string; out VarValue: string): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetRegExpVars(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetRegExpVarPairs(RegExp: TRegExp; const Text: string; const VarNames: array of string; const VarValues: array of PString; InitValues: boolean = True; const VarNameSubExprName: string = 'VARNAME'; const VarValueSubExprName: string = 'VARVALUE'): boolean; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlDoc; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetXmlVar(Xml: TXmlNode; const Path: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlNode; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
      function GetXmlAttr(Xml: TXmlDoc; const Path, Attribute: string; out VarValue: string): boolean; overload; {$IFNDEF MINIMIZEVIRTUAL} virtual; {$ENDIF}
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      property ContentUrl: string read fMovieUrl;
    end;

implementation

uses
  uCompatibility, uMessages;

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
  SetLength(SubtitleUrlRegExps, 0);
  SetLength(SubtitleRegExps, 0);
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
  for i := 0 to Pred(Length(SubtitleUrlRegExps)) do
    RegExFreeAndNil(SubtitleUrlRegExps[i]);
  SetLength(SubtitleUrlRegExps, 0);
  for i := 0 to Pred(Length(SubtitleRegExps)) do
    RegExFreeAndNil(SubtitleRegExps[i]);
  SetLength(SubtitleRegExps, 0);
  {$ENDIF}
  inherited;
end;

function TCommonDownloader.GetFileNameExt: string;
begin
  if Prepared then
    Result := ExtractUrlExt(MovieURL)
  else
    NotPreparedError;
end;

function TCommonDownloader.ExtractUrlExt(const Url: string): string;
var i: integer;
begin
  i := Pos('?', Url);
  if i <= 0 then
    Result := Url
  else
    Result := Copy(Url, 1, Pred(i));
  Result := ExtractFileExt(Result);
  if Pos('/', Result) > 0 then
    Result := '';
end;

function TCommonDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc): boolean;
begin
  Result := GetMovieInfoContent(Http, Url, Page, Xml, hmGET);
end;

function TCommonDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Xml := nil;
  Result := DownloadPage(Http, Url, Page, InfoPageEncoding, Method);
  if Result then
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
function TCommonDownloader.ReadSubtitles(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var i: integer;
    Url: string;
begin
  Result := False;
  Subtitles := '';
  SubtitlesExt := '';
  for i := 0 to Pred(Length(SubtitleUrlRegExps)) do
    if GetRegExpVar(SubtitleUrlRegExps[i], Page, 'SUBTITLES', Url) then
      if DownloadPage(Http, Url) then
        begin
        SetLength(Subtitles, Http.Document.Size);
        Http.Document.Seek(0, 0);
        Http.Document.ReadBuffer(Subtitles[1], Http.Document.Size);
        Http.Document.Seek(0, 0);
        SubtitlesExt := ExtractFileExt(Url);
        Result := True;
        Break;
        end;
  if not Result then
    for i := 0 to Pred(Length(SubtitleRegExps)) do
      if GetRegExpVar(SubtitleRegExps[i], Page, 'SUBTITLES', Subtitles) then
        begin
        //SubtitlesExt := '.txt';
        Result := True;
        Break;
        end;
end;

function TCommonDownloader.WriteSubtitles: boolean;
var Overwrite: boolean;
    SubtitlesFileName: string;
begin
  Result := False;
  if Subtitles <> '' then
    begin
    Overwrite := True;
    SubtitlesFileName := GetSubtitlesFileName;
    if FileExists(SubtitlesFileName) then
      if Assigned(OnFileNameValidate) then
        OnFileNameValidate(Self, SubtitlesFileName, Overwrite);
    if Overwrite then
      with TFileStream.Create(SubtitlesFileName, fmCreate) do
        try
          WriteBuffer(Subtitles[1], Length(Subtitles) * Sizeof(Subtitles[1]));
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
          Subtitles := '';
          SubtitlesExt := '';
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

{$IFDEF SUBTITLES}
function TCommonDownloader.GetSubtitlesFileName: string;
begin
  Result := ChangeFileExt(GetFileName, SubtitlesExt);
end;
{$ENDIF}

function TCommonDownloader.Download: boolean;
begin
  Result := inherited Download;
  {$IFDEF SUBTITLES}
  if Result then
    WriteSubtitles;
  {$ENDIF}
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

end.
