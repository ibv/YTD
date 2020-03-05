(******************************************************************************

______________________________________________________________________________

YTD v1.63                                                    (c) 2019 ibv
______________________________________________________________________________


Copyright (c) 2019 ibv (https://ibv.github.io/YTD/)
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

unit downDash;
{$INCLUDE 'ytd.inc'}
{$DEFINE CONVERTSUBTITLES}
  // Convert subtitles to .srt format


interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  uPCRE, uXml, HttpSend, SynaUtil,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
//      guiOptionsWINAPI_CT,
    {$ELSE}
      guiOptionsLCL_DASH,
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uHLSDownloader, uDASHDownloader;

type
  TDownloader_Dash = class(TDASHDownloader)
    private
      {$IFDEF MULTIDOWNLOADS}
      fNameList: TStringList;
      fUrlList: TStringList;
      fDownloadIndex: integer;
      {$ENDIF}
    protected
      PlaylistInfoRegExp: TRegExp;
      PlaylistUrlRegExp: TRegExp;
      StreamUrlRegExp: TRegExp;
      StreamTitleRegExp: TRegExp;
      StreamTitle2RegExp: TRegExp;
      StreamTitleFromPageRegExp: TRegExp;
      IFrameUrlRegExp: TRegExp;
      {$IFDEF MULTIDOWNLOADS}
      property NameList: TStringList read fNameList;
      property UrlList: TStringList read fUrlList;
      property DownloadIndex: integer read fDownloadIndex write fDownloadIndex;
      {$ENDIF}
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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
      {$IFDEF MULTIDOWNLOADS}
      function Prepare: boolean; override;
      function ValidatePrepare: boolean; override;
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;


const
  OPTION_DASH_VIDEO_SUPPORT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'dash_video_support';
  OPTION_DASH_AUDIO_SUPPORT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'dash_audio_support';


implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uFunctions,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        '^https?:\/\/[^\?]*\.mpd';
  URLREGEXP_AFTER_ID =  '';


class function TDownloader_Dash.Provider: string;
begin
  Result := 'MPEG-DASH';
end;

class function TDownloader_Dash.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_Dash.Features: TDownloaderFeatures;
begin
  Result := inherited Features;
end;


{$IFDEF GUI}
class function TDownloader_DASH.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_DASH;
end;
{$ENDIF}


constructor TDownloader_Dash.Create(const AMovieID: string);
begin
  inherited;
  {$IFDEF MULTIDOWNLOADS}
  fNameList := TStringList.Create;
  fUrlList := TStringList.Create;
  {$ENDIF}
  InfoPageEncoding := peUTF8;
  Referer := GetMovieInfoUrl;
end;

destructor TDownloader_Dash.Destroy;
begin
  {$IFDEF MULTIDOWNLOADS}
  FreeAndNil(fNameList);
  FreeAndNil(fUrlList);
  {$ENDIF}
  inherited;
end;

function TDownloader_Dash.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_Dash.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Title: string;
  {$IFDEF MULTIDOWNLOADS}
  Urls: TStringArray;
  i: integer;
  {$ELSE}
  Url: string;
  {$ENDIF}

begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;

  Title :='video';
  Name := Title;
  MovieURL := {$IFDEF MULTIDOWNLOADS} referer {$ELSE} Url {$ENDIF};
  SetPrepared(True);
  Result := True;
end;


procedure TDownloader_Dash.SetOptions(const Value: TYTDOptions);
var
  Bitrate: integer;
begin
  inherited;
end;


function TDownloader_Dash.GetFileNameExt: string;
begin
  Result:='.mpv';
end;


{$IFDEF MULTIDOWNLOADS}

function TDownloader_Dash.Prepare: boolean;
begin
  NameList.Clear;
  UrlList.Clear;
  DownloadIndex := 0;
  Result := inherited Prepare;
end;

function TDownloader_Dash.ValidatePrepare: boolean;
var
  DownIndex: integer;
begin
  DownIndex := DownloadIndex;
  try
    Result := inherited ValidatePrepare;
  finally
    DownloadIndex := DownIndex;
    end;
end;

function TDownloader_Dash.First: boolean;
begin
  if ValidatePrepare then
    if UrlList.Count <= 0 then
      Result := MovieURL <> ''
    else
      begin
      DownloadIndex := -1;
      Result := Next;
      end
  else
    Result := False;
end;

function TDownloader_Dash.Next: boolean;
begin
  Result := False;
  if ValidatePrepare then
    begin
    DownloadIndex := Succ(DownloadIndex);
    if (DownloadIndex >= 0) and (DownloadIndex < UrlList.Count) then
      begin
      Name := NameList[DownloadIndex];
      SetFileName('');
      MovieURL := UrlList[DownloadIndex];
      Result := True;
      end;
    end;
end;

{$ENDIF}

initialization
  RegisterDownloader(TDownloader_Dash);

end.

