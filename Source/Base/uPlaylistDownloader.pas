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

unit uPlaylistDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uOptions, HttpSend,
  uDownloader, uCommonDownloader;

type
  TPlaylistDownloader = class(TCommonDownloader)
    private
      fUrlList: TStringList;
      fNameList: TStringList;
    protected
      PlayListItemRegExp: TRegExp;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      procedure FindPlaylistItems(var Page: string; PageXml: TXmlDoc; Http: THttpSend); virtual;
      procedure FindPlaylistItemsByRegExp(RegExp: TRegExp; var Page: string; PageXml: TXmlDoc; Http: THttpSend); virtual;
      function GetPlayListItemName(Match: TRegExpMatch; Index: integer): string; virtual;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; virtual;
      function GetItemCount: integer; virtual;
      function GetItemUrl(Index: integer): string; virtual;
      function GetItemName(Index: integer): string; virtual;
      function AddIndexToNames: boolean;virtual;
      property UrlList: TStringList read fUrlList;
      property NameList: TStringList read fNameList;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      property Count: integer read GetItemCount;
      property Urls[Index: integer]: string read GetItemUrl; default;
      property Names[Index: integer]: string read GetItemName;
    end;

implementation

uses
  uLanguages, uMessages;
  
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://.+)';
  URLREGEXP_AFTER_ID =  '$';

{ TPlaylistDownloader }

class function TPlaylistDownloader.Provider: string;
begin
  Result := '-playlist-';
end;

class function TPlaylistDownloader.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + ClassName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TPlaylistDownloader.Create(const AMovieID: string);
begin
  inherited;
  fUrlList := TStringList.Create;
  fNameList := TStringList.Create;
end;

destructor TPlaylistDownloader.Destroy;
begin
  FreeAndNil(fUrlList);
  FreeAndNil(fNameList);
  RegExFreeAndNil(PlayListItemRegExp);
  inherited;
end;

function TPlaylistDownloader.GetMovieInfoUrl: string;
var
  ix: integer;
begin
  Result := MovieID;
  ix := Pos('#', Result);
  if ix > 0 then
    SetLength(Result, ix-1);
end;

function TPlaylistDownloader.GetItemCount: integer;
begin
  Result := UrlList.Count;
end;

function TPlaylistDownloader.GetItemUrl(Index: integer): string;
begin
  Result := UrlList[Index];
end;

function TPlaylistDownloader.GetItemName(Index: integer): string;
begin
  Result := NameList[Index];
end;

function TPlaylistDownloader.GetPlayListItemName(Match: TRegExpMatch; Index: integer): string;
begin
  //Result := Format(MSG_PLAYLIST_ITEM, [Index]);
  Result := '';
end;

function TPlaylistDownloader.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := Match.SubexpressionByName('URL');
end;

procedure TPlaylistDownloader.FindPlaylistItems(var Page: string; PageXml: TXmlDoc; Http: THttpSend);
begin
  FindPlaylistItemsByRegExp(PlayListItemRegExp, Page, PageXml, Http);
end;

procedure TPlaylistDownloader.FindPlaylistItemsByRegExp(RegExp: TRegExp; var Page: string; PageXml: TXmlDoc; Http: THttpSend);
var Url: string;
    i: integer;
begin
  if RegExp <> nil then
    if not RegExp.Match(Page) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      i := 0;
      repeat
        Url := GetPlayListItemURL(RegExp, i);
        if (Url <> '') and (UrlList.IndexOf(Url) < 0) then
          begin
          UrlList.Add(Url);
          NameList.Add(GetPlayListItemName(RegExp, i));
          end;
        Inc(i);
      until not RegExp.MatchAgain;
      end;
end;

function TPlayListDownloader.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  UrlList.Clear;
  FindPlaylistItems(Page, PageXml, Http);
  if UrlList.Count <= 0 then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    Result := True;
    SetPrepared(True);
    end;
end;

function TPlaylistDownloader.Prepare: boolean;
var
  i: integer;
  IndexOpt: TIndexForNames;
begin
  Result := inherited Prepare;
  if Result and AddIndexToNames then
    begin
    IndexOpt := Options.AddIndexToNames;
    if IndexOpt <> ifnNone then
      for i := 0 to Pred(NameList.Count) do
        NameList[i] := ApplyIndexToName(NameList[i], i, NameList.Count);
    end;
end;

function TPlaylistDownloader.AddIndexToNames: boolean;
begin
  Result := NameList.Count > 1;
end;

end.
