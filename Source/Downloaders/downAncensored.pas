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

unit downAncensored;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Ancensored = class(THttpDownloader)
    private
    protected
      Extension: string;
      MovieIDRegExp: TRegExp;
      HashRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function GetHash(Http: THttpSend; out Hash: string): boolean;
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

// http://ancensored.com/clip/clip-victorias-secret-fashion-show-2009-herself-3
const
  URLREGEXP_BEFORE_ID = 'ancensored\.com/clip/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  '<title>(?P<TITLE>.*?)(?:\s*(?:Video\s*Clip\s*)?&lt;.*?)?</title>';
  REGEXP_MOVIE_ID =     '"video"\s*:\s"(?P<ID>.+?)"';
  REGEXP_HASH =         '"hash1"\s*:\s*"(?P<HASH1>[^"]*)"\s*,\s*"hash2"\s*:\s*"(?P<HASH2>.*?)"';

{ TDownloader_Ancensored }

class function TDownloader_Ancensored.Provider: string;
begin
  Result := 'Ancensored.com';
end;

class function TDownloader_Ancensored.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_Ancensored.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID);
  HashRegExp := RegExCreate(REGEXP_HASH);
end;

destructor TDownloader_Ancensored.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  RegExFreeAndNil(HashRegExp);
  inherited;
end;

function TDownloader_Ancensored.GetMovieInfoUrl: string;
begin
  Result := 'http://ancensored.com/clip/' + MovieID;
end;

function TDownloader_Ancensored.GetFileNameExt: string;
begin
  Result := Extension;
end;

function TDownloader_Ancensored.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  ID, Hash: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetHash(Http, Hash) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['hash']))
  else
    begin
    Extension := ExtractFileExt(ID);
    MovieUrl := 'http://ancensored.com/video.php?file=' + ID + '&hash=' + Hash;
    SetPrepared(True);
    Result := True;
    end;
end;

function TDownloader_Ancensored.GetHash(Http: THttpSend; out Hash: string): boolean;
var
  Page, Hash1, Hash2: string;
  i: integer;
begin
  Result := False;
  Hash := '';
  if DownloadPage(Http, 'http://ancensored.com/player/hash/js', Page) then
    if GetRegExpVars(HashRegExp, Page, ['HASH1', 'HASH2'], [@Hash1, @Hash2]) then
      begin
      Hash1 := StringReplace(Hash1, ' ', '', [rfReplaceAll]);
      Hash2 := StringReplace(Hash2, ' ', '', [rfReplaceAll]);
      if Length(Hash1) >= 16 then
        if Length(Hash2) >= 16 then
          begin
          for i := 1 to 8 do
            Hash := Hash + Hash1[2*i-1] + Hash1[2*i] + Hash2[2*i-1] + Hash2[2*i];
          Result := True;
          end;
      end;
  {$IFDEF DIRTYHACKS}
  if not Result then
    begin
    Hash := '9d5e564d74a3059297d19f3aaa85cbc4';
    Result := True;
    end;
  {$ENDIF}
end;

initialization
  RegisterDownloader(TDownloader_Ancensored);

end.
