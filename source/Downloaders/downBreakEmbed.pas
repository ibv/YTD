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

unit downBreakEmbed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBreak;

type
  TDownloader_BreakEmbed = class(THttpDownloader)
    private
    protected
      MovieVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://media1.break.com/static/app/v1/global/swf/player10.swf?sVidLoc=http%3a%2f%2fvideo1.break.com%2fdnet%2fmedia%2f2007%2f8%2fdude-blows-off-firecracker-in-teeth.flv&sThumbLoc=http%3a%2f%2fmedia1.break.com%2fdnet%2fmedia%2f2007%2f8%2fdude-blows-off-firecracker-in-teeth.jpg&contentURL=http%3a%2f%2fwww.break.com%2findex%2fdude-blows-off-firecracker-in-teeth.html&sShareURL=http%3a%2f%2fwww.break.com%2findex%2fdude-blows-off-firecracker-in-teeth.html%23TellAFriendhttp%3a%2f%2fstats.break.com%2finvoke.txt&iContentID=359418&autoplay=0&embed=2&contentidencoded=359418&categoryid=4&userid=1620903&mode=embed&linktitle=Funny+Videos&sVidTitle=EMBED-Dude+Blows+Off+Firecracker+In+Teeth&icon=1B608EE7AFCE3765E176F3C6FBB98002B3D18C64572F2307D76FAB7CA970F3B1D7D7
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*break\.com/.*?';
  URLREGEXP_ID =        '\?(?:.*?&)*(?:sVidLoc|sVidTitle|icon)=.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_VARS = '[?&](?P<VARNAME>[^=]+)=(?P<VARVALUE>[^&]*)';

{ TDownloader_BreakEmbed }

class function TDownloader_BreakEmbed.Provider: string;
begin
  Result := TDownloader_Break.Provider;
end;

class function TDownloader_BreakEmbed.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BreakEmbed.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peNone;
  MovieVarsRegExp := RegExCreate(REGEXP_EXTRACT_VARS, [rcoIgnoreCase]);
end;

destructor TDownloader_BreakEmbed.Destroy;
begin
  RegExFreeAndNil(MovieVarsRegExp);
  inherited;
end;

function TDownloader_BreakEmbed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.break.com'; // No download is needed, but this function must return something
end;

function TDownloader_BreakEmbed.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Page := MovieID;
  Xml := nil;
  Result := MovieID <> '';
end;

function TDownloader_BreakEmbed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const TitlePrefix = 'EMBED-';
var Url, Title, Token: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVarPairs(MovieVarsRegExp, Page, ['sVidLoc', 'sVidTitle', 'icon'], [@Url, @Title, @Token]) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if Url = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['sVidLoc']))
  else if Title = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['sVidTitle']))
  else if Token = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['icon']))
  else
    begin
    MovieUrl := UrlDecode(Url) + '?' + Token;
    if AnsiCompareText(TitlePrefix, Copy(Title, 1, Length(TitlePrefix))) = 0 then
      System.Delete(Title, 1, Length(TitlePrefix)); 
    SetName(UrlDecode(Title));
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BreakEmbed);

end.
