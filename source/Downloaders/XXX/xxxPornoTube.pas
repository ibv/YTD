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

unit xxxPornoTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornoTube = class(THttpDownloader)
    private
    protected
      FlashIdRegExp: TRegExp;
      FlashVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornotube\.com/channels\.php\?';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+class="contentheader">\s*<span>\s*Viewing Video:\s*(?:</span><span[^>]*>)*(?P<TITLE>.*?)\s*</span>\s*</div>';
  REGEXP_FLASHID = '<param\s+name="movie"\s+value="[^"]*[?&]v=(?P<ID>[^"&]+)"';
  REGEXP_FLASHVARS = '[?&](?P<VARNAME>[a-z_][a-z0-9_.]*)=(?P<VARVALUE>[^&]+)';

{ TDownloader_PornoTube }

class function TDownloader_PornoTube.Provider: string;
begin
  Result := 'PornoTube.com';
end;

class function TDownloader_PornoTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornoTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashIdRegExp := RegExCreate(REGEXP_FLASHID, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PornoTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashIdRegExp);
  inherited;
end;

function TDownloader_PornoTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornotube.com/channels.php?' + MovieID;
end;

function TDownloader_PornoTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashID, FlashVars, MediaID, UserID, MediaDomain: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FlashIdRegExp, Page, 'ID', FlashID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not DownloadPage(Http, 'http://www.pornotube.com/player/player.php?' + FlashID, FlashVars) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    GetRegExpVarPairs(FlashVarsRegExp, FlashVars, ['mediaId', 'userId', 'mediaDomain'], [@MediaID, @UserID, @MediaDomain]);
    if MediaID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['mediaId']))
    else if UserID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['userId']))
    else if MediaDomain = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['mediaDomain']))
    else
      begin
      MovieUrl := MediaDomain + '.pornotube.com/' + UserId + '/' + MediaID + '.flv';
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornoTube);
  {$ENDIF}

end.
