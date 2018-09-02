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

unit downMojeVideoSk;
{$INCLUDE 'ytd.inc'}

{
  MojeVideo.sk pouziva heslo, ktere se najde ve funkci buildSecureURL ve vpx.swf.
}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_MojeVideoSk = class(THttpDownloader)
    private
    protected
      VideoIdRegExp: TRegExp;
      TimestampRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.mojevideo.sk/video/6227/krasa_nasej_planety_v_hq.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*mojevideo\.sk/video/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_SOME_STRING = '(?:\s*[''"][^''"]*[''"]\s*,)';
  REGEXP_SOME_NUMBER = '(?:\s*\d+\s*,)';
  REGEXP_EXTRACT_TITLE = REGEXP_TITLE_TITLE;
  REGEXP_EXTRACT_ID = '\bvar\s+rvid\s*=\s*(?P<ID>[0-9]+)';
  REGEXP_EXTRACT_TIMESTAMP = '\bplvad\s*\(' + REGEXP_SOME_STRING + '{2}' + REGEXP_SOME_NUMBER + '{4}' + REGEXP_SOME_STRING + '{1}' + REGEXP_SOME_NUMBER + '{2}' + REGEXP_SOME_STRING + '{1}' + REGEXP_SOME_NUMBER + '{1}\s*(?P<TS>\d+)';

{ TDownloader_MojeVideoSk }

class function TDownloader_MojeVideoSk.Provider: string;
begin
  Result := 'MojeVideoSk.com';
end;

class function TDownloader_MojeVideoSk.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

class function TDownloader_MojeVideoSk.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [{dfRequireSecureToken}];
end;

constructor TDownloader_MojeVideoSk.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE);
  VideoIdRegExp := RegExCreate(REGEXP_EXTRACT_ID);
  TimestampRegExp := RegExCreate(REGEXP_EXTRACT_TIMESTAMP);
end;

destructor TDownloader_MojeVideoSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(VideoIdRegExp);
  RegExFreeAndNil(TimestampRegExp);
  inherited;
end;

function TDownloader_MojeVideoSk.GetMovieInfoUrl: string;
begin
  Result := 'http://www.mojevideo.sk/video/' + MovieID;
end;

function TDownloader_MojeVideoSk.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
const
  Server {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://fs5.mojevideo.sk';
var
  ID, {Signature, Timestamp,} Stream: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  //else if not GetRegExpVar(TimestampRegExp, Page, 'TS', Timestamp) then
  //  SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['timestamp']))
  else
    begin
    Stream := Format('/%s.mp4', [ID]);
    //Timestamp := LowerCase(IntToHex({UnixTimestamp} StrToInt(Timestamp), 8));
    //Signature := HexEncode(MD5( {$IFDEF UNICODE} AnsiString {$ENDIF} (Self.Token + Stream + Timestamp)));
    //MovieUrl := Format('%s/dll/%s/%s%s', [Server, Signature, Timestamp, Stream]);
    MovieUrl := Format('%s/securevd%s', [Server, Stream]);
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_MojeVideoSk);

end.
