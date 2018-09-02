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

unit downSportCZ;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downStream;

type
  TDownloader_SportCZ = class(TDownloader_Stream)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://fotbal.sport.cz/clanek/183261-video-wenger-si-oddechl-rosicky-mel-jen-slaby-otres-mozku.html
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*sport\.cz(?:/.*)?';
  URLREGEXP_AFTER_ID =  '$';

{ TDownloader_SportCZ }

class function TDownloader_SportCZ.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_SportCZ.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUtf8;
end;

destructor TDownloader_SportCZ.Destroy;
begin
  inherited;
end;

function TDownloader_SportCZ.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_SportCZ.GetFlashVarsIdStrings(out ID, cdnLQ, cdnHQ, cdnHD, Title: string): boolean;
begin
  Result := inherited GetFlashVarsIdStrings(ID, cdnLQ, cdnHQ, cdnHD, Title);
  Title := 'gemius_name';
end;

initialization
  RegisterDownloader(TDownloader_SportCZ);

end.
