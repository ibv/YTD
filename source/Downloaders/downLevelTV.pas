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

unit downLevelTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDirectDownloader;

type
  TDownloader_LevelTV = class(THttpDirectDownloader)
    private
    protected
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://leveltv.hrej.cz/include/2012/leden/3/213_rec_serioussam.mp4
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + 'leveltv\.hrej\.cz/.+\.mp4(?:\?.*)?';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_LevelTV }

class function TDownloader_LevelTV.Provider: string;
begin
  Result := 'LevelTV.Hrej.cz';
end;

class function TDownloader_LevelTV.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_LevelTV.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfUserLogin];
end;

constructor TDownloader_LevelTV.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
end;

destructor TDownloader_LevelTV.Destroy;
begin
  inherited;
end;

function TDownloader_LevelTV.Prepare: boolean;
var
  Http: THttpSend;
begin
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://leveltv.hrej.cz/include/loginOvereni.php', {$IFDEF UNICODE} AnsiString {$ENDIF} ('login=' + UrlEncode(UserName)), HTTP_FORM_URLENCODING) then
      begin
      Cookies.Assign(Http.Cookies);
      Result := inherited Prepare;
      end
    else
      begin
      SetLastErrorMsg(ERR_LOGIN_FAILED);
      Result := False;
      end;
  finally
    FreeAndNil(Http);
    end;
end;

initialization
  RegisterDownloader(TDownloader_LevelTV);

end.
