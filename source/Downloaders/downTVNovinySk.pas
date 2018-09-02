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

unit downTVNovinySk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, uCrypto, HttpSend, SynaCode,
  uOptions, uCompatibility,
  uDownloader, uCommonDownloader, downVoyo;

type
  TDownloader_TVNovinySk = class(TDownloader_Voyo)
    protected
      function GetMovieInfoUrl: string; override;
      function GetFlowPlayerConfigRegExp: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  {$IFDEF DIRTYHACKS}
  uFiles,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://tvnoviny.sk/video/videospravy/televizne-noviny/2432970
const
  URLREGEXP_BEFORE_ID = 'tvnoviny\.sk/video/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG = 'var\s+flowConf\d*\s*=\s*\\?''(?P<CONFIG>.+?)\\?''';
  
{ TDownloader_TVNovinySk }

class function TDownloader_TVNovinySk.Provider: string;
begin
  Result := 'Markiza.sk';
end;

class function TDownloader_TVNovinySk.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_TVNovinySk.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_TVNovinySk.Destroy;
begin
  inherited;
end;

function TDownloader_TVNovinySk.GetMovieInfoUrl: string;
begin
  Result := 'http://tvnoviny.sk/video/' + MovieID;
end;

function TDownloader_TVNovinySk.GetFlowPlayerConfigRegExp: string;
begin
  Result := REGEXP_CONFIG;
end;

initialization
  RegisterDownloader(TDownloader_TVNovinySk);

end.
