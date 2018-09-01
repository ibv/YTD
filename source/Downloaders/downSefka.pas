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

unit downSefka;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downJoj_Porady;

type
  TDownloader_Sefka = class(TDownloader_Joj_Porady)
    private
    protected
      function GetMovieInfoUrl: string; override;{*}
      function TheServer: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;{*}
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.sefka.cz/epizody-cz/detail/sefka-20-03-2011.html
// http://www.sefka.sk/epizody/detail/sefka-20-03-2011.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*sefka\.';
  URLREGEXP_ID =        '(cz|sk)/.+?\.html';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Sefka }

class function TDownloader_Sefka.Provider: string;
begin
  Result := 'Sefka.cz';
end;

class function TDownloader_Sefka.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Sefka.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_Sefka.Destroy;
begin
  inherited;
end;

function TDownloader_Sefka.GetMovieInfoUrl: string;
begin
  Result := 'http://www.sefka.' + MovieID;
end;

function TDownloader_Sefka.TheServer: string;
begin
  Result := 'n11.joj.sk';
end;

initialization
  RegisterDownloader(TDownloader_Sefka);

end.
