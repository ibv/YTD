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

unit downZkoukniToEmbed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ZkoukniToEmbed = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
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

// http://www.zkouknito.cz/player/external.swf?vid=70326
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*zkouknito\.cz/player/external\.swf\?vid=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_ZkoukniToEmbed }

class function TDownloader_ZkoukniToEmbed.Provider: string;
begin
  Result := 'ZkoukniTo.cz';
end;

class function TDownloader_ZkoukniToEmbed.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_ZkoukniToEmbed.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peXml;
  InfoPageIsXml := True;
end;

destructor TDownloader_ZkoukniToEmbed.Destroy;
begin
  inherited;
end;

function TDownloader_ZkoukniToEmbed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.zkouknito.cz/player/scripts/videoinfo.php?id=' + MovieID;
end;

function TDownloader_ZkoukniToEmbed.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'file', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    Name := ChangeFileExt(ExtractUrlFileName(Url), '');
    MovieUrl := Url;
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_ZkoukniToEmbed);

end.
