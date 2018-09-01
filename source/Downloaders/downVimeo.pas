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

unit downVimeo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Vimeo = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: TRegExp;
    protected
      function GetFileNameExt: string; override;
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

// http://www.vimeo.com/10777111
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*vimeo\.com/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Vimeo }

class function TDownloader_Vimeo.Provider: string;
begin
  Result := 'Vimeo.com';
end;

class function TDownloader_Vimeo.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TDownloader_Vimeo.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peUTF8;
  InfoPageIsXml := True;
end;

destructor TDownloader_Vimeo.Destroy;
begin
  inherited;
end;

function TDownloader_Vimeo.GetFileNameExt: string;
begin
  Result := '.mp4';
end;

function TDownloader_Vimeo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.vimeo.com/moogaloop/load/clip:' + MovieID + '/';
end;

function TDownloader_Vimeo.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Caption, Signature, Expires: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetXmlVar(PageXml, 'video/caption', Caption) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
  else if not GetXmlVar(PageXml, 'request_signature', Signature) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['request_signature.']))
  else if not GetXmlVar(PageXml, 'request_signature_expires', Expires) then
    SetLastErrorMsg(Format(ERR_VARIABLE_NOT_FOUND, ['request_signature_expires.']))
  else
    begin
    SetName(HtmlDecode(Caption));
    MovieUrl := 'http://www.vimeo.com/moogaloop/play/clip:' + MovieID + '/' + Signature + '/' + Expires + '/';
    Result := True;
    SetPrepared(True);
    end;
end;

initialization
  RegisterDownloader(TDownloader_Vimeo);

end.
