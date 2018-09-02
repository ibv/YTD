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

unit listHTML;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader,
  uDownloadClassifier;

type
  TPlaylist_HTML = class(TPlaylistDownloader)
    private
      fClassifier: TDownloadClassifier;
    protected
      function GetUrlRegExp: string; virtual;
      function GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string; override;
      property Classifier: TDownloadClassifier read fClassifier;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

const
  REGEXP_URL = '(?:\bhref'
             + '|\bsrc'
             + '|<param\s+name=(?P<Q1>["''])movie(?P=Q1)\s+value=)\s*=\s*(?P<Q>["''])(?P<URL>(?:https?|mmsh?|rtmpt?e?)://.+?)(?P=Q)'
             ;

{ TPlaylist_HTML }

constructor TPlaylist_HTML.Create(const AMovieID: string);
begin
  inherited;
  PlaylistItemRegExp := RegExCreate(GetUrlRegExp);
  fClassifier := TDownloadClassifier.Create;
end;

destructor TPlaylist_HTML.Destroy;
begin
  RegExFreeAndNil(PlaylistItemRegExp);
  FreeAndNil(fClassifier);
  inherited;
end;

function TPlaylist_HTML.GetUrlRegExp: string;
begin
  Result := REGEXP_URL;
end;

function TPlaylist_HTML.GetPlayListItemURL(Match: TRegExpMatch; Index: integer): string;
begin
  Result := inherited GetPlayListItemURL(Match, Index);
  Result := HtmlDecode(Result);
  Classifier.Url := Result;
  if Classifier.Downloader = nil then
    Result := '';
end;

end.
