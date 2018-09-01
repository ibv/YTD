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

unit listHTMLfile;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader, listHTML,
  uDownloadClassifier;

type
  TPlaylist_HTMLfile = class(TPlaylist_HTML)
    private
    protected
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uFiles;
  
{ TPlaylist_HTMLfile }

constructor TPlaylist_HTMLfile.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TPlaylist_HTMLfile.Destroy;
begin
  inherited;
end;

function TPlaylist_HTMLfile.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
var MS: TMemoryStream;
    s: AnsiString;
begin
  Result := FileExists(Url);
  if Result then
    begin
    MS := LoadFileIntoMemory(Url);
    try
      if MS.Size = 0 then
        Page := ''
      else
        begin
        SetLength(s, MS.Size);
        Move(MS.Memory^, s[1], MS.Size);
        Page := s;
        end;
    finally
      MS.Free;
      end;
    end;
end;

end.
