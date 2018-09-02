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

unit uHttpDirectDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, blcksock,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  THttpDirectDownloader = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      constructor CreateWithName(const AMovieID, AMovieName: string; ACookies: TStrings = nil); virtual;
      destructor Destroy; override;
      function Prepare: boolean; override;
    end;

implementation

uses
  uLanguages, uMessages;

{ THttpDirectDownloader }

class function THttpDirectDownloader.Provider: string;
begin
  Result := 'HTTP direct download';
end;

class function THttpDirectDownloader.UrlRegExp: string;
begin
  Raise EDownloaderError.Create('THttpDirectDownloader.UrlRegExp is not supported.');
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
end;

constructor THttpDirectDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

constructor THttpDirectDownloader.CreateWithName(const AMovieID, AMovieName: string; ACookies: TStrings);
begin
  Create(AMovieID);
  SetName(AMovieName);
  if ACookies <> nil then
    Cookies.Assign(ACookies);
end;

destructor THttpDirectDownloader.Destroy;
begin
  inherited;
end;

function THttpDirectDownloader.GetMovieInfoUrl: string;
begin
  Result := '';
end;

function THttpDirectDownloader.Prepare: boolean;
begin
  inherited Prepare;
  Result := False;
  if MovieID = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieURL := MovieID;
    SetPrepared(True);
    Result := True;
    end;
end;

end.
