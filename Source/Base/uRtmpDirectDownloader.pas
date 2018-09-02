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

unit uRtmpDirectDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, blcksock,
  uDownloader, uCommonDownloader, uRtmpDownloader,
  RtmpDump_DLL;

type
  TRtmpDirectDownloader = class(TRtmpDownloader)
    private
      fSavedRtmpDumpOptions: TRtmpDumpOptions;
    protected
      function GetMovieInfoUrl: string; override;
      property SavedRtmpDumpOptions: TRtmpDumpOptions read fSavedRtmpDumpOptions write fSavedRtmpDumpOptions;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      constructor CreateWithName(const AMovieID, AMovieName: string); virtual;
      destructor Destroy; override;
      function Prepare: boolean; override;
      procedure SaveRtmpDumpOptions;
      procedure RestoreRtmpDumpOptions;
    end;

implementation

uses
  uDownloadClassifier,
  uLanguages, uMessages;

// rtmp://...
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'rtmpt?e?://.+';
  URLREGEXP_AFTER_ID =  '';

{ TRtmpDirectDownloader }

class function TRtmpDirectDownloader.Provider: string;
begin
  Result := 'RTMP direct download';
end;

class function TRtmpDirectDownloader.UrlRegExp: string;
begin
  Result := Format(URLREGEXP_BEFORE_ID + '(?P<%s>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID, [MovieIDParamName]);;
end;

constructor TRtmpDirectDownloader.Create(const AMovieID: string);
begin
  inherited;
  SetLength(fSavedRtmpDumpOptions, 0);
end;

constructor TRtmpDirectDownloader.CreateWithName(const AMovieID, AMovieName: string);
begin
  Create(AMovieID);
  Name := AMovieName;
end;

destructor TRtmpDirectDownloader.Destroy;
begin
  inherited;
end;

function TRtmpDirectDownloader.GetMovieInfoUrl: string;
begin
  Result := '';
end;

function TRtmpDirectDownloader.Prepare: boolean;
begin
  inherited Prepare;
  Result := False;
  if MovieID = '' then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    if UnpreparedName = '' then
      Name := ExtractUrlFileName(MovieID);
    RestoreRtmpDumpOptions;
    MovieURL := MovieID;
    if RtmpUrl = '' then
      RtmpUrl := MovieID;
    SetPrepared(True);
    Result := True;
    end;
end;

procedure TRtmpDirectDownloader.RestoreRtmpDumpOptions;
var
  i: integer;
begin
  for i := 0 to Pred(Length(SavedRtmpDumpOptions)) do
    SetRtmpDumpOption( {$IFDEF UNICODE} Char {$ENDIF} (SavedRtmpDumpOptions[i].ShortOption), {$IFDEF UNICODE} string {$ENDIF} (SavedRtmpDumpOptions[i].Argument));
end;

procedure TRtmpDirectDownloader.SaveRtmpDumpOptions;
begin
  SavedRtmpDumpOptions := RtmpDumpOptions;
end;

initialization
  {$IFDEF DIRECTDOWNLOADERS}
  RegisterDownloader(TRtmpDirectDownloader);
  {$ENDIF}

end.
