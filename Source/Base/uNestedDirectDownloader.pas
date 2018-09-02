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

unit uNestedDirectDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend, blcksock, 
  uDownloader, uCommonDownloader, uNestedDownloader,
  uOptions;

type
  TNestedDirectDownloader = class(TNestedDownloader)
    private
      fForcedName: string;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean; override;
      function GetTransformedUrl(out Url: string): boolean; override;
      function GetFileName: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      constructor CreateWithName(const AMovieID, AMovieName: string); virtual;
      destructor Destroy; override;
    end;

implementation

uses
  uMessages;
  
{ TNestedDirectDownloader }

class function TNestedDirectDownloader.Provider: string;
begin
  Result := 'Nested direct download';
end;

class function TNestedDirectDownloader.UrlRegExp: string;
begin
  Raise EDownloaderError.Create('TNestedDirectDownloader.UrlRegExp is not supported.');
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
end;

constructor TNestedDirectDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

constructor TNestedDirectDownloader.CreateWithName(const AMovieID, AMovieName: string);
begin
  Create(AMovieID);
  fForcedName := AMovieName;
end;

destructor TNestedDirectDownloader.Destroy;
begin
  inherited;
end;

function TNestedDirectDownloader.GetMovieInfoUrl: string;
begin
  Result := '.';
end;

function TNestedDirectDownloader.GetTransformedUrl(out Url: string): boolean;
begin
  Url := MovieID;
  Result := True;
end;

function TNestedDirectDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; out Xml: TXmlDoc; Method: THttpMethod): boolean;
begin
  Page := '';
  Xml := nil;
  Result := True;
end;

function TNestedDirectDownloader.GetFileName: string;
begin
  if fForcedName <> '' then
    Result := GetThisFileName
  else
    Result := inherited GetFileName;
end;

function TNestedDirectDownloader.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
begin
  Result := inherited AfterPrepareFromPage(Page, PageXml, Http);
  if fForcedName <> '' then
    Name := fForcedName;
end;

end.
