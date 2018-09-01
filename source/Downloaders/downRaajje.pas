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

unit downRaajje;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Raajje = class(THttpDownloader)
    private
    protected
      MovieFileNameRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.raajje.tv/video/991/president-anni-diving
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*Raajje\.tv/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+id="viewvideo-title">\s*(?P<TITLE>.*?)\s*</div>';
  REGEXP_MOVIE_FILENAME = '<param\s+name="movie"\s+value="[^"\s>]*[?&]video=(?P<FILENAME>[^\s&>]+)';

{ TDownloader_PornoTube }

class function TDownloader_Raajje.Provider: string;
begin
  Result := 'Raajje.com';
end;

class function TDownloader_Raajje.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Raajje.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieFileNameRegExp := RegExCreate(REGEXP_MOVIE_FILENAME, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Raajje.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieFileNameRegExp);
  inherited;
end;

function TDownloader_Raajje.GetMovieInfoUrl: string;
begin
  Result := 'http://www.raajje.tv/video/' + MovieID + '/';
end;

function TDownloader_Raajje.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    FileName, Path: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieFileNameRegExp, Page, 'FILENAME', FileName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Filename']))
  else if not DownloadXml(Http, 'http://www.raajje.tv/csplayer.config.php', Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not GetXmlVar(Xml, 'VIDEOS_PATH', Path) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Videos Path']))
      else if Path = '' then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := Path + FileName;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_Raajje);
  {$ENDIF}

end.
