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

unit downTVVysocina;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_TVVysocina = class(TMSDownloader)
    private
    protected
      MoviePathRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      {$IFDEF DIRTYHACKS}
      function Download: boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.tv-vysocina.cz/index.php?page=2&datum=2011-12-16%2018:10:00&vselect1=4927&jednotlive=1
const
  URLREGEXP_BEFORE_ID = 'tv-vysocina\.cz/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_PATH = '<embed\b[^>]*\s+src="(?P<PATH>.+?\.asx)"\s+name="MediaPlayer"';

const
  URL_ROOT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://www.tv-vysocina.cz/';

{ TDownloader_TVVysocina }

class function TDownloader_TVVysocina.Provider: string;
begin
  Result := 'TV-Vysocina.cz';
end;

class function TDownloader_TVVysocina.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_TVVysocina.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUtf8;
  MoviePathRegExp := RegExCreate(REGEXP_MOVIE_PATH);
end;

destructor TDownloader_TVVysocina.Destroy;
begin
  RegExFreeAndNil(MoviePathRegExp);
  inherited;
end;

function TDownloader_TVVysocina.GetMovieInfoUrl: string;
begin
  Result := URL_ROOT + MovieID;
end;

function TDownloader_TVVysocina.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Path: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MoviePathRegExp, Page, 'PATH', Path) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
  else
    begin
    MovieUrl := URL_ROOT + Path;
    Name := ChangeFileExt(ExtractUrlFileName(MovieUrl), '');
    SetPrepared(True);
    Result := True;
    Exit;
    end;
end;

{$IFDEF DIRTYHACKS}
function TDownloader_TVVysocina.Download: boolean;
var
  Http: THttpSend;
  InfoXml: TXmlDoc;
  Href: string;
begin
  Result := inherited Download;
  if not Result then
    begin
    Http := CreateHttp;
    try
      if DownloadXml(Http, MovieUrl, InfoXml) then
        try
          if GetXmlAttr(InfoXml, 'ENTRY/REF', 'HREF', Href) then
            if Href <> '' then
              if Href[1] = '.' then
                begin
                MovieUrl := ExtractUrlPath(MovieUrl) + Href;
                Result := inherited Download;
                end;
        finally
          FreeAndNil(InfoXml);
          end;
    finally
      FreeAndNil(Http);
      end;
    end;
end;
{$ENDIF}

initialization
  RegisterDownloader(TDownloader_TVVysocina);

end.
