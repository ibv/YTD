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

unit downCT;
{$INCLUDE 'ytd.inc'}
{.DEFINE PREFER_REALMEDIA}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_CT = class(TMSDownloader)
    private
      fRealMedia: boolean;
    protected
      MovieObjectRegExp: TRegExp;
      IVysilaniUrlRegExp: TRegExp;
    protected
      function GetFileNameExt: string; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean; virtual;
      procedure SetOptions(const Value: TYTDOptions); override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      property RealMedia: boolean read fRealMedia write fRealMedia;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.ceskatelevize.cz/ivysilani/309292320520025-den-d-ii-rada/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ceskatelevize\.cz/ivysilani/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_MOVIE_OBJECT = '<object\s+id="(?:programmeObject|WMP)"(?:\s+data|.*?<param\s+name="(?:url|src)"\s+value)="(?P<OBJURL>[^"]+)"';
  //REGEXP_IVYSILANI_URL = '^(?P<URL>rtsp://[^/]+/iVysilani\.hash\?.*)$';
  REGEXP_IVYSILANI_URL = '(?P<URL>(?:https?|rtsp)://[^/]+/iVysilani\.(?:hash\?|archive).*)';

{ TDownloader_CT }

class function TDownloader_CT.Provider: string;
begin
  Result := 'CeskaTelevize.cz';
end;

class function TDownloader_CT.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieObjectRegExp := RegExCreate(REGEXP_MOVIE_OBJECT, [rcoIgnoreCase, rcoSingleLine]);
  IVysilaniUrlRegExp := RegExCreate(REGEXP_IVYSILANI_URL, [rcoIgnoreCase]);
  RealMedia := {$IFDEF PREFER_REALMEDIA} True {$ELSE} False {$ENDIF};
end;

destructor TDownloader_CT.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieObjectRegExp);
  RegExFreeAndNil(IVysilaniUrlRegExp);
  inherited;
end;

procedure TDownloader_CT.SetOptions(const Value: TYTDOptions);
var s: string;
begin
  inherited;
  if Value.ReadProviderOption(Provider, 'prefer_real_media', s) then
    RealMedia := StrToIntDef(s, Integer(RealMedia)) <> 0;
end;

function TDownloader_CT.GetFileNameExt: string;
begin
  if RealMedia then
    Result := '.rm'
  else
    Result := '.asf';
end;

function TDownloader_CT.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ceskatelevize.cz/ivysilani/' + MovieID + '/?streamtype=';
  if RealMedia then
    Result := Result + 'RL3'
  else
    Result := Result + 'WM3';
end;

function TDownloader_CT.GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean;
begin
  Result := GetRegExpVar(MovieObjectRegExp, Page, 'OBJURL', Url);
end;

function TDownloader_CT.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const REKLAMA = 'Reklama:';
      REKLAMA_LENGTH = Length(REKLAMA);
var HREF, URL, ObjectDef, Title: string;
    Xml: TXmlDoc;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetMovieObjectUrl(Http, Page, Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, URL, ObjectDef, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    // Jsou dve varianty. Pro ASF stream prijde XML, pro RM stream textak
    ObjectDef := Trim(ObjectDef);
    if ObjectDef = '' then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else if ObjectDef[1] = '<' then
      begin
      Xml := TXmlDoc.Create;
      try
        Xml.xml := ObjectDef;
        for i := 0 to Pred(Xml.Root.NodeCount) do
          if Xml.Root.Nodes[i].Name = 'ENTRY' then
            if GetXmlAttr(Xml.Root.Nodes[i], 'REF', 'HREF', HREF) then
              if GetXmlVar(Xml.Root.Nodes[i], 'TITLE', Title) then
                if AnsiCompareText(REKLAMA, Copy(Title, 1, REKLAMA_LENGTH)) <> 0 then
                  //if GetRegExpVar(IVysilaniUrlRegExp, HREF, 'URL', Url) then
                    begin
                    //if GetXmlVar(Xml.childNode[i], 'TITLE', Title) then
                    //  SetName(Title);
                    MovieUrl := HREF;
                    Result := True;
                    SetPrepared(True);
                    Exit;
                    end;
      finally
        Xml.Free;
        end;
      end
    else if not GetRegExpVar(IVysilaniUrlRegExp, ObjectDef, 'URL', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      MovieURL := Url;
      Result := True;
      SetPrepared(True);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CT);

end.
