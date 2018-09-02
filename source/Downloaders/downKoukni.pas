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

unit downKoukni;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Koukni = class(THttpDownloader)
    private
    protected
      MovieInfoRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uFunctions,
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://koukni.cz/95074707
const
  URLREGEXP_BEFORE_ID = 'koukni\.cz/';
  URLREGEXP_ID =        REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE =  REGEXP_TITLE_META_OGTITLE;
  REGEXP_MOVIE_INFO =   '(?P<INFO><video\b.*?</video>)';

{ TDownloader_Koukni }

class function TDownloader_Koukni.Provider: string;
begin
  Result := 'Koukni.cz';
end;

class function TDownloader_Koukni.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_Koukni.Features: TDownloaderFeatures;
begin
  Result := inherited Features
    {$IFDEF SUBTITLES}
    + [dfSubtitles]
    {$ENDIF}
    ;
end;

constructor TDownloader_Koukni.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  InfoPageEncoding := peAnsi;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
  MovieInfoRegExp := RegExCreate(REGEXP_MOVIE_INFO);
end;

destructor TDownloader_Koukni.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoRegExp);
  inherited;
end;

function TDownloader_Koukni.GetMovieInfoUrl: string;
begin
  Result := 'http://koukni.cz/' + MovieID;
end;

function TDownloader_Koukni.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  InfoXml: TXmlDoc;
  Node: TXmlNode;
  Info, Stream, SubsUrl: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoRegExp, Page, 'INFO', Info) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    InfoXml := TXmlDoc.Create;
    try
      InfoXml.LoadFromBinaryString( {$IFDEF UNICODE} AnsiString {$ENDIF} (Info));
      if not XmlNodeByPathAndAttr(InfoXml, 'source', 'type', 'video/mp4', Node) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
      else if not GetXmlAttr(Node, '', 'src', Stream) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_STREAM)
      else
        begin
        MovieUrl := GetRelativeUrl(GetMovieInfoUrl, Stream);
        {$IFDEF SUBTITLES}
        if GetXmlAttr(InfoXml, 'track', 'src', SubsUrl) then
          fSubtitleUrl := GetRelativeUrl(GetMovieInfoUrl, SubsUrl);
        {$ENDIF}
        SetPrepared(True);
        Result := True;
        end;
    finally
      FreeAndNil(InfoXml);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Koukni);

end.
