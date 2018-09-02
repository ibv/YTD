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

unit xxx8teenXXX;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_8teenXXX = class(THttpDownloader)
    private
    protected
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

const
  URLREGEXP_BEFORE_ID = '8teenxxx\.com/watch\.php\?tag=';
  URLREGEXP_ID =        REGEXP_PARAM_COMPONENT;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_META_DESCRIPTION;

{ TDownloader_8teenXXX }

class function TDownloader_8teenXXX.Provider: string;
begin
  Result := '8teenXXX.com';
end;

class function TDownloader_8teenXXX.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_8teenXXX.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peAnsi;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE);
end;

destructor TDownloader_8teenXXX.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_8teenXXX.GetMovieInfoUrl: string;
begin
  Result := 'http://www.8teenxxx.com/watch.php?tag=' + MovieID;
end;

function TDownloader_8teenXXX.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  Xml: TXmlDoc;
  Node: TXmlNode;
  i: integer;
  Url, BaseName, s: string;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  BaseName := UnpreparedName;
  if not DownloadXml(Http, 'http://www.8teenxxx.com/external_feed.php?tag=' + MovieID, Xml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    try
      if XmlNodeByPath(Xml, 'trackList', Node) then
        for i := 0 to Pred(Node.NodeCount) do
          if Node[i].Name = 'track' then
            if GetXmlVar(Node[i], 'location', Url) and (Url <> '') then
              if not (GetXmlVar(Node[i], 'album', s) and (s = 'overlay')) then
                begin
                if GetXmlVar(Node[i], 'title', s) and (s <> '') then
                  SetName(BaseName + ' ' + s)
                else
                  SetName(BaseName);
                MovieUrl := Url;
                SetPrepared(True);
                Result := True;
                {$IFDEF MULTIDOWNLOADS}
                NameList.Add(Name);
                UrlList.Add(MovieUrl);
                {$ELSE}
                Break;
                {$ENDIF}
                end;
    finally
      FreeAndNil(Xml);
      end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_8teenXXX);
  {$ENDIF}

end.
