(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit downWordPressTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_WordPressTV = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: TRegExp;
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
  uAMF,
  uDownloadClassifier,
  uMessages;

// http://wordpress.tv/2010/09/18/jane-wells-how-wordpress-decisions-get-made/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*wordpress\.tv/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+name="og:title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_MOVIE_ID = '<meta\s+name="og:video"\s+content="[^"]*\bguid=(?P<ID>[0-9a-z]+)"';

const
  AMF_REQUEST_PACKET =
    'AAMAAAABABJtYW5pZmVzdC5mcm9tX2d1aWQAAi8xAAAAGQoAAAACAgAIZHpxb1JZdjYAQILo' +
    'AAAAAAA=';

{ TDownloader_WordPressTV }

class function TDownloader_WordPressTV.Provider: string;
begin
  Result := 'WordPress.tv';
end;

class function TDownloader_WordPressTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_WordPressTV.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieIDRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase]);
end;

destructor TDownloader_WordPressTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIDRegExp);
  inherited;
end;

function TDownloader_WordPressTV.GetMovieInfoUrl: string;
begin
  Result := 'http://wordpress.tv/' + MovieID;
end;

function TDownloader_WordPressTV.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var AMFRequest, AMFResponse: TAMFPacket;
    ID, BestUrl: string;
    Title, Url, sWidth, sHeight, sBitrate: TAMFValue;
    MediaList: TAMFCommonArray;
    i: integer;
    BestResolution, BestBitrate, Resolution, Bitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else
    begin
    AMFRequest := TAMFPacket.Create;
    try
      AMFRequest.LoadFromString(AnsiString(Base64Decode(AMF_REQUEST_PACKET)));
      // Note: I don't need to check types (or make sure pointers are not null)
      // because I use a pre-made packet which has all required properties. That
      // is not true while parsing response packets!
      TAMFCommonArray(AMFRequest.Body[0].Content).Items[0].Value := ID;
      if not DownloadAMF(Http, 'http://videopress.com/data/gateway.amf', AMFRequest, AMFResponse) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        try
          if AMFResponse.HasBody(0) then
            begin
            if AMFResponse.Body[0].Content.FindValueByPath('mediaTitle', Title, TAMFString) then
              SetName(Title);
            MediaList := TAMFCommonArray(AMFResponse.Body[0].Content.FindByPath('mediaElement/media', TAMFCommonArray));
            BestUrl := '';
            BestResolution := -1;
            BestBitrate := -1;
            if MediaList <> nil then
              for i := 0 to Pred(MediaList.Count) do
                if MediaList.Items[i].FindValueByPath('url', Url) then
                  begin
                  if MediaList.Items[i].FindValueByPath('width', sWidth, TAMFNumber) and MediaList.Items[i].FindValueByPath('height', sHeight, TAMFNumber) then
                    Resolution := StrToIntDef(sWidth, 0) * StrToIntDef(sHeight, 0)
                  else
                    Resolution := 0;
                  if MediaList.Items[i].FindValueByPath('bitrate', sBitrate, TAMFNumber) then
                    Bitrate := StrToIntDef(sBitrate, 0)
                  else
                    Bitrate := 0;
                  if (Resolution > BestResolution) or ((Resolution = BestResolution) and (Bitrate > BestBitrate)) then
                    begin
                    BestResolution := Resolution;
                    BestBitrate := Bitrate;
                    BestUrl := Url;
                    end;
                  end;
            if BestUrl <> '' then
              begin
              MovieUrl := BestUrl;
              SetPrepared(True);
              Result := True;
              end;
            end;
        finally
          AMFResponse.Free;
          end;
    finally
      AMFRequest.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_WordPressTV);

end.
