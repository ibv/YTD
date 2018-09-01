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

unit downFlickr;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Flickr = class(THttpDownloader)
    private
    protected
      VideoSrcRegExp: TRegExp;
      PhotoSecretRegexp: TRegExp;
      PhotoIdRegExp: TRegExp;
      VideoExtensionRegExp: TRegExp;
      Extension: string;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.flickr.com/photos/landersz/2403626167/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*flickr\.com/photos/';
  URLREGEXP_ID =        '[^/]+/[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_VIDEO_SRC = '<link\s+rel="video_src"\s+href="(?P<URL>https?://.*?)"';
  REGEXP_PHOTOSECRET = '[?&]photo_secret=(?P<VALUE>[a-z0-9]+)';
  REGEXP_PHOTOID = '[?&]photo_id=(?P<VALUE>[0-9]+)';
  REGEXP_EXTENSION = '&m=video%2F(?P<EXT>.+?)&';

{ TDownloader_Flickr }

class function TDownloader_Flickr.Provider: string;
begin
  Result := 'Flickr.com';
end;

class function TDownloader_Flickr.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Flickr.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  VideoSrcRegExp := RegExCreate(REGEXP_VIDEO_SRC, [rcoIgnoreCase, rcoSingleLine]);
  PhotoSecretRegexp := RegExCreate(REGEXP_PHOTOSECRET, [rcoIgnoreCase, rcoSingleLine]);
  PhotoIdRegexp := RegExCreate(REGEXP_PHOTOID, [rcoIgnoreCase, rcoSingleLine]);
  VideoExtensionRegExp := RegExCreate(REGEXP_EXTENSION, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Flickr.Destroy;
begin
  RegExFreeAndNil(VideoSrcRegExp);
  RegExFreeAndNil(PhotoSecretRegexp);
  RegExFreeAndNil(PhotoIdRegexp);
  RegExFreeAndNil(VideoExtensionRegExp);
  inherited;
end;

function TDownloader_Flickr.GetMovieInfoUrl: string;
begin
  Result := 'http://www.flickr.com/photos/' + MovieID + '/';
end;

function TDownloader_Flickr.GetFileNameExt: string;
begin
  if Extension = '' then
    Result := inherited GetFileNameExt
  else if Extension[1] = '.' then
    Result := Extension
  else
    Result := '.' + Extension;
end;

function TDownloader_Flickr.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var Url, Secret, ID, NodeID, Title, Host, Path: string;
    Xml, ItemXml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(VideoSrcRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(PhotoSecretRegexp, Url, 'VALUE', Secret) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['photo_secret']))
  else if not GetRegExpVar(PhotoIdRegexp, Url, 'VALUE', ID) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['photo_id']))
  else if not DownloadXml(Http, 'http://www.flickr.com/apps/video/video_mtl_xml.gne?v=x&photo_id=' + ID + '&secret=' + Secret + '&olang=en-us&noBuffer=null&bitrate=700&target=_self', Xml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    try
      if not Xml.NodeByPathAndAttr('Data/Item', 'id', 'id', Node) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlVar(Node, '', NodeId) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if NodeId = '' then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not DownloadXml(Http, 'http://www.flickr.com/video_playlist.gne?node_id=' + NodeID + '&tech=flash&mode=playlist&bitrate=700&secret=' + Secret + '&rd=video.yahoo.com&noad=1', ItemXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        try
          if not GetXmlVar(ItemXml, 'SEQUENCE-ITEM/META/TITLE', Title) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
          else if not GetXmlAttr(ItemXml, 'SEQUENCE-ITEM/STREAM', 'APP', Host) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
          else if not GetXmlAttr(ItemXml, 'SEQUENCE-ITEM/STREAM', 'FULLPATH', Path) then
            SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
          else
            begin
            GetRegExpVar(VideoExtensionRegExp, Path, 'EXT', Extension);
            SetName(Title);
            MovieUrl := Host + Path;
            SetPrepared(True);
            Result := True;
            end;
        finally
          ItemXml.Free;
          end;
    finally
      Xml.Free;
      end;
end;

initialization
  RegisterDownloader(TDownloader_Flickr);

end.
