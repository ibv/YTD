unit downFlickr;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Flickr = class(THttpDownloader)
    private
    protected
      VideoSrcRegExp: IRegEx;
      PhotoSecretRegexp: IRegEx;
      PhotoIdRegExp: IRegEx;
      VideoExtensionRegExp: IRegEx;
      Extension: string;
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  janXmlParser2,
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
  SetInfoPageEncoding(peUTF8);
  VideoSrcRegExp := RegExCreate(REGEXP_VIDEO_SRC, [rcoIgnoreCase, rcoSingleLine]);
  PhotoSecretRegexp := RegExCreate(REGEXP_PHOTOSECRET, [rcoIgnoreCase, rcoSingleLine]);
  PhotoIdRegexp := RegExCreate(REGEXP_PHOTOID, [rcoIgnoreCase, rcoSingleLine]);
  VideoExtensionRegExp := RegExCreate(REGEXP_EXTENSION, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Flickr.Destroy;
begin
  VideoSrcRegExp := nil;
  PhotoSecretRegexp := nil;
  PhotoIdRegexp := nil;
  VideoExtensionRegExp := nil;
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

function TDownloader_Flickr.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Secret, ID, InfoXml, NodeID, s, Title, Host, Path: string;
    Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoSrcRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(PhotoSecretRegexp, Url, 'VALUE', Secret) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['photo_secret']))
  else if not GetRegExpVar(PhotoIdRegexp, Url, 'VALUE', ID) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['photo_id']))
  else if not DownloadPage(Http, 'http://www.flickr.com/apps/video/video_mtl_xml.gne?v=x&photo_id=' + ID + '&secret=' + Secret + '&olang=en-us&noBuffer=null&bitrate=700&target=_self', InfoXml, peUTF8) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;      
    try
      Xml.Xml := InfoXml;
      Node := Xml.getChildByPath('Data');
      NodeId := '';
      if Node <> nil then
        for i := 0 to Pred(Node.childCount) do
          if (Node.childNode[i].name = 'Item') and GetXmlAttr(Node.childNode[i], '', 'id', s) and (s = 'id') then
            begin
            NodeId := Trim(Node.childNode[i].text);
            Break;
            end;
      if NodeId = '' then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not DownloadPage(Http, 'http://www.flickr.com/video_playlist.gne?node_id=' + NodeID + '&tech=flash&mode=playlist&bitrate=700&secret=' + Secret + '&rd=video.yahoo.com&noad=1', InfoXml, peUTF8) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        begin
        Xml.Xml := InfoXml;
        if not GetXmlVar(Xml, 'SEQUENCE-ITEM/META/TITLE', Title) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
        else if not GetXmlAttr(Xml, 'SEQUENCE-ITEM/STREAM', 'APP', Host) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else if not GetXmlAttr(Xml, 'SEQUENCE-ITEM/STREAM', 'FULLPATH', Path) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          GetRegExpVar(VideoExtensionRegExp, Path, 'EXT', Extension);
          SetName(Title);
          MovieUrl := Host + Path;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Flickr);

end.
