unit downMarkiza;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Markiza = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: TRegExp;
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

// http://video.markiza.sk/archiv-tv-markiza/dnes/36829
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*markiza\.sk/archiv-tv-markiza/';
  URLREGEXP_ID =        '.+?';
  URLREGEXP_AFTER_ID =  '/?$';

const
  REGEXP_FILEINFO = '"(?P<URL>https?://(?:[a-z0-9-]+\.)?markiza\.sk/xml/video/parts\.rss\?ID_entity=[0-9]+&page=.+?)"';

{ TDownloader_Markiza }

class function TDownloader_Markiza.Provider: string;
begin
  Result := 'Markiza.sk';
end;

class function TDownloader_Markiza.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Markiza.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  FileInfoRegExp := RegExCreate(REGEXP_FILEINFO, [rcoIgnoreCase]);
end;

destructor TDownloader_Markiza.Destroy;
begin
  RegExFreeAndNil(FileInfoRegExp);
  inherited;
end;

function TDownloader_Markiza.GetMovieInfoUrl: string;
begin
  Result := 'http://video.markiza.sk/archiv-tv-markiza/' + MovieID;
end;

function TDownloader_Markiza.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, FileInfo, Description: string;
    Xml: TXmlDoc;
    Channel, ContentNode: TXmlNode;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FileInfoRegExp, Page, 'URL', URL) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, URL, FileInfo, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := FileInfo;
      if Xml.NodeByPath('channel', Channel) then
        for i := 0 to Pred(Channel.NodeCount) do
          if Channel.Nodes[i].Name = 'item' then
            if GetXmlVar(Channel.Nodes[i], 'description', Description) then
              if XmlNodeByPath(Channel.Nodes[i], 'media:content', ContentNode) then
                if ContentNode.AttributeByName['list'] <> 'false' then
                  if GetXmlAttr(ContentNode, '', 'url', Url) then
                    if Url <> '' then
                      begin
                      {$IFDEF MULTIDOWNLOADS}
                      NameList.Add(Description);
                      UrlList.Add(Url);
                      {$ELSE}
                      SetName(Description);
                      MovieURL := Url;
                      Result := True;
                      SetPrepared(True);
                      Exit;
                      {$ENDIF}
                      end;
      {$IFDEF MULTIDOWNLOADS}
      if UrlList.Count <= 0 then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetPrepared(True);
        Result := First;
        end;
      {$ELSE}
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL));
      {$ENDIF}
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Markiza);

end.
