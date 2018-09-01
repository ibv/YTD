unit downMarkiza;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Markiza = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: IRegEx;
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
  janXmlParser2,
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
  FileInfoRegExp := nil;
  inherited;
end;

function TDownloader_Markiza.GetMovieInfoUrl: string;
begin
  Result := 'http://video.markiza.sk/archiv-tv-markiza/' + MovieID;
end;

function TDownloader_Markiza.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, FileInfo, Description: string;
    Xml: TjanXmlParser2;
    Channel, ContentNode: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FileInfoRegExp, Page, 'URL', URL) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT)
  else if not DownloadPage(Http, URL, FileInfo, peUTF8) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_EMBEDDED_OBJECT)
  else
    begin
    // TjanXmlParser2 requires spaces within attributes, not whitespace :-(
    for i := 1 to Length(FileInfo) do
      if FileInfo[i] in [#0, #10, #13, #9] then
        FileInfo[i] := ' ';
    Xml := TjanXmlParser2.Create;
    try
      Xml.xml := FileInfo;
      Channel := Xml.GetChildByPath('channel');
      if Channel <> nil then
        for i := 0 to Pred(Channel.childCount) do
          if Channel.childNode[i].name = 'item' then
            if GetXmlVar(channel.childNode[i], 'description', Description) then
              begin
              ContentNode := Channel.childNode[i].GetChildByPath('media:content');
              if (ContentNode <> nil) and (ContentNode.attribute['list'] <> 'false') then
                begin
                Url := ContentNode.attribute['url'];
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
                end;
              end;
      {$IFDEF MULTIDOWNLOADS}
      if UrlList.Count <= 0 then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
      else
        begin
        SetPrepared(True);
        Result := First;
        end;
      {$ELSE}
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL);
      {$ENDIF}
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Markiza);

end.
