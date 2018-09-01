unit uDownloader_Markiza;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Markiza = class(THttpDownloader)
    private
    protected
      FileInfoRegExp: IRegEx;
      function GetInfoPageEncoding: TPageEncoding; override;
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uStringUtils,
  janXmlParser2;

const FILEINFO_REGEXP = '"(?P<URL>https?://(?:[a-z0-9-]+\.)?markiza\.sk/xml/video/parts\.rss\?ID_entity=[0-9]+&page=.+?)"';

{ TDownloader_Markiza }

constructor TDownloader_Markiza.Create(const AMovieID: string);
begin
  inherited;
  FileInfoRegExp := RegExCreate(FILEINFO_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_Markiza.Destroy;
begin
  FileInfoRegExp := nil;
  inherited;
end;

class function TDownloader_Markiza.Provider: string;
begin
  Result := 'Markiza.sk';
end;

class function TDownloader_Markiza.MovieIDParamName: string;
begin
  Result := 'MARKIZA';
end;

class function TDownloader_Markiza.UrlRegExp: string;
begin
  // http://video.markiza.sk/archiv-tv-markiza/dnes/36829
  Result := '^https?://(?:[a-z0-9-]+\.)?markiza\.sk/archiv-tv-markiza/(?P<' + MovieIDParamName + '>.+?)/?$';
end;

function TDownloader_Markiza.GetMovieInfoUrl: string;
begin
  Result := 'http://video.markiza.sk/archiv-tv-markiza/' + MovieID;
end;

function TDownloader_Markiza.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

function TDownloader_Markiza.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, FileInfo, Description: string;
    Xml: TjanXmlParser2;
    Channel, NameNode, ContentNode: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FileInfoRegExp, Page, 'URL', URL) then
    SetLastErrorMsg('Failed to find FileInfo URL.')
  else if not DownloadPage(Http, URL, FileInfo) then
    SetLastErrorMsg('Failed to download FileInfo.')
  else
    begin
    FileInfo := WideToAnsi(Utf8ToWide(FileInfo));
    // TjanXmlParser requires spaces within attributes, not whitespace :-(
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
            begin
            NameNode := Channel.childNode[i].GetChildByPath('description');
            ContentNode := Channel.childNode[i].GetChildByPath('media:content');
            if (NameNode <> nil) and (ContentNode <> nil) and (ContentNode.attribute['list'] <> 'false') then
              begin
              Description := NameNode.text;
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
        SetLastErrorMsg('Failed to locate useful stream.')
      else
        begin
        SetPrepared(True);
        Result := First;
        end;
      {$ELSE}
      SetLastErrorMsg('Failed to locate useful stream.');
      {$ENDIF}
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Markiza);

end.
