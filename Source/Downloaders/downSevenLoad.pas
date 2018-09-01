unit downSevenLoad;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SevenLoad = class(THttpDownloader)
    private
    protected
      ConfigUrlRegExp: TRegExp;
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

// http://en.sevenload.com/shows/Food-Drink/episodes/4YXLHBt-How-To-Make-Elegant-No-Bake-Cheesecake
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*sevenload\.com/shows/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG_URL = '<param\s+name="flashVars"\s+value="(?:[^"&]+&)*configPath=(?P<URL>https?[^"]+?)(?:&amp;|")';

{ TDownloader_SevenLoad }

class function TDownloader_SevenLoad.Provider: string;
begin
  Result := 'SevenLoad.com';
end;

class function TDownloader_SevenLoad.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_SevenLoad.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  ConfigUrlRegExp := RegExCreate(REGEXP_CONFIG_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_SevenLoad.Destroy;
begin
  RegExFreeAndNil(ConfigUrlRegExp);
  inherited;
end;

function TDownloader_SevenLoad.GetMovieInfoUrl: string;
begin
  Result := 'http://en.sevenload.com/shows/' + MovieID;
end;

function TDownloader_SevenLoad.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, InfoXml, Title, BestUrl, StreamWidth, StreamHeight: string;
    i, BestQuality, Quality: integer;
    Xml: TjanXmlParser2;
    Node, Streams: TjanXmlNode2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(ConfigUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, UrlDecode(Url), InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      Node := Xml.getChildByPath('playlists/playlist/items/item');
      if Node = nil then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlVar(Node, 'title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else
        begin
        BestUrl := '';
        BestQuality := 0;
        Streams := Node.getChildByPath('videos/video/streams');
        if Streams <> nil then
          for i := 0 to Pred(Streams.childCount) do
            if Streams.childNode[i].name = 'stream' then
              if (GetXmlAttr(Streams.childNode[i], '', 'width', StreamWidth) and GetXmlAttr(Streams.childNode[i], '', 'height', StreamHeight)) or (BestQuality = 0) then
                begin
                Quality := StrToIntDef(StreamWidth, 0) * StrToIntDef(StreamHeight, 0);
                if Quality >= BestQuality then
                  if GetXmlVar(Streams.childNode[i], 'locations/location', Url) then
                    begin
                    BestQuality := Quality;
                    BestUrl := Url;
                    end;
                end;
        if BestUrl = '' then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieUrl := BestUrl;
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
  RegisterDownloader(TDownloader_SevenLoad);

end.
