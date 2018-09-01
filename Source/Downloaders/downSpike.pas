unit downSpike;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Spike = class(THttpDownloader)
    private
    protected
      ConfigUrlRegExp: IRegEx;
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

// http://www.spike.com/video/prince-of-persia/3355664
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*spike\.com/video/[^/]+/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_CONFIG_URL = '<param\s+name="flashVars"\s+value="CONFIG_URL=(?P<URL>/[^"]+)"';

{ TDownloader_Spike }

class function TDownloader_Spike.Provider: string;
begin
  Result := 'Spike.com';
end;

class function TDownloader_Spike.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Spike.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  ConfigUrlRegExp := RegExCreate(REGEXP_CONFIG_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Spike.Destroy;
begin
  ConfigUrlRegExp := nil;
  inherited;
end;

function TDownloader_Spike.GetMovieInfoUrl: string;
begin
  Result := 'http://www.spike.com/video/dummy/' + MovieID;
end;

function TDownloader_Spike.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
    Url, InfoXml, Title, BitrateStr, BestUrl: string;
    i, Bitrate, BestBitrate: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(ConfigUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.spike.com' + UrlDecode(Url), InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'gui/share/embed/title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else if not DownloadPage(Http, 'http://www.spike.com/ui/xml/mediaplayer/mediagen.groovy?videoId=' + MovieID + '&royaltyReport=true&duration=152&width=640&height=391&impressiontype=18', InfoXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        begin
        Xml.Xml := InfoXml;
        BestUrl := '';
        BestBitrate := 0;
        Node := Xml.getChildByPath('video/item');
        if Node <> nil then
          for i := 0 to Pred(Node.childCount) do
            if (Node.childNode[i].name = 'rendition') and GetXmlAttr(Node.childNode[i], '', 'bitrate', BitrateStr) then
              begin
              Bitrate := StrToIntDef(BitrateStr, 0);
              if Bitrate > BestBitrate then
                if GetXmlVar(Node.childNode[i], 'src', Url) then
                  begin
                  BestUrl := Url;
                  BestBitrate := Bitrate;
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
  RegisterDownloader(TDownloader_Spike);

end.
