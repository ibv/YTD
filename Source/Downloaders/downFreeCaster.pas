unit downFreeCaster;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeCaster = class(THttpDownloader)
    private
    protected
      StreamIdRegExp: TRegExp;
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

// http://freecaster.tv/freeski/1012253
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*freecaster\.tv/';
  URLREGEXP_ID =        '[^/]+/[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_STREAM_ID = '\.addVariable\s*\(\s*"stream"\s*,\s*"(?P<ID>.*?)"';

{ TDownloader_FreeCaster }

class function TDownloader_FreeCaster.Provider: string;
begin
  Result := 'FreeCaster.tv';
end;

class function TDownloader_FreeCaster.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FreeCaster.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  StreamIdRegExp := RegExCreate(REGEXP_STREAM_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_FreeCaster.Destroy;
begin
  RegExFreeAndNil(StreamIdRegExp);
  inherited;
end;

function TDownloader_FreeCaster.GetMovieInfoUrl: string;
begin
  Result := 'http://freecaster.tv/' + MovieID;
end;

function TDownloader_FreeCaster.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var StreamID, InfoXml, MaxUrl, Url, UrlBase, Title, Bitrate: string;
    MaxBitRate, i: integer;
    Xml: TXmlDoc;
    Node: TXmlNode;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(StreamIdRegExp, Page, 'ID', StreamID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://freecaster.tv/player/info/' + StreamID, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not Xml.NodeByPath('streams', Node) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlAttr(Node, '', 'server', UrlBase) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Xml, 'video/title', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else
        begin
        MaxUrl := '';
        MaxBitRate := 0;
        for i := 0 to Pred(Node.NodeCount) do
          if Node.Nodes[i].Name = 'stream' then
            if GetXmlAttr(Node.Nodes[i], '', 'bitrate', Bitrate) then
              if StrToIntDef(Bitrate, 0) > MaxBitRate then
                begin
                Url := Trim(XmlValueIncludingCData(Node.Nodes[i]));
                if Url <> '' then
                  begin
                  MaxUrl := Url;
                  MaxBitRate := StrToInt(BitRate);
                  end;
                end;
        if MaxUrl = '' then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieUrl := UrlBase + MaxUrl;
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
  RegisterDownloader(TDownloader_FreeCaster);

end.
