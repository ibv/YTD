unit downBarrandovTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_BarrandovTV = class(TRtmpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetInfoPageEncoding: TPageEncoding; override;
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
  janXmlParser2;

{ TDownloader_BarrandovTV }

constructor TDownloader_BarrandovTV.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_BarrandovTV.Destroy;
begin
  inherited;
end;

class function TDownloader_BarrandovTV.Provider: string;
begin
  Result := 'Barrandov.tv';
end;

class function TDownloader_BarrandovTV.MovieIDParamName: string;
begin
  Result := 'BARRANDOV';
end;

class function TDownloader_BarrandovTV.UrlRegExp: string;
begin
  // http://www.barrandov.tv/54698-nikdy-nerikej-nikdy-upoutavka-epizoda-12
  Result := '^https?://(?:[a-z0-9-]+\.)?barrandov\.tv/(?P<' + MovieIDParamName + '>[0-9]+)';
end;

function TDownloader_BarrandovTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.barrandov.tv/special/videoplayerdata/' + MovieID;
end;

function TDownloader_BarrandovTV.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

function TDownloader_BarrandovTV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
    HostName: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.create;
  try
    Xml.xml := Page;
    Node := Xml.getChildByPath('videotitle');
    if Node <> nil then
      begin
      SetName(Node.text);
      Node := Xml.getChildByPath('hostname');
      if Node <> nil then
        begin
        HostName := Node.text;
        Node := Xml.getChildByPath('streamname');
        if Node <> nil then
          begin
          RtmpPlayPath := Node.text;
          RtmpUrl := 'rtmp://' + HostName + '/' + RtmpPlayPath;
          MovieUrl := RtmpUrl;
          Result := True;
          SetPrepared(True);
          end;
        end;
      end;
  finally
    Xml.Free;
    end;

end;

initialization
  RegisterDownloader(TDownloader_BarrandovTV);

end.
