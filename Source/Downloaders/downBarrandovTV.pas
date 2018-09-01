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
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  janXmlParser2;

// http://www.barrandov.tv/54698-nikdy-nerikej-nikdy-upoutavka-epizoda-12
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*barrandov\.tv/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_BarrandovTV }

class function TDownloader_BarrandovTV.Provider: string;
begin
  Result := 'Barrandov.tv';
end;

class function TDownloader_BarrandovTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BarrandovTV.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_BarrandovTV.Destroy;
begin
  inherited;
end;

function TDownloader_BarrandovTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.barrandov.tv/special/videoplayerdata/' + MovieID;
end;

function TDownloader_BarrandovTV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Title, HostName, StreamName: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.create;
  try
    Xml.xml := Page;
    if not GetXmlVar(Xml, 'videotitle', Title) then
      SetLastErrorMsg('Failed to find video title.')
    else if not GetXmlVar(Xml, 'hostname', HostName) then
      SetLastErrorMsg('Failed to find host name.')
    else if not GetXmlVar(Xml, 'streamname', StreamName) then
      SetLastErrorMsg('Failed to find stream name.')
    else
      begin
      SetName(Title);
      MovieUrl := 'rtmp://' + HostName + '/' + StreamName;
      AddRtmpDumpOption('r', MovieURL);
      AddRtmpDumpOption('y', StreamName);
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BarrandovTV);

end.
