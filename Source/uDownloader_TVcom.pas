unit uDownloader_TVcom;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_TVcom = class(TMSDownloader)
    private
    protected
      ConfigXmlRegExp: IRegEx;
      MMSUrlRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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
  uStringUtils,
  janXmlParser2;

const MOVIE_TITLE_REGEXP = '<h2>(?P<TITLE>.*?)</h2>';
const CONFIG_XML_REGEXP = '<param name="initParams" value="config=(?P<URL>https?://[^,"]+)';
const MMSURL_REGEXP = 'playlist\.asx\?video=(?P<URL>[^&]+)';

{ TDownloader_TVcom }

constructor TDownloader_TVcom.Create(const AMovieID: string);
begin
  inherited;
  MovieTitleRegExp := RegExCreate(MOVIE_TITLE_REGEXP, [rcoIgnoreCase]);
  ConfigXmlRegExp := RegExCreate(CONFIG_XML_REGEXP, [rcoIgnoreCase]);
  MMSUrlRegExp := RegExCreate(MMSURL_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_TVcom.Destroy;
begin
  MovieTitleRegExp := nil;
  ConfigXmlRegExp := nil;
  MMSUrlRegExp := nil;
  inherited;
end;

class function TDownloader_TVcom.Provider: string;
begin
  Result := 'TVcom.cz';
end;

class function TDownloader_TVcom.MovieIDParamName: string;
begin
  Result := 'TVCOM';
end;

class function TDownloader_TVcom.UrlRegExp: string;
begin
  // http://bojove-sporty.tvcom.cz/video/545-budo-show-zlin-2006-dil-1.htm
  Result := '^(?P<' + MovieIDParamName + '>https?://(?:[a-z0-9-]+\.)?tvcom\.cz/video/.+)$';
end;

function TDownloader_TVcom.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_TVcom.GetFileNameExt: string;
begin
  Result := '.asf';
end;

function TDownloader_TVcom.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUtf8;
end;

function TDownloader_TVcom.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, ConfigXml: string;
    Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(ConfigXmlRegExp, Page, 'URL', URL) then
    SetLastErrorMsg('Failed to find ConfigXML URL.')
  else if not DownloadPage(Http, URL, ConfigXml) then
    SetLastErrorMsg('Failed to download ConfigXML.')
  else
    begin
    ConfigXml := WideToAnsi(Utf8ToWide(ConfigXml));
    Xml := TjanXmlParser2.Create;
    try
      Xml.xml := ConfigXml;
      Node := Xml.GetChildByPath('Video');
      if Node = nil then
        SetLastErrorMsg('Failed to find video URL.')
      else if not GetRegExpVar(MMSUrlRegExp, Node.text, 'URL', Url) then
        SetLastErrorMsg('Failed to find video URL (2).')
      else
        begin
        MovieURL := URL;
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TVcom);

end.
