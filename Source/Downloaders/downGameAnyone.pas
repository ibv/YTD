unit downGameAnyone;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downYouTube;

type
  TDownloader_GameAnyone = class(TNestedDownloader)
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
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.gameanyone.com/video/88319
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*gameanyone\.com/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<div\s+style="font-weight:bold;padding-left:25px;width:910px;text-align:left;font-size:15px;padding-bottom:3px;float:left;">\s*(?P<TITLE>.*?)\s*</div>';
  REGEXP_EXTRACT_YOUTUBE_ID = '\sflashvars="(?:[^"]*&amp;)?file=(?P<URL>https?://(?:[a-z0-9-]+\.)*youtube\.com/watch\?v=(?P<ID>[^"]+?))&amp;';

{ TDownloader_GameAnyone }

class function TDownloader_GameAnyone.Provider: string;
begin
  Result := 'GameAnyone.com';
end;

class function TDownloader_GameAnyone.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_GameAnyone.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedIDRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE_ID, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_YOUTUBE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_GameAnyone.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedIDRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  inherited;
end;

function TDownloader_GameAnyone.GetMovieInfoUrl: string;
begin
  Result := 'http://www.gameanyone.com/video/' + MovieID;
end;

function TDownloader_GameAnyone.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Info, Url: string;
begin
  Result := False;
  if DownloadPage(Http, 'http://www.gameanyone.com/gameanyone.xml?id=' + MovieID, Info, peXml) then
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := Info;
      if GetXmlVar(Xml, 'file', Url) then
        if CreateNestedDownloaderFromURL(Url) then
          begin
          SetPrepared(True);
          Result := True;
          end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_GameAnyone);

end.
