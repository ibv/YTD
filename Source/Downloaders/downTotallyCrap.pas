unit downTotallyCrap;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_TotallyCrap = class(TRtmpDownloader)
    private
    protected
      InfoUrlRegExp: TRegExp;
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

// http://www.totallycrap.com/videos/videos_man_tries_to_commit_suicide_by_laying_under_a_truck/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*totallycrap\.com/videos/videos_';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';
  REGEXP_INFO_URL = '\bso\.addVariable\s*\(\s*''config''\s*,\s*''(?P<URL>https?://.+?)''';

{ TDownloader_TotallyCrap }

class function TDownloader_TotallyCrap.Provider: string;
begin
  Result := 'TotallyCrap.com';
end;

class function TDownloader_TotallyCrap.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TotallyCrap.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_TotallyCrap.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(InfoUrlRegExp);
  inherited;
end;

function TDownloader_TotallyCrap.GetMovieInfoUrl: string;
begin
  Result := 'http://www.totallycrap.com/videos/videos_' + MovieID + '/';
end;

function TDownloader_TotallyCrap.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, InfoXml, BasePath, PlayPath: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, Url, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'streamer', BasePath) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else if not GetXmlVar(Xml, 'file', PlayPath) then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else
        begin
        MovieUrl := BasePath + '/mp4:' + PlayPath;
        AddRtmpDumpOption('r', MovieURL);
        AddRtmpDumpOption('y', 'mp4:' + PlayPath);
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_TotallyCrap);

end.
