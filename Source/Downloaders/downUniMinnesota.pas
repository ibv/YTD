unit downUniMinnesota;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_UniMinnesota = class(TRtmpDownloader)
    private
    protected
      InstanceIdRegExp: IRegEx;
      VideoIdRegExp: IRegEx;
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

// http://www.ima.umn.edu/videos/?id=1187
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ima\.umn\.edu/videos/.*?[?&]id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+style=''color:#fff;''>\s*<b>(?P<TITLE>.*?)</b></div>';
  REGEXP_INSTANCE_ID = '<param\s+name="movie"\s+value="[^"]*[?&]instance=(?P<ID>[^"&]+)';
  REGEXP_VIDEO_ID = '<param\s+name="movie"\s+value="[^"]*[?&]video=(?P<ID>[^"&]+)';

{ TDownloader_UniMinnesota }

class function TDownloader_UniMinnesota.Provider: string;
begin
  Result := 'ima.umn.edu';
end;

class function TDownloader_UniMinnesota.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_UniMinnesota.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  VideoIdRegExp := RegExCreate(REGEXP_VIDEO_ID, [rcoIgnoreCase, rcoSingleLine]);
  InstanceIdRegExp := RegExCreate(REGEXP_INSTANCE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_UniMinnesota.Destroy;
begin
  MovieTitleRegExp := nil;
  VideoIdRegExp := nil;
  InstanceIdRegExp := nil;
  inherited;
end;

function TDownloader_UniMinnesota.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ima.umn.edu/videos/?id=' + MovieID;
end;

function TDownloader_UniMinnesota.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var VideoID, InstanceID, InfoXml, BaseUrl, Path: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(VideoIdRegExp, Page, 'ID', VideoID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not GetRegExpVar(InstanceIdRegExp, Page, 'ID', InstanceID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.ima.umn.edu/videos/xml/' + InstanceID + '/' + VideoID + '.xml', InfoXml, peUTF8) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlAttr(Xml, 'head/meta', 'base', BaseUrl) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlAttr(Xml, 'body/video', 'src', Path) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := BaseUrl + Path;
        AddRtmpDumpOption('r', MovieURL);
        Result := True;
        SetPrepared(True);
        Exit;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_UniMinnesota);

end.
