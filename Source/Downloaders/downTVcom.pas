unit downTVcom;
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
    protected
      function GetFileNameExt: string; override;
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

// http://bojove-sporty.tvcom.cz/video/545-budo-show-zlin-2006-dil-1.htm
const
  URLREGEXP_BEFORE_ID = '^';
  URLREGEXP_ID =        'https?://(?:[a-z0-9-]+\.)*tvcom\.cz/video/.+';
  URLREGEXP_AFTER_ID =  '$';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';
  REGEXP_CONFIG_XML = '<param name="initParams" value="config=(?P<URL>https?://[^,"]+)';
  REGEXP_MMSURL = 'playlist\.asx\?video=(?P<URL>[^&]+)';

{ TDownloader_TVcom }

class function TDownloader_TVcom.Provider: string;
begin
  Result := 'TVcom.cz';
end;

class function TDownloader_TVcom.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TVcom.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  ConfigXmlRegExp := RegExCreate(REGEXP_CONFIG_XML, [rcoIgnoreCase]);
  MMSUrlRegExp := RegExCreate(REGEXP_MMSURL, [rcoIgnoreCase]);
end;

destructor TDownloader_TVcom.Destroy;
begin
  MovieTitleRegExp := nil;
  ConfigXmlRegExp := nil;
  MMSUrlRegExp := nil;
  inherited;
end;

function TDownloader_TVcom.GetFileNameExt: string;
begin
  Result := '.asf';
end;

function TDownloader_TVcom.GetMovieInfoUrl: string;
begin
  Result := MovieID;
end;

function TDownloader_TVcom.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, ConfigXml, VideoUrl: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(ConfigXmlRegExp, Page, 'URL', URL) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, URL, ConfigXml, peUTF8) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.xml := ConfigXml;
      if not GetXmlVar(Xml, 'Video', VideoUrl) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetRegExpVar(MMSUrlRegExp, VideoUrl, 'URL', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
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
