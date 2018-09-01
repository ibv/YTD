unit downCrunchyRoll;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_CrunchyRoll = class(TRtmpDownloader)
    private
      fFileNameExt: string;
    protected
      InfoUrlRegExp: IRegEx;
      property FileNameExt: string read fFileNameExt write fFileNameExt; 
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://www.crunchyroll.com/library/Naruto_Shippuden
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*crunchyroll\.com/library/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<meta\s+property="og:title"\s+content="(?P<TITLE>.*?)"';
  REGEXP_INFO_URL = '"config_url"\s*:\s*"(?P<URL>.*?)"';

{ TDownloader_CrunchyRoll }

class function TDownloader_CrunchyRoll.Provider: string;
begin
  Result := 'CrunchyRoll.com';
end;

class function TDownloader_CrunchyRoll.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CrunchyRoll.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_CrunchyRoll.Destroy;
begin
  MovieTitleRegExp := nil;
  InfoUrlRegExp := nil;
  inherited;
end;

function TDownloader_CrunchyRoll.GetMovieInfoUrl: string;
begin
  Result := 'http://www.crunchyroll.com/library/' + MovieID;
end;

function TDownloader_CrunchyRoll.GetFileNameExt: string;
begin
  Result := FileNameExt;
end;

function TDownloader_CrunchyRoll.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var InfoXml, Url, FlvHost, FlvStream: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, UrlDecode(Url), InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'default:preload/stream_info/host', FlvHost) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else if not GetXmlVar(Xml, 'default:preload/stream_info/file', FlvStream) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieURL := FlvHost;
        AddRtmpDumpOption('r', FlvHost);
        AddRtmpDumpOption('y', FlvStream);
        FileNameExt := ExtractFileExt(FlvStream);
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CrunchyRoll);

end.
