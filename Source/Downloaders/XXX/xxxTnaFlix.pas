unit xxxTnaFlix;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_TnaFlix = class(THttpDownloader)
    private
    protected
      MovieInfoUrlRegExp: TRegExp;
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
  uXML,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*tnaflix\.com/view_video\.php\?(?:[^&]*&)*viewkey=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2[^>]*>(?P<TITLE>.*?)</h2>';
  REGEXP_MOVIE_INFOURL = '\bso\.addVariable\s*\(\s*''config''\s*,\s*''(?P<URL>http.+?)''';

{ TDownloader_TnaFlix }

class function TDownloader_TnaFlix.Provider: string;
begin
  Result := 'TnaFlix.com';
end;

class function TDownloader_TnaFlix.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_TnaFlix.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUtf8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieInfoUrlRegExp := RegExCreate(REGEXP_MOVIE_INFOURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_TnaFlix.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieInfoUrlRegExp);
  inherited;
end;

function TDownloader_TnaFlix.GetMovieInfoUrl: string;
begin
  Result := 'http://www.tnaflix.com/view_video.php?viewkey=' + MovieID;
end;

function TDownloader_TnaFlix.GetFileNameExt: string;
begin
  Result := '.flv';
end;

function TDownloader_TnaFlix.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Url, InfoXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, UrlDecode(Url), InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'file', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := HtmlDecode(Url);
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_TnaFlix);
  {$ENDIF}

end.
