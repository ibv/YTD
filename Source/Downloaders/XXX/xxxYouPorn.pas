unit xxxYouPorn;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_YouPorn = class(THttpDownloader)
    private
    protected
      MovieInfoUrlRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; override;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*youporn\.com/watch/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>\s*(?:<img[^>]*>)?\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_MOVIE_INFOURL = '\.addVariable\s*\(\s*\''file''\s*,[^;'']*''(?P<URL>https?://[^'']+)''';

{ TDownloader_YouPorn }

class function TDownloader_YouPorn.Provider: string;
begin
  Result := 'YouPorn.com';
end;

class function TDownloader_YouPorn.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_YouPorn.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieInfoUrlRegExp := RegExCreate(REGEXP_MOVIE_INFOURL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_YouPorn.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieInfoUrlRegExp := nil;
  inherited;
end;

function TDownloader_YouPorn.GetMovieInfoUrl: string;
begin
  Result := 'http://www.youporn.com/watch/' + MovieID + '/';
end;

function TDownloader_YouPorn.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Url, InfoXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieInfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
  else if not DownloadPage(Http, Url, InfoXml) then
    SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'trackList/track/location', Url) then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
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

function TDownloader_YouPorn.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  Http.Cookies.Add('age_check=1');
  Result := inherited GetMovieInfoContent(Http, Url, Page, Method);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_YouPorn);
  {$ENDIF}

end.
