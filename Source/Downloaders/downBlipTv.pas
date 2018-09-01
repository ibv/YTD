unit downBlipTv;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BlipTv = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: IRegEx; 
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

// http://blip.tv/play/hIVV4sNUAg
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*blip\.tv/play/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_URL = '[?&]file=http(?:%3A%2F%2F|://)(?:www\.)?blip\.tv(?:%2F|/)rss(?:%2F|/)flash(?:%2F|/)(?P<ID>[0-9]+)';

{ TDownloader_BlipTv }

class function TDownloader_BlipTv.Provider: string;
begin
  Result := 'Blip.tv';
end;

class function TDownloader_BlipTv.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BlipTv.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieIdFromUrlRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTv.Destroy;
begin
  MovieIdFromUrlRegExp := nil;
  inherited;
end;

function TDownloader_BlipTv.GetMovieInfoUrl: string;
var Http: THttpSend;
    Url, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    Http.Document.Clear;
    if Http.HttpMethod('GET', 'http://blip.tv/play/' + MovieID) then
      if CheckRedirect(Http, Url) then
        if GetRegExpVar(MovieIdFromUrlRegExp, Url, 'ID', ID) then
          Result := 'http://blip.tv/rss/flash/' + ID;
  finally
    Http.Free;
    end;
end;

function TDownloader_BlipTv.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Title, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.create;
  try
    Xml.xml := Page;
    if not GetXmlVar(Xml, 'channel/item/media:title', Title) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_TITLE)
    else if not GetXmlAttr(Xml, 'channel/item/media:group/media:content', 'url', Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      SetName(Title);
      MovieURL := Url;
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BlipTV);

end.
