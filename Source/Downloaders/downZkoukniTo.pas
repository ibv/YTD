unit downZkoukniTo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ZkoukniTo = class(THttpDownloader)
    private
    protected
      MovieIDRegExp: IRegEx;
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

// http://www.zkouknito.cz/video_59813_holcicka-strasila-medveda
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*zkouknito\.cz/';
  URLREGEXP_ID =        'video_[0-9]+.*';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<meta\s+name="title"\s+content="(?P<TITLE>[^"]+)"';
  REGEXP_EXTRACT_ID = '<param\s+name="movie"\s+value="[^"]*[?&]vid=(?P<ID>[0-9]+)"';

{ TDownloader_ZkoukniTo }

class function TDownloader_ZkoukniTo.Provider: string;
begin
  Result := 'ZkoukniTo.cz';
end;

class function TDownloader_ZkoukniTo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_ZkoukniTo.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieIDRegExp := RegExCreate(REGEXP_EXTRACT_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_ZkoukniTo.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieIDRegExp := nil;
  inherited;
end;

function TDownloader_ZkoukniTo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.zkouknito.cz/' + MovieID;
end;

function TDownloader_ZkoukniTo.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var ID, Url, InfoXml: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieIDRegExp, Page, 'ID', ID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://www.zkouknito.cz/player/scripts/videoinfo.php?id=' + ID, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'file', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := Url;
        Result := True;
        SetPrepared(True);
        end;
    finally
      Xml.Free;
      end;
    end;
end;

function TDownloader_ZkoukniTo.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  {$IFDEF XXX}
  Http.Cookies.Add('confirmed=1');
  {$ENDIF}
  Result := inherited GetMovieInfoContent(Http, Url, Page, Method);
end;

initialization
  RegisterDownloader(TDownloader_ZkoukniTo);

end.
