unit downRaajje;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Raajje = class(THttpDownloader)
    private
    protected
      MovieFileNameRegExp: IRegEx;
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

// http://www.raajje.tv/video/991/president-anni-diving
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*Raajje\.tv/video/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+id="viewvideo-title">\s*(?P<TITLE>.*?)\s*</div>';
  REGEXP_MOVIE_FILENAME = '<param\s+name="movie"\s+value="[^"\s>]*[?&]video=(?P<FILENAME>[^\s&>]+)';

{ TDownloader_PornoTube }

class function TDownloader_Raajje.Provider: string;
begin
  Result := 'Raajje.com';
end;

class function TDownloader_Raajje.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Raajje.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieFileNameRegExp := RegExCreate(REGEXP_MOVIE_FILENAME, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Raajje.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieFileNameRegExp := nil;
  inherited;
end;

function TDownloader_Raajje.GetMovieInfoUrl: string;
begin
  Result := 'http://www.raajje.tv/video/' + MovieID + '/';
end;

function TDownloader_Raajje.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    FileName, Path, ConfigXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieFileNameRegExp, Page, 'FILENAME', FileName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Filename']))
  else if not DownloadPage(Http, 'http://www.raajje.tv/csplayer.config.php', ConfigXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := ConfigXml;
      if not GetXmlVar(Xml, 'VIDEOS_PATH', Path) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Videos Path']))
      else if Path = '' then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieUrl := Path + FileName;
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
  RegisterDownloader(TDownloader_Raajje);
  {$ENDIF}

end.
