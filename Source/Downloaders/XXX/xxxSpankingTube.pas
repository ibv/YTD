unit xxxSpankingTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_SpankingTube = class(THttpDownloader)
    private
    protected
      MovieFileNameRegExp: TRegExp;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*spankingtube\.com/watch/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s[^>]*\bid="content_head"[^>]*>(?P<TITLE>.+?)</div>';
  REGEXP_MOVIE_FILENAME = '<param\s+name=movie\s+value=[^\s>]*[?&]video=(?P<FILENAME>[^\s&>]+)';

{ TDownloader_SpankingTube }

class function TDownloader_SpankingTube.Provider: string;
begin
  Result := 'SpankingTube.com';
end;

class function TDownloader_SpankingTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_SpankingTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieFileNameRegExp := RegExCreate(REGEXP_MOVIE_FILENAME, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_SpankingTube.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieFileNameRegExp);
  inherited;
end;

function TDownloader_SpankingTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.spankingtube.com/watch/' + MovieID + '/';
end;

function TDownloader_SpankingTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    FileName, Path, ConfigXml: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieFileNameRegExp, Page, 'FILENAME', FileName) then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['Filename']))
  else if not DownloadPage(Http, 'http://www.spankingtube.com/csplayer_dark.config.php', ConfigXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
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
  RegisterDownloader(TDownloader_SpankingTube);
  {$ENDIF}

end.
