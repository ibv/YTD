unit downFileCabi;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FileCabi = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.filecabi.net/video/hiddendogstopsrobbery.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*filecabi\.net/video/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<font\s+size=2>\s*<b>\s*<font\s+size="3"[^>]*>(?P<TITLE>.*?)</font>\s*</b>\s*</font>';
  REGEXP_EXTRACT_URL = '\bso\.addVariable\s*\(\s*''file''\s*,\s*''(?P<URL>https?://.*?)''';

{ TDownloader_FileCabi }

class function TDownloader_FileCabi.Provider: string;
begin
  Result := 'FileCabi.net';
end;

class function TDownloader_FileCabi.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FileCabi.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_FileCabi.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_FileCabi.GetMovieInfoUrl: string;
begin
  Result := 'http://www.filecabi.net/video/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_FileCabi);

end.
