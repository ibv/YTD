unit downAngryAlien;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_AngryAlien = class(THttpDownloader)
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

// http://www.angryalien.com/0605/freddyjasonbuns.asp
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*angryalien\.com/';
  URLREGEXP_ID =        '[0-9]+/.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_EXTRACT_URL = '<embed\s+src="(?P<URL>https?://.+?)"';

{ TDownloader_AngryAlien }

class function TDownloader_AngryAlien.Provider: string;
begin
  Result := 'AngryAlien.com';
end;

class function TDownloader_AngryAlien.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_AngryAlien.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_AngryAlien.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_AngryAlien.GetMovieInfoUrl: string;
begin
  Result := 'http://www.angryalien.com/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_AngryAlien);

end.
