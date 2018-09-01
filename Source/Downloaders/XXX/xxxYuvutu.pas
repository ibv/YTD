unit xxxYuvutu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Yuvutu = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*yuvutu\.com/.*[?&]video_id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>[^<]+-\s*(?P<TITLE>[^-<]+?)\s*</title>';
  REGEXP_MOVIE_URL = '<param\s+name="flashvars"\s+value="file=(?P<URL>https?://[^&"]+)';

{ TDownloader_PornoTube }

class function TDownloader_Yuvutu.Provider: string;
begin
  Result := 'Yuvutu.com';
end;

class function TDownloader_Yuvutu.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Yuvutu.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_MOVIE_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Yuvutu.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_Yuvutu.GetMovieInfoUrl: string;
begin
  Result := 'http://www.yuvutu.com/modules.php?name=Video&op=view&video_id=' + MovieID + '&proceed=yes';
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_Yuvutu);
  {$ENDIF}

end.
