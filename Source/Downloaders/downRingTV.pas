unit downRingTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader,
  downGrindTV;

type
  TDownloader_RingTV = class(TDownloader_GrindTV)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function BaseUrl: string; override;
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

// http://www.ringtv.com/video/frankie_gomez_training/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*ringtv\.com/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)</title>';
  REGEXP_MOVIE_ID = '\bvar\s+video\s*=\s*(?P<VIDEOID>[0-9]+)\s*;';

{ TDownloader_RingTV }

class function TDownloader_RingTV.Provider: string;
begin
  Result := 'RingTV.com';
end;

class function TDownloader_RingTV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_RingTV.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  RegExFreeAndNil(MovieIdRegExp);
  MovieIdRegExp := RegExCreate(REGEXP_MOVIE_ID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_RingTV.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieIdRegExp);
  inherited;
end;

function TDownloader_RingTV.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ringtv.com/video/' + MovieID + '/';
end;

function TDownloader_RingTV.BaseUrl: string;
begin
  Result := 'http://videos.ringtv.com/7/';
end;

initialization
  RegisterDownloader(TDownloader_RingTV);

end.
