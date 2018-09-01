unit downBlipTvV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBlipTv;

type
  TDownloader_BlipTvV2 = class(TDownloader_BlipTv)
    private
    protected
      MovieIDFromPageRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

// http://blip.tv/file/108391
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*blip\.tv/file/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_PAGE = '<link\s+rel="video_src"\s+href="http://(?:www\.)?blip\.tv/play/(?P<MOVIEID>[^"]+)"';

{ TDownloader_BlipTvV2 }

class function TDownloader_BlipTvV2.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BlipTvV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PAGE, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTvV2.Destroy;
begin
  MovieIDFromPageRegExp := nil;
  inherited;
end;

function TDownloader_BlipTvV2.GetMovieInfoUrl: string;
var Http: THttpSend;
    Page, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://blip.tv/file/' + MovieID, Page) then
      if GetRegExpVar(MovieIDFromPageRegExp, Page, 'MOVIEID', ID) then
        begin
        MovieID := ID;
        Result := inherited GetMovieInfoUrl;
        end;
  finally
    Http.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BlipTvV2);

end.
