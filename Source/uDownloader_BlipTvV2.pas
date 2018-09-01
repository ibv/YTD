unit uDownloader_BlipTvV2;
// http://blip.tv/file/108391

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uDownloader_BlipTv;

type
  TDownloader_BlipTvV2 = class(TDownloader_BlipTv)
    private
    protected
      MovieIDFromPageRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier;

const MOVIE_ID_FROM_PAGE_REGEXP = '<link\s+rel="video_src"\s+href="http://(?:www\.)?blip\.tv/play/(?P<MOVIEID>[^"]+)"';

{ TDownloader_BlipTvV2 }

constructor TDownloader_BlipTvV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(MOVIE_ID_FROM_PAGE_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTvV2.Destroy;
begin
  MovieIDFromPageRegExp := nil;
  inherited;
end;

function TDownloader_BlipTvV2.GetMovieInfoUrl: string;
var Http: THttpSend;
    Page: string;
    Match: IMatch;
begin
  Result := '';
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://blip.tv/file/' + MovieID, Page) then
      begin
      Match := MovieIDFromPageRegExp.Match(Page);
      if Match.Matched then
        begin
        MovieID := Match.Groups.ItemsByName['MOVIEID'].Value;
        Result := inherited GetMovieInfoUrl;
        end;
      end;
  finally
    Http.Free;
    end;
end;

class function TDownloader_BlipTvV2.MovieIDParamName: string;
begin
  Result := 'BLIPTVV2';
end;

class function TDownloader_BlipTvV2.UrlRegExp: string;
begin
  // http://blip.tv/file/108391
  Result := '^https?://(?:[a-z0-9-]+\.)?blip\.tv/file/(?P<' + MovieIDParamName + '>[0-9]+)';
end;

initialization
  RegisterDownloader(TDownloader_BlipTvV2);

end.
