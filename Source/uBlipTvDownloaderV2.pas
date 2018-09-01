unit uBlipTvDownloaderV2;
// http://blip.tv/file/108391

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uBlipTvDownloader;

type
  TBlipTvDownloaderV2 = class(TBlipTvDownloader)
    private
    protected
      MovieIDFromPageRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

const MOVIE_ID_FROM_PAGE_REGEXP = '<link\s+rel="video_src"\s+href="http://(?:www\.)?blip\.tv/play/(?P<MOVIEID>[^"]+)"';

{ TBlipTvDownloaderV2 }

constructor TBlipTvDownloaderV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(MOVIE_ID_FROM_PAGE_REGEXP, [rcoIgnoreCase]);
end;

destructor TBlipTvDownloaderV2.Destroy;
begin
  MovieIDFromPageRegExp := nil;
  inherited;
end;

function TBlipTvDownloaderV2.GetMovieInfoUrl: string;
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

end.
