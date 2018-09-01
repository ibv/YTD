unit downClipFishV2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downClipfish;

type
  TDownloader_ClipfishV2 = class(TDownloader_Clipfish)
    private
    protected
      MovieIDFromPageRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.clipfish.de/special/lets-dance/home/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*clipfish\.de/special/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_ID_FROM_PAGE = '/devxml/videoinfo/(?P<MOVIEID>[0-9]+)';

{ TDownloader_ClipfishV2 }

class function TDownloader_ClipfishV2.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_ClipfishV2.Create(const AMovieID: string);
begin
  inherited;
  MovieIDFromPageRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PAGE, [rcoIgnoreCase]);
end;

destructor TDownloader_ClipfishV2.Destroy;
begin
  RegExFreeAndNil(MovieIDFromPageRegExp);
  inherited;
end;

function TDownloader_ClipfishV2.GetMovieInfoUrl: string;
var Http: THttpSend;
    Page, ID: string;
begin
  Result := '';
  Http := CreateHttp;
  try
    if DownloadPage(Http, 'http://www.clipfish.de/special/' + MovieID, Page) then
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
  RegisterDownloader(TDownloader_ClipfishV2);

end.
