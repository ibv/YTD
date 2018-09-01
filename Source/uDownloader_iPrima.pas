unit uDownloader_iPrima;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  HttpSend, PCRE,
  uDownloader, uHttpDownloader, uDownloader_Stream;

type
  TDownloader_iPrima = class(TDownloader_Stream)
    private
      StreamIDRegExp: IRegEx;
    protected
    public
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function GetMovieInfoUrl: string; override;
    end;

implementation

uses
  uDownloadClassifier,
  uStringUtils;
  
const STREAM_ID_REGEXP = '<param\s+name="movie"\s+value="http://(?:www\.)?stream\.cz/object/(?P<STREAMID>[0-9]+)';

{ TDownloader_iPrima }

constructor TDownloader_iPrima.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  StreamIDRegExp := RegExCreate(STREAM_ID_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_iPrima.Destroy;
begin
  StreamIDRegExp := nil;
  inherited;
end;

function TDownloader_iPrima.GetMovieInfoUrl: string;
var Info: THttpSend;
    Url, Page, ID: string;
begin
  Result := '';
  Info := CreateHttp;
  try
    Url := Format('http://www.iprima.cz/videoarchiv/%s/all/all', [MovieID]);
    if DownloadPage(Info, Url, Page) then
      begin
      Page := WideToAnsi(Utf8ToWide(Page));
      if GetRegExpVar(StreamIDRegExp, Page, 'STREAMID', ID) then
        Result := GetMovieInfoUrlForID(ID);
      end;
  finally
    Info.Free;
    end;
end;

class function TDownloader_iPrima.MovieIDParamName: string;
begin
  Result := 'IPRIMA';
end;

class function TDownloader_iPrima.UrlRegExp: string;
begin
  // http://www.iprima.cz/videoarchiv/44524/all/all
  Result := '^https?://(?:[a-z0-9-]+\.)?iprima.cz/videoarchiv/(?P<' + MovieIDParamName + '>[0-9]+)';
end;

initialization
  RegisterDownloader(TDownloader_iPrima);

end.
