unit uDownloader_iPrima;

// http://www.iprima.cz/videoarchiv/44524/all/all
// ---> <div class="views-field-title"><div class="field-content">Partie - Miroslava Nìmcová</div></div>
// ---> <object width="512" height="414"><param name="movie" value="http://www.stream.cz/object/439128-partie-miroslava-nemcova-ods-jan-kasl-kdu-csl">
// http://www.stream.cz/object/439128-partie-miroslava-nemcova-ods-jan-kasl-kdu-csl
// ---> http://flash.stream.cz/swf/streamPlayer_558.swf?cdnID=1219408&id=439128&skin=None&autoPlay=0&link=http://www.stream.cz/video/439128&external=1&ratio=1.3300&image_url=http://i.stream.cz/video_flash/5393/375393.jpg
// No a tim mam dane cdnID, id i soubor s videem

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
    Url, Page: string;
    Match: IMatch;
begin
  Result := '';
  Info := CreateHttp;
  try
    Url := Format('http://www.iprima.cz/videoarchiv/%s/all/all', [MovieID]);
    if DownloadPage(Info, Url, Page) then
      begin
      Page := WideToAnsi(Utf8ToWide(Page));
      Match := StreamIDRegExp.Match(Page);
      if Match.Matched then
        Result := GetMovieInfoUrlForID(Match.Groups.ItemsByName['STREAMID'].Value);
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
