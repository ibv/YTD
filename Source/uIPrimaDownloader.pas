unit uIPrimaDownloader;

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
  uDownloader, uCommonDownloader, uStreamDownloader;

type
  TIPrimaDownloader = class(TStreamDownloader)
    private
      StreamIDRegExp: IRegEx;
    protected
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function GetMovieInfoUrl: string; override;
    end;

implementation

uses
  uStringUtils;
  
const STREAM_ID_REGEXP = '<param\s+name="movie"\s+value="http://(?:www\.)?stream\.cz/object/(?P<STREAMID>[0-9]+)';

{ TIPrimaDownloader }

constructor TIPrimaDownloader.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  StreamIDRegExp := RegExCreate(STREAM_ID_REGEXP, [rcoIgnoreCase]);
end;

destructor TIPrimaDownloader.Destroy;
begin
  StreamIDRegExp := nil;
  inherited;
end;

function TIPrimaDownloader.GetMovieInfoUrl: string;
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

end.
