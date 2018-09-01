unit downIPrima;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downStream;

type
  TDownloader_iPrima = class(TDownloader_Stream)
    private
    protected
      StreamIDRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UltimateProvider: string; override;
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.iprima.cz/videoarchiv/44524/all/all
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*iprima.cz/videoarchiv/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_STREAM_ID = '<param\s+name="movie"\s+value="http://(?:www\.)?stream\.cz/object/(?P<STREAMID>[0-9]+)';

{ TDownloader_iPrima }

class function TDownloader_iPrima.UltimateProvider: string;
begin
  Result := inherited Provider;
end;

class function TDownloader_iPrima.Provider: string;
begin
  Result := 'iPrima.cz';
end;

class function TDownloader_iPrima.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_iPrima.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  StreamIDRegExp := RegExCreate(REGEXP_STREAM_ID, [rcoIgnoreCase]);
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
    Url := 'http://www.iprima.cz/videoarchiv/' + MovieID + '/all/all';
    if DownloadPage(Info, Url, Page, peUTF8) then
      if GetRegExpVar(StreamIDRegExp, Page, 'STREAMID', ID) then
        Result := GetMovieInfoUrlForID(ID);
  finally
    Info.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_iPrima);

end.
