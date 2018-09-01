unit downFreeSk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_FreeSk = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://free.zoznam.sk/video/Splhajuci-buldozer
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*free\.zoznam\.sk/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<input\s+type="hidden"\s+name="module_name"\s+value="(?P<TITLE>[^"]+)"';

  //<param name="movie" value="http://free.zoznam.sk/media/player/player.swf?fID=66598&emb=0&sads=true&image=http://free.zoznam.sk/openMedia.php?path=440f093c8401bbcf2713b6e36ebd6aed229f435bfe9100a8886152214efd5c245bfd51fd16c659ef&from=free.zoznam.sk&video=http://free.zoznam.sk/openMedia.php?path=440f093c8401bbcf2713b6e36ebd6aed229f435bfe9100a8886152214efd5c249a265a4652e49fa3&furi=Splhajuci-buldozer&__toto_replacenut__" />
  REGEXP_EXTRACT_URL = '<param\s+name="movie"\s+value="[^">]*[?&]video=(?P<URL>https?://[^"]+)"';

{ TDownloader_FreeSk }

class function TDownloader_FreeSk.Provider: string;
begin
  Result := 'Free.zoznam.sk';
end;

class function TDownloader_FreeSk.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_FreeSk.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_FreeSk.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieUrlRegExp);
  inherited;
end;

function TDownloader_FreeSk.GetMovieInfoUrl: string;
begin
  Result := 'http://free.zoznam.sk/video/' + MovieID;
end;

function TDownloader_FreeSk.GetFileNameExt: string;
begin
  Result := '.flv';
end;

initialization
  RegisterDownloader(TDownloader_FreeSk);

end.
