unit downBlennus;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uNestedDownloader,
  downYouTube;

type
  TDownloader_Blennus = class(TNestedDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
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

// http://www.blennus.com/index.php?option=content&task=view&id=1027&Itemid=
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*blennus\.com/(?:index\.php)?\?(?:[^&]*&)*id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<td\s+class="contentheading"[^>]*>\s+(?P<TITLE>.*?)\s*(?:&nbsp;\s*)*</td>';
  REGEXP_EXTRACT_URL = '<param\s+name="movie"\s+value="(?P<URL>https?://.+?)"';

{ TDownloader_Blennus }

class function TDownloader_Blennus.Provider: string;
begin
  Result := 'Blennus.com';
end;

class function TDownloader_Blennus.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Blennus.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_EXTRACT_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  NestedUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Blennus.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  inherited;
end;

function TDownloader_Blennus.GetMovieInfoUrl: string;
begin
  Result := 'http://www.blennus.com/index.php?option=content&task=view&id=' + MovieID + '&Itemid=';
end;

initialization
  RegisterDownloader(TDownloader_Blennus);

end.
