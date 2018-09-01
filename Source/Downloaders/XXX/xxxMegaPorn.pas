unit xxxMegaPorn;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, 
  uDownloader, uCommonDownloader, uHttpDownloader, downMegaVideo;

type
  TDownloader_MegaPorn = class(TDownloader_MegaVideo)
    private
    protected
      function MasterDomain: string; override;
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*megaporn\.com/(?:video/?)?\?v=';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_MegaPorn }

class function TDownloader_MegaPorn.Provider: string;
begin
  Result := 'MegaPorn.com';
end;

class function TDownloader_MegaPorn.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_MegaPorn.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TDownloader_MegaPorn.Destroy;
begin
  inherited;
end;

function TDownloader_MegaPorn.MasterDomain: string;
begin
  Result := 'megaporn.com';
end;

function TDownloader_MegaPorn.GetMovieInfoUrl: string;
begin
  Result := 'http://www.' + MasterDomain + '/video/xml/videolink.php?v=' + MovieID;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_MegaPorn);
  {$ENDIF}

end.
