unit xxxEmpFlix;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, xxxTnaFlix;

type
  TDownloader_EmpFlix = class(TDownloader_TnaFlix)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*empflix\.com/view\.php\?(?:[^&]*&)*id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_EmpFlix }

class function TDownloader_EmpFlix.Provider: string;
begin
  Result := 'EmpFlix.com';
end;

class function TDownloader_EmpFlix.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_EmpFlix.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUtf8);
end;

destructor TDownloader_EmpFlix.Destroy;
begin
  inherited;
end;

function TDownloader_EmpFlix.GetMovieInfoUrl: string;
begin
  Result := 'http://www.empflix.com/view.php?id=' + MovieID;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_EmpFlix);
  {$ENDIF}

end.
