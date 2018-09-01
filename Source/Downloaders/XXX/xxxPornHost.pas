unit xxxPornHost;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornHost = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
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
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*pornhost\.com/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_URL = '<label>download this file</label>\s*<a\s+href="(?P<URL>https?://[^"]+)"';

{ TDownloader_PornHost }

class function TDownloader_PornHost.Provider: string;
begin
  Result := 'PornHost.com';
end;

class function TDownloader_PornHost.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_PornHost.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peANSI);
  MovieUrlRegExp := RegExCreate(REGEXP_EXTRACT_URL, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_PornHost.Destroy;
begin
  MovieUrlRegExp := nil;
  inherited;
end;

function TDownloader_PornHost.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhost.com/' + MovieID + '/';
end;

function TDownloader_PornHost.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  inherited AfterPrepareFromPage(Page, Http);
  SetName(MovieID + ExtractFileExt(MovieURL));
  Result := Prepared;
end;

initialization
  RegisterDownloader(TDownloader_PornHost);

end.
