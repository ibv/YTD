unit downVideoAlbumyAzet;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_VideoAlbumyAzet = class(THttpDownloader)
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
  uXML,
  uDownloadClassifier,
  uMessages;

// http://videoalbumy.azet.sk/land-rover/A1OFbSJBBXOF6yyp/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*videoalbumy\.azet\.sk/[^/]+/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<div\s+id="l">\s*<h2[^>]*>\s*<a[^>]*>(?P<TITLE>.*?)</a>';

{ TDownloader_VideoAlbumyAzet }

class function TDownloader_VideoAlbumyAzet.Provider: string;
begin
  Result := 'VideoAlbumy.azet.com';
end;

class function TDownloader_VideoAlbumyAzet.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_VideoAlbumyAzet.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peANSI);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_VideoAlbumyAzet.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_VideoAlbumyAzet.GetMovieInfoUrl: string;
begin
  Result := 'http://videoalbumy.azet.sk/dummy/' + MovieID + '/';
end;

function TDownloader_VideoAlbumyAzet.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    InfoXml, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not DownloadPage(Http, 'http://videoalbumy.azet.sk/players/jw/plConf.phtml?&h=' + MovieID, InfoXml, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'file', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        MovieURL := Url;
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_VideoAlbumyAzet);

end.
