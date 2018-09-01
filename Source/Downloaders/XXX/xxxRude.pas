unit xxxRude;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Rude = class(THttpDownloader)
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

const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*rude\.com/v/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Rude }

class function TDownloader_Rude.Provider: string;
begin
  Result := 'Rude.com';
end;

class function TDownloader_Rude.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Rude.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
end;

destructor TDownloader_Rude.Destroy;
begin
  inherited;
end;

function TDownloader_Rude.GetMovieInfoUrl: string;
begin
  Result := 'http://www.rude.com/v/' + MovieID + '/';
end;

function TDownloader_Rude.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Title, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  // I couldn't download this page directly because I need the cookie
  if not DownloadPage(Http, 'http://www.rude.com/v/' + MovieID + '/view_xml', Page, peXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TXmlDoc.Create;
    try
      Xml.Xml := Page;
      if not GetXmlVar(Xml, 'video/titleShort', Title) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
      else if not GetXmlVar(Xml, 'video/streams', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetName(Title);
        MovieUrl := UrlDecode(Url);
        SetPrepared(True);
        Result := True;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_Rude);
  {$ENDIF}

end.
