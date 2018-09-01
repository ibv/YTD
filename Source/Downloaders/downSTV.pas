unit downSTV;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uOptions,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_STV = class(TMSDownloader)
    private
    protected
      MovieObjectRegExp: TRegExp;
      IVysilaniUrlRegExp: TRegExp;
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://stv.livetv.sk/tvarchive//video/video.html?video=52655
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stv\.livetv\.sk/tvarchive/+video/video\.html\?video=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_STV }

class function TDownloader_STV.Provider: string;
begin
  Result := 'STV.livetv.sk';
end;

class function TDownloader_STV.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_STV.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
end;

destructor TDownloader_STV.Destroy;
begin
  inherited;
end;

function TDownloader_STV.GetMovieInfoUrl: string;
begin
  Result := 'http://stv.livetv.sk/tvarchive/video/playlist/playlist.wvx?video=' + MovieID;
end;

function TDownloader_STV.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url: string;
    i: integer;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    if not GetXmlAttr(Xml, 'entry/ref', 'href', Url) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      SetName('STV-video' + MovieID); // No title is provided by the server
      i := Length(Url);
      while i > 0 do
        if Url[i] = '/' then
          begin
          if i < Length(Url) then
            SetName(ChangeFileExt(Copy(Url, Succ(i), MaxInt), ''));
          Break;
          end
        else
          Dec(i);
      MovieURL := Url;
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_STV);

end.
