unit downVimeo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Vimeo = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: TRegExp;
    protected
      function GetFileNameExt: string; override;
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

// http://www.vimeo.com/10777111
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*vimeo\.com/';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Vimeo }

class function TDownloader_Vimeo.Provider: string;
begin
  Result := 'Vimeo.com';
end;

class function TDownloader_Vimeo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Vimeo.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_Vimeo.Destroy;
begin
  inherited;
end;

function TDownloader_Vimeo.GetFileNameExt: string;
begin
  Result := '.mp4';
end;

function TDownloader_Vimeo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.vimeo.com/moogaloop/load/clip:' + MovieID + '/';
end;

function TDownloader_Vimeo.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TXmlDoc;
    Caption, Signature, Expires: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TXmlDoc.create;
  try
    Xml.xml := Page;
    if not GetXmlVar(Xml, 'video/caption', Caption) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else if not GetXmlVar(Xml, 'request_signature', Signature) then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['request_signature.']))
    else if not GetXmlVar(Xml, 'request_signature_expires', Expires) then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['request_signature_expires.']))
    else
      begin
      SetName(HtmlDecode(Caption));
      MovieUrl := 'http://www.vimeo.com/moogaloop/play/clip:' + MovieID + '/' + Signature + '/' + Expires + '/';
      Result := True;
      SetPrepared(True);
      end;
  finally
    Xml.Free;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Vimeo);

end.
