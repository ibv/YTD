unit downMpora;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Mpora = class(THttpDownloader)
    private
    protected
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
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

// http://video.mpora.com/watch/xfnGGmZDC/
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*video\.mpora\.com/watch/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_Mpora }

class function TDownloader_Mpora.Provider: string;
begin
  Result := 'Mpora.com';
end;

class function TDownloader_Mpora.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Mpora.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_Mpora.Destroy;
begin
  inherited;
end;

function TDownloader_Mpora.GetMovieInfoUrl: string;
begin
  Result := 'http://api.mpora.com/tv/player/load/vid/' + MovieID + '/';
end;

function TDownloader_Mpora.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, Title, InfoXml: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    if not GetXmlVar(Xml, 'configuration/title', Title) then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
    else if not DownloadPage(Http, 'http://api.mpora.com/tv/player/playlist/vid/' + MovieID + '/', InfoXml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      begin
      Xml.Xml := InfoXml;
      if not GetXmlAttr(Xml, 'channel/item/enclosure', 'url', Url) then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
      else
        begin
        SetName(Title);
        MovieUrl := Url;
        SetPrepared(True);
        Result := True;
        end;
      end;
  finally
    Xml.Free;
    end;
end;

function TDownloader_Mpora.GetFileNameExt: string;
begin
  // MovieURL: 'http://cdn2.video.mporatrons.com/play/video/xfnGGmZDC/mp4/'
  if Prepared then
    Result := '.' + ExtractFileName(ExcludeTrailingBackslash(StringReplace(MovieURL, '/', '\', [rfReplaceAll])))
  else
    Result := inherited GetFileNameExt;
end;

initialization
  RegisterDownloader(TDownloader_Mpora);

end.
