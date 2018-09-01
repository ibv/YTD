unit downBofunk;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Bofunk = class(THttpDownloader)
    private
    protected
      InfoUrlRegExp: IRegEx;
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

// http://www.bofunk.com/video/10444/ingenious_way_to_mow_your_grass.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*bofunk\.com/video/';
  URLREGEXP_ID =        '.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1\s+class=''title''>(?P<TITLE>.*?)</h1>';
  REGEXP_INFO_URL = '<embed\s+src="(?P<URL>/.+?)"';

{ TDownloader_Bofunk }

class function TDownloader_Bofunk.Provider: string;
begin
  Result := 'Bofunk.com';
end;

class function TDownloader_Bofunk.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Bofunk.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUnknown);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  InfoUrlRegExp := RegExCreate(REGEXP_INFO_URL, [rcoIgnoreCase]);
end;

destructor TDownloader_Bofunk.Destroy;
begin
  MovieTitleRegExp := nil;
  InfoUrlRegExp := nil;
  inherited;
end;

function TDownloader_Bofunk.GetMovieInfoUrl: string;
begin
  Result := 'http://www.bofunk.com/video/' + MovieID;
end;

function TDownloader_Bofunk.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, InfoXml: string;
    Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(InfoUrlRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://flv.bofunk.com' + Url, InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    // Note: Bofunk's XML is malformed in several ways. This one is critical
    InfoXml := StringReplace(InfoXml, '">>Next', '"&gt;&gt;Next', [rfReplaceAll, rfIgnoreCase]);
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      Node := Xml.getChildByPath('SETTINGS');
      if Node = nil then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else
        for i := 0 to Pred(Node.childCount) do
          if Node.childNode[i].name = 'PLAYER_SETTINGS' then
            if Node.childNode[i].attribute['Name'] = 'FLVPath' then
              begin
              if not Node.childNode[i].hasAttribute('Value') then
                SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
              else
                begin
                MovieUrl := Node.childNode[i].attribute['Value'];
                Result := True;
                SetPrepared(True);
                end;
              Break;
              end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Bofunk);

end.
