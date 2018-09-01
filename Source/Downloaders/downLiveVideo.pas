unit downLiveVideo;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  HttpSend, PCRE,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_LiveVideo = class(THttpDownloader)
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://www.livevideo.com/liveshow/brian3maria
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*livevideo\.com/liveshow/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_LiveVideo }

class function TDownloader_LiveVideo.Provider: string;
begin
  Result := 'LiveVideo.com';
end;

class function TDownloader_LiveVideo.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_LiveVideo.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
end;

destructor TDownloader_LiveVideo.Destroy;
begin
  inherited;
end;

function TDownloader_LiveVideo.GetMovieInfoUrl: string;
begin
  Result := 'http://www.livevideo.com/livetv/schedule.ashx?uname=' + MovieID;
end;

function TDownloader_LiveVideo.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    Archives: TjanXmlNode2;
    Title, Url: string;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.Create;
  try
    Xml.Xml := Page;
    Archives := Xml.getChildByPath('archives');
    if Archives <> nil then
      for i := 0 to Pred(Archives.childCount) do
        if Archives.childNode[i].name = 'archive' then
          if GetXmlVar(Archives.childNode[i], 'url', Url) and GetXmlVar(Archives.childNode[i], 'title', Title) then
            begin
            Title := MovieID + ' - ' + Title;
            Url := HtmlDecode(Url);
            if DownloadPage(Http, Url, hmHEAD) then
              Url := LastUrl;
            {$IFDEF MULTIDOWNLOADS}
            NameList.Add(Title);
            UrlList.Add(Url);
            {$ELSE}
            SetName(Title);
            MovieURL := Url;
            Result := True;
            SetPrepared(True);
            Exit;
            {$ENDIF}
            end;
    {$IFDEF MULTIDOWNLOADS}
    if UrlList.Count <= 0 then
      SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
    else
      begin
      SetPrepared(True);
      Result := First;
      end;
    {$ELSE}
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL));
    {$ENDIF}
  finally
    Xml.Free;
    end;
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_LiveVideo);
  {$ENDIF}

end.
