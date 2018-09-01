unit downNova;
{$INCLUDE 'ytd.inc'}
{.DEFINE LOW_QUALITY}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uRtmpDownloader;

type
  TDownloader_Nova = class(TRtmpDownloader)
    private
    protected
      MovieVariablesRegExp: IRegEx;
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
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://archiv.nova.cz/multimedia/ulice-1683-1684-dil.html
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*archiv\.nova\.cz/multimedia/';
  URLREGEXP_ID =        '.+?';
  URLREGEXP_AFTER_ID =  '\.html?';

const
  REGEXP_MOVIE_VARIABLES = '\svar\s(?P<VARNAME>[a-z_][a-z0-9_]*)\s*=\s*(["'']?)(?P<VARVALUE>.*?)\2\s*;';

{ TDownloader_Nova }

class function TDownloader_Nova.Provider: string;
begin
  Result := 'Nova.cz';
end;

class function TDownloader_Nova.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Nova.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieVariablesRegExp := RegExCreate(REGEXP_MOVIE_VARIABLES, [rcoIgnoreCase, rcoSingleLine])
end;

destructor TDownloader_Nova.Destroy;
begin
  MovieVariablesRegExp := nil;
  inherited;
end;

function TDownloader_Nova.GetFileNameExt: string;
begin
  Result := {$IFDEF LOW_QUALITY} '.flv' {$ELSE} '.mp4' {$ENDIF};
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := 'http://archiv.nova.cz/multimedia/' + MovieID + '.html';
end;

function TDownloader_Nova.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Variables: IMatchCollection;
    i: integer;
    Name, Value: string;
    MediaID, SiteID, SectionID, SessionID, UserAdID: string;
    ServersUrl, Servers, VideosUrl, Videos: string;
    FlvServer, FlvStream, FlvName: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  MediaID := '';
  SiteID := '';
  SectionID := '';
  SessionID := '';
  UserAdID := '';
  Variables := MovieVariablesRegExp.Matches(Page);
  try
    for i := 0 to Pred(Variables.Count) do
      begin
      Name := Variables.Items[i].Groups.ItemsByName['VARNAME'].Value;
      Value := Variables.Items[i].Groups.ItemsByName['VARVALUE'].Value;
      if AnsiCompareText(Name, 'media_id') = 0 then
        MediaID := Value
      else if AnsiCompareText(Name, 'site_id') = 0 then
        SiteID := Value
      else if AnsiCompareText(Name, 'section_id') = 0 then
        SectionID := Value;
      end;
  finally
    Variables := nil;
    end;
  for i := 0 to Pred(Http.Cookies.Count) do
    begin
    Name := Http.Cookies.Names[i];
    Value := Http.Cookies.Values[Name];
    if AnsiCompareText(Name, 'bit') = 0 then
      UserAdID := Value
    else if AnsiCompareText(Name, 'c4d') = 0 then
      SessionID := Value;
    end;
  Http.Cookies.Values['bit'] := UserAdID;
  if SiteID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SiteID']))
  else if SectionID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SectionID']))
  else if MediaID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['MediaID']))
  else if SessionID = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['SessionID']))
  //else if UserAdID = '' then
  //  SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['UserAdID']))
  else
    begin
    ServersUrl := 'http://tn.nova.cz/bin/player/config.php?site_id=' + SiteID + '&';
    VideosUrl := 'http://tn.nova.cz/bin/player/serve.php' +
                 '?site_id=' + SiteID +
                 '&media_id=' + MediaID +
                 '&userad_id=' + UserAdID +
                 '&section_id=' + SectionID +
                 '&noad_count=0' +
                 '&fv=WIN 10,0,45,2' +
                 '&session_id=' + SessionID +
                 '&ad_file=noad';
    if not DownloadPage(Http, ServersUrl, Servers, peUTF8) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_SERVER_LIST))
    else if not DownloadPage(Http, VideosUrl, Videos, peUTF8) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      begin
      Xml := TjanXmlParser2.Create;
      try
        Xml.Xml := Servers;
        GetXmlAttr(Xml, 'flvserver', 'url', FlvServer);
        Xml.Xml := Videos;
        GetXmlAttr(Xml, 'item', 'src', FlvStream);
        GetXmlAttr(Xml, 'item', 'txt', FlvName);
      finally
        Xml.Free;
        end;
      if FlvName = '' then
        FlvName := MediaID;
      if FlvServer = '' then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_SERVER))
      else if FlvStream = '' then
        SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_STREAM))
      else
        begin
        SetName(FlvName);
        {$IFNDEF LOW_QUALITY}
        FlvStream := 'mp4:' + FlvStream;
        {$ENDIF}
        MovieUrl := FlvServer + '/' + FlvStream;
        AddRtmpDumpOption('r', MovieURL);
        AddRtmpDumpOption('y', FlvStream);
        Result := True;
        SetPrepared(True);
        end;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Nova);

end.
