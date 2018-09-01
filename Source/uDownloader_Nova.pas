unit uDownloader_Nova;
{$INCLUDE 'ytd.inc'}

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
      function GetMovieInfoUrl: string; override;
      function GetFileNameExt: string; override;
      function GetInfoPageEncoding: TPageEncoding; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function MovieIDParamName: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uStringUtils,
  janXmlParser2;
  
const MOVIE_VARIABLES_REGEXP = '\svar\s(?P<VARNAME>[a-z_][a-z0-9_]*)\s*=\s*(["'']?)(?P<VARVALUE>.*?)\2\s*;';

{ TDownloader_Nova }

constructor TDownloader_Nova.Create(const AMovieID: string);
begin
  inherited;
  MovieVariablesRegExp := RegExCreate(MOVIE_VARIABLES_REGEXP, [rcoIgnoreCase, rcoSingleLine])
end;

destructor TDownloader_Nova.Destroy;
begin
  MovieVariablesRegExp := nil;
  inherited;
end;

class function TDownloader_Nova.Provider: string;
begin
  Result := 'TV Nova';
end;

class function TDownloader_Nova.MovieIDParamName: string;
begin
  Result := 'TVNOVA';
end;

class function TDownloader_Nova.UrlRegExp: string;
begin
  // http://archiv.nova.cz/multimedia/ulice-1683-1684-dil.html
  Result := '^https?://archiv\.nova\.cz/multimedia/(?P<' + MovieIDParamName + '>.+?)\.html?';
end;

function TDownloader_Nova.GetMovieInfoUrl: string;
begin
  Result := 'http://archiv.nova.cz/multimedia/' + MovieID + '.html';
end;

function TDownloader_Nova.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

function TDownloader_Nova.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Variables: IMatchCollection;
    i: integer;
    Name, Value: string;
    MediaID, SiteID, SectionID, SessionID, UserAdID: string;
    ServersUrl, Servers, VideosUrl, Videos: string;
    FlvServer, FlvStream, FlvName: string;
    Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
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
    SetLastErrorMsg('SiteID not found.')
  else if SectionID = '' then
    SetLastErrorMsg('SectionID not found.')
  else if MediaID = '' then
    SetLastErrorMsg('MediaID not found.')
  else if SessionID = '' then
    SetLastErrorMsg('SessionID not found.')
  //else if UserAdID = '' then
  //  SetLastErrorMsg('UserAdID not found.')
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
    if not DownloadPage(Http, ServersUrl, Servers) then
      SetLastErrorMsg('Failed loading server list.')
    else if not DownloadPage(Http, VideosUrl, Videos) then
      SetLastErrorMsg('Failed loading video list.')
    else
      begin
      Servers := WideToAnsi(Utf8ToWide(Servers));
      Videos := WideToAnsi(Utf8ToWide(Videos));
      FlvServer := '';
      FlvStream := '';
      FlvName := '';
      Xml := TjanXmlParser2.Create;
      try
        Xml.Xml := Servers;
        Node := Xml.GetChildByPath('flvserver');
        if Node <> nil then
          FlvServer := Node.attribute['url'];
        Xml.Xml := Videos;
        Node := Xml.GetChildByPath('item');
        if Node <> nil then
          begin
          FlvStream := Node.attribute['src'];
          FlvName := Node.Attribute['txt'];
          end;
      finally
        Xml.Free;
        end;
      if FlvName = '' then
        FlvName := MediaID;
      if FlvServer = '' then
        SetLastErrorMsg('Failed to locate streaming server.')
      else if FlvStream = '' then
        SetLastErrorMsg('Failed to locate video stream.')
      else
        begin
        SetName(FlvName);
        RtmpPlayPath := FlvStream;
        RtmpUrl := FlvServer + '/' + FlvStream;
        MovieUrl := RtmpUrl;
        Result := True;
        SetPrepared(True);
        end;
      end;
    end;
end;

function TDownloader_Nova.GetFileNameExt: string;
begin
  Result := '.flv';
end;

initialization
  RegisterDownloader(TDownloader_Nova);

end.
