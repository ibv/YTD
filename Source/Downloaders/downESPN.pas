unit downESPN;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_ESPN = class(THttpDownloader)
    private
    protected
      PlayerIDRegExp: TRegExp;
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

// http://espn.go.com/video/clip?id=5163631
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*espn\.go\.com/video/clip\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_TITLE = '<h2>\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_PLAYERID = '\bvideoPlayers\s*:\s*\{\s*(?P<PLAYER>[a-z_][a-z0-9_]*)';

{ TDownloader_ESPN }

class function TDownloader_ESPN.Provider: string;
begin
  Result := 'ESPN.com';
end;

class function TDownloader_ESPN.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_ESPN.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  PlayerIDRegExp := RegExCreate(REGEXP_PLAYERID, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_ESPN.Destroy;
begin
  RegExFreeAndNil(PlayerIDRegExp);
  inherited;
end;

function TDownloader_ESPN.GetMovieInfoUrl: string;
begin
  Result := 'http://espn.go.com/video/clip?id=' + MovieID;
end;

function TDownloader_ESPN.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Player, InfoXml, MediaUrl, PlaylistUrl, PlayListXml, Title, FileName: string;
    Xml: TjanXmlParser2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(PlayerIDRegExp, Page, 'PLAYER', Player) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, 'http://espn.go.com/videohub/mpf/config.prodXml?player=' + Player + '&adminOver=none', InfoXml) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := InfoXml;
      if not GetXmlVar(Xml, 'globalPlayerConfig/mediaUrl', MediaUrl) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['mediaUrl']))
      else if not GetXmlVar(Xml, 'globalPlayerConfig/playlistURL', PlaylistUrl) then
        SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['playlistURL']))
      else if not DownloadPage(Http, PlaylistUrl + '?id=' + MovieID + '&player=' + Player, PlaylistXml) then
        SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
      else
        begin
        Xml.Xml := PlaylistXml;
        if not GetXmlVar(Xml, 'channel/item/headline', Title) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_TITLE))
        else if not GetXmlVar(Xml, 'channel/item/asseturl', FileName) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          SetName(Title);
          MovieURL := MediaUrl + FileName;
          SetPrepared(True);
          Result := True;
          end;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_ESPN);

end.
