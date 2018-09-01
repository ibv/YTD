unit downMuzu;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Muzu = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarsVariablesRegExp: TRegExp;
      NetworkID, VideoID, ChannelID: string;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function GetMuzuMediaUrl(out Url: string): boolean; virtual;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://www.muzu.tv/elizarickman/cinnamon-bone-music-video/670078
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*muzu\.tv/';
  URLREGEXP_ID =        '(?:[^/]+/){2}[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1\s+id="videosPageMainTitleH1">\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_FLASHVARS = '\bflashvars\s*:[^"]*"(?P<FLASHVARS>&[^"]+)"';
  REGEXP_FLASHVARS_VARIABLES = '&(?P<VARNAME>[^=&]+)(?:=(?P<VARVALUE>[^&]*))?';

{ TDownloader_Muzu }

class function TDownloader_Muzu.Provider: string;
begin
  Result := 'Muzu.tv';
end;

class function TDownloader_Muzu.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Muzu.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS, [rcoIgnoreCase, rcoSingleLine]);
  FlashVarsVariablesRegExp := RegExCreate(REGEXP_FLASHVARS_VARIABLES, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_Muzu.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarsVariablesRegExp);
  inherited;
end;

function TDownloader_Muzu.GetMovieInfoUrl: string;
begin
  Result := 'http://www.muzu.tv/' + MovieID;
end;

function TDownloader_Muzu.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var FlashVarsInfo, CountryID, NetworkVersion, InfoXml, Url: string;
    Xml: TXmlDoc;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVarsInfo) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT))
  else
    begin
    GetRegExpVarPairs(FlashVarsVariablesRegExp, FlashVarsInfo, ['networkId', 'vidId', 'countryIdentity', 'networkVersion'], [@NetworkID, @VideoID, @CountryID, @NetworkVersion]);
    if NetworkID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['networkId']))
    else if VideoID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['vidId']))
    else if CountryID = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['countryIdentity']))
    else if NetworkVersion = '' then
      SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['networkVersion']))
    else if not DownloadPage(Http, 'http://www.muzu.tv/player/networkVideos/' + NetworkID + '?countryIdentity=' + CountryID + '&networkVersion=' + NetworkVersion + '&hostName=http%3A%2F%2Fwww%2Emuzu%2Etv', InfoXml, peXml) then
      SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
    else
      begin
      Xml := TXmlDoc.Create;
      try
        Xml.Xml := InfoXml;
        if not GetXmlAttr(Xml, 'channels/channel', 'id', ChannelID) then
          SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['channelId']))
        else if not GetMuzuMediaUrl(Url) then
          SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
        else
          begin
          MovieUrl := Url;
          SetPrepared(True);
          Result := True;
          end;
      finally
        Xml.Free;
        end;
      end;
    end;
end;

function TDownloader_Muzu.GetMuzuMediaUrl(out Url: string): boolean;
var Http: THttpSend;
    Xml: TXmlDoc;
    InfoXml, Src: string;
begin
  Result := False;
  Url := '';
  if (NetworkID <> '') and (VideoID <> '') and (ChannelID <> '') then
    begin
    Http := CreateHttp;
    try
      if DownloadPage(Http, 'http://www.muzu.tv/player/playAsset?id=' + NetworkID + '&assetId=' + VideoID + '&videoType=1&playlistId=' + ChannelID, InfoXml, peXml) then
        begin
        Xml := TXmlDoc.Create;
        try
          Xml.Xml := InfoXml;
          if GetXmlAttr(Xml, 'body/video', 'src', Src) then
            if Src <> '' then
              begin
              Url := Src;
              Result := True;
              end;
        finally
          Xml.Free;
          end;
        end;
    finally
      Http.Free;
      end;
    end;
end;

function TDownloader_Muzu.Download: boolean;
var Url: string;
begin
  if GetMuzuMediaUrl(Url) then
    begin
    MovieUrl := Url;
    Result := inherited Download;
    end
  else
    Result := False;
end;

initialization
  RegisterDownloader(TDownloader_Muzu);

end.
