unit uDownloader_BlipTv;
// http://blip.tv/play/hIVV4sNUAg

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_BlipTv = class(THttpDownloader)
    private
    protected
      MovieIdFromUrlRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
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
  janXmlParser2;
  
const MOVIE_ID_FROM_URL_REGEXP = '[?&]file=http(?:%3A%2F%2F|://)(?:www\.)?blip\.tv(?:%2F|/)rss(?:%2F|/)flash(?:%2F|/)(?P<ID>[0-9]+)';

{ TDownloader_BlipTv }

constructor TDownloader_BlipTv.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  MovieIdFromUrlRegExp := RegExCreate(MOVIE_ID_FROM_URL_REGEXP, [rcoIgnoreCase]);
end;

destructor TDownloader_BlipTv.Destroy;
begin
  MovieIdFromUrlRegExp := nil;
  inherited;
end;

function TDownloader_BlipTv.GetMovieInfoUrl: string;
var Http: THttpSend;
    Url, ID: string;
    Match: IMatch;
begin
  Result := '';
  Http := CreateHttp;
  try
    if Http.HttpMethod('GET', 'http://blip.tv/play/' + MovieID) then
      if CheckRedirect(Http, Url) then
        begin
        Match := MovieIdFromUrlRegExp.Match(Url);
        if Match.Matched then
          begin
          ID := Match.Groups.ItemsByName['ID'].Value;
          if ID <> '' then
            Result := 'http://blip.tv/rss/flash/' + ID;
          end;
        end;
  finally
    Http.Free;
    end;
end;

function TDownloader_BlipTv.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Xml: TjanXmlParser2;
    TitleNode, ContentNode: TjanXmlNode2;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Xml := TjanXmlParser2.create;
  try
    Xml.xml := Page;
    TitleNode := Xml.getChildByPath('channel/item/media:title');
    if TitleNode <> nil then
      begin
      SetName(TitleNode.text);
      ContentNode := Xml.GetChildByPath('channel/item/media:group/media:content');
      if ContentNode <> nil then
        begin
        MovieUrl := ContentNode.attribute['url'];
        Result := True;
        SetPrepared(True);
        end;
      end;
  finally
    Xml.Free;
    end;
end;

class function TDownloader_BlipTv.Provider: string;
begin
  Result := 'Blip.tv';
end;

class function TDownloader_BlipTv.MovieIDParamName: string;
begin
  Result := 'BLIPTV';
end;

class function TDownloader_BlipTv.UrlRegExp: string;
begin
  // http://blip.tv/play/hIVV4sNUAg
  Result := '^https?://(?:[a-z0-9-]+\.)?blip\.tv/play/(?P<' + MovieIDParamName + '>[^/&?]+)';
end;

function TDownloader_BlipTv.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

initialization
  RegisterDownloader(TDownloader_BlipTV);

end.
