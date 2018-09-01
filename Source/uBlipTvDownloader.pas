unit uBlipTvDownloader;
// http://blip.tv/play/hIVV4sNUAg

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader;

type
  TBlipTvDownloader = class(TCommonDownloader)
    private
    protected
      MovieIdFromUrlRegExp: IRegEx;
      function GetMovieInfoUrl: string; override;
      function GetProvider: string; override;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  janXmlParser2,
  uStringUtils;
  
const MOVIE_ID_FROM_URL_REGEXP = '[?&]file=http(?:%3A%2F%2F|://)(?:www\.)?blip\.tv(?:%2F|/)rss(?:%2F|/)flash(?:%2F|/)(?P<ID>[0-9]+)';

{ TBlipTvDownloader }

constructor TBlipTvDownloader.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  MovieIdFromUrlRegExp := RegExCreate(MOVIE_ID_FROM_URL_REGEXP, [rcoIgnoreCase]);
end;

destructor TBlipTvDownloader.Destroy;
begin
  MovieIdFromUrlRegExp := nil;
  inherited;
end;

function TBlipTvDownloader.GetMovieInfoUrl: string;
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

function TBlipTvDownloader.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := inherited BeforePrepareFromPage(Page, Http);
  Page := WideToAnsi(Utf8ToWide(Page));
end;

function TBlipTvDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
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

function TBlipTvDownloader.GetProvider: string;
begin
  Result := 'Blip.tv';
end;

end.
