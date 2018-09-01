unit downGodTube;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_GodTube = class(THttpDownloader)
    private
    protected
      PlaylistRegExp: IRegEx;
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

// http://www.godtube.com/featured/video/jesus-your-co-pilot-or-leader
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*godtube\.com/featured/video/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_PLAYLIST = '\bflashvars\.playlistPath\s*=\s*''(?P<URL>https?://.+?)''';

{ TDownloader_GodTube }

class function TDownloader_GodTube.Provider: string;
begin
  Result := 'GodTube.com';
end;

class function TDownloader_GodTube.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_GodTube.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  PlaylistRegExp := RegExCreate(REGEXP_PLAYLIST, [rcoIgnoreCase]);
end;

destructor TDownloader_GodTube.Destroy;
begin
  PlaylistRegExp := nil;
  inherited;
end;

function TDownloader_GodTube.GetMovieInfoUrl: string;
begin
  Result := 'http://www.godtube.com/featured/video/' + MovieID;
end;

function TDownloader_GodTube.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var Url, PlayList, Title: string;
    Xml: TjanXmlParser2;
    Node: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(PlayListRegExp, Page, 'URL', Url) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if not DownloadPage(Http, Url, PlayList) then
    SetLastErrorMsg(_(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE))
  else
    begin
    Xml := TjanXmlParser2.Create;
    try
      Xml.Xml := PlayList;
      Node := Xml.getChildByPath('playlist');
      if Node = nil then
        SetLastErrorMsg(_(ERR_INVALID_MEDIA_INFO_PAGE))
      else
        begin
        for i := 0 to Pred(Node.childCount) do
          if Node.childNode[i].name = 'item' then
            if GetXmlVar(Node.childNode[i], 'filelocation', Url) and GetXmlVar(Node.childNode[i], 'title', Title) then
              begin
              {$IFDEF MULTIDOWNLOADS}
              NameList.Add(Title);
              UrlList.Add(Url);
              {$ELSE}
              SetName(Title);
              MovieUrl := Url;
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
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_GodTube);

end.
