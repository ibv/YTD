unit uDownloader_CT;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uMSDownloader;

type
  TDownloader_CT = class(TMSDownloader)
    private
    protected
      MovieObjectRegExp: IRegEx;
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
  
const MOVIE_OBJECT_REGEXP = '<object\s+id="programmeObject"(?:\s+data|.*?<param\s+name="url"\s+value)="(?P<OBJURL>[^"]+)"';

{ TDownloader_CT }

constructor TDownloader_CT.Create(const AMovieID: string);
begin
  inherited;
  MovieObjectRegExp := RegExCreate(MOVIE_OBJECT_REGEXP, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT.Destroy;
begin
  MovieObjectRegExp := nil;
  inherited;
end;

class function TDownloader_CT.Provider: string;
begin
  Result := 'Ceska televize';
end;

class function TDownloader_CT.MovieIDParamName: string;
begin
  Result := 'CZECHTV';
end;

class function TDownloader_CT.UrlRegExp: string;
begin
  // http://www.ceskatelevize.cz/ivysilani/309292320520025-den-d-ii-rada/
  Result := '^https?://(?:[a-z0-9-]+\.)?ceskatelevize\.cz/ivysilani/(?P<' + MovieIDParamName + '>[^/]+)/?';
end;

function TDownloader_CT.GetMovieInfoUrl: string;
begin
  Result := 'http://www.ceskatelevize.cz/ivysilani/' + MovieID + '/';
end;

function TDownloader_CT.GetFileNameExt: string;
begin
  Result := '.asf';
end;

function TDownloader_CT.GetInfoPageEncoding: TPageEncoding;
begin
  Result := peUTF8;
end;

function TDownloader_CT.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var URL, AsxDef: string;
    Xml: TjanXmlParser2;
    TitleNode, TitleNode2, EntryNode, RefNode: TjanXmlNode2;
    i: integer;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVar(MovieObjectRegExp, Page, 'OBJURL', URL) then
    SetLastErrorMsg('Failed to find ASX object.')
  else if not DownloadPage(Http, URL, AsxDef) then
    SetLastErrorMsg('Failed to download ASX info.')
  else
    begin
    AsxDef := WideToAnsi(Utf8ToWide(AsxDef));
    Xml := TjanXmlParser2.Create;
    try
      Xml.xml := AsxDef;
      TitleNode := Xml.GetChildByPath('TITLE');
      if TitleNode = nil then
        TitleNode := Xml.GetChildByPath('ENTRY/TITLE');
      if TitleNode = nil then
        SetLastErrorMsg('Downloaded ASX object is invalid: ' + AsxDef)
      else
        begin
        EntryNode := Xml.GetChildByPath('ENTRY');
        for i := 0 to Pred(Xml.ChildCount) do
          if Xml.ChildNode[i].name = 'ENTRY' then
            begin
            TitleNode2 := Xml.ChildNode[i].GetChildByName('TITLE');
            if TitleNode2 <> nil then
              if TitleNode2.text = TitleNode.text then
                begin
                EntryNode := Xml.ChildNode[i];
                Break;
                end;
            end;
        if EntryNode = nil then
          SetLastErrorMsg('Failed to locate video info.')
        else
          begin
          RefNode := EntryNode.GetChildByName('REF');
          TitleNode := EntryNode.GetChildByName('TITLE');
          if RefNode = nil then
            SetLastErrorMsg('Failed to locate REF node.')
          else if TitleNode = nil then
            SetLastErrorMsg('Failed to locate TITLE node.')
          else
            begin
            SetName(TitleNode.text);
            MovieUrl := RefNode.attribute['HREF'];
            if MovieUrl <> '' then
              begin
              Result := True;
              SetPrepared(True);
              end;
            end;
          end;
        end;
    finally
      Xml.Free;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CT);

end.
