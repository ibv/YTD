unit downStream;
{$INCLUDE 'ytd.inc'}
{.DEFINE XMLINFO}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_Stream = class(THttpDownloader)
    private
    protected
      MovieParamsRegExp: TRegExp;
      MovieIdFromParamsRegExp: TRegExp;
      MovieHDIdFromParamsRegExp: TRegExp;
      MovieCdnIdFromParamsRegExp: TRegExp;
      function GetMovieInfoUrlForID(const ID: string): string; virtual;
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
  {$IFDEF XMLINFO}
  uXML,
  {$ENDIF}
  uDownloadClassifier,
  uMessages;

// http://www.stream.cz/reklamozrouti/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/video/410282-reklamozrouti-medvedi-reklama
// http://www.stream.cz/object/410282-reklamozrouti-medvedi-reklama
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*stream\.cz/(?:[^/]+/)*';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<title>(?P<TITLE>.*?)\s+-[^<-]+</title>';
  REGEXP_MOVIE_PARAMS = '<param\s+name="flashvars"\s+value="(?P<PARAM>.*?)"|\swriteSWF\s*\((?P<PARAM2>.*?)\)\s*;';
  REGEXP_MOVIE_ID_FROM_PARAMS = '[&'']id=(?P<ID>[0-9]+)';
  REGEXP_MOVIE_HDID_FROM_PARAMS = '[&'']hdID=(?P<ID>[0-9]+)';
  REGEXP_MOVIE_CDNID_FROM_PARAMS = '[&'']cdnID=(?P<ID>[0-9]+)';

{ TDownloader_Stream }

class function TDownloader_Stream.Provider: string;
begin
  Result := 'Stream.cz';
end;

class function TDownloader_Stream.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_Stream.Create(const AMovieID: string);
begin
  inherited Create(AMovieID);
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase]);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  MovieIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_ID_FROM_PARAMS, [rcoIgnoreCase]);
  MovieHDIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_HDID_FROM_PARAMS, [rcoIgnoreCase]);
  MovieCdnIdFromParamsRegExp := RegExCreate(REGEXP_MOVIE_CDNID_FROM_PARAMS, [rcoIgnoreCase]);
end;

destructor TDownloader_Stream.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  RegExFreeAndNil(MovieParamsRegExp);
  RegExFreeAndNil(MovieIdFromParamsRegExp);
  RegExFreeAndNil(MovieHDIdFromParamsRegExp);
  RegExFreeAndNil(MovieCdnIdFromParamsRegExp);
  inherited;
end;

function TDownloader_Stream.GetMovieInfoUrl: string;
begin
  Result := GetMovieInfoUrlForID(MovieID);
end;

function TDownloader_Stream.GetMovieInfoUrlForID(const ID: string): string;
begin
  Result := 'http://www.stream.cz/video/' + ID;
end;

function TDownloader_Stream.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var {$IFDEF XMLINFO}
    Info, Title: string;
    Xml: TXmlDoc;
    TitleNode, ContentNode: TjanXmlNode2;
    {$ENDIF}
    Params, CdnID, ID: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  Params := '';
  if MovieParamsRegExp.Match(Page) then
    if not MovieParamsRegExp.SubexpressionByName('PARAM', Params) then
      Params := MovieParamsRegExp.SubexpressionByName('PARAM2');
  if Params = '' then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO))
  else if not GetRegExpVar(MovieCdnIdFromParamsRegExp, Params, 'ID', CdnID) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_URL))
  else
    begin
    if GetRegExpVar(MovieHDIdFromParamsRegExp, Params, 'ID', ID) then
      CdnID := ID;
    {$IFDEF XMLINFO}
    if GetRegExpVar(MovieIdFromParamsRegExp, Params, 'ID', ID) then
      try
        if DownloadPage(Http, 'http://flash.stream.cz/get_info/' + ID, Info, peXml) then
          begin
          Xml := TXmlDoc.create;
          try
            Xml.xml := Info;
            if GetXmlVar(Xml, 'video/title', Title) then
              SetName(Title);
          finally
            Xml.Free;
            end;
          end;
      except
        ;
        end;
    {$ENDIF}
    if DownloadPage(Http, 'http://cdn-dispatcher.stream.cz/?id=' + CdnID, hmHEAD) then
      begin
      MovieURL := LastUrl;
      Result := True;
      SetPrepared(True);
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_Stream);

end.
