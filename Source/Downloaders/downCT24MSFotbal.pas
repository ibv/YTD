unit downCT24MSFotbal;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT;

type
  TDownloader_CT24MSFotbal = class(TDownloader_CT)
    private
    protected
      MovieParamsRegExp: IRegEx;
      MovieVarsRegExp: IRegEx;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  janXmlParser2,
  uDownloadClassifier,
  uMessages;

// http://msfotbal.ct24.cz/article.asp?id=339
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*msfotbal\.ct24\.cz/article\.asp\?id=';
  URLREGEXP_ID =        '[0-9]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h1>(?P<TITLE>.*?)</h1>';
  REGEXP_MOVIE_PARAMS = '<param\s+name="initParams"\s+value="(?P<PARAMS>.*?)"';
  REGEXP_MOVIE_VARS = '(?P<VARNAME>[a-z_][a-z0-9_]*)=(?P<VARVALUE>[^,]*),?';

const
  SOAP_URL = 'http://ctdir.visual.cz/ivysilani/services/streaming/SLP.asmx';
  SOAP_ACTION = 'http://ivysilani.visual.cz/services/GetPlaylistUrl';
  SOAP_REQUEST = '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
                   '<s:Body>' +
                     '<GetPlaylistUrl xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ivysilani.visual.cz/services">' +
                       '<request>' +
                         '<Format>%s</Format>' +
                         '<ClientAddress>%s</ClientAddress>' +
                         '<Expiration>%s</Expiration>' +
                         '<Playlist>' +
                           '<PlaylistItem>' +
                             '<Type>Archive</Type>' +
                             '<Identifier>%s</Identifier>' +
                             '<Begin>0</Begin>' +
                             '<Duration i:nil="true" /> ' +
                             '<NoSkip i:nil="true" /> ' +
                           '</PlaylistItem>' +
                         '</Playlist>' +
                       '</request>' +
                     '</GetPlaylistUrl>' +
                   '</s:Body>' +
                 '</s:Envelope>';

{ TDownloader_CT24MSFotbal }

class function TDownloader_CT24MSFotbal.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT24MSFotbal.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  MovieTitleRegExp := nil;
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
  MovieParamsRegExp := RegExCreate(REGEXP_MOVIE_PARAMS, [rcoIgnoreCase, rcoSingleLine]);
  MovieVarsRegExp := RegExCreate(REGEXP_MOVIE_VARS, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT24MSFotbal.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieParamsRegExp := nil;
  MovieVarsRegExp := nil;
  inherited;
end;

function TDownloader_CT24MSFotbal.GetMovieInfoUrl: string;
begin
  Result := 'http://msfotbal.ct24.cz/article.asp?id=' + MovieID;
end;

function TDownloader_CT24MSFotbal.GetMovieObjectUrl(Http: THttpSend; const Page: string; out Url: string): boolean;
var Request: string;
    RequestStream: TStringStream;
    Params, VarName, VarValue, Token, VideoID, Quality: string;
    i: integer;
    Vars: IMatchCollection;
    Info: string;
    Xml: TjanXmlParser2;
begin
  Result := False;
  if GetRegExpVar(MovieParamsRegExp, Page, 'PARAMS', Params) then
    begin
    Token := '';
    VideoID := '';
    Quality := '';
    Vars := MovieVarsRegExp.Matches(Params);
    try
      for i := 0 to Pred(Vars.Count) do
        begin
        VarName := Vars[i].Groups.ItemsByName['VARNAME'].Value;
        VarValue := Vars[i].Groups.ItemsByName['VARVALUE'].Value;
        if VarName = 'token' then
          Token := VarValue
        else if VarName = 'videoId' then
          VideoID := VarValue
        else if VarName = 'quality' then
          Quality := VarValue;
        end;
    finally
      Vars := nil;
      end;
    if (Token <> '') and (VideoID <> '') and (Quality <> '') then
      begin
      Request := Format(SOAP_REQUEST, [Quality, Token, FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss".00000+02:00"', Now + 1), VideoID]);
      RequestStream := TStringStream.Create(Request);
      try
        Http.Clear;
        Http.Headers.Add('SOAPAction: "' + SOAP_ACTION + '"');
        Http.MimeType := 'text/xml; charset=utf-8';
        Http.InputStream := RequestStream;
        if DownloadPage(Http, SOAP_URL, Info, peUTF8, hmPOST, False) then
          begin
          Xml := TjanXmlParser2.Create;
          try
            Xml.Xml := Info;
            Result := GetXmlVar(Xml, 'soap:Body/GetPlaylistUrlResponse/GetPlaylistUrlResult', Url);
          finally
            Xml.Free;
            end;
          end;
      finally
        Http.InputStream := nil;
        RequestStream.Free;
        end;
      end;
    end;
end;

initialization
  RegisterDownloader(TDownloader_CT24MSFotbal);

end.
