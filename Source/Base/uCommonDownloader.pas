unit uCommonDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, blcksock, janXmlParser2,
  uDownloader;

type
  TCommonDownloader = class(TDownloader)
    private
      fMovieURL: string;
      fInfoPageEncoding: TPageEncoding;
    protected
      function GetMovieInfoUrl: string; virtual; abstract;
    protected
      MovieTitleRegExp: IRegEx;
      MovieUrlRegExp: IRegEx;
      function GetInfoPageEncoding: TPageEncoding; virtual;
      procedure SetInfoPageEncoding(const Value: TPageEncoding); virtual;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; virtual;
      property MovieUrl: string read fMovieUrl write fMovieUrl;
    protected
      function GetFileNameExt: string; override;
      function BuildMovieUrl(out Url: string): boolean; virtual;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
    protected
      function GetRegExpVar(RegExp: IRegEx; const Text, VarName: string; out VarValue: string): boolean; virtual;
      function GetXmlVar(Xml: TjanXmlNode2; const Path: string; out VarValue: string): boolean; virtual;
      function GetXmlAttr(Xml: TjanXmlNode2; const Path, Attribute: string; out VarValue: string): boolean; virtual;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      property ContentUrl: string read fMovieUrl;
    end;

implementation

uses
  uMessages;
  
{ TCommonDownloader }

constructor TCommonDownloader.Create(const AMovieID: string);
begin
  inherited;
  fInfoPageEncoding := peUnknown;
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
end;

destructor TCommonDownloader.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

function TCommonDownloader.GetFileNameExt: string;
var Url: string;
    i: integer;
begin
  if Prepared then
    begin
    i := Pos('?', MovieURL);
    if i <= 0 then
      Url := MovieUrl
    else
      Url := Copy(MovieUrl, 1, Pred(i));
    Result := ExtractFileExt(Url);
    if Pos('/', Result) > 0 then
      Result := '';
    end
  else
    NotPreparedError;
end;

function TCommonDownloader.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  Result := DownloadPage(Http, Url, Page, GetInfoPageEncoding, Method);
end;

function TCommonDownloader.Prepare: boolean;
var Info: THttpSend;
    URL, Page, s: string;
begin
  SetLastErrorMsg('');
  Result := False;
  SetPrepared(False);
  Info := CreateHttp;
  try
    // Download the media info page.
    URL := GetMovieInfoUrl;
    if URL = '' then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE)
    else if not GetMovieInfoContent(Info, URL, Page) then
      SetLastErrorMsg(ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE)
    else if not BeforePrepareFromPage(Page, Info) then
      SetLastErrorMsg(ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE)
    else
      begin
      SetName('');
      MovieURL := '';
      // If regular expression for TITLE is set, use it to get title.
      if MovieTitleRegExp <> nil then
        if GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', s) then
          SetName(s);
      // If a function for building URL is provided, use it.
      if BuildMovieURL(s) then
        MovieURL := s
      // Otherwise if regular expression for URL is set, use it.
      else
        if MovieUrlRegExp <> nil then
          if GetRegExpVar(MovieURLRegExp, Page, 'URL', s) then
            MovieURL := s;
      // If URL was set, Prepare was successful.
      if MovieUrl <> '' then
        SetPrepared(True);
      // Try additional processing of page data
      if not AfterPrepareFromPage(Page, Info) then
        SetPrepared(False);
      if (not Prepared) and (LastErrorMsg = '') then
        SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO);
      Result := Prepared;
      end;
  finally
    Info.Free;
    end;
end;

function TCommonDownloader.BuildMovieUrl(out Url: string): boolean;
begin
  Result := False;
end;

function TCommonDownloader.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.GetInfoPageEncoding: TPageEncoding;
begin
  Result := fInfoPageEncoding;
end;

procedure TCommonDownloader.SetInfoPageEncoding(const Value: TPageEncoding);
begin
  fInfoPageEncoding := Value;
end;

function TCommonDownloader.GetRegExpVar(RegExp: IRegEx; const Text, VarName: string; out VarValue: string): boolean;
var Match: IMatch;
begin
  Match := RegExp.Match(Text);
  try
    Result := Match.Matched;
    if Result then
      VarValue := Match.Groups.ItemsByName[VarName].Value
    else
      VarValue := '';
  finally
    Match := nil;
    end;
end;

function TCommonDownloader.GetXmlVar(Xml: TjanXmlNode2; const Path: string; out VarValue: string): boolean;
var Node: TjanXmlNode2;
begin
  Node := Xml.GetChildByPath(Path);
  Result := Node <> nil;
  if Result then
    VarValue := Node.Text
  else
    VarValue := '';
end;

function TCommonDownloader.GetXmlAttr(Xml: TjanXmlNode2; const Path, Attribute: string; out VarValue: string): boolean;
var Node: TjanXmlNode2;
begin
  if Path = '' then
    Node := Xml
  else
    Node := Xml.GetChildByPath(Path);
  Result := (Node <> nil) and Node.hasAttribute(Attribute);
  if Result then
    VarValue := Node.Attribute[Attribute]
  else
    VarValue := '';
end;

end.
