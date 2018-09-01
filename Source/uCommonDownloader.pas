unit uCommonDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, blcksock,
  uDownloader, uStringUtils;

type
  TPageEncoding = (peUnknown, peANSI, peUTF8, peUTF16);

  TCommonDownloader = class(TDownloader)
    private
      fMovieURL: string;
    protected
      MovieTitleRegExp: IRegEx;
      MovieUrlRegExp: IRegEx;
      function GetFileNameExt: string; override;
      function GetInfoPageEncoding: TPageEncoding; virtual;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
      function GetMovieInfoUrl: string; virtual; abstract;
      function GetRegExpVar(RegExp: IRegEx; const Text, VarName: string; out VarValue: string): boolean; virtual;
      property MovieUrl: string read fMovieUrl write fMovieUrl;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      property ContentUrl: string read fMovieUrl;
    end;

implementation

{ TCommonDownloader }

constructor TCommonDownloader.Create(const AMovieID: string);
begin
  inherited;
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
begin
  if Prepared then
    Result := ExtractFileExt(MovieURL)
  else
    Raise EDownloaderError.Create('Downloader is not prepared!');
end;

function TCommonDownloader.Prepare: boolean;
var Info: THttpSend;
    URL, Page, s: string;
begin
  SetLastErrorMsg('');
  SetPrepared(False);
  Info := CreateHttp;
  try
    URL := GetMovieInfoUrl;
    if URL <> '' then
      if DownloadPage(Info, URL, Page) then
        begin
        case GetInfoPageEncoding of
          peUnknown:
            ;
          peANSI:
            ;
          peUTF8:
            Page := WideToAnsi(Utf8ToWide(Page));
          peUTF16:
            Page := WideToAnsi(Page);
          end;
        if BeforePrepareFromPage(Page, Info) then
          begin
          MovieURL := '';
          if MovieTitleRegExp <> nil then
            if GetRegExpVar(MovieTitleRegExp, Page, 'TITLE', s) then
              SetName(s);
          if MovieUrlRegExp <> nil then
            if GetRegExpVar(MovieURLRegExp, Page, 'URL', s) then
              MovieURL := s;
          if (MovieUrl <> '') then
            SetPrepared(True);
          if not AfterPrepareFromPage(Page, Info) then
            SetPrepared(False);
          if (not Prepared) and (LastErrorMsg = '') then
            SetLastErrorMsg('Failed to locate video info.');
          end;
        end
      else
        SetLastErrorMsg('Failed to download video page.')
    else
      SetLastErrorMsg('Failed to locate video page.');
  finally
    Info.Free;
    end;
  Result := Prepared;
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
  Result := peUnknown;
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

end.
