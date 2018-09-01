unit uCommonDownloader;

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader;

type
  TCommonDownloader = class(TDownloader)
    private
      fMovieID: string;
      fMovieURL: string;
    protected
      MovieTitleRegExp: IRegEx;
      MovieUrlRegExp: IRegEx;
      function GetFileNameExt: string; override;
      procedure SetMovieID(const Value: string); virtual;
      function BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; virtual;
      function BeforeDownload(Http: THttpSend): boolean; virtual;
      function GetMovieInfoUrl: string; virtual; abstract;
      property MovieUrl: string read fMovieUrl write fMovieUrl;
    public
      constructor Create(const AMovieID: string); reintroduce; virtual;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      property MovieID: string read fMovieID write SetMovieID;
      property ContentUrl: string read fMovieUrl;
    end;

implementation

{ TCommonDownloader }

constructor TCommonDownloader.Create(const AMovieID: string);
begin
  inherited Create;
  MovieID := AMovieID;
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
end;

destructor TCommonDownloader.Destroy;
begin
  MovieTitleRegExp := nil;
  MovieUrlRegExp := nil;
  inherited;
end;

procedure TCommonDownloader.SetMovieID(const Value: string);
begin
  fMovieID := Value;
  SetPrepared(False);
end;

function TCommonDownloader.GetFileNameExt: string;
begin
  if Prepared then
    Result := ExtractFileExt(MovieURL)
  else
    Raise EDownloader.Create('Downloader is not prepared!');
end;

function TCommonDownloader.Prepare: boolean;
var Match: IMatch;
    Info: THttpSend;
    URL, Page: string;
begin
  SetLastErrorMsg('');
  SetPrepared(False);
  Info := CreateHttp;
  try
    URL := GetMovieInfoUrl;
    if URL <> '' then
      if DownloadPage(Info, URL, Page) then
        begin
        if BeforePrepareFromPage(Page, Info) then
          begin
          MovieURL := '';
          if MovieTitleRegExp <> nil then
            begin
            Match := MovieTitleRegExp.Match(Page);
            if Match.Matched then
              SetName(Match.Groups.ItemsByName['TITLE'].Value);
            end;
          if MovieUrlRegExp <> nil then
            begin
            Match := MovieURLRegExp.Match(Page);
            if Match.Matched then
              MovieURL := Match.Groups.ItemsByName['URL'].Value;
            end;
          if (MovieUrl <> '') then
            SetPrepared(True);
          if not AfterPrepareFromPage(Page, Info) then
            SetPrepared(False);
          if not Prepared then
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

function TCommonDownloader.Download: boolean;
begin
  inherited Download;
  Result := False;
  if MovieURL <> '' then
    begin
    VideoDownloader := CreateHttp;
    try
      if BeforeDownload(VideoDownloader) then
        begin
        VideoDownloader.OutputStream := TFileStream.Create(FileName, fmCreate);
        try
          try
            VideoDownloader.Sock.OnStatus := SockStatusMonitor;
            BytesTransferred := 0;
            if DownloadPage(VideoDownloader, MovieURL) then
              Result := VideoDownloader.OutputStream.Size > 0;
          finally
            VideoDownloader.Sock.OnStatus := nil;
            VideoDownloader.OutputStream.Free;
            VideoDownloader.OutputStream := nil;
            end;
        except
          if FileExists(FileName) then
            DeleteFile(FileName);
          Raise;
          end;
        end;
    finally
      VideoDownloader.Free;
      VideoDownloader := nil;
      end;
    end;
end;

function TCommonDownloader.BeforePrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
begin
  Result := True;
end;

function TCommonDownloader.BeforeDownload(Http: THttpSend): boolean;
begin
  Result := True;
end;

end.
