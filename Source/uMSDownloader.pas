unit uMSDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uDownloader, uCommonDownloader, uExternalDownloader,
  Msdl_Dll;

type
  EMSDownloaderError = class(EExternalDownloaderError);

  TMSDownloader = class(TExternalDownloader)
    private
    protected
      procedure OnMsdlDownloadProgress(DownloadedSize, TotalSize: integer; var DoAbort: integer); virtual;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
      //function Prepare: boolean; override;
    end;

implementation

procedure MsdlDownloadProgressCallback(Tag: integer; DownloadedSize, TotalSize: integer; var DoAbort: integer); cdecl;
begin
  TMSDownloader(Tag).OnMsdlDownloadProgress(DownloadedSize, TotalSize, DoAbort);
end;

{ TMSDownloader }

constructor TMSDownloader.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TMSDownloader.Destroy;
begin
  inherited;
end;

function TMSDownloader.Download: boolean;
var LogFileName, VideoFileName, Url: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  if MovieUrl <> '' then
    begin
    LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
    if FileExists(LogFileName) then
      DeleteFile(PChar(LogFileName));
    SetLastErrorMsg('See error log in "' + LogFileName + '"');
    VideoFileName := FileName;
    Url := MovieUrl;
    if not Msdl_Init then
      Raise EMSDownloaderError.Create('Failed to load msdl_dll.dll or one of its prerequisites.');
    RetCode := Msdl_Download(Integer(Self), MsdlDownloadProgressCallback, PChar(LogFileName), PChar(VideoFileName), PChar(Url));
    if RetCode >= 0 then
      Result := True;
    end;
end;

procedure TMSDownloader.OnMsdlDownloadProgress(DownloadedSize, TotalSize: integer; var DoAbort: integer);
begin
  DownloadedBytes := DownloadedSize;
  TotalBytes := TotalSize;
  DoProgress;
  if Aborted then
    DoAbort := 1
  else
    DoAbort := 0;
end;

end.
