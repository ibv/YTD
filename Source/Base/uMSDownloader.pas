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
      fMsdlOptions: TMsdlOptions;
    protected
      procedure ClearMsdlOptions; virtual;
      procedure AddMsdlOption(ShortOption: char; const Argument: string = ''); virtual;
      procedure OnMsdlDownloadProgress(DownloadedSize, TotalSize: integer; var DoAbort: integer); virtual;
      property MsdlOptions: TMsdlOptions read fMsdlOptions;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Download: boolean; override;
      //function Prepare: boolean; override;
    end;

implementation

uses
  uMessages;

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

procedure TMSDownloader.ClearMsdlOptions;
begin
  SetLength(fMsdlOptions, 0);
end;

procedure TMSDownloader.AddMsdlOption(ShortOption: char; const Argument: string);
var n: integer;
begin
  n := Length(fMsdlOptions);
  SetLength(fMsdlOptions, Succ(n));
  fMsdlOptions[n].ShortOption := ShortOption;
  fMsdlOptions[n].Argument := Argument;
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

function TMSDownloader.Download: boolean;
var LogFileName, ProxyString: string;
    RetCode: integer;
begin
  inherited Download;
  DownloadedBytes := 0;
  TotalBytes := -1;
  Aborted := False;
  Result := False;
  if Options.ProxyHost <> '' then
    begin
    ProxyString := Options.ProxyHost + ':' + Options.ProxyPort;
    if Options.ProxyUser <> '' then
      if Options.ProxyPassword <> '' then
        ProxyString := Options.ProxyUser + ':' + Options.ProxyPassword + '@' + ProxyString
      else
        ProxyString := Options.ProxyUser + '@' + ProxyString;
    AddMsdlOption('y', ProxyString); // Note: MSDL has no option 'y', it's an extra option of MSDL_DLL
    end;
  AddMsdlOption('o', FileName);
  LogFileName := GetTempDir + ExtractFileName(FileName) + '.log';
  if FileExists(LogFileName) then
    DeleteFile(PChar(LogFileName));
  SetLastErrorMsg(Format(_(ERR_SEE_LOGFILE), [LogFileName]));
  AddMsdlOption('l', LogFileName);
  AddMsdlOption(MSDL_OPTION_URL, MovieURL);
  if not Msdl_Init then
    Raise EMSDownloaderError.CreateFmt(_(ERR_FAILED_TO_LOAD_DLL), ['msdl_dll.dll']);
  RetCode := Msdl_Download(Integer(Self), MsdlDownloadProgressCallback, MsdlOptions);
  if RetCode >= 0 then
    Result := True;
end;

end.
