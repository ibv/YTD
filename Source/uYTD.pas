unit uYTD;

interface

uses
  SysUtils, Classes, FileCtrl,
  PCRE,
  uConsoleApp, uDownloader, uDownloadClassifier;

type
  TYTD = class(TConsoleApp)
    private
      fLastProgressPercentage: int64;
      fDownloadClassifier: TDownloadClassifier;
      fDestinationPath: string;
    protected
      function AppTitle: string; override;
      function AppVersion: string; override;
      function DoExecute: boolean; override;
      procedure ShowSyntax(const Error: string = ''); override;
      procedure ParamInitialize; override;
    protected
      function DoDownload(Downloader: TDownloader): boolean; virtual;
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
      function DownloadURL(const URL: string): boolean; virtual;
      function DownloadURLsFromFileList(const FileName: string): integer; virtual;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
    public
      constructor Create; override;
      destructor Destroy; override;
      property DestinationPath: string read fDestinationPath write fDestinationPath;
    end;

implementation

{ TYTD }

constructor TYTD.Create;
begin
  inherited;
  fDownloadClassifier := TDownloadClassifier.Create;
end;

destructor TYTD.Destroy;
begin
  FreeAndNil(fDownloadClassifier);
  inherited;
end;

function TYTD.AppTitle: string;
begin
  Result := 'YouTubeDownloader';
end;

function TYTD.AppVersion: string;
begin
  Result := {$INCLUDE 'ytd.version'} ;
end;

procedure TYTD.ShowSyntax(const Error: string);
begin
  inherited;
  WriteColored(ccWhite, '<arg> [<arg>] ...');
  Writeln;
  Writeln;
  WriteColored(ccWhite, ' -h, -?'); Writeln(' ...... show this help screen.');
  WriteColored(ccWhite, ' -i <file>'); Writeln(' ... load URL list from <file> (one URL per line).');
  WriteColored(ccWhite, ' -o <path>'); Writeln(' ... store files to <path> (default is current directory).');
  Writeln;
  WriteColored(ccWhite, ' <url>'); Writeln(' ....... URL to download.');
  Write('               Supported:');
  WriteColored(ccWhite, '  youtube.com');
  WriteColored(ccWhite, '  blip.tv');
  WriteColored(ccWhite, '  n-joy.cz');
  Writeln;
  Writeln;
end;

procedure TYTD.ParamInitialize;
begin
  inherited;
  DestinationPath := '';
end;

function TYTD.DoExecute: boolean;
var Param: string;
begin
  Result := False;
  if ParamCount = 0 then
    ShowSyntax
  else
    begin
    ParamInitialize;
    while ParamGetNext(Param) do
      if Param[1] = '-' then
        begin
        if (Param = '-?') or (Param = '-h') then
          ShowSyntax
        else if (Param = '-i') then
          if ParamGetNext(Param) then
            if FileExists(Param) then
              begin
              if DownloadURLsFromFileList(Param) > 0 then
                Result := True;
              end
            else
              ShowSyntax('URL list-file "%s" not found.', [Param])
          else
            ShowSyntax('With -i a filename must be provided.')
        else if (Param = '-o') then
          if ParamGetNext(Param) then
            if DirectoryExists(Param) then
              DestinationPath := Param
            else
              ShowSyntax('Destination directory "%s" not found.', [Param])
          else
            ShowSyntax('With -o a directory name must be provided.')
        else
          ShowSyntax('Unknown parameter "%s".', [Param]);
        end
      else
        if DownloadURL(Param) then
          Result := True;
    if not Result then
      ShowError('No valid URLs found.');
    end;
end;

function Int64ToStrF(Value: int64): string;
var Sign: string;
begin
  if Value = 0 then
    Result := '0'
  else if (PByteArray(@Value)^[0]=$80) and ((Value and $00ffffffffffffff) = 0) then
    Result := '-9' + ThousandSeparator + '223' + ThousandSeparator + '372' + ThousandSeparator + '036' + ThousandSeparator + '854' + ThousandSeparator + '775' + ThousandSeparator + '808'
  else
    begin
    if Value < 0 then
      begin
      Sign := '-';
      Value := -Value;
      end;
    Result := '';
    while Value >= 1000 do
      begin
      Result := Format('%s%03.3d%s', [ThousandSeparator, Value mod 1000, Result]);
      Value := Value div 1000;
      end;
    Result := IntToStr(Value) + Result;
    end;
end;

procedure TYTD.DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean);
const EmptyProgressBar = '                             ';
      ProgressBarLength = Length(EmptyProgressBar);
var Proc: int64;
    i, n: integer;
    ProgressBar: string;
begin
  if TotalSize > 1 then
    begin
    Proc := 1000 * DownloadedSize div TotalSize;
    if (not StdOutRedirected) and (Proc <> fLastProgressPercentage) then
      begin
      fLastProgressPercentage := Proc;
      n := Proc div (1000 div ProgressBarLength);
      ProgressBar := EmptyProgressBar;
      for i := 1 to n do
        ProgressBar[i] := '#';     
      Write(Format('    Downloading: <%s> %d.%d%% (%s/%s)'#13, [ProgressBar, Proc div 10, Proc mod 10, Int64ToStrF(DownloadedSize), Int64ToStrF(TotalSize)]));
      end;
    end;
end;

function TYTD.DownloadURLsFromFileList(const FileName: string): integer;
var T: TextFile;
    s: string;
begin
  Result := 0;
  AssignFile(T, FileName);
  Reset(T);
  try
    while not Eof(T) do
      begin
      Readln(T, s);
      s := Trim(s);
      if s <> '' then
        if DownloadURL(s) then
          Inc(Result);
      end;
  finally
    CloseFile(T);
    end;
end;

function TYTD.DoDownload(Downloader: TDownloader): boolean;
begin
  Result := False;
  try
    fLastProgressPercentage := -1;
    Downloader.DestinationPath := DestinationPath;
    Downloader.OnProgress := DownloaderProgress;
    if Downloader.Prepare then
      begin
      Write('    Video title: '); WriteColored(ccWhite, Downloader.Name); Writeln;
      Write('      File name: '); WriteColored(ccWhite, Downloader.FileName); Writeln;
      Result := Downloader.Download;
      if fLastProgressPercentage >= 0 then
        Writeln;
      if Result then
        begin
        WriteColored(ccWhite, '    SUCCESS.');
        Writeln;
        Writeln;
        end
      else
        ShowError('    ERROR: ' + Downloader.LastErrorMsg);
      end
    else
      ShowError('    ERROR: ' + Downloader.LastErrorMsg);
  except
    on E: EAbort do
      begin
      ShowError('    ABORTED BY USER');
      Raise;
      end;
    on E: Exception do
      begin
      ShowError('ERROR ' + E.ClassName + ': ' + E.Message);
      Result := False;
      end;
    end;
end;

function TYTD.DownloadURL(const URL: string): boolean;
begin
  Result := False;
  Write('Url: ');
  WriteColored(ccLightCyan, URL);
  Writeln;
  DownloadClassifier.URL := URL;
  if DownloadClassifier.Downloader <> nil then
    Result := DoDownload(DownloadClassifier.Downloader)
  else
    ShowError('Unknown URL.');
end;

end.
