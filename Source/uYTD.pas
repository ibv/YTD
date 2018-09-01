unit uYTD;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, FileCtrl,
  PCRE,
  uConsoleApp,
  uDownloader, uCommonDownloader, uDownloadClassifier;

type
  TOverwriteMode = (omNever, omAlways, omRename, omAsk);
  
  TYTD = class(TConsoleApp)
    private
      fLastProgressPercentage: int64;
      fDownloadClassifier: TDownloadClassifier;
      fDestinationPath: string;
      fOverwriteMode: TOverwriteMode;
      fUrlList: TStringList;
    protected
      function AppTitle: string; override;
      function AppVersion: string; override;
      function DoExecute: boolean; override;
      procedure ShowSyntax(const Error: string = ''); override;
      procedure ParamInitialize; override;
      property UrlList: TStringList read fUrlList;
    protected
      function DoDownload(Downloader: TDownloader): boolean; virtual;
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
      procedure DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      procedure DownloaderMoreUrls(Sender: TObject; const Url: string); virtual;
      function DownloadUrlList: integer; virtual;
      function DownloadURL(const URL: string): boolean; virtual;
      function DownloadURLsFromFileList(const FileName: string): integer; virtual;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
    public
      constructor Create; override;
      destructor Destroy; override;
      property DestinationPath: string read fDestinationPath write fDestinationPath;
      property OverwriteMode: TOverwriteMode read fOverwriteMode write fOverwriteMode;
    end;

implementation

{ TYTD }

constructor TYTD.Create;
begin
  inherited;
  fDownloadClassifier := TDownloadClassifier.Create;
  fOverwriteMode := omAsk;
  fUrlList := TStringList.Create;
end;

destructor TYTD.Destroy;
begin
  FreeAndNil(fDownloadClassifier);
  FreeAndNil(fUrlList);
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
//var i: integer;
begin
  inherited;
  WriteColored(ccWhite, '<arg> [<arg>] ...');
  Writeln;
  Writeln;
  WriteColored(ccWhite, ' -h, -?'); Writeln(' ...... Show this help screen.');
  WriteColored(ccWhite, ' -i <file>'); Writeln(' ... Load URL list from <file> (one URL per line).');
  WriteColored(ccWhite, ' -o <path>'); Writeln(' ... Store files to <path> (default is current directory).');
  WriteColored(ccWhite, ' -n'); Writeln(' .......... Never overwrite existing files.');
  WriteColored(ccWhite, ' -a'); Writeln(' .......... Always overwrite existing files.');
  WriteColored(ccWhite, ' -r'); Writeln(' .......... Rename files to a new name if they already exist.');
  WriteColored(ccWhite, ' -k'); Writeln(' .......... Ask what to do with existing files (default).');
  Writeln;
  WriteColored(ccWhite, ' <url>'); Writeln(' ....... URL to download.');
  {
  Writeln('               Supported:');
  for i := 0 to Pred(DownloadClassifier.ProviderCount) do
    begin
    WriteColored(ccWhite, '                 ' + DownloadClassifier.Providers[i].Provider);
    Writeln;
    end;
  }
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
        else if (Param = '-n') then
          OverwriteMode := omNever
        else if (Param = '-a') then
          OverwriteMode := omAlways
        else if (Param = '-r') then
          OverwriteMode := omRename
        else if (Param = '-k') then
          OverwriteMode := omAsk
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
  if TotalSize >= 1 then
    begin
    Proc := 1000 * DownloadedSize div TotalSize;
    if (not StdOutRedirected) and (Proc <> fLastProgressPercentage) then
      begin
      fLastProgressPercentage := Proc;
      n := Proc div (1000 div ProgressBarLength);
      ProgressBar := EmptyProgressBar;
      for i := 1 to n do
        ProgressBar[i] := '#';     
      Write(Format('  Downloading: <%s> %d.%d%% (%s/%s)'#13, [ProgressBar, Proc div 10, Proc mod 10, Int64ToStrF(DownloadedSize), Int64ToStrF(TotalSize)]));
      end;
    end;
end;

function TYTD.DoDownload(Downloader: TDownloader): boolean;
begin
  Result := False;
  try
    fLastProgressPercentage := -1;
    Downloader.DestinationPath := DestinationPath;
    Downloader.OnProgress := DownloaderProgress;
    Downloader.OnFileNameValidate := DownloaderFileNameValidate;
    Downloader.OnMoreUrls := DownloaderMoreUrls;
    if Downloader.Prepare {$IFDEF MULTIDOWNLOADS} and Downloader.First {$ENDIF} then
      begin
      {$IFDEF MULTIDOWNLOADS}
      repeat
      {$ENDIF}
      Write('  Video title: '); WriteColored(ccWhite, Downloader.Name); Writeln;
      Write('    File name: '); WriteColored(ccWhite, Downloader.FileName); Writeln;
      if Downloader is TCommonDownloader then
        Write('  Content URL: '); WriteColored(ccWhite, TCommonDownloader(Downloader).ContentUrl); Writeln;
      Result := Downloader.ValidateFileName and Downloader.Download;
      if fLastProgressPercentage >= 0 then
        Writeln;
      if Result then
        begin
        WriteColored(ccWhite, '  SUCCESS.');
        Writeln;
        Writeln;
        end
      else
        ShowError('  ERROR: ' + Downloader.LastErrorMsg);
      {$IFDEF MULTIDOWNLOADS}
      until (not Result) or (not Downloader.Next);
      {$ENDIF}
      end
    else
      ShowError('  ERROR: ' + Downloader.LastErrorMsg);
  except
    on E: EAbort do
      begin
      ShowError('  ABORTED BY USER');
      Raise;
      end;
    on E: Exception do
      begin
      ShowError('ERROR ' + E.ClassName + ': ' + E.Message);
      Result := False;
      end;
    end;
end;

function TYTD.DownloadUrlList: integer;
begin
  Result := 0;
  while UrlList.Count > 0 do
    begin
    WriteColored(ccLightCyan, UrlList[0]);
    Writeln;
    DownloadClassifier.URL := UrlList[0];
    if DownloadClassifier.Downloader = nil then
      ShowError('Unknown URL.')
    else
      if DoDownload(DownloadClassifier.Downloader) then
        Inc(Result);
    UrlList.Delete(0);
    end;
end;

function TYTD.DownloadURL(const URL: string): boolean;
begin
  UrlList.Add(URL);
  Result := DownloadUrlList > 0;
end;

function TYTD.DownloadURLsFromFileList(const FileName: string): integer;
var T: TextFile;
    s: string;
begin
  AssignFile(T, FileName);
  Reset(T);
  try
    while not Eof(T) do
      begin
      Readln(T, s);
      s := Trim(s);
      if s <> '' then
        UrlList.Add(s);
      end;
  finally
    CloseFile(T);
    end;
  Result := DownloadUrlList;
end;

procedure TYTD.DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean);
var FilePath, Answer: string;

    function AutoRename(var FileName: string): boolean;
      var FileNameBase, FileNameExt: string;
          Index: integer;
      begin
        Index := 1;
        FileNameExt := ExtractFileExt(FileName);
        FileNameBase := ChangeFileExt(FileName, '');
        repeat
          FileName := Format('%s%s.%d%s', [FilePath, FileNameBase, Index, FileNameExt]);
          Inc(Index);
        until not FileExists(FileName);
        Result := True;
      end;

begin
  FilePath := (Sender as TDownloader).DestinationPath;
  if FileExists(FilePath + FileName) then
    case OverwriteMode of
      omNever:
        Valid := False;
      omAlways:
        Valid := True;
      omRename:
        begin
        Valid := AutoRename(FileName);
        if Valid then
          begin
          Write('    File name: '); WriteColored(ccWhite, FileName); Writeln;
          end;
        end;
      omAsk:
        begin
        repeat
          Write('  File ');
          WriteColored(ccWhite, FileName);
          Writeln(' already exists.');
          Write('  Do you want to: ');
          WriteColored(ccLightCyan, '[S]'); Write('kip it, ');
          WriteColored(ccLightCyan, '[O]'); Write('verwrite it, or ');
          WriteColored(ccLightCyan, '[R]'); Write('ename it? ');
          Readln(Answer);
          if Answer <> '' then
            case Upcase(Answer[1]) of
              'S':
                begin
                Valid := False;
                Break;
                end;
              'O':
                begin
                Valid := True;
                Break;
                end;
              'R':
                begin
                Write('  New filename: ');
                Readln(Answer);
                if Answer <> '' then
                  begin
                  FileName := Answer;
                  Valid := True;
                  if not FileExists(FilePath + Answer) then
                    Break;
                  end;
                end;
              else
                ShowError('Incorrect answer.');
              end;
        until False;
        end;
      end;
end;

procedure TYTD.DownloaderMoreUrls(Sender: TObject; const Url: string);
begin
  UrlList.Add(Url);
end;

end.
