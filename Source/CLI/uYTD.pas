unit uYTD;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, {$IFNDEF FPC} FileCtrl, {$ENDIF}
  PCRE,
  uConsoleApp, uOptions,
  uDownloader, uCommonDownloader,
  uPlaylistDownloader, listHTML, listHTMLfile,
  uDownloadClassifier;

type
  TYTD = class(TConsoleApp)
    private
      fLastProgressPercentage: int64;
      fDownloadClassifier: TDownloadClassifier;
      fHtmlPlaylist: TPlaylist_HTML;
      fHtmlFilePlaylist: TPlaylist_HTMLfile;
      fUrlList: TStringList;
      fOptions: TYTDOptions;
    protected
      function AppTitle: string; override;
      function AppVersion: string; override;
      function DoExecute: boolean; override;
      procedure ShowSyntax(const Error: string = ''); override;
      procedure ParamInitialize; override;
      property UrlList: TStringList read fUrlList;
    protected
      function DoDownload(const Url: string; Downloader: TDownloader): boolean; virtual;
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
      procedure DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      function DownloadUrlList: integer; virtual;
      function DownloadURL(const URL: string): boolean; virtual;
      function DownloadURLsFromFileList(const FileName: string): integer; virtual;
      function DownloadURLsFromHTML(const Source: string): integer; virtual;
      procedure ShowProviders; virtual;
      procedure ShowVersion; virtual;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
      property HtmlPlaylist: TPlaylist_HTML read fHtmlPlaylist;
      property HtmlFilePlaylist: TPlaylist_HTMLfile read fHtmlFilePlaylist;
    public
      constructor Create; override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions;
    end;

implementation

{ TYTD }

constructor TYTD.Create;
begin
  inherited;
  fOptions := TYTDOptions.Create;
  fDownloadClassifier := TDownloadClassifier.Create;
  fHtmlPlaylist := TPlaylist_HTML.Create('');
  fHtmlFilePlaylist := TPlaylist_HTMLfile.Create('');
  fUrlList := TStringList.Create;
end;

destructor TYTD.Destroy;
begin
  FreeAndNil(fOptions);
  FreeAndNil(fDownloadClassifier);
  FreeAndNil(fHtmlPlaylist);
  FreeAndNil(fHtmlFilePlaylist);
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
  WriteColored(ccWhite, '<arg> [<arg>] ...'); Writeln;
  Writeln;
  WriteColored(ccWhite, ' -h, -?'); Writeln(' ...... Show this help screen.');
  WriteColored(ccWhite, ' -i <file>'); Writeln(' ... Load URL list from <file> (one URL per line).');
  WriteColored(ccWhite, ' -o <path>'); Writeln(' ... Store files to <path> (default is current directory).');
  WriteColored(ccWhite, ' -e <file>'); Writeln(' ... Save failed URLs to <file>.');
  WriteColored(ccWhite, ' -s <src>'); Writeln(' .... Load links from a HTML source. <src> can be a file or an URL.');
  WriteColored(ccWhite, ' -n'); Writeln(' .......... Never overwrite existing files.');
  WriteColored(ccWhite, ' -a'); Writeln(' .......... Always overwrite existing files.');
  WriteColored(ccWhite, ' -r'); Writeln(' .......... Rename files to a new name if they already exist.');
  WriteColored(ccWhite, ' -k'); Writeln(' .......... Ask what to do with existing files (default).');
  WriteColored(ccWhite, ' -l'); Writeln(' .......... List all available providers.');
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

procedure TYTD.ShowProviders;
{$DEFINE GROUPPED}
var i: integer;
begin
  Writeln;
  WriteColored(ccWhite, 'Available providers:'); Writeln;
  for i := 0 to Pred({$IFDEF GROUPPED} DownloadClassifier.NameCount {$ELSE} DownloadClassifier.ProviderCount {$ENDIF}) do
    begin
    Write('  - ');
    WriteColored(ccLightCyan, {$IFDEF GROUPPED} DownloadClassifier.Names[i] {$ELSE} DownloadClassifier.Providers[i].Provider {$ENDIF});
    Writeln(' (' + {$IFDEF GROUPPED} DownloadClassifier.NameClasses[i] {$ELSE} DownloadClassifier.Providers[i].ClassName {$ENDIF} + ')');
    end;
  Write('Total: ');
  WriteColored(ccWhite, IntToStr({$IFDEF GROUPPED} DownloadClassifier.NameCount {$ELSE} DownloadClassifier.ProviderCount {$ENDIF}));
  Writeln(' providers.');
  Writeln;
end;

procedure TYTD.ShowVersion;
var Url, Version: string;
begin
  Write('Current version: '); WriteColored(ccWhite, AppVersion); Writeln;
  Write('Newest version:  ');
  if not Options.GetNewestVersion(Version, Url) then
    WriteColored(ccLightRed, 'check failed')
  else if Version <= AppVersion then
    WriteColored(ccWhite, Version)
  else
    begin
    WriteColored(ccLightCyan, Version); Writeln;
    Write('Download URL:    '); WriteColored(ccWhite, Url);
    end;
  Writeln;
  Writeln;
end;

procedure TYTD.ParamInitialize;
begin
  inherited;
  if Options <> nil then
    Options.Init;
end;

function TYTD.DoExecute: boolean;
var Param: string;
    n: integer;
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
          begin
          ShowSyntax;
          Result := True;
          end
        else if (Param = '-l') then
          begin
          ShowProviders;
          Result := True;
          end
        else if (Param = '-v') then
          begin
          ShowVersion;
          Result := True;
          end
        else if (Param = '-n') then
          Options.OverwriteMode := omNever
        else if (Param = '-a') then
          Options.OverwriteMode := omAlways
        else if (Param = '-r') then
          Options.OverwriteMode := omRename
        else if (Param = '-k') then
          Options.OverwriteMode := omAsk
        else if (Param = '-e') then
          if ParamGetNext(Param) then
            begin
            Options.ErrorLog := Param;
            if FileExists(Param) then
              DeleteFile(Param);
            end
          else
            ShowSyntax('With -e a filename must be provided.')
        else if (Param = '-s') then
          if ParamGetNext(Param) then
            begin
            n := DownloadURLsFromHTML(Param);
            if n > 0 then
              Result := True
            else if n = 0 then
              ShowSyntax('HTML source "%s" doesn''t contain any useful links.', [Param])
            else
              ShowSyntax('HTML source "%s" not found.', [Param]);
            end
          else
            ShowSyntax('With -h a filename or an URL must be provided.')
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
              Options.DestinationPath := Param
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
    Result := Sign + IntToStr(Value) + Result;
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

function TYTD.DoDownload(const Url: string; Downloader: TDownloader): boolean;

  procedure ShowDownloadError(const Url, Msg: string);
    begin
      ShowError('  ERROR: ' + Msg);
      if Options.ErrorLog <> '' then
        Log(Options.ErrorLog, 'FAILED "%s": %s', [Url, Msg]);
    end;

var Playlist: TPlaylistDownloader;
    i: integer;
begin
  Result := False;
  try
    Downloader.InitOptions(Options);
    if Downloader is TPlaylistDownloader then
      begin
      PlayList := TPlaylistDownloader(Downloader);
      if Playlist.Prepare then
        begin
        for i := 0 to Pred(Playlist.Count) do
          begin
          Result := True;
          UrlList.Add(Playlist[i]);
          Write('  Playlist item: ');
          if Playlist.Names[i] <> '' then
            begin
            WriteColored(ccWhite, Playlist.Names[i]);
            Writeln;
            Write('            URL: ');
            end;
          WriteColored(ccWhite, Playlist[i]);
          Writeln;
          end;
        end
      else
        ShowDownloadError(Url, Downloader.LastErrorMsg);
      end
    else
      begin
      fLastProgressPercentage := -1;
      Downloader.DestinationPath := Options.DestinationPath;
      Downloader.OnProgress := DownloaderProgress;
      Downloader.OnFileNameValidate := DownloaderFileNameValidate;
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
          ShowDownloadError(Url, Downloader.LastErrorMsg);
        {$IFDEF MULTIDOWNLOADS}
        until (not Result) or (not Downloader.Next);
        {$ENDIF}
        end
      else
        ShowDownloadError(Url, Downloader.LastErrorMsg);
      end;
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
      if DoDownload(DownloadClassifier.URL, DownloadClassifier.Downloader) then
        Inc(Result);
    UrlList.Delete(0);
    end;
end;

function TYTD.DownloadURL(const URL: string): boolean;
begin
  UrlList.Add(URL);
  Result := DownloadUrlList > 0;
end;

function TYTD.DownloadURLsFromHTML(const Source: string): integer;
const HTTP_PREFIX = 'http://';
      HTTPS_PREFIX = 'https://';
var Playlist: TPlaylist_HTML;
begin
  if (AnsiCompareText(Copy(Source, 1, Length(HTTP_PREFIX)), HTTP_PREFIX) = 0) or (AnsiCompareText(Copy(Source, 1, Length(HTTPS_PREFIX)), HTTPS_PREFIX) = 0) then
    Playlist := HtmlPlaylist
  else
    Playlist := HtmlFilePlaylist;
  Playlist.MovieID := Source;
  if DoDownload(Source, Playlist) then
    Result := DownloadUrlList
  else
    Result := -1;
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
    case Options.OverwriteMode of
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
  if Valid and FileExists(FilePath + FileName) then
    DeleteFile(FilePath + FileName);
end;

end.
