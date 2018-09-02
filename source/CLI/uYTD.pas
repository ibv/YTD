(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit uYTD;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, {$IFNDEF DELPHIXE2_UP} FileCtrl, {$ENDIF}
  uConsoleApp, uOptions, uLanguages, uMessages, uFunctions,
  uDownloader, uCommonDownloader,
  uPlaylistDownloader, listHTML, listHTMLfile,
  {$IFDEF SETUP}
  uHttpDirectDownloader, uSetup,
  {$ENDIF}
  uDownloadClassifier;

type
  TYTD = class(TConsoleApp)
    protected
      function AppTitle: string; override;
      function AppVersion: string; override;
      function DoExecute: integer; override;
      procedure ShowSyntax(const Error: string = ''); override;
      procedure ParamInitialize; override;
    private
      fNextProgressUpdate: DWORD;
      fDownloadClassifier: TDownloadClassifier;
      fHtmlPlaylist: TPlaylist_HTML;
      fHtmlFilePlaylist: TPlaylist_HTMLfile;
      fUrlList: TStringList;
      fOptions: TYTDOptions;
      fPrepareOnly: boolean;
    protected
      function DoDownload(const Url: string; Downloader: TDownloader): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure DownloaderFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadUrlList: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadURL(const URL: string): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadURLsFromFileList(const FileName: string): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function DownloadURLsFromHTML(const Source: string): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure ShowProviders; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function ShowVersion(DoUpgrade: boolean): boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      property UrlList: TStringList read fUrlList;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
      property HtmlPlaylist: TPlaylist_HTML read fHtmlPlaylist;
      property HtmlFilePlaylist: TPlaylist_HTMLfile read fHtmlFilePlaylist;
    public
      constructor Create; override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions;
    end;

const
  RESCODE_DOWNLOADFAILED = 1;
  RESCODE_NOURLS = 2;
  RESCODE_BADPARAMS = 3;
  RESCODE_BADDATA = 4;
  RESCODE_BADUPGRADE = 5;

implementation

{ TYTD }

constructor TYTD.Create;
begin
  inherited;
  fOptions := TYTDOptions.Create;
  UseLanguage(Options.Language);
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
  Result := APPLICATION_TITLE;
end;

function TYTD.AppVersion: string;
begin
  Result := APPLICATION_VERSION;
end;

procedure TYTD.ShowSyntax(const Error: string);
begin
  inherited;
  WriteColored(ccWhite, '<arg> [<arg>] ...'); Writeln; // Intentionally no _(...) - this should not be translated
  Writeln;
  Writeln(_('Command-line version options:'));
  WriteColored(ccWhite, ' -h, -?'); Writeln(_(' ...... Show this help screen.')); // CLI: Help for -h/-? command line argument
  WriteColored(ccWhite, ' -i <file>'); Writeln(_(' ... Load URL list from <file> (one URL per line).')); // CLI: Help for -i command line argument
  WriteColored(ccWhite, ' -o <path>'); Writeln(_(' ... Store files to <path> (default is current directory).')); // CLI: Help for -o command line argument
  WriteColored(ccWhite, ' -e <file>'); Writeln(_(' ... Save failed URLs to <file>.')); // CLI: Help for -e command line argument
  WriteColored(ccWhite, ' -s <src>'); Writeln(_(' .... Load links from a HTML source. <src> can be a file or an URL.')); // CLI: Help for -s command line argument
  WriteColored(ccWhite, ' -n'); Writeln(_(' .......... Never overwrite existing files.')); // CLI: Help for -n command line argument
  WriteColored(ccWhite, ' -a'); Writeln(_(' .......... Always overwrite existing files.')); // CLI: Help for -a command line argument
  WriteColored(ccWhite, ' -r'); Writeln(_(' .......... Rename files to a new name if they already exist.')); // CLI: Help for -r command line argument
  WriteColored(ccWhite, ' -k'); Writeln(_(' .......... Ask what to do with existing files (default).')); // CLI: Help for -k command line argument
  WriteColored(ccWhite, ' -l'); Writeln(_(' .......... List all available providers.')); // CLI: Help for -l command line argument
  WriteColored(ccWhite, ' -v'); Writeln(_(' .......... Test for updated version of YTD.')); // CLI: Help for -v command line argument
  {$IFDEF SETUP}
  WriteColored(ccWhite, ' -u'); Writeln(_(' .......... Test for a new version and upgrade if possible.')); // CLI: Help for -u command line argument
  {$ENDIF}
  WriteColored(ccWhite, ' -ah[-]'); Writeln(_(' ...... [Don''t] Automatically try HTML parser for unknown URLs.')); // CLI: Help for -ah command line argument
  WriteColored(ccWhite, ' -nd'); Writeln(_(' ......... Don''t actually download, just show the parameters.')); // CLI: Help for -nd command line argument
  Writeln;
  {$IFDEF SINGLEINSTANCE}
  {$IFDEF GUI}
  Writeln(_('Graphical version options:'));
  WriteColored(ccWhite, ' --gui'); Writeln(_(' ....... Must be used if there are GUI arguments.'));
  WriteColored(ccWhite, ' -new'); Writeln(_(' ........ Run a new instance of YTD.'));
  Writeln;
  {$ENDIF}
  {$ENDIF}
  Writeln(_('Common options:'));
  WriteColored(ccWhite, ' <url>'); Writeln(_(' ....... URL to download.')); // CLI: Help for <url> command line argument
  Writeln;
  Writeln;
end;

procedure TYTD.ShowProviders;
{$DEFINE GROUPPED}
var i: integer;
begin
  Writeln;
  WriteColored(ccWhite, _('Available providers:')); Writeln; // CLI: Title for the list of providers (-l command line argument)
  for i := 0 to Pred({$IFDEF GROUPPED} DownloadClassifier.NameCount {$ELSE} DownloadClassifier.ProviderCount {$ENDIF}) do
    begin
    Write('  - ');
    WriteColored(ccLightCyan, {$IFDEF GROUPPED} DownloadClassifier.Names[i] {$ELSE} DownloadClassifier.Providers[i].Provider {$ENDIF});
    Writeln(' (' + {$IFDEF GROUPPED} DownloadClassifier.NameClasses[i] {$ELSE} DownloadClassifier.Providers[i].ClassName {$ENDIF} + ')');
    end;
  Write(_('Total: ')); // CLI: The "Total: " part of "Total: 123 providers." Note the ending space
  WriteColored(ccWhite, IntToStr({$IFDEF GROUPPED} DownloadClassifier.NameCount {$ELSE} DownloadClassifier.ProviderCount {$ENDIF}));
  Writeln(_(' providers.')); // CLI: The " providers" part of "Total: 123 providers." Note the starting space
  Writeln;
end;

function TYTD.ShowVersion(DoUpgrade: boolean): boolean;
var Url, Version: string;
    {$IFDEF SETUP}
    FileName: string;
    {$ENDIF}
begin
  Write(_('Current version: ')); WriteColored(ccWhite, AppVersion); Writeln; // CLI: Note: pad with spaces to the same length as "Newest version:"
  Write(_('Newest version:  ')); // CLI: Note: pad with spaces to the same length as "Current version:"
  Result := Options.GetNewestVersion(Version, Url);
  if not Result then
    WriteColored(ccLightRed, _('check failed')) // CLI: Couldn't check for a newer version
  else if IsNewerVersion(Version) then
    begin
    WriteColored(ccLightCyan, Version); Writeln;
    {$IFDEF SETUP}
    if DoUpgrade then
      begin
      Result := False;
      if Options.DownloadNewestVersion(FileName) then
        if (AnsiCompareText(ExtractFileExt(FileName), '.exe') = 0) and Run(FileName, Format('%s "%s"', [SETUP_PARAM_UPGRADE, ExtractFilePath(ParamStr(0))])) then
          begin
          Write(MSG_UPGRADING);
          Result := True;
          end
        else
          WriteColored(ccLightRed, Format(MSG_FAILED_TO_UPGRADE, [FileName])) // CLI: Failed to start the update code
      else
        begin
        WriteColored(ccLightRed, MSG_FAILED_TO_DOWNLOAD_UPGRADE); // CLI: Couldn't download the upgraded version
        WriteColored(ccWhite, Url);
        end;
      end
    else
    {$ENDIF}
      begin
      Write(_('Download URL:    ')); WriteColored(ccWhite, Url); // CLI: Note: pad with spaces to the same length as "Current version:"
      end;
    end
  else
    WriteColored(ccWhite, Version);
  Writeln;
  Writeln;
end;

procedure TYTD.ParamInitialize;
begin
  inherited;
  if Options <> nil then
    Options.Init;
  fPrepareOnly := False;
end;

function TYTD.DoExecute: integer;
var Param: string;
    n: integer;
begin
  if ParamCount = 0 then
    begin
    ShowSyntax;
    Result := RESCODE_OK;
    end
  else
    begin
    Result := RESCODE_NOURLS;
    ParamInitialize;
    while ParamGetNext(Param) do
      if Param[1] = '-' then
        begin
        if (Param = '-?') or (Param = '-h') then
          begin
          ShowSyntax;
          if Result in [RESCODE_OK, RESCODE_NOURLS] then
            Result := RESCODE_OK;
          Break;
          end
        else if (Param = '-l') then
          begin
          ShowProviders;
          if Result in [RESCODE_OK, RESCODE_NOURLS] then
            Result := RESCODE_OK;
          Break;
          end
        else if (Param = '-v') {$IFDEF SETUP} or (Param = '-u') {$ENDIF} then
          begin
          if ShowVersion( {$IFDEF SETUP} Param = '-u' {$ELSE} False {$ENDIF} ) then
            begin
            if Result in [RESCODE_OK, RESCODE_NOURLS] then
              Result := RESCODE_OK;
            end
          else
            Result := RESCODE_BADUPGRADE;
          Break;
          end
        else if (Param = '-n') then
          Options.OverwriteMode := omNever
        else if (Param = '-a') then
          Options.OverwriteMode := omAlways
        else if (Param = '-r') then
          Options.OverwriteMode := omRename
        else if (Param = '-k') then
          Options.OverwriteMode := omAsk
        else if (Param = '-nd') then
          fPrepareOnly := True
        else if (Param = '-e') then
          if ParamGetNext(Param) then
            begin
            Options.ErrorLog := Param;
            if FileExists(Param) then
              SysUtils.DeleteFile(Param);
            end
          else
            begin
            ShowSyntax(_('With -e a filename must be provided.')); // CLI: Error message for invalid command line argument
            Result := RESCODE_BADPARAMS;
            Break;
            end
        else if (Param = '-ah') then
          Options.AutoTryHtmlParser := True
        else if (Param = '-ah-') then
          Options.AutoTryHtmlParser := False
        else if (Param = '-s') then
          if ParamGetNext(Param) then
            begin
            n := DownloadURLsFromHTML(Param);
            if n = 0 then
              begin
              ShowSyntax(_('HTML source "%s" doesn''t contain any useful links.'), [Param]); // CLI: Error message for invalid command line argument
              Result := RESCODE_DOWNLOADFAILED;
              end
            else if n < 0 then
              begin
              ShowSyntax(_('HTML source "%s" not found.'), [Param]); // CLI: Error message for invalid command line argument
              Result := RESCODE_BADDATA;
              Break;
              end
            else
              if Result = RESCODE_NOURLS then
                Result := RESCODE_OK;
            end
          else
            begin
            ShowSyntax(_('With -s a filename or an URL must be provided.')); // CLI: Error message for invalid command line argument
            Result := RESCODE_BADPARAMS;
            Break;
            end
        else if (Param = '-i') then
          if ParamGetNext(Param) then
            if FileExists(Param) then
              if DownloadURLsFromFileList(Param) > 0 then
                begin
                if Result = RESCODE_NOURLS then
                  Result := RESCODE_OK;
                end
              else
                Result := RESCODE_DOWNLOADFAILED
            else
              begin
              ShowSyntax(_('URL list-file "%s" not found.'), [Param]); // CLI: Error message for invalid command line argument
              Result := RESCODE_BADDATA;
              Break;
              end
          else
            begin
            ShowSyntax(_('With -i a filename must be provided.')); // CLI: Error message for invalid command line argument
            Result := RESCODE_BADPARAMS;
            Break;
            end
        else if (Param = '-o') then
          if ParamGetNext(Param) then
            if DirectoryExists(Param) then
              Options.DestinationPath := Param
            else
              begin
              ShowSyntax(_('Destination directory "%s" not found.'), [Param]); // CLI: Error message for invalid command line argument
              Result := RESCODE_BADDATA;
              Break;
              end
          else
            begin
            ShowSyntax(_('With -o a directory name must be provided.')); // CLI: Error message for invalid command line argument
            Result := RESCODE_BADPARAMS;
            Break;
            end
        else
          begin
          ShowSyntax(_('Unknown parameter "%s".'), [Param]); // CLI: Error message for invalid command line argument
          Result := RESCODE_BADPARAMS;
          Break;
          end
        end
      else
        if DownloadURL(Param) then
          begin
          if Result = RESCODE_NOURLS then
            Result := RESCODE_OK;
          end
        else
          Result := RESCODE_DOWNLOADFAILED;
    if Result = RESCODE_NOURLS then
      ShowError(_('No valid URLs found.')); // CLI: Error message for invalid command line argument
    end;
end;

procedure TYTD.DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean);
const EmptyProgressBar = '                             ';
      ProgressBarLength = Length(EmptyProgressBar);
      NewLine = '  '#13;
var Proc: int64;
    i, n: integer;
    ProgressBar: string;
    Ticks: DWORD;
begin
  if (not StdOutRedirected) then
    begin
    Ticks := GetTickCount;
    if (fNextProgressUpdate = 0) or (Ticks > fNextProgressUpdate) or ((fNextProgressUpdate > $f0000000) and (Ticks < $10000000)) then
      begin
      fNextProgressUpdate := Ticks + 250; // 0.25 sec.
      if TotalSize > 0 then
        begin
        Proc := 1000 * DownloadedSize div TotalSize;
        n := Proc div (1000 div ProgressBarLength);
        ProgressBar := EmptyProgressBar;
        for i := 1 to n do
          ProgressBar[i] := '#';
        Write(Format(_('  Downloading: <%s> %d.%d%% (%s/%s)') + NewLine, [ProgressBar, Proc div 10, Proc mod 10, PrettySize(DownloadedSize), PrettySize(TotalSize)])); // CLI progress bar. %: Progress bar "graphics", Percent done (integer part), Percent done (fractional part), Downloaded size, Total size
        end
      else
        begin
        Write(Format(_('  Downloading: %s') + NewLine, [PrettySize(DownloadedSize)])); // CLI progress bar. %: Downloaded size
        end;
      end;
    end;
end;

function TYTD.DoDownload(const Url: string; Downloader: TDownloader): boolean;

  procedure ShowDownloadError(const Url, Msg: string);
    begin
      ShowError(_('  ERROR: ') + Msg);
      if Options.ErrorLog <> '' then
        Log(Options.ErrorLog, _('FAILED "%s": %s'), [Url, Msg]); // CLI: Error message to be written to the log file. %: URL, message
    end;

var Playlist: TPlaylistDownloader;
    i: integer;
begin
  Result := False;
  try
    Downloader.Options := Options;
    if Downloader is TPlaylistDownloader then
      begin
      PlayList := TPlaylistDownloader(Downloader);
      if Playlist.Prepare then
        begin
        for i := 0 to Pred(Playlist.Count) do
          begin
          Result := True;
          UrlList.Add(Playlist[i]);
          Write(_('  Playlist item: ')); // CLI: Title shown before playlist item's name. Pad with spaces to the same length as "URL:"
          if Playlist.Names[i] <> '' then
            begin
            WriteColored(ccWhite, Playlist.Names[i]);
            Writeln;
            Write(_('            URL: ')); // CLI: Title shown before playlist item's URL. Pad with spaces to the same length as "Playlist item:"
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
      fNextProgressUpdate := 0;
      Downloader.OnProgress := DownloaderProgress;
      Downloader.OnFileNameValidate := DownloaderFileNameValidate;
      if Downloader.Prepare {$IFDEF MULTIDOWNLOADS} and Downloader.First {$ENDIF} then
        begin
        {$IFDEF MULTIDOWNLOADS}
        repeat
        {$ENDIF}
          Write(_('  Media title: ')); WriteColored(ccWhite, Downloader.Name); Writeln; // CLI: Title shown before media title. Pad to the same length as "File name:'
          Write(_('    File name: ')); WriteColored(ccWhite, Downloader.FileName); Writeln; // CLI: Title shown before media file name. Pad to the same length as "Media title:'
          Write(_('  Content URL: ')); WriteColored(ccWhite, Downloader.ContentUrl); Writeln; // CLI: Title shown before media URL. Pad to the same length as "Media title:'
          if fPrepareOnly then
            Result := True
          else if not Downloader.ValidateFileName then
            begin
            Result := False;
            ShowDownloadError(Url, _('Download skipped.'));
            end
          else
            begin
            Result := Downloader.Download;
            if fNextProgressUpdate <> 0 then
              Writeln;
            if Result then
              begin
              WriteColored(ccWhite, _('  SUCCESS.')); // CLI: Media downloaded successfully
              Writeln;
              Writeln;
              end
            else
              ShowDownloadError(Url, Downloader.LastErrorMsg);
            end;
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
      ShowError(_('  ABORTED BY USER')); // CLI: User aborted the download
      Raise;
      end;
    on E: Exception do
      begin
      ShowError(_('ERROR %s: %s'), [E.ClassName, E.Message]); // CLI: Error. %: Exception type, Exception message
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
      ShowError(_('Unknown URL.')) // CLI: URL couldn't be assigned to any available downloader
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
  if not Result then
    if Options.AutoTryHtmlParser then
      begin
      DownloadClassifier.Url := URL;
      if DownloadClassifier.Downloader = nil then
        begin
        Writeln(_('Trying to parse as HTML page...'));
        Result := DownloadURLsFromHTML(URL) > 0;
        end;
      end;
end;

function TYTD.DownloadURLsFromHTML(const Source: string): integer;
var
  Playlist: TPlaylist_HTML;
begin
  if IsHttpProtocol(Source) then
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
var Answer: string;

    function AutoRename(var FileName: string): boolean;
      var FileNameBase, FileNameExt: string;
          Index: integer;
      begin
        Index := 1;
        FileNameExt := ExtractFileExt(FileName);
        FileNameBase := ChangeFileExt(FileName, '');
        repeat
          FileName := Format('%s.%d%s', [FileNameBase, Index, FileNameExt]);
          Inc(Index);
        until not FileExists(FileName);
        Result := True;
      end;

begin
  if FileExists(FileName) then
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
          Write(_('    File name: ')); WriteColored(ccWhite, FileName); Writeln; // CLI: File already exists, renaming it to a new filename
          end;
        end;
      omAsk:
        begin
        repeat
          Write(_('  File ')); // CLI: "File ... already exists. What do you want me to do?"
          WriteColored(ccWhite, FileName);
          Writeln(_(' already exists.'));
          Write(_('  Do you want to: '));
          WriteColored(ccLightCyan, '[S]'); Write(_('kip it, ')); // CLI: File already exists. [S]kip it
          WriteColored(ccLightCyan, '[O]'); Write(_('verwrite it, or ')); // CLI: File already exists. [O]verwrite it
          WriteColored(ccLightCyan, '[R]'); Write(_('ename it? ')); // CLI: File already exists. [R]ename it
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
                Write(_('  New filename: ')); // CLI: File already exists. Asking user to provide a new filename.
                Readln(Answer);
                if Answer <> '' then
                  begin
                  FileName := ExtractFilePath(FileName) + Answer;
                  Valid := True;
                  if not FileExists(FileName) then
                    Break;
                  end;
                end;
              else
                ShowError(_('Incorrect answer.'));  // CLI: File already exists. User answered something else than [S]kip, [O]verwrite or [R]ename
              end;
        until False;
        end;
      end;
  if Valid and FileExists(FileName) then
    SysUtils.DeleteFile(FileName);
end;

end.
