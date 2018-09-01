program YTD;
{$INCLUDE 'ytd.inc'}

{$IFDEF CLI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}
{%File 'ytd.version'}
{%File 'ytd.inc'}

uses
  SysUtils,
  uConsoleApp,
  uYTD in 'uYTD.pas',
  Windows,
  Forms,
  uDownloadListItem in 'uDownloadListItem.pas',
  uDownloadThread in 'uDownloadThread.pas',
  uYTDGUI in 'uYTDGUI.pas' {FormYTD},
  uDownloadList in 'uDownloadList.pas',
  uDownloadClassifier in 'uDownloadClassifier.pas',
  uYTDregexp in 'uYTDregexp.pas',
  uDownloader in 'uDownloader.pas',
  uCommonDownloader in 'uCommonDownloader.pas',
  uYouTubeDownloader in 'uYouTubeDownloader.pas',
  uNJoyDownloader in 'uNJoyDownloader.pas',
  uBlipTvDownloader in 'uBlipTvDownloader.pas',
  uBlipTvDownloaderV2 in 'uBlipTvDownloaderV2.pas',
  uStreamDownloader in 'uStreamDownloader.pas',
  uIPrimaDownloader in 'uIPrimaDownloader.pas';

begin
  {$IFNDEF NO_GUI}
  if (ParamCount <= 0) then
    begin
    FreeConsole;
    Application.Initialize;
    Application.Title := 'YouTube Downloader';
    Application.CreateForm(TFormYTD, FormYTD);
  Application.Run;
    end
  else
  {$ENDIF}
    begin
    {$IFNDEF NO_CLI}
    ExecuteConsoleApp(TYTD);
    if DebugHook <> 0 then
      begin
      Writeln;
      Write('Press any key to quit.');
      Readln;
      end;
    {$ENDIF}
    end;
end.

