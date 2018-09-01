program YTD;
{$APPTYPE CONSOLE}
{$R *.res}
{%File 'ytd.version'}

uses
  SysUtils,
  Windows,
  Forms,
  uConsoleApp,
  uYTD in 'uYTD.pas',
  uYTDregexp in 'uYTDregexp.pas',
  uDownloader in 'uDownloader.pas',
  uYouTubeDownloader in 'uYouTubeDownloader.pas',
  uNJoyDownloader in 'uNJoyDownloader.pas',
  uBlipTvDownloader in 'uBlipTvDownloader.pas',
  uCommonDownloader in 'uCommonDownloader.pas',
  uBlipTvDownloaderV2 in 'uBlipTvDownloaderV2.pas',
  uDownloadClassifier in 'uDownloadClassifier.pas',
  uDownloadListItem in 'uDownloadListItem.pas',
  uDownloadThread in 'uDownloadThread.pas',
  uYTDGUI in 'uYTDGUI.pas' {FormYTD},
  uDownloadList in 'uDownloadList.pas',
  uStreamDownloader in 'uStreamDownloader.pas';

begin
  if (ParamCount <= 0) then
    begin
    FreeConsole;
    Application.Initialize;
    Application.Title := 'YouTube Downloader';
    Application.CreateForm(TFormYTD, FormYTD);
  Application.Run;
    end
  else
    begin
    ExecuteConsoleApp(TYTD);
    if DebugHook <> 0 then
      begin
      Writeln;
      Write('Press any key to quit.');
      Readln;
      end;
    end;
end.

