program YTD;
{$APPTYPE CONSOLE}
{$R *.res}
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
  uYTDGUI in 'Lib\Pepak\uYTDGUI.pas' {FormYTD};

begin
  if (ParamCount = 0) then
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

