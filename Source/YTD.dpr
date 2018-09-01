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
  uDownloader in 'uDownloader.pas',
  uCommonDownloader in 'uCommonDownloader.pas',
  downYouTube in 'Downloaders\downYouTube.pas',
  downNJoy in 'Downloaders\downNJoy.pas',
  downBlipTv in 'Downloaders\downBlipTv.pas',
  downBlipTvV2 in 'Downloaders\downBlipTvV2.pas',
  downStream in 'Downloaders\downStream.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  uRtmpDownloader in 'uRtmpDownloader.pas',
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
  uHttpDownloader in 'uHttpDownloader.pas',
  downNova in 'Downloaders\downNova.pas',
  downCT in 'Downloaders\downCT.pas',
  uMSDownloader in 'uMSDownloader.pas',
  uExternalDownloader in 'uExternalDownloader.pas',
  downTVcom in 'Downloaders\downTVcom.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downCT_Port in 'Downloaders\downCT_Port.pas',
  listYouTube in 'Playlists\listYouTube.pas',
  uPlaylistDownloader in 'uPlaylistDownloader.pas',
  listHTML in 'Playlists\listHTML.pas',
  listHTMLfile in 'Playlists\listHTMLfile.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  listYouTubePage in 'Playlists\listYouTubePage.pas';

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

