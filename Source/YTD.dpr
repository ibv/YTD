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
  uDownloader_YouTube in 'uDownloader_YouTube.pas',
  uDownloader_NJoy in 'uDownloader_NJoy.pas',
  uDownloader_BlipTv in 'uDownloader_BlipTv.pas',
  uDownloader_BlipTvV2 in 'uDownloader_BlipTvV2.pas',
  uDownloader_Stream in 'uDownloader_Stream.pas',
  uDownloader_iPrima in 'uDownloader_iPrima.pas',
  uRtmpDownloader in 'uRtmpDownloader.pas',
  uDownloader_BarrandovTV in 'uDownloader_BarrandovTV.pas',
  uHttpDownloader in 'uHttpDownloader.pas',
  uDownloader_Nova in 'uDownloader_Nova.pas',
  uDownloader_CT in 'uDownloader_CT.pas',
  uMSDownloader in 'uMSDownloader.pas',
  uExternalDownloader in 'uExternalDownloader.pas',
  uDownloader_TVcom in 'uDownloader_TVcom.pas',
  uDownloader_Markiza in 'uDownloader_Markiza.pas',
  uDownloader_Rozhlas in 'uDownloader_Rozhlas.pas',
  uDownloader_CT_Port in 'uDownloader_CT_Port.pas',
  uDownloader_Playlist_YouTube in 'uDownloader_Playlist_YouTube.pas',
  uPlaylistDownloader in 'uPlaylistDownloader.pas';

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

