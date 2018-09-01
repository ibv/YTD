program YTD;
{$INCLUDE 'YTD.inc'}

{$IFDEF CLI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}
{%File 'YTD.version'}
{%File 'YTD.inc'}

uses
  SysUtils,
  Windows,
  Forms,
  // Command Line Version
  {$IFDEF CLI}
  uYTD in 'CLI\uYTD.pas',
  uConsoleApp,
  {$ENDIF}
  // GUI version
  {$IFDEF GUI}
  uYTDGUI in 'GUI\uYTDGUI.pas' {FormYTD},
  uYTDAbout in 'GUI\uYTDAbout.pas' {FormAbout},
  uDownloadList in 'GUI\uDownloadList.pas',
  uDownloadListItem in 'GUI\uDownloadListItem.pas',
  uDownloadThread in 'GUI\uDownloadThread.pas',
  {$ENDIF}
  // Base objects and units
  uMessages in 'uMessages.pas',
  uOptions in 'uOptions.pas',
  uDownloadClassifier in 'Base\uDownloadClassifier.pas',
  uDownloader in 'Base\uDownloader.pas',
  uCommonDownloader in 'Base\uCommonDownloader.pas',
  uHttpDownloader in 'Base\uHttpDownloader.pas',
  uExternalDownloader in 'Base\uExternalDownloader.pas',
  uMSDownloader in 'Base\uMSDownloader.pas',
  uRtmpDownloader in 'Base\uRtmpDownloader.pas',
  uPlaylistDownloader in 'Base\uPlaylistDownloader.pas',
  // Downloaders
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
  downBlipTv in 'Downloaders\downBlipTv.pas',
  downBlipTvV2 in 'Downloaders\downBlipTvV2.pas',
  downBofunk in 'Downloaders\downBofunk.pas',
  downBreak in 'Downloaders\downBreak.pas',
  downCekniTo in 'Downloaders\downCekniTo.pas',
  downClipfish in 'Downloaders\downClipfish.pas',
  downClipfishV2 in 'Downloaders\downClipfishV2.pas',
  downCollegeHumor in 'Downloaders\downCollegeHumor.pas',
  downCrunchyRoll in 'Downloaders\downCrunchyRoll.pas',
  downCT in 'Downloaders\downCT.pas',
  downCT_Port in 'Downloaders\downCT_Port.pas',
  downDailyHaha in 'Downloaders\downDailyHaha.pas',
  downDailyMotion in 'Downloaders\downDailyMotion.pas',
  downEbaumsWorld in 'Downloaders\downEbaumsWorld.pas',
  downESPN in 'Downloaders\downESPN.pas',
  downFileCabi in 'Downloaders\downFileCabi.pas',
  downFreeSk in 'Downloaders\downFreeSk.pas',
  downFreeRide in 'Downloaders\downFreeRide.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMetaCafe in 'Downloaders\downMetaCafe.pas',
  downMySpace in 'Downloaders\downMySpace.pas',
  downNJoy in 'Downloaders\downNJoy.pas',
  downNova in 'Downloaders\downNova.pas',
  downRaajje in 'Downloaders\downRaajje.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  downStickam in 'Downloaders\downStickam.pas',
  downStream in 'Downloaders\downStream.pas',
  downStreetFire in 'Downloaders\downStreetFire.pas',
  downTangle in 'Downloaders\downTangle.pas',
  downTeacherTube in 'Downloaders\downTeacherTube.pas',
  downTontuyau in 'Downloaders\downTontuyau.pas',
  downTVcom in 'Downloaders\downTVcom.pas',
  downVideoClipsDump in 'Downloaders\downVideoClipsDump.pas',
  downVideu in 'Downloaders\downVideu.pas',
  downVimeo in 'Downloaders\downVimeo.pas',
  downYouTube in 'Downloaders\downYouTube.pas',
  downZkoukniTo in 'Downloaders\downZkoukniTo.pas',
  {$IFDEF XXX}
  xxxPornHost in 'Downloaders\XXX\xxxPornHost.pas',
  xxxPornHub in 'Downloaders\XXX\xxxPornHub.pas',
  xxxPornoTube in 'Downloaders\XXX\xxxPornoTube.pas',
  xxxRedTube in 'Downloaders\XXX\xxxRedTube.pas',
  xxxRude in 'Downloaders\XXX\xxxRude.pas',
  xxxShufuni in 'Downloaders\XXX\xxxShufuni.pas',
  xxxSpankingTube in 'Downloaders\XXX\xxxSpankingTube.pas',
  xxxTube8 in 'Downloaders\XXX\xxxTube8.pas',
  xxxXHamster in 'Downloaders\XXX\xxxXHamster.pas',
  xxxXNXX in 'Downloaders\XXX\xxxXNXX.pas',
  xxxXTube in 'Downloaders\XXX\xxxXTube.pas',
  xxxXVideoHost in 'Downloaders\XXX\xxxXVideoHost.pas',
  xxxXVideos in 'Downloaders\XXX\xxxXVideos.pas',
  xxxYouPorn in 'Downloaders\XXX\xxxYouPorn.pas',
  xxxYuvutu in 'Downloaders\XXX\xxxYuvutu.pas',
  {$ENDIF}
  // Playlist handlers
  listHTML in 'Playlists\listHTML.pas',
  listHTMLfile in 'Playlists\listHTMLfile.pas',
  listBing in 'Playlists\listBing.pas',
  listYouTube in 'Playlists\listYouTube.pas',
  listYouTubePage in 'Playlists\listYouTubePage.pas';

begin
  {$IFDEF GUI}
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
    {$IFDEF CLI}
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
