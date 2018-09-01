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
  {$IFDEF FPC}
    Interfaces,
  {$ENDIF}
  {$IFDEF GUI}
    Forms,
  {$ENDIF}
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
  uNestedDownloader in 'Base\uNestedDownloader.pas',
  uRtmpDownloader in 'Base\uRtmpDownloader.pas',
  uPlaylistDownloader in 'Base\uPlaylistDownloader.pas',
  // Downloaders
  down5min in 'Downloaders\down5min.pas',
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
  downEHow in 'Downloaders\downEHow.pas',
  downESPN in 'Downloaders\downESPN.pas',
  downFacebook in 'Downloaders\downFacebook.pas',
  downFileCabi in 'Downloaders\downFileCabi.pas',
  downFlickr in 'Downloaders\downFlickr.pas',
  downFreeCaster in 'Downloaders\downFreeCaster.pas',
  downFreeSk in 'Downloaders\downFreeSk.pas',
  downFreeRide in 'Downloaders\downFreeRide.pas',
  downFreeVideoRu in 'Downloaders\downFreeVideoRu.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downGrindTV in 'Downloaders\downGrindTV.pas',
  downJoj in 'Downloaders\downJoj.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  downKukaj in 'Downloaders\downKukaj.pas',
  downLibimSeTi in 'Downloaders\downLibimSeTi.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMegaVideo in 'Downloaders\downMegaVideo.pas',
  downMetaCafe in 'Downloaders\downMetaCafe.pas',
  downMetropolTV in 'Downloaders\downMetropolTV.pas',
  downMojeVideo in 'Downloaders\downMojeVideo.pas',
  downMpora in 'Downloaders\downMpora.pas',
  downMuzu in 'Downloaders\downMuzu.pas',
  downMySpace in 'Downloaders\downMySpace.pas',
  downMyUbo in 'Downloaders\downMyUbo.pas',
  downNJoy in 'Downloaders\downNJoy.pas',
  downNova in 'Downloaders\downNova.pas',
  downRaajje in 'Downloaders\downRaajje.pas',
  downRingTV in 'Downloaders\downRingTV.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downSevenLoad in 'Downloaders\downSevenLoad.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  downSpike in 'Downloaders\downSpike.pas',
  downStagevu in 'Downloaders\downStagevu.pas',
  downStickam in 'Downloaders\downStickam.pas',
  downStream in 'Downloaders\downStream.pas',
  downStreetFire in 'Downloaders\downStreetFire.pas',
  downStupidVideos in 'Downloaders\downStupidVideos.pas',
  downTangle in 'Downloaders\downTangle.pas',
  downTeacherTube in 'Downloaders\downTeacherTube.pas',
  downTodaysBigThing in 'Downloaders\downTodaysBigThing.pas',
  downTontuyau in 'Downloaders\downTontuyau.pas',
  downTVcom in 'Downloaders\downTVcom.pas',
  downTVNoe in 'Downloaders\downTVNoe.pas',
  downUniMinnesota in 'Downloaders\downUniMinnesota.pas',
  downUStream in 'Downloaders\downUStream.pas',
  downVideaCesky in 'Downloaders\downVideaCesky.pas',
  downVideoAlbumyAzet in 'Downloaders\downVideoAlbumyAzet.pas',
  downVideoClipsDump in 'Downloaders\downVideoClipsDump.pas',
  downVideu in 'Downloaders\downVideu.pas',
  downVimeo in 'Downloaders\downVimeo.pas',
  downVitalMtb in 'Downloaders\downVitalMtb.pas',
  downWimp in 'Downloaders\downWimp.pas',
  downWrzuta in 'Downloaders\downWrzuta.pas',
  downYouTube in 'Downloaders\downYouTube.pas',
  downZkoukniTo in 'Downloaders\downZkoukniTo.pas',
  {$IFDEF XXX}
    xxxExtremeTube in 'Downloaders\XXX\xxxExtremeTube.pas',
    xxxKeezMovies in 'Downloaders\XXX\xxxKeezMovies.pas',
    xxxMegaPorn in 'Downloaders\XXX\xxxMegaPorn.pas',
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

var
  ErrorMsg: string;

begin
  try
    {$IFDEF GUI}
      if (ParamCount <= 0) then
        begin
        {$IFNDEF DEBUG}
          {$IFNDEF FPC}
            FreeConsole;
          {$ENDIF}
        {$ENDIF}
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
        {$IFNDEF FPC}
          if DebugHook <> 0 then
            begin
            Writeln;
            Write('Press any key to quit.');
            Readln;
            end;
        {$ENDIF}
      {$ENDIF}
      end;
  except
    on E: Exception do
      begin
      ErrorMsg := Format('Exception %s with message:'#13'%s', [E.ClassName, E.Message]);
      {$IFDEF FPC}
        Writeln(ErrorMsg);
      {$ELSE}
        {$IFDEF CLI}
        if TConsoleApp.HasConsole = csOwnConsole then
          Writeln(ErrorMsg)
        else
        {$ENDIF}
          MessageBox(0, PChar(ErrorMsg), PChar('YouTube Downloader'), MB_OK or MB_ICONERROR);
      {$ENDIF}
      ExitCode := 255;
      end;
    end;
end.
