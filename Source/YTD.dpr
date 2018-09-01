program YTD;
{$INCLUDE 'YTD.inc'}

{$IFDEF CLI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}
{%File 'YTD.version'}
{%File 'YTD.inc'}

uses
  uLanguages in 'Common\uLanguages.pas',
  SysUtils,
  Windows,
  {$IFDEF FPC}
    Interfaces,
  {$ENDIF}
  {$IFDEF GUI}
    Forms,
  {$ENDIF}
  // Base objects and units
  uMessages in 'Common\uMessages.pas',
  uOptions in 'Common\uOptions.pas',
  uPCRE in 'Common\uPCRE.pas',
  uDownloadClassifier in 'Common\uDownloadClassifier.pas',
  uDownloader in 'Base\uDownloader.pas',
  uCommonDownloader in 'Base\uCommonDownloader.pas',
  uHttpDownloader in 'Base\uHttpDownloader.pas',
  uExternalDownloader in 'Base\uExternalDownloader.pas',
  uMSDownloader in 'Base\uMSDownloader.pas',
  uNestedDownloader in 'Base\uNestedDownloader.pas',
  uRtmpDownloader in 'Base\uRtmpDownloader.pas',
  uPlaylistDownloader in 'Base\uPlaylistDownloader.pas',
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
  // Downloaders
  down5min in 'Downloaders\down5min.pas',
  downAktualne in 'Downloaders\downAktualne.pas',
  downAutoTube in 'Downloaders\downAutoTube.pas',
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
  downBlipTv in 'Downloaders\downBlipTv.pas',
  downBlipTvV2 in 'Downloaders\downBlipTvV2.pas',
  downBofunk in 'Downloaders\downBofunk.pas',
  downBomba in 'Downloaders\downBomba.pas',
  downBreak in 'Downloaders\downBreak.pas',
  downCasSk in 'Downloaders\downCasSk.pas',
  downCekniTo in 'Downloaders\downCekniTo.pas',
  downCestyKSobe in 'Downloaders\downCestyKSobe.pas',
  downClipfish in 'Downloaders\downClipfish.pas',
  downClipfishV2 in 'Downloaders\downClipfishV2.pas',
  downCollegeHumor in 'Downloaders\downCollegeHumor.pas',
  downCrunchyRoll in 'Downloaders\downCrunchyRoll.pas',
  downCT in 'Downloaders\downCT.pas',
  downCT_Port in 'Downloaders\downCT_Port.pas',
  downCT24 in 'Downloaders\downCT24.pas',
  downCT24MSFotbal in 'Downloaders\downCT24MSFotbal.pas',
  downCT24MSFotbal_V2 in 'Downloaders\downCT24MSFotbal_V2.pas',
  downDailyHaha in 'Downloaders\downDailyHaha.pas',
  downDailyMotion in 'Downloaders\downDailyMotion.pas',
  downDeutscheBahn in 'Downloaders\downDeutscheBahn.pas',
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
  downGameAnyone in 'Downloaders\downGameAnyone.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downGrindTV in 'Downloaders\downGrindTV.pas',
  downiHned in 'Downloaders\downIHned.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  downJoj in 'Downloaders\downJoj.pas',
  downKukaj in 'Downloaders\downKukaj.pas',
  downLibimSeTi in 'Downloaders\downLibimSeTi.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveLeakEmbedded in 'Downloaders\downLiveLeakEmbedded.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMediaSport in 'Downloaders\downMediaSport.pas',
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
  downNovinky in 'Downloaders\downNovinky.pas',
  downPublicTV in 'Downloaders\downPublicTV.pas',
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
  downSTV in 'Downloaders\downSTV.pas',
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
  listGameAnyone in 'Playlists\listGameAnyone.pas',
  listHTML in 'Playlists\listHTML.pas',
  listHTMLfile in 'Playlists\listHTMLfile.pas',
  listBing in 'Playlists\listBing.pas',
  listYouTube in 'Playlists\listYouTube.pas',
  listYouTubePage in 'Playlists\listYouTubePage.pas';

var
  ErrorMsg: string;

begin
  try
    ExitCode := 0;
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
        ExitCode := ExecuteConsoleApp(TYTD);
        {$IFNDEF FPC}
          if DebugHook <> 0 then
            begin
            Writeln;
            Write(_(MSG_PRESS_ANY_KEY_TO_QUIT));
            Readln;
            end;
        {$ENDIF}
      {$ENDIF}
      end;
  except
    on E: Exception do
      begin
      ErrorMsg := Format(_(ERR_EXCEPTION_MESSAGE), [E.ClassName, E.Message]);
      {$IFDEF FPC}
        Writeln(ErrorMsg);
      {$ELSE}
        {$IFDEF CLI}
        if TConsoleApp.HasConsole = csOwnConsole then
          Writeln(ErrorMsg)
        else
        {$ENDIF}
          MessageBox(0, PChar(ErrorMsg), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR);
      {$ENDIF}
      ExitCode := 255;
      end;
    end;
end.
