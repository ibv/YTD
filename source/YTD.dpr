(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

program YTD;
{$INCLUDE 'YTD.inc'}
{$INCLUDE 'YTD_Warnings.inc'}

{$IFDEF CLI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}
{%File 'YTD.version'}
{%File 'YTD.inc'}

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  uLanguages in 'Common\uLanguages.pas',
  SysUtils,
  Windows,
  CommCtrl,
  {$IFDEF SETUP}
  ShlObj,
  FileCtrl,
  {$ENDIF}
  {$IFDEF GUI}
    {$IFNDEF GUI_WINAPI}
      Forms,
    {$ENDIF}
  {$ENDIF}
  uCompatibility,
  // Base objects and units
  uFunctions in 'Common\uFunctions.pas',
  uMessages in 'Common\uMessages.pas',
  uOptions in 'Common\uOptions.pas',
  uStringConsts in 'Common\uStringConsts.pas',
  {$IFDEF SUBTITLES}
  uSubtitles in 'Common\uSubtitles.pas',
  {$ENDIF}
  {$IFDEF SETUP}
  uSetup in 'Common\uSetup.pas',
  {$ENDIF}
  uXML in 'Common\uXML.pas',
  uDownloadClassifier in 'Common\uDownloadClassifier.pas',
  uDownloader in 'Base\uDownloader.pas',
  uCommonDownloader in 'Base\uCommonDownloader.pas',
  uHttpDownloader in 'Base\uHttpDownloader.pas',
  uHttpDirectDownloader in 'Base\uHttpDirectDownloader.pas',
  uExternalDownloader in 'Base\uExternalDownloader.pas',
  uMSDownloader in 'Base\uMSDownloader.pas',
  uMSDirectDownloader in 'Base\uMSDirectDownloader.pas',
  uNestedDownloader in 'Base\uNestedDownloader.pas',
  uVarNestedDownloader in 'Base\uVarNestedDownloader.pas',
  uRtmpDownloader in 'Base\uRtmpDownloader.pas',
  uRtmpDirectDownloader in 'Base\uRtmpDirectDownloader.pas',
  uPlaylistDownloader in 'Base\uPlaylistDownloader.pas',
  // Command Line Version
  {$IFDEF CLI}
    uYTD in 'CLI\uYTD.pas',
    uConsoleApp,
  {$ENDIF}
  // GUI version
  {$IFDEF GUI}
    {$IFDEF GUI_WINAPI}
      guiMainWINAPI in 'GUI\WinAPI\guiMainWINAPI.pas',
      guiAboutWINAPI in 'GUI\WinAPI\guiAboutWINAPI.pas',
      {$IFDEF SETUP_GUI}
      guiSetupWINAPI in 'GUI\WinAPI\guiSetupWINAPI.pas',
      {$ENDIF}
      guiOptionsWINAPI in 'GUI\WinAPI\guiOptionsWINAPI.pas',
      guiOptionsWINAPI_Main in 'GUI\WinAPI\Options\guiOptionsWINAPI_Main.pas',
      guiOptionsWINAPI_Downloads in 'GUI\WinAPI\Options\guiOptionsWINAPI_Downloads.pas',
      guiOptionsWINAPI_Network in 'GUI\WinAPI\Options\guiOptionsWINAPI_Network.pas',
      guiOptionsWINAPI_Downloaders in 'GUI\WinAPI\Options\guiOptionsWINAPI_Downloaders.pas',
      guiOptionsWINAPI_Downloader in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_Downloader.pas',
      guiOptionsWINAPI_CommonDownloader in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_CommonDownloader.pas',
      guiOptionsWINAPI_Barrandov in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_Barrandov.pas',
      guiOptionsWINAPI_CT in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_CT.pas',
      guiOptionsWINAPI_Nova in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_Nova.pas',
      guiOptionsWINAPI_YouTube in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_YouTube.pas',
      {$IFDEF CONVERTERS}
      guiConverterWINAPI in 'GUI\WinAPI\guiConverterWINAPI.pas',
      {$ENDIF}
    {$ELSE}
      guiMainVCL in 'GUI\VCL\guiMainVCL.pas' {FormYTD},
      guiAboutVCL in 'GUI\VCL\guiAboutVCL.pas' {FormAbout},
      {$IFDEF SETUP_GUI}
      guiSetupVCL in 'GUI\VCL\guiSetupVCL.pas' {FormSetup},
      {$ENDIF}
      guiOptionsVCL in 'GUI\VCL\guiOptionsVCL.pas' {FormOptions},
      guiOptionsVCL_Downloader in 'GUI\VCL\Downloaders\guiOptionsVCL_Downloader.pas' {FrameDownloaderOptionsPageVCL: TFrame},
      guiOptionsVCL_CommonDownloader in 'GUI\VCL\Downloaders\guiOptionsVCL_CommonDownloader.pas' {FrameDownloaderOptionsPageCommonVCL: TFrame},
      guiOptionsVCL_Barrandov in 'GUI\VCL\Downloaders\guiOptionsVCL_Barrandov.pas' {FrameDownloaderOptionsPage_Barrandov: TFrame},
      guiOptionsVCL_CT in 'GUI\VCL\Downloaders\guiOptionsVCL_CT.pas' {FrameDownloaderOptionsPage_CT: TFrame},
      guiOptionsVCL_Nova in 'GUI\VCL\Downloaders\guiOptionsVCL_Nova.pas' {FrameDownloaderOptionsPage_Nova: TFrame},
      guiOptionsVCL_YouTube in 'GUI\VCL\Downloaders\guiOptionsVCL_YouTube.pas' {FrameDownloaderOptionsPage_YouTube: TFrame},
      {$IFDEF CONVERTERS}
      guiConverterVCL in 'GUI\VCL\guiConverterVCL.pas' {FormSelectConverter},
      {$ENDIF}
    {$ENDIF}
    guiConsts in 'GUI\guiConsts.pas',
    guiDownloaderOptions in 'GUI\guiDownloaderOptions.pas',
    guiFunctions in 'GUI\guiFunctions.pas',
    guiOptions in 'GUI\guiOptions.pas',
    uDownloadList in 'GUI\uDownloadList.pas',
    uDownloadListItem in 'GUI\uDownloadListItem.pas',
    uDownloadThread in 'GUI\uDownloadThread.pas',
  {$ENDIF}
  // Downloaders
  down123VideoNL in 'Downloaders\down123VideoNL.pas',
  down1hdRo in 'Downloaders\down1hdRo.pas',
  down5min in 'Downloaders\down5min.pas',
  downAgresori in 'Downloaders\downAgresori.pas',
  downAktualne in 'Downloaders\downAktualne.pas',
  downAlternativaTV in 'Downloaders\downAlternativaTV.pas',
  downAngryAlien in 'Downloaders\downAngryAlien.pas',
  downAutoSalonTV in 'Downloaders\downAutoSalonTV.pas',
  downAutoTube in 'Downloaders\downAutoTube.pas',
  downBahnorama in 'Downloaders\downBahnorama.pas',
  downBandZone in 'Downloaders\downBandZone.pas',
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
  downBBCNews in 'Downloaders\downBBCNews.pas',
  downBlennus in 'Downloaders\downBlennus.pas',
  downBlipTv in 'Downloaders\downBlipTv.pas',
  downBlipTvV2 in 'Downloaders\downBlipTvV2.pas',
  downBofunk in 'Downloaders\downBofunk.pas',
  downBolt in 'Downloaders\downBolt.pas',
  downBomba in 'Downloaders\downBomba.pas',
  downBreak in 'Downloaders\downBreak.pas',
  downBreakEmbed in 'Downloaders\downBreakEmbed.pas',
  downBreakEmbedV2 in 'Downloaders\downBreakEmbedV2.pas',
  downCasSk in 'Downloaders\downCasSk.pas',
  downCekniTo in 'Downloaders\downCekniTo.pas',
  downCeskeDrahy in 'Downloaders\downCeskeDrahy.pas',
  downCesnet in 'Downloaders\downCesnet.pas',
  downCestyKSobe in 'Downloaders\downCestyKSobe.pas',
  downCKKlic in 'Downloaders\downCKKlic.pas',
  downClevver in 'Downloaders\downClevver.pas',
  downClipfish in 'Downloaders\downClipfish.pas',
  downClipfishV2 in 'Downloaders\downClipfishV2.pas',
  downCNBC in 'Downloaders\downCNBC.pas',
  downCollegeHumor in 'Downloaders\downCollegeHumor.pas',
  downCSmaTalent in 'Downloaders\downCSmaTalent.pas',
  downCT in 'Downloaders\downCT.pas',
  downCurrent in 'Downloaders\downCurrent.pas',
  downDailyHaha in 'Downloaders\downDailyHaha.pas',
  downDailyMotion in 'Downloaders\downDailyMotion.pas',
  downDeutscheBahn in 'Downloaders\downDeutscheBahn.pas',
  downDenik in 'Downloaders\downDenik.pas',
  downDevilDucky in 'Downloaders\downDevilDucky.pas',
  downDokumentarniTV in 'Downloaders\downDokumentarniTV.pas',
  downDotSub in 'Downloaders\downDotSub.pas',
  downDoubleAgent in 'Downloaders\downDoubleAgent.pas',
  downDrsnySvet in 'Downloaders\downDrsnySvet.pas',
  downDUB in 'Downloaders\downDUB.pas',
  downEbaumsWorld in 'Downloaders\downEbaumsWorld.pas',
  downEHow in 'Downloaders\downEHow.pas',
  downESPN in 'Downloaders\downESPN.pas',
  downEVTV1 in 'Downloaders\downEVTV1.pas',
  downFacebook in 'Downloaders\downFacebook.pas',
  downFileCabi in 'Downloaders\downFileCabi.pas',
  downFishki in 'Downloaders\downFishki.pas',
  downFlickr in 'Downloaders\downFlickr.pas',
  downFreeCaster in 'Downloaders\downFreeCaster.pas',
  downFreeSk in 'Downloaders\downFreeSk.pas',
  downFreeRide in 'Downloaders\downFreeRide.pas',
  downFreeVideoRu in 'Downloaders\downFreeVideoRu.pas',
  downFunnyOrDie in 'Downloaders\downFunnyOrDie.pas',
  downG4TV in 'Downloaders\downG4TV.pas',
  downGameAnyone in 'Downloaders\downGameAnyone.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downGrindTV in 'Downloaders\downGrindTV.pas',
  downHasici150 in 'Downloaders\downHasici150.pas',
  downHellTV in 'Downloaders\downHellTV.pas',
  downHrej in 'Downloaders\downHrej.pas',
  downHudebniVideoKlipy in 'Downloaders\downHudebniVideoKlipy.pas',
  downiConcerts in 'Downloaders\downIConcerts.pas',
  downiDnes_Embed in 'Downloaders\downIDnes_Embed.pas',
  downiDnes in 'Downloaders\downIDnes.pas',
  downiHned in 'Downloaders\downIHned.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  downiViewTube in 'Downloaders\downIViewTube.pas',
  downJoj in 'Downloaders\downJoj.pas',
  downJoj_Porady in 'Downloaders\downJoj_Porady.pas',
  downKontraband in 'Downloaders\downKontraband.pas',
  downKukaj in 'Downloaders\downKukaj.pas',
  downLibimSeTi in 'Downloaders\downLibimSeTi.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveLeakEmbedded in 'Downloaders\downLiveLeakEmbedded.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downLoupak in 'Downloaders\downLoupak.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMarkizaParticka in 'Downloaders\downMarkizaParticka.pas',
  downMediaSport in 'Downloaders\downMediaSport.pas',
  downMegaVideo in 'Downloaders\downMegaVideo.pas',
  downMetaCafe in 'Downloaders\downMetaCafe.pas',
  downMetooCz in 'Downloaders\downMetooCz.pas',
  downMetropolTV in 'Downloaders\downMetropolTV.pas',
  downMojeTelevize in 'Downloaders\downMojeTelevize.pas',
  downMojeVideo in 'Downloaders\downMojeVideo.pas',
  downMojeVideoSk in 'Downloaders\downMojeVideoSk.pas',
  downMpora in 'Downloaders\downMpora.pas',
  downMTV in 'Downloaders\downMTV.pas',
  downMTVEmbed in 'Downloaders\downMTVEmbed.pas',
  downMultimediaVseCz in 'Downloaders\downMultimediaVseCz.pas',
  downMusicStreamCz in 'Downloaders\downMusicStreamCz.pas',
  downMustWatch in 'Downloaders\downMustWatch.pas',
  downMuvi in 'Downloaders\downMuvi.pas',
  downMuzu in 'Downloaders\downMuzu.pas',
  downMySpace in 'Downloaders\downMySpace.pas',
  downMyUbo in 'Downloaders\downMyUbo.pas',
  downNaStojaka in 'Downloaders\downNaStojaka.pas',
  downNBC in 'Downloaders\downNBC.pas',
  downNavratDoReality in 'Downloaders\downNavratDoReality.pas',
  downNJoy in 'Downloaders\downNJoy.pas',
  downNothingToxic in 'Downloaders\downNothingToxic.pas',
  downNova in 'Downloaders\downNova.pas',
  {$IFDEF DIRTYHACKS}
  downNovaTN in 'Downloaders\downNovaTN.pas',
  {$ENDIF}
  downNovaMov in 'Downloaders\downNovaMov.pas',
  downNovaMov_Embed in 'Downloaders\downNovaMov_Embed.pas',
  downNovinky in 'Downloaders\downNovinky.pas',
  downOverStream in 'Downloaders\downOverStream.pas',
  downPBS in 'Downloaders\downPBS.pas',
  downPCPlanets in 'Downloaders\downPCPlanets.pas',
  downPracticalMethod in 'Downloaders\downPracticalMethod.pas',
  downPrahovaHD in 'Downloaders\downPrahovaHD.pas',
  downPrazdninyVTelci in 'Downloaders\downPrazdninyVTelci.pas',
  downPrimaCool in 'Downloaders\downPrimaCool.pas',
  downProglas in 'Downloaders\downProglas.pas',
  downPublicTV in 'Downloaders\downPublicTV.pas',
  downQipRu_Embed in 'Downloaders\downQipRu_Embed.pas',
  downRaajje in 'Downloaders\downRaajje.pas',
  downRevver in 'Downloaders\downRevver.pas',
  downRingTV in 'Downloaders\downRingTV.pas',
  downRockstarGames in 'Downloaders\downRockstarGames.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downRTA in 'Downloaders\downRTA.pas',
  downRTA_Embed in 'Downloaders\downRTA_Embed.pas',
  downRtlNl in 'Downloaders\downRtlNl.pas',
  downSefka in 'Downloaders\downSefka.pas',
  downSerialOnline in 'Downloaders\downSerialOnline.pas',
  downSerialyCZ in 'Downloaders\downSerialyCZ.pas',
  downSevenLoad in 'Downloaders\downSevenLoad.pas',
  downSmeSK in 'Downloaders\downSmeSK.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  downSoundCloud in 'Downloaders\downSoundCloud.pas',
  downSpike in 'Downloaders\downSpike.pas',
  downSportCZ in 'Downloaders\downSportCZ.pas',
  downSportStream in 'Downloaders\downSportStream.pas',
  downStagevu in 'Downloaders\downStagevu.pas',
  downStastneVdovy in 'Downloaders\downStastneVdovy.pas',
  downStickam in 'Downloaders\downStickam.pas',
  downStream in 'Downloaders\downStream.pas',
  downStreetFire in 'Downloaders\downStreetFire.pas',
  downStudioPlus in 'Downloaders\downStudioPlus.pas',
  downStupidVideos in 'Downloaders\downStupidVideos.pas',
  downSTV in 'Downloaders\downSTV.pas',
  downSuper in 'Downloaders\downSuper.pas',
  downTangle in 'Downloaders\downTangle.pas',
  downTeacherTube in 'Downloaders\downTeacherTube.pas',
  downTed in 'Downloaders\downTed.pas',
  downThatVideoSite in 'Downloaders\downThatVideoSite.pas',
  downTodaysBigThing in 'Downloaders\downTodaysBigThing.pas',
  downTontuyau in 'Downloaders\downTontuyau.pas',
  downTotallyCrap in 'Downloaders\downTotallyCrap.pas',
  downTV4PlaySE in 'Downloaders\downTV4PlaySE.pas',
  downTV7 in 'Downloaders\downTV7.pas',
  downTVcom in 'Downloaders\downTVcom.pas',
  downTVLux in 'Downloaders\downTVLux.pas',
  downTVNoe in 'Downloaders\downTVNoe.pas',
  downTVSpl in 'Downloaders\downTVSpl.pas',
  downTyzdenSk in 'Downloaders\downTyzdenSk.pas',
  downUniMinnesota in 'Downloaders\downUniMinnesota.pas',
  downUStream in 'Downloaders\downUStream.pas',
  downVideaCesky in 'Downloaders\downVideaCesky.pas',
  downVideoAlbumyAzet in 'Downloaders\downVideoAlbumyAzet.pas',
  downVideoClipsDump in 'Downloaders\downVideoClipsDump.pas',
  downVideoPortalSfTV in 'Downloaders\downVideoPortalSfTV.pas',
  downVideoTiscaliCZ in 'Downloaders\downVideoTiscaliCZ.pas',
  downVideu in 'Downloaders\downVideu.pas',
  downVimeo in 'Downloaders\downVimeo.pas',
  downVitalMtb in 'Downloaders\downVitalMtb.pas',
  downVKontakteRuEmbed in 'Downloaders\downVKontakteRuEmbed.pas',
  downWeGame in 'Downloaders\downWeGame.pas',
  downWimp in 'Downloaders\downWimp.pas',
  downWordPressTV in 'Downloaders\downWordPressTV.pas',
  downWrzuta in 'Downloaders\downWrzuta.pas',
  downYikers in 'Downloaders\downYikers.pas',
  downYouTube in 'Downloaders\downYouTube.pas',
  downZ1TV in 'Downloaders\downZ1TV.pas',
  downZDF in 'Downloaders\downZDF.pas',
  downZkoukniTo in 'Downloaders\downZkoukniTo.pas',
  downZkoukniToEmbed in 'Downloaders\downZkoukniToEmbed.pas',
  downZmozek in 'Downloaders\downZmozek.pas',
  {$IFDEF XXX}
    xxxAdultLoop in 'Downloaders\XXX\xxxAdultLoop.pas',
    xxxBeeg in 'Downloaders\XXX\xxxBeeg.pas',
    xxxBrazzers in 'Downloaders\XXX\xxxBrazzers.pas',
    xxxDachix in 'Downloaders\XXX\xxxDachix.pas',
    xxxDancingBear in 'Downloaders\XXX\xxxDancingBear.pas',
    xxxEmpFlix in 'Downloaders\XXX\xxxEmpFlix.pas',
    xxxExtremeTube in 'Downloaders\XXX\xxxExtremeTube.pas',
    xxxFreePornoZdarma in 'Downloaders\XXX\xxxFreePornoZdarma.pas',
    xxxFreeVideoCz in 'Downloaders\XXX\xxxFreeVideoCz.pas',
    xxxGrinvi in 'Downloaders\XXX\xxxGrinvi.pas',
    xxxHardSexTube in 'Downloaders\XXX\xxxHardSexTube.pas',
    xxxJenProMuze in 'Downloaders\XXX\xxxJenProMuze.pas',
    xxxKeezMovies in 'Downloaders\XXX\xxxKeezMovies.pas',
    xxxMachoVideo in 'Downloaders\XXX\xxxMachoVideo.pas',
    xxxMegaPorn in 'Downloaders\XXX\xxxMegaPorn.pas',
    xxxPornHost in 'Downloaders\XXX\xxxPornHost.pas',
    xxxPornHub in 'Downloaders\XXX\xxxPornHub.pas',
    xxxPornHubEmbed in 'Downloaders\XXX\xxxPornHubEmbed.pas',
    xxxPornoTube in 'Downloaders\XXX\xxxPornoTube.pas',
    xxxPornoZdarma in 'Downloaders\XXX\xxxPornoZdarma.pas',
    xxxRedTube in 'Downloaders\XXX\xxxRedTube.pas',
    xxxRozzlobeniMuzi in 'Downloaders\XXX\xxxRozzlobeniMuzi.pas',
    xxxRude in 'Downloaders\XXX\xxxRude.pas',
    xxxSexDoma in 'Downloaders\XXX\xxxSexDoma.pas',
    xxxShufuni in 'Downloaders\XXX\xxxShufuni.pas',
    xxxSlutLoad in 'Downloaders\XXX\xxxSlutLoad.pas',
    xxxSpankingTube in 'Downloaders\XXX\xxxSpankingTube.pas',
    xxxSpankWire in 'Downloaders\XXX\xxxSpankWire.pas',
    xxxTheYNC in 'Downloaders\XXX\xxxTheYNC.pas',
    xxxTnaFlix in 'Downloaders\XXX\xxxTnaFlix.pas',
    xxxTube8 in 'Downloaders\XXX\xxxTube8.pas',
    xxxTubeSSS in 'Downloaders\XXX\xxxTubeSSS.pas',
    xxxXHamster in 'Downloaders\XXX\xxxXHamster.pas',
    xxxXNXX in 'Downloaders\XXX\xxxXNXX.pas',
    xxxXTube in 'Downloaders\XXX\xxxXTube.pas',
    xxxXVideoHost in 'Downloaders\XXX\xxxXVideoHost.pas',
    xxxXVideos in 'Downloaders\XXX\xxxXVideos.pas',
    xxxYouJizz in 'Downloaders\XXX\xxxYouJizz.pas',
    xxxYouPorn in 'Downloaders\XXX\xxxYouPorn.pas',
    xxxYuvutu in 'Downloaders\XXX\xxxYuvutu.pas',
    xxxZakulisi in 'Downloaders\XXX\xxxZakulisi.pas',
  {$ENDIF}
  {$IFDEF NONWORKING}
    // Needs some improved RTMP handling
    downCrunchyRoll in 'Downloaders\Non-working\downCrunchyRoll.pas',
    // Download only works for logged-in users
    downGameTrailers in 'Downloaders\Non-working\downGameTrailers.pas',
    // Some transformation of movie ID is needed
    downTipovani in 'Downloaders\Non-working\downTipovani.pas',
    // Download from http://www.wat.tv/get/web/ doesn't work
    downWat in 'Downloaders\Non-working\downWat.pas',
  {$ENDIF}
  // Playlist handlers
  listBlipTV in 'Playlists\listBlipTV.pas',
  listGameAnyone in 'Playlists\listGameAnyone.pas',
  listHTML in 'Playlists\listHTML.pas',
  listHTMLfile in 'Playlists\listHTMLfile.pas',
  listBing in 'Playlists\listBing.pas',
  listYouTube in 'Playlists\listYouTube.pas',
  listYouTubePage in 'Playlists\listYouTubePage.pas';

type
  TStartupType = ( {$IFDEF CLI} stCLI, {$ENDIF} {$IFDEF GUI} stGUI, stGUIexplicit, {$ENDIF} {$IFDEF SETUP} stInstall, {$ENDIF} stNone);

var
  StartedFromIDE: boolean;
  {$IFDEF GUI}
    {$IFDEF CLI}
    RunExternal: boolean;
    {$ENDIF}
  {$ENDIF}
  ErrorMsg: string;
  {$IFDEF SETUP}
  InstallDir: string;
  DesktopShortcut, StartMenuShortcut, RestartYTD: boolean;
  {$ENDIF}

function FindStartupType( {$IFDEF SETUP} var InstallDir: string; var DesktopShortcut, StartMenuShortcut, RestartYTD: boolean {$ENDIF} ): TStartupType;
{$IFDEF SETUP}
var
  i: integer;
  Param: string;
  {$IFDEF SETUP_GUI}
  F: TFormSetup;
  {$ENDIF}
{$ENDIF}
begin
  Result := Low(TStartupType);
  // No parameters runs GUI if available, otherwise CLI
  if ParamCount = 0 then
    Result := {$IFDEF GUI} stGUI {$ENDIF}
  // Otherwise check for startup-type parameters
  {$IFDEF SETUP}
  else for i := 1 to ParamCount do
    begin
    Param := ParamStr(i);
    if False then
      begin
      end
    {$IFDEF GUI}
    else if Param = SETUP_PARAM_GUI then
      begin
      Result := stGUIexplicit;
      Break;
      end
    {$ENDIF}
    {$IFDEF SETUP_GUI}
    else if Param = SETUP_PARAM_SETUP then
      begin
      {$IFNDEF DEBUG}
        {$IFNDEF FPC}
          FreeConsole;
          IsConsole := False;
        {$ENDIF}
      {$ENDIF}
      F := TFormSetup.Create(nil);
      try
        case F.ShowModal of
          idOK:
            begin
            Result := stInstall;
            InstallDir := F.DestinationDir;
            DesktopShortcut := F.DesktopShortcut;
            StartMenuShortcut := F.StartMenuShortcut;
            RestartYTD := True;
            end;
          idIgnore:
            Result := {$IFDEF GUI} stGUI {$ELSE} {$IFDEF CLI} stCli {$ELSE} stNone {$ENDIF} {$ENDIF} ;
          else
            Result := stNone;
          end;
      finally
        F.Free;
        end;
      Break;
      end
    {$ENDIF}
    else if (Param = SETUP_PARAM_UPGRADE) or (Param = SETUP_PARAM_UPGRADE_GUI) or (Param = SETUP_PARAM_INSTALL) or (Param = SETUP_PARAM_INSTALL_GUI) then
      begin
      if i < ParamCount then
        begin
        Result := stInstall;
        InstallDir := ParamStr(Succ(i));
        DesktopShortcut := (Param = SETUP_PARAM_INSTALL) or (Param = SETUP_PARAM_INSTALL_GUI);
        StartMenuShortcut := (Param = SETUP_PARAM_INSTALL) or (Param = SETUP_PARAM_INSTALL_GUI);
        RestartYTD := (Param = SETUP_PARAM_UPGRADE_GUI) or (Param = SETUP_PARAM_INSTALL_GUI);
        Sleep(500); // to give some time for the caller to quit
        Break;
        end;
      end;
    end;
  {$ELSE}
    ;
  {$ENDIF}
end;

{$IFDEF CLI}
procedure RunCLI;
begin
  ExitCode := ExecuteConsoleApp(TYTD);
  if StartedFromIDE then
    begin
    Writeln;
    Write(MSG_PRESS_ANY_KEY_TO_QUIT);
    Readln;
    end;
end;
{$ENDIF}

{$IFDEF GUI}
procedure RunGUI;
begin
  {$IFNDEF DEBUG}
    {$IFNDEF FPC}
      FreeConsole;
      IsConsole := False;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF GUI_WINAPI}
    with TFormMain.Create do
      try
        ShowModal;
      finally
        Free;
        end;
  {$ELSE}
    Application.Initialize;
    Application.Title := 'YouTube Downloader';
    Application.CreateForm(TFormYTD, FormYTD);
    Application.Run;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF SETUP}
procedure RunInstall(const InstallDir: string; DesktopShortcut, StartMenuShortcut, RestartYTD: boolean);

  function CopyFiles(const SourceDir, DestinationDir: string): boolean;
    var SR: TSearchRec;
    begin
      Result := True;
      ForceDirectories(DestinationDir);
      if FindFirst(SourceDir + '*.*', faAnyFile, SR) = 0 then
        try
          repeat
            if Longbool(SR.Attr and faDirectory) then
              begin
              if (SR.Name <> '.') and (SR.Name <> '..') then
                if not CopyFiles(SourceDir + SR.Name + '\', DestinationDir + SR.Name + '\') then
                  Result := False;
              end
            else
              begin
              if not CopyFile(PChar(SourceDir + SR.Name), PChar(DestinationDir + SR.Name), False) then
                Result := False;
              end;
          until FindNext(SR) <> 0;
        finally
          SysUtils.FindClose(SR);
          end;
    end;

var OK: boolean;
    InstDir, InstExe: string;
begin
  OK := False;
  InstDir := IncludeTrailingPathDelimiter(InstallDir);
  InstExe := InstDir + ExtractFileName(ParamStr(0));
  if InstallDir <> '' then
    begin
    OK := CopyFiles(ExtractFilePath(ParamStr(0)), InstDir);
    if OK then
      begin
      if DesktopShortcut then
        CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_DESKTOPDIRECTORY, InstExe);
      if StartMenuShortcut then
        CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_PROGRAMS, InstExe);
      end;
    end;
  if not OK then
    begin
    {$IFDEF FPC}
      Writeln(ERR_INSTALL_FAILED);
    {$ELSE}
      {$IFDEF CLI}
      if TConsoleApp.HasConsole = csOwnConsole then
        Writeln(ERR_INSTALL_FAILED)
      else
      {$ENDIF}
        MessageBox(0, PChar(ERR_INSTALL_FAILED), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or MB_TASKMODAL);
    {$ENDIF}
    ExitCode := 253;
    end
  else
    begin
    ExitCode := 0;
    if RestartYTD then
      Run(InstExe, '', ExcludeTrailingPathDelimiter(InstDir));
    end;
end;
{$ENDIF}

begin
  try
    ExitCode := 0;
    InitCommonControls; // Needed because of the manifest file
    // Test for IDE
    StartedFromIDE := False;
    {$IFNDEF FPC}
      {$IFDEF DELPHI2009_UP}
        {$WARN SYMBOL_PLATFORM OFF}
      {$ENDIF}
      if DebugHook <> 0 then
        StartedFromIDE := True;
      {$IFDEF DELPHI2009_UP}
        {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
    {$ENDIF}
    // Determine the startup type and parameters
    {$IFDEF SETUP}
    InstallDir := '';
    DesktopShortcut := False;
    StartMenuShortcut := False;
    RestartYTD := False;
    {$ENDIF}
    case FindStartupType( {$IFDEF SETUP} InstallDir, DesktopShortcut, StartMenuShortcut, RestartYTD {$ENDIF} ) of
      {$IFDEF CLI}
      stCLI:
        RunCLI;
      {$ENDIF}
      {$IFDEF GUI}
      stGUIexplicit:
        RunGUI;
      stGUI:
        begin
        {$IFDEF CLI}
          RunExternal := (not StartedFromIDE);
          {$IFNDEF FPC}
            if RunExternal then
              begin
              FreeConsole;
              if not TConsoleApp.ParentHasConsole then
                RunExternal := False;
              end;
          {$ENDIF}
          if (not RunExternal) or (not Run(ParamStr(0), SETUP_PARAM_GUI)) then
        {$ENDIF}
          RunGUI;
        end;
      {$ENDIF}
      {$IFDEF SETUP}
      stInstall:
        RunInstall(InstallDir, DesktopShortcut, StartMenuShortcut, RestartYTD);
      {$ENDIF}
      end;
  except
    on E: Exception do
      begin
      ErrorMsg := Format(ERR_EXCEPTION_MESSAGE, [E.ClassName, E.Message]);
      {$IFDEF FPC}
        Writeln(ErrorMsg);
      {$ELSE}
        {$IFDEF CLI}
        if TConsoleApp.HasConsole = csOwnConsole then
          Writeln(ErrorMsg)
        else
        {$ENDIF}
          MessageBox(0, PChar(ErrorMsg), PChar(APPLICATION_TITLE), MB_OK or MB_ICONERROR or MB_TASKMODAL);
      {$ENDIF}
      ExitCode := 255;
      end;
    end;
end.
