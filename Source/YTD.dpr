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
  // Base objects and units
  uMain in 'Common\uMain.pas',
  uFlvFile in 'Common\uFlvFile.pas',
  uFunctions in 'Common\uFunctions.pas',
  uHttp in 'Common\uHttp.pas',
  uMessages in 'Common\uMessages.pas',
  uOptions in 'Common\uOptions.pas',
  uStringConsts in 'Common\uStringConsts.pas',
  uScripts in 'Common\uScripts.pas',
  uUpgrade in 'Common\uUpgrade.pas',
  {$IFDEF SUBTITLES}
  uSubtitles in 'Common\uSubtitles.pas',
  {$ENDIF}
  {$IFDEF SETUP}
  uSetup in 'Common\uSetup.pas',
  {$ENDIF}
  ssl_openssl,
  uDownloadClassifier in 'Common\uDownloadClassifier.pas',
  uDownloader in 'Base\uDownloader.pas',
  uCommonDownloader in 'Base\uCommonDownloader.pas',
  uDummyDownloader in 'Base\uDummyDownloader.pas',
  uHttpDownloader in 'Base\uHttpDownloader.pas',
  uHttpDirectDownloader in 'Base\uHttpDirectDownloader.pas',
  uExternalDownloader in 'Base\uExternalDownloader.pas',
  uMSDownloader in 'Base\uMSDownloader.pas',
  uMSDirectDownloader in 'Base\uMSDirectDownloader.pas',
  uNestedDownloader in 'Base\uNestedDownloader.pas',
  uNestedDirectDownloader in 'Base\uNestedDirectDownloader.pas',
  uVarNestedDownloader in 'Base\uVarNestedDownloader.pas',
  uRtmpDownloader in 'Base\uRtmpDownloader.pas',
  uRtmpDirectDownloader in 'Base\uRtmpDirectDownloader.pas',
  uPlaylistDownloader in 'Base\uPlaylistDownloader.pas',
  uHDSDownloader in 'Base\uHDSDownloader.pas',
  uHDSDirectDownloader in 'Base\uHDSDirectDownloader.pas',
  uHLSDownloader in 'Base\uHLSDownloader.pas',
  uHLSDirectDownloader in 'Base\uHLSDirectDownloader.pas',
  uScriptedDownloader in 'Base\uScriptedDownloader.pas',
  uDASHDownloader in 'Base\uDASHDownloader.pas',
  // Command Line Version
  {$IFDEF CLI}
    uYTD in 'CLI\uYTD.pas',
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
      //guiOptionsWINAPI_CT in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_CT.pas',
      guiOptionsWINAPI_EuroSeptik in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_EuroSeptik.pas',
      guiOptionsWINAPI_Joj in 'GUI\WinAPI\Downloaders\guiOptionsWINAPI_Joj.pas',
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
      guiOptionsVCL_CT in 'GUI\VCL\Downloaders\guiOptionsVCL_CT.pas' {FrameDownloaderOptionsPage_CT: TFrame},
      guiOptionsVCL_EuroSeptik in 'GUI\VCL\Downloaders\guiOptionsVCL_EuroSeptik.pas' {FrameDownloaderOptionsPage_EuroSeptik: TFrame},
      guiOptionsVCL_Joj in 'GUI\VCL\Downloaders\guiOptionsVCL_Joj.pas' {FrameDownloaderOptionsPage_Joj: TFrame},
      guiOptionsVCL_YouTube in 'GUI\VCL\Downloaders\guiOptionsVCL_YouTube.pas' {FrameDownloaderOptionsPage_YouTube: TFrame},
      guiOptionsVCL_Prima in 'GUI\VCL\Downloaders\guiOptionsVCL_Prima.pas' {FrameDownloaderOptionsPage_Prima: TFrame},
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
  down1hdRo in 'Downloaders\down1hdRo.pas',
  down3NewsCoNz in 'Downloaders\down3NewsCoNz.pas',
  down4TV in 'Downloaders\down4TV.pas',
  down5min in 'Downloaders\down5min.pas',
  downAcademicEarth in 'Downloaders\downAcademicEarth.pas',
  downAgresori in 'Downloaders\downAgresori.pas',
  downAlternativaTV in 'Downloaders\downAlternativaTV.pas',
  downAncensored in 'Downloaders\downAncensored.pas',
  downAngryAlien in 'Downloaders\downAngryAlien.pas',
  downAnimacekTv in 'Downloaders\downAnimacekTv.pas',
  downArchive in 'Downloaders\downArchive.pas',
  downAutoTube in 'Downloaders\downAutoTube.pas',
  downAzetSk in 'Downloaders\downAzetSk.pas',
  downBahnorama in 'Downloaders\downBahnorama.pas',
  downBandZone in 'Downloaders\downBandZone.pas',
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
  downBBCNews in 'Downloaders\downBBCNews.pas',
  downBlennus in 'Downloaders\downBlennus.pas',
  downBlipTv_Embed in 'Downloaders\downBlipTv_Embed.pas',
  downBlipTv in 'Downloaders\downBlipTv.pas',
  downBMovies in 'Downloaders\downBMovies.pas',
  downBofunk in 'Downloaders\downBofunk.pas',
  downBolt in 'Downloaders\downBolt.pas',
  downBomba in 'Downloaders\downBomba.pas',
  downBreak in 'Downloaders\downBreak.pas',
  downBreakEmbed in 'Downloaders\downBreakEmbed.pas',
  downBreakEmbedV2 in 'Downloaders\downBreakEmbedV2.pas',
  downBritishPathe in 'Downloaders\downBritishPathe.pas',
  downBTVCz in 'Downloaders\downBTVCz.pas',
  downCasSk in 'Downloaders\downCasSk.pas',
  downCeskeDrahy in 'Downloaders\downCeskeDrahy.pas',
  downCesnet in 'Downloaders\downCesnet.pas',
  downCKKlic in 'Downloaders\downCKKlic.pas',
  downClevver in 'Downloaders\downClevver.pas',
  downClipfish in 'Downloaders\downClipfish.pas',
  downClipfishV2 in 'Downloaders\downClipfishV2.pas',
  downCNBC in 'Downloaders\downCNBC.pas',
  downCollegeHumor in 'Downloaders\downCollegeHumor.pas',
  downCriticalPast in 'Downloaders\downCriticalPast.pas',
  downCT in 'Downloaders\downCT.pas',
  downCurrent in 'Downloaders\downCurrent.pas',
  downDailyHaha in 'Downloaders\downDailyHaha.pas',
  downDeutscheBahn in 'Downloaders\downDeutscheBahn.pas',
  downDenik in 'Downloaders\downDenik.pas',
  downDevilDucky in 'Downloaders\downDevilDucky.pas',
  downDokumentarniTV in 'Downloaders\downDokumentarniTV.pas',
  downDotSub in 'Downloaders\downDotSub.pas',
  downDoubleAgent in 'Downloaders\downDoubleAgent.pas',
  downDUB in 'Downloaders\downDUB.pas',
  downEbaumsWorld in 'Downloaders\downEbaumsWorld.pas',
  downEHow in 'Downloaders\downEHow.pas',
  downEKucharkaNet in 'Downloaders\downEKucharkaNet.pas',
  downESPN in 'Downloaders\downESPN.pas',
  downEuroGamer in 'Downloaders\downEuroGamer.pas',
  downEuroSeptik in 'Downloaders\downEuroseptik.pas',
  downEVTV1 in 'Downloaders\downEVTV1.pas',
  downEyeneer in 'Downloaders\downEyeneer.pas',
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
  downGamePark in 'Downloaders\downGamePark.pas',
  downGameZone in 'Downloaders\downGameZone.pas',
  downGamesTiscaliCz in 'Downloaders\downGamesTiscaliCz.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGoogleVideo_Embed in 'Downloaders\downGoogleVideo_Embed.pas',
  downGoogleVideo in 'Downloaders\downGoogleVideo.pas',
  downGordonUraCz in 'Downloaders\downGordonUraCz.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downGrindTV in 'Downloaders\downGrindTV.pas',
  downHasici150 in 'Downloaders\downHasici150.pas',
  downHejbejSe in 'Downloaders\downHejbejSe.pas',
  downHellTV in 'Downloaders\downHellTV.pas',
  downHokejCbCz in 'Downloaders\downHokejCbCz.pas',
  downHokejCz in 'Downloaders\downHokejCz.pas',
  downHrej in 'Downloaders\downHrej.pas',
  downiConcerts in 'Downloaders\downIConcerts.pas',
  downiHned in 'Downloaders\downIHned.pas',
  downILikeBike in 'Downloaders\downILikeBike.pas',
  downiViewTube in 'Downloaders\downIViewTube.pas',
  downJenProZeny in 'Downloaders\downJenProZeny.pas',
  downJoj in 'Downloaders\downJoj.pas',
  downKanalTurk in 'Downloaders\downKanalTurk.pas',
  downKinobox in 'Downloaders\downKinobox.pas',
  downKiwi in 'Downloaders\downKiwi.pas',
  downKontraband in 'Downloaders\downKontraband.pas',
  downKoukni in 'Downloaders\downKoukni.pas',
  downKukaj in 'Downloaders\downKukaj.pas',
  downLaola1 in 'Downloaders\downLaola1.pas',
  downLenkaFilipova in 'Downloaders\downLenkaFilipova.pas',
  downLevelTV in 'Downloaders\downLevelTV.pas',
  downLibimSeTi in 'Downloaders\downLibimSeTi.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveLeakEmbedded in 'Downloaders\downLiveLeakEmbedded.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downLoupak in 'Downloaders\downLoupak.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMarkizaParticka in 'Downloaders\downMarkizaParticka.pas',
  downMatrix2001 in 'Downloaders\downMatrix2001.pas',
  downMediaSport in 'Downloaders\downMediaSport.pas',
  downMegaVideo in 'Downloaders\downMegaVideo.pas',
  downMegaVideo_Embed in 'Downloaders\downMegaVideo_Embed.pas',
  downMetaCafe in 'Downloaders\downMetaCafe.pas',
  downMetooCz in 'Downloaders\downMetooCz.pas',
  downMetooSk in 'Downloaders\downMetooSk.pas',
  downMetropolCZ in 'Downloaders\downMetropolCZ.pas',
  downMetropolCZ_Zpravy in 'Downloaders\downMetropolCZ_Zpravy.pas',
  downMetropolTV in 'Downloaders\downMetropolTV.pas',
  downMetroProjekt in 'Downloaders\downMetroProjekt.pas',
  downMojeTelevize in 'Downloaders\downMojeTelevize.pas',
  downMojeVideo in 'Downloaders\downMojeVideo.pas',
  downMojeVideoSk in 'Downloaders\downMojeVideoSk.pas',
  downMovtex in 'Downloaders\downMovtex.pas',
  downMpora in 'Downloaders\downMpora.pas',
  downMTV in 'Downloaders\downMTV.pas',
  downMTVEmbed in 'Downloaders\downMTVEmbed.pas',
  downMULitvinov in 'Downloaders\downMULitvinov.pas',
  downMultimediaVseCz in 'Downloaders\downMultimediaVseCz.pas',
  downMustWatch in 'Downloaders\downMustWatch.pas',
  downMuvi in 'Downloaders\downMuvi.pas',
  downMuzu in 'Downloaders\downMuzu.pas',
  downMySpace in 'Downloaders\downMySpace.pas',
  downMyUbo in 'Downloaders\downMyUbo.pas',
  downMyvi in 'Downloaders\downMyvi.pas',
  downMyvi_Embed in 'Downloaders\downMyvi_Embed.pas',
  downNaHnoji in 'Downloaders\downNaHnoji.pas',
  downNaStojaka in 'Downloaders\downNaStojaka.pas',
  downNBC in 'Downloaders\downNBC.pas',
  downNebickoVPapulke in 'Downloaders\downNebickoVPapulke.pas',
  downNHL in 'Downloaders\downNHL.pas',
  downNJoy in 'Downloaders\downNJoy.pas',
  downNothingToxic in 'Downloaders\downNothingToxic.pas',
  downNovaMov in 'Downloaders\downNovaMov.pas',
  downNovaMov_Embed in 'Downloaders\downNovaMov_Embed.pas',
  downNRSR in 'Downloaders\downNRSR.pas',
  downOKTeam in 'Downloaders\downOKTeam.pas',
  //downOldStream in 'Downloaders\downOldStream.pas',
  downOsobnostiCZ in 'Downloaders\downOsobnostiCZ.pas',
  downOverStream in 'Downloaders\downOverStream.pas',
  downOwn3D_Embed in 'Downloaders\downOwn3D_Embed.pas',
  downPBS in 'Downloaders\downPBS.pas',
  downPCPlanets in 'Downloaders\downPCPlanets.pas',
  downPicSpoon in 'Downloaders\downPicSpoon.pas',
  downPlayFm in 'Downloaders\downPlayFm.pas',
  downPlayZone in 'Downloaders\downPlayZone.pas',
  downPolarCz in 'Downloaders\downPolarCz.pas',
  downPracticalMethod in 'Downloaders\downPracticalMethod.pas',
  downPrahovaHD in 'Downloaders\downPrahovaHD.pas',
  downPrazdninyVTelci in 'Downloaders\downPrazdninyVTelci.pas',
  downPrima in 'Downloaders\downPrima.pas',
  downProglas in 'Downloaders\downProglas.pas',
  downProstoPleer in 'Downloaders\downProstoPleer.pas',
  downPublicTV in 'Downloaders\downPublicTV.pas',
  downQipRu_Embed in 'Downloaders\downQipRu_Embed.pas',
  downRaajje in 'Downloaders\downRaajje.pas',
  downRajce in 'Downloaders\downRajce.pas',
  downReflex in 'Downloaders\downReflex.pas',
  downRevver in 'Downloaders\downRevver.pas',
  downRingTV in 'Downloaders\downRingTV.pas',
  downRockstarGames in 'Downloaders\downRockstarGames.pas',
  downRockstarGames_IV in 'Downloaders\downRockstarGames_IV.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downRTA in 'Downloaders\downRTA.pas',
  downRTA_Embed in 'Downloaders\downRTA_Embed.pas',
  downRtlNl in 'Downloaders\downRtlNl.pas',
  downRuTube in 'Downloaders\downRuTube.pas',
  downSchoolOfCare in 'Downloaders\downSchoolOfCare.pas',
  downSerialOnline in 'Downloaders\downSerialOnline.pas',
  downSevenLoad in 'Downloaders\downSevenLoad.pas',
  downShackNews in 'Downloaders\downShackNews.pas',
  downSibnet in 'Downloaders\downSibnet.pas',
  downSmeSK in 'Downloaders\downSmeSK.pas',
  downSmotri in 'Downloaders\downSmotri.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  downSoundCloud in 'Downloaders\downSoundCloud.pas',
  downSpike in 'Downloaders\downSpike.pas',
  downSportStream in 'Downloaders\downSportStream.pas',
  downStagevu in 'Downloaders\downStagevu.pas',
  downStastneVdovy in 'Downloaders\downStastneVdovy.pas',
  downSteamPowered in 'Downloaders\downSteamPowered.pas',
  downStickam in 'Downloaders\downStickam.pas',
  downStream in 'Downloaders\downStream.pas',
  downStreetFire in 'Downloaders\downStreetFire.pas',
  downStudioPlus in 'Downloaders\downStudioPlus.pas',
  downStupidVideos in 'Downloaders\downStupidVideos.pas',
  downTangle in 'Downloaders\downTangle.pas',
  downTeacherTube in 'Downloaders\downTeacherTube.pas',
  downTed in 'Downloaders\downTed.pas',
  downThatVideoSite in 'Downloaders\downThatVideoSite.pas',
  downTMusic in 'Downloaders\downTMusic.pas',
  downTodaysBigThing in 'Downloaders\downTodaysBigThing.pas',
  downTontuyau in 'Downloaders\downTontuyau.pas',
  downTopReceptyCZ in 'Downloaders\downTopReceptyCZ.pas',
  downTotallyCrap in 'Downloaders\downTotallyCrap.pas',
  downTV2Nord in 'Downloaders\downTV2Nord.pas',
  downTV4PlaySE in 'Downloaders\downTV4PlaySE.pas',
  downTVHNOnlineSk in 'Downloaders\downTVHNOnlineSk.pas',
  downTVLux in 'Downloaders\downTVLux.pas',
  downTVNoe in 'Downloaders\downTVNoe.pas',
  downTVSpl in 'Downloaders\downTVSpl.pas',
  downTVVysocina in 'Downloaders\downTVVysocina.pas',
  downTyzdenSk in 'Downloaders\downTyzdenSk.pas',
  downUlozTo in 'Downloaders\downUlozTo.pas',
  downUniMinnesota in 'Downloaders\downUniMinnesota.pas',
  downUStream in 'Downloaders\downUStream.pas',
  downVCTV in 'Downloaders\downVCTV.pas',
  downVeeHD in 'Downloaders\downVeeHD.pas',
  downVideaCesky in 'Downloaders\downVideaCesky.pas',
  downVideoAlbumyAzet in 'Downloaders\downVideoAlbumyAzet.pas',
  downVideoBB in 'Downloaders\downVideoBB.pas',
  downVideoClipsDump in 'Downloaders\downVideoClipsDump.pas',
  //downVideoNurKz in 'Downloaders\downVideoNurKz.pas',
  downVideoPortalSfTV in 'Downloaders\downVideoPortalSfTV.pas',
  downVideu in 'Downloaders\downVideu.pas',
  downViiSk in 'Downloaders\downViiSk.pas',
  downViki in 'Downloaders\downViki.pas',
  downVineCo in 'Downloaders\downVineCo.pas',
  downVitalMtb in 'Downloaders\downVitalMtb.pas',
  downVKontakteRuEmbed in 'Downloaders\downVKontakteRuEmbed.pas',
  downWeGame in 'Downloaders\downWeGame.pas',
  downWimp in 'Downloaders\downWimp.pas',
  downWordPressTV in 'Downloaders\downWordPressTV.pas',
  downWorldClub in 'Downloaders\downWorldClub.pas',
  downWrzuta in 'Downloaders\downWrzuta.pas',
  downYikers in 'Downloaders\downYikers.pas',
  downYouTube in 'Downloaders\downYouTube.pas',
  downYouTu_Be in 'Downloaders\downYouTu_Be.pas',
  downYouTubeNoCookie in 'Downloaders\downYouTubeNoCookie.pas',
  downZ1TV in 'Downloaders\downZ1TV.pas',
  downZAKTV in 'Downloaders\downZAKTV.pas',
  downZapiksFr in 'Downloaders\downZapiksFr.pas',
  downZDF in 'Downloaders\downZDF.pas',
  downZkoukniTo in 'Downloaders\downZkoukniTo.pas',
  downZkoukniToEmbed in 'Downloaders\downZkoukniToEmbed.pas',
  downZmozek in 'Downloaders\downZmozek.pas',
  {$IFDEF XXX}
    xxx8teenXXX in 'Downloaders\XXX\xxx8teenXXX.pas',
    xxxAdultLoop in 'Downloaders\XXX\xxxAdultLoop.pas',
    xxxBrazzers in 'Downloaders\XXX\xxxBrazzers.pas',
    xxxCastingXXX in 'Downloaders\XXX\xxxCastingXXX.pas',
    xxxDachix in 'Downloaders\XXX\xxxDachix.pas',
    xxxDancingBear in 'Downloaders\XXX\xxxDancingBear.pas',
    xxxEmpFlix in 'Downloaders\XXX\xxxEmpFlix.pas',
    xxxEmpFlix_V2 in 'Downloaders\XXX\xxxEmpFlix_V2.pas',
    xxxExtremeTube in 'Downloaders\XXX\xxxExtremeTube.pas',
    xxxGavinXXX in 'Downloaders\XXX\xxxGavinXXX.pas',
    xxxGrinvi in 'Downloaders\XXX\xxxGrinvi.pas',
    xxxHardSexTube in 'Downloaders\XXX\xxxHardSexTube.pas',
    xxxKeezMovies in 'Downloaders\XXX\xxxKeezMovies.pas',
    xxxKeezMovies_Embed in 'Downloaders\XXX\xxxKeezMovies_Embed.pas',
    xxxMachoVideo in 'Downloaders\XXX\xxxMachoVideo.pas',
    xxxMegaPorn in 'Downloaders\XXX\xxxMegaPorn.pas',
    xxxMentalZero in 'Downloaders\XXX\xxxMentalZero.pas',
    xxxMojePornoSK in 'Downloaders\XXX\xxxMojePornoSK.pas',
    xxxMotherless in 'Downloaders\XXX\xxxMotherless.pas',
    xxxPornHost in 'Downloaders\XXX\xxxPornHost.pas',
    xxxPornoTube in 'Downloaders\XXX\xxxPornoTube.pas',
    xxxPornoZdarma in 'Downloaders\XXX\xxxPornoZdarma.pas',
    xxxPornTube in 'Downloaders\XXX\xxxPornTube.pas',
    xxxRawTube in 'Downloaders\XXX\xxxRawTube.pas',
    xxxRozzlobeniMuzi in 'Downloaders\XXX\xxxRozzlobeniMuzi.pas',
    xxxRude in 'Downloaders\XXX\xxxRude.pas',
    xxxSexDoma in 'Downloaders\XXX\xxxSexDoma.pas',
    xxxShufuni in 'Downloaders\XXX\xxxShufuni.pas',
    xxxSlutLoad in 'Downloaders\XXX\xxxSlutLoad.pas',
    xxxSpankWire in 'Downloaders\XXX\xxxSpankWire.pas',
    xxxTeenMpegs in 'Downloaders\XXX\xxxTeenMpegs.pas',
    xxxTheYNC in 'Downloaders\XXX\xxxTheYNC.pas',
    xxxTnaFlix in 'Downloaders\XXX\xxxTnaFlix.pas',
    xxxTube8 in 'Downloaders\XXX\xxxTube8.pas',
    xxxTubeSSS in 'Downloaders\XXX\xxxTubeSSS.pas',
    xxxXVideoHost in 'Downloaders\XXX\xxxXVideoHost.pas',
    xxxXVideos in 'Downloaders\XXX\xxxXVideos.pas',
    xxxYouJizz in 'Downloaders\XXX\xxxYouJizz.pas',
    xxxYouPorn in 'Downloaders\XXX\xxxYouPorn.pas',
    xxxYuvutu in 'Downloaders\XXX\xxxYuvutu.pas',
    xxxZakulisi in 'Downloaders\XXX\xxxZakulisi.pas',
    xxxZlutySnih in 'Downloaders\XXX\xxxZlutySnih.pas',
    xxxZvraceny in 'Downloaders\XXX\xxxZvraceny.pas',
  {$ENDIF}
  {$IFDEF NONWORKING}
    downArchivioLuce in 'Downloaders\Non-working\downArchivioLuce.pas',
      // No idea why it doesn't work
    downAutoSalonTV in 'Downloaders\Non-working\downAutoSalonTV.pas',
      // Removed support because it now uses Silverlight streaming and downloads in thousands little chunks
    downBeAtTv in 'Downloaders\Non-working\downBeAtTv.pas',
      // Audio doesn't download, and would have to be combined with the video anyway.
    downCrunchyRoll in 'Downloaders\Non-working\downCrunchyRoll.pas',
      // Needs some improved RTMP handling
    downDosGamesArchive in 'Downloaders\Non-working\downDosGamesArchive.pas',
      // Requires FTP support
    downEuroVisionSports in 'Downloaders\Non-working\downEuroVisionSports.pas',
      // Requires a token
    downGameTrailers in 'Downloaders\Non-working\downGameTrailers.pas',
      // Download only works for logged-in users
    downHuste in 'Downloaders\Non-working\downHuste.pas',
      // Requires login and password, should be easy enough to finish when I get them
    downTipovani in 'Downloaders\Non-working\downTipovani.pas',
      // Some transformation of movie ID is needed
    downWat in 'Downloaders\Non-working\downWat.pas',
      // Download from http://www.wat.tv/get/web/ doesn't work
  {$ENDIF}
  // Playlist handlers
  listHTML in 'Playlists\listHTML.pas',
  listHTMLfile in 'Playlists\listHTMLfile.pas',
  listTXT in 'Playlists\listTXT.pas',
  listArchive in 'Playlists\listArchive.pas',
  listBing in 'Playlists\listBing.pas',
  listBlipTV in 'Playlists\listBlipTV.pas',
  listGameAnyone in 'Playlists\listGameAnyone.pas',
  listYouTube in 'Playlists\listYouTube.pas',
  listYouTubePage in 'Playlists\listYouTubePage.pas';

begin
  Main;
end.
