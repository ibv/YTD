(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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
  uLanguages in 'Common\uLanguages.pas',
  SysUtils,
  Windows,
  CommCtrl,
  {$IFDEF FPC}
    Interfaces,
  {$ENDIF}
  {$IFDEF GUI}
    {$IFNDEF GUI_WINAPI}
      Forms,
    {$ENDIF}
  {$ENDIF}
  // Base objects and units
  uMessages in 'Common\uMessages.pas',
  uOptions in 'Common\uOptions.pas',
  uXML in 'Common\uXML.pas',
  uDownloadClassifier in 'Common\uDownloadClassifier.pas',
  uDownloader in 'Base\uDownloader.pas',
  uCommonDownloader in 'Base\uCommonDownloader.pas',
  uHttpDownloader in 'Base\uHttpDownloader.pas',
  uHttpDirectDownloader in 'Base\uHttpDirectDownloader.pas',
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
    {$IFDEF GUI_WINAPI}
      guiMainWINAPI in 'GUI\WinAPI\guiMainWINAPI.pas',
      guiAboutWINAPI in 'GUI\WinAPI\guiAboutWINAPI.pas',
    {$ELSE}
      guiMainVCL in 'GUI\VCL\guiMainVCL.pas' {FormYTD},
      guiAboutVCL in 'GUI\VCL\guiAboutVCL.pas' {FormAbout},
      guiOptionsVCL in 'GUI\VCL\guiOptionsVCL.pas' {FormOptions},
      {$IFDEF CONVERTERS}
      guiConverterVCL in 'GUI\VCL\guiConverterVCL.pas', {FormSelectConverter}
      {$ENDIF}
    {$ENDIF}
    guiConsts in 'GUI\guiConsts.pas',
    guiFunctions in 'GUI\guiFunctions.pas',
    guiOptions in 'GUI\guiOptions.pas',
    uDownloadList in 'GUI\uDownloadList.pas',
    uDownloadListItem in 'GUI\uDownloadListItem.pas',
    uDownloadThread in 'GUI\uDownloadThread.pas',
  {$ENDIF}
  // Downloaders
  down5min in 'Downloaders\down5min.pas',
  downAktualne in 'Downloaders\downAktualne.pas',
  downAlternativaTV in 'Downloaders\downAlternativaTV.pas',
  downAngryAlien in 'Downloaders\downAngryAlien.pas',
  downAutoTube in 'Downloaders\downAutoTube.pas',
  downBarrandovTV in 'Downloaders\downBarrandovTV.pas',
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
  downCestyKSobe in 'Downloaders\downCestyKSobe.pas',
  downClipfish in 'Downloaders\downClipfish.pas',
  downClipfishV2 in 'Downloaders\downClipfishV2.pas',
  downCollegeHumor in 'Downloaders\downCollegeHumor.pas',
  downCrunchyRoll in 'Downloaders\downCrunchyRoll.pas',
  downCSmaTalent in 'Downloaders\downCSmaTalent.pas',
  downCT in 'Downloaders\downCT.pas',
  downCT_Port in 'Downloaders\downCT_Port.pas',
  downCT_Program in 'Downloaders\downCT_Program.pas',
  downCT24 in 'Downloaders\downCT24.pas',
  downCT24MSFotbal in 'Downloaders\downCT24MSFotbal.pas',
  downCT24MSFotbal_V2 in 'Downloaders\downCT24MSFotbal_V2.pas',
  downCurrent in 'Downloaders\downCurrent.pas',
  downDailyHaha in 'Downloaders\downDailyHaha.pas',
  downDailyMotion in 'Downloaders\downDailyMotion.pas',
  downDeutscheBahn in 'Downloaders\downDeutscheBahn.pas',
  downDevilDucky in 'Downloaders\downDevilDucky.pas',
  downDoubleAgent in 'Downloaders\downDoubleAgent.pas',
  downDrsnySvet in 'Downloaders\downDrsnySvet.pas',
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
  downGameAnyone in 'Downloaders\downGameAnyone.pas',
  downGodTube in 'Downloaders\downGodTube.pas',
  downGuba in 'Downloaders\downGuba.pas',
  downGrindTV in 'Downloaders\downGrindTV.pas',
  downHasici150 in 'Downloaders\downHasici150.pas',
  downiHned in 'Downloaders\downIHned.pas',
  downiPrima in 'Downloaders\downIPrima.pas',
  downJoj in 'Downloaders\downJoj.pas',
  downKontraband in 'Downloaders\downKontraband.pas',
  downKukaj in 'Downloaders\downKukaj.pas',
  downLibimSeTi in 'Downloaders\downLibimSeTi.pas',
  downLiveLeak in 'Downloaders\downLiveLeak.pas',
  downLiveLeakEmbedded in 'Downloaders\downLiveLeakEmbedded.pas',
  downLiveVideo in 'Downloaders\downLiveVideo.pas',
  downLoupak in 'Downloaders\downLoupak.pas',
  downMarkiza in 'Downloaders\downMarkiza.pas',
  downMediaSport in 'Downloaders\downMediaSport.pas',
  downMegaVideo in 'Downloaders\downMegaVideo.pas',
  downMetaCafe in 'Downloaders\downMetaCafe.pas',
  downMetropolTV in 'Downloaders\downMetropolTV.pas',
  downMojeVideo in 'Downloaders\downMojeVideo.pas',
  downMojeVideoSk in 'Downloaders\downMojeVideoSk.pas',
  downMpora in 'Downloaders\downMpora.pas',
  downMustWatch in 'Downloaders\downMustWatch.pas',
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
  downNovinky in 'Downloaders\downNovinky.pas',
  downOverStream in 'Downloaders\downOverStream.pas',
  downPCPlanets in 'Downloaders\downPCPlanets.pas',
  downPrazdninyVTelci in 'Downloaders\downPrazdninyVTelci.pas',
  downPrimaCool in 'Downloaders\downPrimaCool.pas',
  downPublicTV in 'Downloaders\downPublicTV.pas',
  downQipRu_Embed in 'Downloaders\downQipRu_Embed.pas',
  downRaajje in 'Downloaders\downRaajje.pas',
  downRevver in 'Downloaders\downRevver.pas',
  downRingTV in 'Downloaders\downRingTV.pas',
  downRozhlas in 'Downloaders\downRozhlas.pas',
  downRTA in 'Downloaders\downRTA.pas',
  downRTA_Embed in 'Downloaders\downRTA_Embed.pas',
  downSevenLoad in 'Downloaders\downSevenLoad.pas',
  downSnotr in 'Downloaders\downSnotr.pas',
  downSpike in 'Downloaders\downSpike.pas',
  downStagevu in 'Downloaders\downStagevu.pas',
  downStickam in 'Downloaders\downStickam.pas',
  downStream in 'Downloaders\downStream.pas',
  downStreetFire in 'Downloaders\downStreetFire.pas',
  downStudioPlus in 'Downloaders\downStudioPlus.pas',
  downStupidVideos in 'Downloaders\downStupidVideos.pas',
  downSTV in 'Downloaders\downSTV.pas',
  downTangle in 'Downloaders\downTangle.pas',
  downTeacherTube in 'Downloaders\downTeacherTube.pas',
  downTodaysBigThing in 'Downloaders\downTodaysBigThing.pas',
  downTontuyau in 'Downloaders\downTontuyau.pas',
  downTotallyCrap in 'Downloaders\downTotallyCrap.pas',
  downTV7 in 'Downloaders\downTV7.pas',
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
  downZ1TV in 'Downloaders\downZ1TV.pas',
  downZkoukniTo in 'Downloaders\downZkoukniTo.pas',
  {$IFDEF XXX}
    xxxBeeg in 'Downloaders\XXX\xxxBeeg.pas',
    xxxBrazzers in 'Downloaders\XXX\xxxBrazzers.pas',
    xxxDachix in 'Downloaders\XXX\xxxDachix.pas',
    xxxEmpFlix in 'Downloaders\XXX\xxxEmpFlix.pas',
    xxxExtremeTube in 'Downloaders\XXX\xxxExtremeTube.pas',
    xxxGrinvi in 'Downloaders\XXX\xxxGrinvi.pas',
    xxxKeezMovies in 'Downloaders\XXX\xxxKeezMovies.pas',
    xxxMegaPorn in 'Downloaders\XXX\xxxMegaPorn.pas',
    xxxPornHost in 'Downloaders\XXX\xxxPornHost.pas',
    xxxPornHub in 'Downloaders\XXX\xxxPornHub.pas',
    xxxPornHubEmbed in 'Downloaders\XXX\xxxPornHubEmbed.pas',
    xxxPornoTube in 'Downloaders\XXX\xxxPornoTube.pas',
    xxxRedTube in 'Downloaders\XXX\xxxRedTube.pas',
    xxxRozzlobeniMuzi in 'Downloaders\XXX\xxxRozzlobeniMuzi.pas',
    xxxRude in 'Downloaders\XXX\xxxRude.pas',
    xxxSexDoma in 'Downloaders\XXX\xxxSexDoma.pas',
    xxxShufuni in 'Downloaders\XXX\xxxShufuni.pas',
    xxxSlutLoad in 'Downloaders\XXX\xxxSlutLoad.pas',
    xxxSpankingTube in 'Downloaders\XXX\xxxSpankingTube.pas',
    xxxSpankWire in 'Downloaders\XXX\xxxSpankWire.pas',
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
  {$ENDIF}
  {$IFDEF NONWORKING}
    // Download only works for logged-in users
    downGameTrailers in 'Downloaders\Non-working\downGameTrailers.pas',
    // RTMPDUMP fails to connect
    downiDnes in 'Downloaders\Non-working\downIDnes.pas',
    // Some transformation of movie ID is needed
    downTipovani in 'Downloaders\downTipovani.pas',
    // Download from http://www.wat.tv/get/web/ doesn't work
    downWat in 'Downloaders\Non-working\downWat.pas',
  {$ENDIF}
  // Playlist handlers
  listCT24 in 'Playlists\listCT24.pas',
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
    InitCommonControls; // Needed because of the manifest file
    {$IFDEF GUI}
      if (ParamCount <= 0) then
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
        end
      else
    {$ENDIF}
      begin
      {$IFDEF CLI}
        ExitCode := ExecuteConsoleApp(TYTD);
        {$IFNDEF FPC}
          {$IFDEF DELPHI2009_UP}
            {$WARN SYMBOL_PLATFORM OFF}
          {$ENDIF}
          if DebugHook <> 0 then
            begin
            Writeln;
            Write(_(MSG_PRESS_ANY_KEY_TO_QUIT));
            Readln;
            end;
          {$IFDEF DELPHI2009_UP}
            {$WARN SYMBOL_PLATFORM ON}
          {$ENDIF}
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
          MessageBox(0, PChar(ErrorMsg), APPLICATION_TITLE, MB_OK or MB_ICONERROR or MB_APPLMODAL);
      {$ENDIF}
      ExitCode := 255;
      end;
    end;
end.

