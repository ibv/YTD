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

unit guiOptions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  {$IFNDEF GUI_WINAPI} 
    Dialogs,
    {$IFDEF DELPHIXE4_UP}
    UITypes,
    {$ENDIF}
  {$ENDIF}
  uOptions, uLanguages, uMessages, uXml;

type
  TYTDOptionsGUI = class(TYTDOptions)
    private
      fLoadSuccessful: boolean;
    protected
      function GetMainFormLeft: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMainFormLeft(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetMainFormTop: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMainFormTop(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetMainFormWidth: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMainFormWidth(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetMainFormHeight: integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMainFormHeight(const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetDownloadListColumnWidth(Index: integer): integer; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetDownloadListColumnWidth(Index: integer; const Value: integer); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetCheckForNewVersionOnStartup: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetCheckForNewVersionOnStartup(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      function GetMonitorClipboard: boolean; {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      procedure SetMonitorClipboard(const Value: boolean); {$IFNDEF MINIMIZESIZE} virtual; {$ENDIF}
      {$IFDEF SYSTRAY}
      function GetMinimizeToTray: boolean;
      procedure SetMinimizeToTray(const Value: boolean);
      {$ENDIF}
    protected
      function Load(IgnoreErrors: boolean = True): boolean; override;
      function IgnoreInitErrors: boolean; override;
      procedure BuildDefaultConfig;
    public
      procedure Init; override;
    public
      property MainFormLeft: integer read GetMainFormLeft write SetMainFormLeft;
      property MainFormTop: integer read GetMainFormTop write SetMainFormTop;
      property MainFormWidth: integer read GetMainFormWidth write SetMainFormWidth;
      property MainFormHeight: integer read GetMainFormHeight write SetMainFormHeight;
      property DownloadListColumnWidth[Index: integer]: integer read GetDownloadListColumnWidth write SetDownloadListColumnWidth;
      property CheckForNewVersionOnStartup: boolean read GetCheckForNewVersionOnStartup write SetCheckForNewVersionOnStartup;
      property MonitorClipboard: boolean read GetMonitorClipboard write SetMonitorClipboard;
      {$IFDEF SYSTRAY}
      property MinimizeToTray: boolean read GetMinimizeToTray write SetMinimizeToTray;
      {$ENDIF}
    end;

implementation

{ TYTDOptionsGUI }

{gnugettext: scan-all}
const
  INITIALRUN_WELCOMEMSG = '' +
      'This is the first time you are running the GUI version'#10 +
      'of YTD, or the configuration file isn''t available.'#10 +
      'To protect your privacy, the program will now ask you'#10 +
      'about some default values which may influence it.'
      ;
  INITIALRUN_WANTPORTABLE = '' +
      'Would you like to run YTD in portable mode?'#10#10 +
      'In portable mode, YTD stores its settings in file "ytd.xml"'#10 +
      'in its application directory (which must be writable in order'#10 +
      'for the settings to be stored). In normal (non-portable) mode'#10 +
      'the settings are stored in the current user''s profile.'#10#10 +
      'YES = portable mode, NO = normal mode'
      ;
  INITIALRUN_WANTNEWVERSIONCHECK =
      'Would you like YTD to check for new versions'#10 +
      'on GUI startup?'#10#10 +
      'In order to perform the check, YTD''s server must'#10 +
      'be contacted, and that means your IP address will be'#10 +
      'known to it. While this is not a privacy issue for'#10 +
      'most users, it might be significant for you.'#10 +
      'If you turn the automatic check off, you can still'#10 +
      'check for new versions manually by either visiting'#10 +
      'the program''s website or by entering the About window.'#10#10 +
      'Should YTD automatically check for new versions?'
      ;
  DEFAULT_CONVERTER_TO_AVI =
      'Change container to .AVI'
      ;
  DEFAULT_CONVERTER_TO_XVID =
      'Convert movie to AVI/XVID'
      ;
  DEFAULT_CONVERTER_TO_H264 =
      'Convert movie to AVI/H264'
      ;
  DEFAULT_CONVERTER_TO_MP3 =
      'Convert movie to MP3'
      ;
  ERROR_LOADING_CONFIG =
    'Error loading configuration file. Error %s:'#10 +
    '%s'#10 +
    'Default config file will be created.';

{gnugettext: reset}

const
  XML_PATH_MAINFORMLEFT = 'gui/main_form/left';
  XML_PATH_MAINFORMTOP = 'gui/main_form/top';
  XML_PATH_MAINFORMWIDTH = 'gui/main_form/width';
  XML_PATH_MAINFORMHEIGHT = 'gui/main_form/height';
  XML_PATH_DOWNLOADLISTCOLUMNWIDTH = 'gui/download_list/column_%d_width';
  XML_PATH_CHECKFORNEWVERSIONONSTARTUP = 'gui/check_for_new_version';
  XML_PATH_MONITORCLIPBOARD = 'gui/monitor_clipboard';
  XML_PATH_MINIMIZETOTRAY = 'gui/minimize_to_tray';

const
  XML_DEFAULT_MAINFORMLEFT = -32768;
  XML_DEFAULT_MAINFORMTOP = -32768;
  XML_DEFAULT_MAINFORMWIDTH = -32768;
  XML_DEFAULT_MAINFORMHEIGHT = -32768;
  XML_DEFAULT_CHECKFORNEWVERSIONONSTARTUP = True;
  XML_DEFAULT_MONITORCLIPBOARD = False;
  XML_DEFAULT_MINIMIZETOTRAY = True;

procedure TYTDOptionsGUI.Init;
begin
  fLoadSuccessful := False;
  inherited;
  if not fLoadSuccessful then
    begin
    {$IFDEF GUI_WINAPI}
    MessageBox(0, PChar(_(INITIALRUN_WELCOMEMSG)), PChar(APPLICATION_TITLE), MB_OK or MB_ICONWARNING or MB_TASKMODAL);
    {$ELSE}
    MessageDlg(_(INITIALRUN_WELCOMEMSG), mtWarning, [mbOK], 0);
    {$ENDIF}
    BuildDefaultConfig;
//    {$IFDEF CONVERTERS}
//    Xml.LoadFromBinaryString(AnsiString(Format(DEFAULT_OPTIONS_XML, [_(DEFAULT_CONVERTER_TO_AVI), _(DEFAULT_CONVERTER_TO_XVID), _(DEFAULT_CONVERTER_TO_H264)])));
//    {$ENDIF}
    {$IFDEF GUI_WINAPI}
    PortableMode := MessageBox(0, PChar(_(INITIALRUN_WANTPORTABLE)), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = idYes;
    {$ELSE}
    PortableMode := MessageDlg(_(INITIALRUN_WANTPORTABLE), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    {$ENDIF}
    {$IFDEF GUI_WINAPI}
    CheckForNewVersionOnStartup := MessageBox(0, PChar(_(INITIALRUN_WANTNEWVERSIONCHECK)), PChar(APPLICATION_TITLE), MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = idYes;
    {$ELSE}
    CheckForNewVersionOnStartup := MessageDlg(_(INITIALRUN_WANTNEWVERSIONCHECK), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    {$ENDIF}
    end;
end;

procedure TYTDOptionsGUI.BuildDefaultConfig;
  {$IFDEF CONVERTERS}
    procedure InitConverter(const ID, Title, ExePath, CommandLine: string; Visibility: TConverterVisibility);
      var C: TConverter;
      begin
        C.ID := ID;
        C.Title := Title;
        C.ExePath := ExePath;
        C.CommandLine := CommandLine;
        C.Visibility := Visibility;
        WriteConverter(C);
      end;

    const
      CONVERTER_ID_MENCODER    {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'mencoder';
      CONVERTER_ID_FFMPEG      {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'ffmpeg';
      CONVERTER_ID_TO_AVI      {$IFDEF MINIMIZESIZE} :string {$ENDIF} = '-to-avi';
      CONVERTER_ID_TO_XVID     {$IFDEF MINIMIZESIZE} :string {$ENDIF} = '-convert-to-xvid';
      CONVERTER_ID_TO_H264     {$IFDEF MINIMIZESIZE} :string {$ENDIF} = '-convert-to-h264';
      CONVERTER_ID_TO_MP3      {$IFDEF MINIMIZESIZE} :string {$ENDIF} = '-convert-to-mp3';
      CONVERTER_TITLE_MENCODER {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'Mencoder - ';
      CONVERTER_TITLE_FFMPEG   {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'FFmpeg - ';
      CONVERTER_TITLE_TO_AVI   {$IFDEF MINIMIZESIZE} :string {$ENDIF} = DEFAULT_CONVERTER_TO_AVI;
      CONVERTER_TITLE_TO_XVID  {$IFDEF MINIMIZESIZE} :string {$ENDIF} = DEFAULT_CONVERTER_TO_XVID;
      CONVERTER_TITLE_TO_H264  {$IFDEF MINIMIZESIZE} :string {$ENDIF} = DEFAULT_CONVERTER_TO_H264;
      CONVERTER_TITLE_TO_MP3   {$IFDEF MINIMIZESIZE} :string {$ENDIF} = DEFAULT_CONVERTER_TO_MP3;
      CONVERTER_EXE_MENCODER   {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'mencoder.exe';
      CONVERTER_EXE_FFMPEG     {$IFDEF MINIMIZESIZE} :string {$ENDIF} = 'ffmpeg.exe';
  {$ENDIF}
begin
  {$IFDEF CONVERTERS}
    {$IFDEF CONVERTERSMUSTBEACTIVATED}
      ConvertersActivated := False;
    {$ENDIF}
    InitConverter(CONVERTER_ID_FFMPEG   + CONVERTER_ID_TO_AVI,  CONVERTER_TITLE_FFMPEG   + _(CONVERTER_TITLE_TO_AVI),  CONVERTER_EXE_FFMPEG,   '-i "{$FULLPATH}" -acodec copy -vcodec copy -y "{$FULLPATH}.avi"', cvMinimized);
    InitConverter(CONVERTER_ID_FFMPEG   + CONVERTER_ID_TO_XVID, CONVERTER_TITLE_FFMPEG   + _(CONVERTER_TITLE_TO_XVID), CONVERTER_EXE_FFMPEG,   '-i "{$FULLPATH}" -acodec libmp3lame -vcodec mpeg4 -b 1200k -vtag DX50 -y "{$FULLPATH}.avi"', cvMinimized);
    InitConverter(CONVERTER_ID_FFMPEG   + CONVERTER_ID_TO_H264, CONVERTER_TITLE_FFMPEG   + _(CONVERTER_TITLE_TO_H264), CONVERTER_EXE_FFMPEG,   '-i "{$FULLPATH}" -vcodec libx264 -preset fast -tune film -profile main -crf 22 -threads 0  -acodec aac -strict experimental -y "{$FULLPATH}.avi"', cvMinimized);
    InitConverter(CONVERTER_ID_FFMPEG   + CONVERTER_ID_TO_MP3,  CONVERTER_TITLE_FFMPEG   + _(CONVERTER_TITLE_TO_MP3),  CONVERTER_EXE_FFMPEG,   '-i "{$FULLPATH}" -q:a 0 -vn -y "{$FULLPATH}.mp3"', cvMinimized);
    InitConverter(CONVERTER_ID_MENCODER + CONVERTER_ID_TO_AVI,  CONVERTER_TITLE_MENCODER + _(CONVERTER_TITLE_TO_AVI),  CONVERTER_EXE_MENCODER, '-oac copy -ovc copy -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"', cvMinimized);
    InitConverter(CONVERTER_ID_MENCODER + CONVERTER_ID_TO_XVID, CONVERTER_TITLE_MENCODER + _(CONVERTER_TITLE_TO_XVID), CONVERTER_EXE_MENCODER, '-oac mp3lame -ovc lavc -lavcopts vcodec=mpeg4:vbitrate=1200 -ffourcc xvid -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"', cvMinimized);
    InitConverter(CONVERTER_ID_MENCODER + CONVERTER_ID_TO_H264, CONVERTER_TITLE_MENCODER + _(CONVERTER_TITLE_TO_H264), CONVERTER_EXE_MENCODER, '-oac mp3lame -ovc x264 -x264encopts bitrate=1200 -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"', cvMinimized);
  {$ENDIF}
end;

function TYTDOptionsGUI.Load(IgnoreErrors: boolean): boolean;
begin
  try
    Result := inherited Load(IgnoreErrors);
    if Result then
      fLoadSuccessful := True;
  except
    on E: Exception do
      Result := (MessageBox(0, PChar(Format(_(ERROR_LOADING_CONFIG), [E.ClassName, E.Message])), PChar(APPLICATION_TITLE), MB_OK or MB_ICONSTOP or MB_TASKMODAL) = idYes);
    end;
end;

function TYTDOptionsGUI.GetMainFormLeft: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_MAINFORMLEFT), XML_DEFAULT_MAINFORMLEFT);
end;

procedure TYTDOptionsGUI.SetMainFormLeft(const Value: integer);
begin
  SetOption(XML_PATH_MAINFORMLEFT, IntToStr(Value));
end;

function TYTDOptionsGUI.GetMainFormTop: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_MAINFORMTOP), XML_DEFAULT_MAINFORMTOP);
end;

procedure TYTDOptionsGUI.SetMainFormTop(const Value: integer);
begin
  SetOption(XML_PATH_MAINFORMTOP, IntToStr(Value));
end;

function TYTDOptionsGUI.GetMainFormWidth: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_MAINFORMWIDTH), XML_DEFAULT_MAINFORMWIDTH);
end;

procedure TYTDOptionsGUI.SetMainFormWidth(const Value: integer);
begin
  SetOption(XML_PATH_MAINFORMWIDTH, IntToStr(Value));
end;

function TYTDOptionsGUI.GetMainFormHeight: integer;
begin
  Result := StrToIntDef(GetOption(XML_PATH_MAINFORMHEIGHT), XML_DEFAULT_MAINFORMHEIGHT);
end;

procedure TYTDOptionsGUI.SetMainFormHeight(const Value: integer);
begin
  SetOption(XML_PATH_MAINFORMHEIGHT, IntToStr(Value));
end;

function TYTDOptionsGUI.GetDownloadListColumnWidth(Index: integer): integer;
begin
  Result := StrToIntDef(GetOption(Format(XML_PATH_DOWNLOADLISTCOLUMNWIDTH, [Index])), -1);
end;

procedure TYTDOptionsGUI.SetDownloadListColumnWidth(Index: integer; const Value: integer);
begin
  SetOption(Format(XML_PATH_DOWNLOADLISTCOLUMNWIDTH, [Index]), IntToStr(Value));
end;

function TYTDOptionsGUI.GetCheckForNewVersionOnStartup: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_CHECKFORNEWVERSIONONSTARTUP), XML_DEFAULT_CHECKFORNEWVERSIONONSTARTUP);
end;

procedure TYTDOptionsGUI.SetCheckForNewVersionOnStartup(const Value: boolean);
begin
  SetOption(XML_PATH_CHECKFORNEWVERSIONONSTARTUP, BooleanToXml(Value));
end;

function TYTDOptionsGUI.GetMonitorClipboard: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_MONITORCLIPBOARD), XML_DEFAULT_MONITORCLIPBOARD);
end;

procedure TYTDOptionsGUI.SetMonitorClipboard(const Value: boolean);
begin
  SetOption(XML_PATH_MONITORCLIPBOARD, BooleanToXml(Value));
end;

function TYTDOptionsGUI.IgnoreInitErrors: boolean;
begin
  Result := False;
end;

{$IFDEF SYSTRAY}
function TYTDOptionsGUI.GetMinimizeToTray: boolean;
begin
  Result := XmlToBoolean(GetOption(XML_PATH_MINIMIZETOTRAY), XML_DEFAULT_MINIMIZETOTRAY);
end;

procedure TYTDOptionsGUI.SetMinimizeToTray(const Value: boolean);
begin
  SetOption(XML_PATH_MINIMIZETOTRAY, BooleanToXml(Value));
end;
{$ENDIF}

end.
