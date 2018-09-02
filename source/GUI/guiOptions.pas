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

unit guiOptions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  {$IFNDEF GUI_WINAPI} Dialogs, {$ENDIF}
  uOptions, uLanguages, uMessages;

type
  TYTDOptionsGUI = class(TYTDOptions)
    private
      fLoadSuccessful: boolean;
    protected
      function Load(IgnoreErrors: boolean = True): boolean; override;
    public
      procedure Init; override;
    end;

implementation

{ TYTDOptionsGUI }

{$IFDEF CONVERTERS}
const
  DEFAULT_OPTIONS_XML
    = '<?xml version="1.0" encoding="windows-1250"?>' +
      '<ytd>' +
        '<converters>' +
          {$IFDEF CONVERTERSMUSTBEACTIVATED}
          '<activated>0</activated>' +
          {$ENDIF}
          '<converter id="to-avi">' +
            '<title>%s</title>' +
            '<exe_path>mencoder.exe</exe_path>' +
            '<command_line>-oac copy -ovc copy -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"</command_line>' +
            '<visibility>minimized</visibility>' +
          '</converter>' +
          '<converter id="convert-to-xvid">' +
            '<title>%s</title>' +
            '<exe_path>mencoder.exe</exe_path>' +
            '<command_line>-oac mp3lame -ovc lavc -lavcopts vcodec=mpeg4:vbitrate=1200 -ffourcc xvid -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"</command_line>' +
            '<visibility>minimized</visibility>' +
          '</converter>' +
          '<converter id="convert-to-h264">' +
            '<title>%s</title>' +
            '<exe_path>mencoder.exe</exe_path>' +
            '<command_line>-oac mp3lame -ovc x264 -x264encopts bitrate=1200 -of avi -o "{$FULLPATH}.avi" "{$FULLPATH}"</command_line>' +
            '<visibility>minimized</visibility>' +
          '</converter>' +
        '</converters>' +
      '</ytd>';
{$ENDIF}

{gnugettext: scan-all}
const
  INITIALRUN_WELCOMEMSG = '' +
      'This is the first time you are running the GUI version'#10 +
      'of YouTube Downloader, or the configuration file isn''t'#10 +
      'available. To protect your privacy, the program will now'#10 +
      'ask you about some default values which may influence it.'
      ;
  INITIALRUN_WANTPORTABLE = '' +
      'Would you like to run YouTube Downloader in portable mode?'#10#10 +
      'In portable mode, YTD stores its settings in file "ytd.xml"'#10 +
      'in its application directory (which must be writable in order'#10 +
      'for the settings to be stored). In normal (non-portable) mode'#10 +
      'the settings are stored in the current user''s profile.'#10#10 +
      'YES = portable mode, NO = normal mode'
      ;
  INITIALRUN_WANTNEWVERSIONCHECK =
      'Would you like YouTube Downloader to check for new versions'#10 +
      'on GUI startup?'#10#10 +
      'In order to perform the check, YouTube Downloader''s server'#10 +
      'must be contacted, and that means your IP address will be'#10 +
      'known to it. While this is not a privacy issue for most users,'#10 +
      'it might be significant for you.'#10 +
      'If you turn the automatic check off, you can still check for'#10 +
      'new versions manually by either visiting the program''s website'#10 +
      'or by entering the About window.'#10#10 +
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
  ERROR_LOADING_CONFIG =
    'Error loading configuration file. Error %s:'#10 +
    '%s'#10 +
    'Default config file will be created.';

{gnugettext: reset}

procedure TYTDOptionsGUI.Init;
begin
  fLoadSuccessful := False;
  inherited;
  if not fLoadSuccessful then
    begin
    {$IFDEF GUI_WINAPI}
    MessageBox(0, PChar(_(INITIALRUN_WELCOMEMSG)), APPLICATION_TITLE, MB_OK or MB_ICONWARNING or MB_TASKMODAL);
    {$ELSE}
    MessageDlg(_(INITIALRUN_WELCOMEMSG), mtWarning, [mbOK], 0);
    {$ENDIF}
    {$IFDEF CONVERTERS}
    Xml.LoadFromBinaryString(AnsiString(Format(DEFAULT_OPTIONS_XML, [_(DEFAULT_CONVERTER_TO_AVI), _(DEFAULT_CONVERTER_TO_XVID), _(DEFAULT_CONVERTER_TO_H264)])));
    {$ENDIF}
    {$IFDEF GUI_WINAPI}
    PortableMode := MessageBox(0, PChar(_(INITIALRUN_WANTPORTABLE)), APPLICATION_TITLE, MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = idYes;
    {$ELSE}
    PortableMode := MessageDlg(_(INITIALRUN_WANTPORTABLE), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    {$ENDIF}
    {$IFDEF GUI_WINAPI}
    CheckForNewVersionOnStartup := MessageBox(0, PChar(_(INITIALRUN_WANTNEWVERSIONCHECK)), APPLICATION_TITLE, MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = idYes;
    {$ELSE}
    CheckForNewVersionOnStartup := MessageDlg(_(INITIALRUN_WANTNEWVERSIONCHECK), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    {$ENDIF}
    end;
end;

function TYTDOptionsGUI.Load(IgnoreErrors: boolean): boolean;
begin
  try
    Result := inherited Load(IgnoreErrors);
    if Result then
      fLoadSuccessful := True;
  except
    on E: Exception do
      Result := (MessageBox(0, PChar(Format(_(ERROR_LOADING_CONFIG), [E.ClassName, E.Message])), APPLICATION_TITLE, MB_OK or MB_ICONSTOP or MB_TASKMODAL) = idYes);
    end;
end;

end.
