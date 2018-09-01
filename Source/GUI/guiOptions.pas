unit guiOptions;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Dialogs,
  uOptions, uLanguages;

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
          '<converter id="convert-to-xvid">' +
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

{gnugettext: reset}

procedure TYTDOptionsGUI.Init;
begin
  fLoadSuccessful := False;
  inherited;
  if not fLoadSuccessful then
    begin
    MessageDlg(_(INITIALRUN_WELCOMEMSG), mtWarning, [mbOK], 0);
    {$IFDEF CONVERTERS}
    Xml.LoadFromBinaryString(Format(DEFAULT_OPTIONS_XML, [_('Change container to .AVI'), _('Convert movie to AVI/XVID'), _('Convert movie to AVI/H264')]));
    {$ENDIF}
    PortableMode := MessageDlg(_(INITIALRUN_WANTPORTABLE), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    if PortableMode then
      XmlFileName := ChangeFileExt(ParamStr(0), '.xml');
    CheckForNewVersionOnStartup := MessageDlg(_(INITIALRUN_WANTNEWVERSIONCHECK), mtConfirmation, [mbYes, mbNo], 0) = idYes;
    end;
end;

function TYTDOptionsGUI.Load(IgnoreErrors: boolean): boolean;
begin
  Result := inherited Load(IgnoreErrors);
  if Result then
    fLoadSuccessful := True;
end;

end.
