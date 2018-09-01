unit uYTDGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ClipBrd, FileCtrl,
  uDownloadClassifier;

type
  TFormYTD = class(TForm)
    PanelSettings: TPanel;
    PanelProgress: TPanel;
    Label1: TLabel;
    EditUrl: TEdit;
    BtnDownload: TBitBtn;
    Label2: TLabel;
    EditDestination: TEdit;
    BtnSelectDir: TBitBtn;
    ProgressBar1: TProgressBar;
    LabelVideoName: TLabel;
    LabelVideoProgress: TLabel;
    BtnAbort: TBitBtn;
    procedure BtnSelectDirClick(Sender: TObject);
    procedure BtnDownloadClick(Sender: TObject);
    procedure BtnAbortClick(Sender: TObject);
    procedure EditUrlChange(Sender: TObject);
  private
  protected
    TransferAborted: boolean;
    DownloadClassifier: TDownloadClassifier;
    LastProc: int64;
    procedure DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormYTD: TFormYTD;

implementation

{$R *.DFM}

{ TFormYTD }

constructor TFormYTD.Create(AOwner: TComponent);
var s: string;
begin
  inherited;
  DownloadClassifier := TDownloadClassifier.Create;
  Caption := Application.Title + ' v' + {$INCLUDE 'ytd.version'} ;
  BtnDownload.Enabled := False;
  EditUrl.Text := '';
  EditDestination.Text := GetCurrentDir;
  PanelProgress.Visible := False;
  PanelProgress.Top := PanelSettings.Top;
  if PanelProgress.Height > PanelSettings.Height then
    ClientHeight := PanelProgress.Height
  else
    ClientHeight := PanelSettings.Height;
  if Clipboard.HasFormat(CF_TEXT) then
    begin
    s := Trim(Clipboard.AsText);
    if (AnsiCompareText(Copy(s, 1, 7), 'http://') = 0) or (AnsiCompareText(Copy(s, 1, 8), 'https://') = 0) then
      EditUrl.Text := s;
    end;
end;

destructor TFormYTD.Destroy;
begin
  FreeAndNil(DownloadClassifier);
  inherited;
end;

procedure TFormYTD.BtnSelectDirClick(Sender: TObject);
var Dir: string;
begin
  Dir := EditDestination.Text;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate], 0) then
    EditDestination.Text := Dir;
end;

procedure TFormYTD.EditUrlChange(Sender: TObject);
begin
  DownloadClassifier.Url := EditUrl.Text;
  BtnDownload.Enabled := DownloadClassifier.Downloader <> nil;
end;

procedure TFormYTD.BtnAbortClick(Sender: TObject);
begin
  if MessageDlg('Do you really want to abort?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    TransferAborted := True;
end;

procedure TFormYTD.BtnDownloadClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourglass;
    PanelSettings.Enabled := False;
    TransferAborted := False;
    ProgressBar1.Position := 0;
    LabelVideoName.Caption := '';
    LabelVideoProgress.Caption := '0.0%';
    BtnAbort.Visible := True;
    BtnDownload.Enabled := False;
    PanelProgress.Visible := True;
    LastProc := -1;
    with DownloadClassifier do
      begin
      Downloader.DestinationPath := EditDestination.Text;
      Downloader.OnProgress := DownloaderProgress;
      if Downloader.Prepare then
        begin
        LabelVideoName.Caption := Downloader.Name;
        if not Downloader.Download then
          MessageDlg('Download failed.', mtError, [mbOK], 0);
        end
      else
        MessageDlg(Downloader.LastErrorMsg, mtError, [mbOK], 0);
      end;
  finally
    PanelProgress.Visible := False;
    PanelSettings.Enabled := True;
    Screen.Cursor := crDefault;
    BtnAbort.Visible := False;
    BtnDownload.Enabled := True;
    end;
end;

procedure TFormYTD.DownloaderProgress(Sender: TObject; TotalSize, DownloadedSize: int64; var DoAbort: boolean);
var Proc: int64;
begin
  if TotalSize > 0 then
    begin
    Proc := 1000 * DownloadedSize div TotalSize;
    if Proc <> LastProc then
      begin
      LastProc := Proc;
      ProgressBar1.Position := Proc div 10;
      LabelVideoProgress.Caption := Format('%d.%d %%', [Proc div 10, Proc mod 10]);
      end;
    end;
  Application.ProcessMessages;
  DoAbort := TransferAborted;
end;

end.
