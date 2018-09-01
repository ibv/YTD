unit uNestedDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, blcksock, 
  uDownloader, uCommonDownloader,
  uOptions;

type
  TNestedDownloader = class(TCommonDownloader)
    private
      fNestedDownloader: TDownloader;
      {$IFDEF MULTIDOWNLOADS}
      fFirstItem: boolean;
      {$ENDIF}
    protected
      NestedIDRegExp: TRegExp;
      NestedUrlRegExp: TRegExp;
    protected
      function GetFileName: string; override;
      function GetThisFileName: string; virtual;
      procedure SetNestedDownloader(Value: TDownloader); virtual;
      function CreateNestedDownloaderFromID(const MovieID: string): boolean; virtual;
      function CreateNestedDownloaderFromURL(var Url: string): boolean; virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
      procedure NestedFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      property NestedDownloader: TDownloader read fNestedDownloader write SetNestedDownloader;
      {$IFDEF MULTIDOWNLOADS}
      property FirstItem: boolean read fFirstItem write fFirstItem;
      {$ENDIF}
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
      function Prepare: boolean; override;
      function Download: boolean; override;
      {$IFDEF MULTIDOWNLOADS}
      function First: boolean; override;
      function Next: boolean; override;
      {$ENDIF}
    end;

implementation

uses
  uMessages,
  uDownloadClassifier;
  
{ TNestedDownloader }

constructor TNestedDownloader.Create(const AMovieID: string);
begin
  inherited;
  fNestedDownloader := nil;
end;

destructor TNestedDownloader.Destroy;
begin
  FreeAndNil(fNestedDownloader);
  RegExFreeAndNil(NestedIDRegExp);
  RegExFreeAndNil(NestedUrlRegExp);
  inherited;
end;

procedure TNestedDownloader.SetNestedDownloader(Value: TDownloader);
begin
  FreeAndNil(fNestedDownloader);
  fNestedDownloader := Value;
end;

function TNestedDownloader.CreateNestedDownloaderFromID(const MovieID: string): boolean;
begin
  Result := False;
end;

function TNestedDownloader.CreateNestedDownloaderFromURL(var Url: string): boolean;
var DC: TDownloadClassifier;
begin
  Result := False;
  DC := TDownloadClassifier.Create;
  try
    DC.OwnsDownloader := False;
    DC.Url := Url;
    if DC.Downloader <> nil then
      begin
      MovieURL := Url;
      NestedDownloader := DC.Downloader;
      NestedDownloader.Options := Options;
      NestedDownloader.OnProgress := OnProgress;
      NestedDownloader.OnFileNameValidate := NestedFileNameValidate;
      Result := True;
      end;
  finally
    DC.Free;
    end;
end;

function TNestedDownloader.GetFileName: string;
var NestedFN: string;
begin
  Result := GetThisFileName;
  if NestedDownloader <> nil then
    begin
    NestedFN := NestedDownloader.FileName;
    {$IFDEF MULTIDOWNLOADS}
    if not FirstItem then
      Result := NestedDownloader.FileName
    else
    {$ENDIF}
      Result := {ChangeFileExt(}Result{, '')} + ExtractFileExt(NestedFN);
    end;
end;

function TNestedDownloader.GetThisFileName: string;
begin
  Result := inherited GetFileName;
end;

function TNestedDownloader.Prepare: boolean;
begin
  NestedDownloader := nil;
  Result := False;
  if inherited Prepare then
    if NestedDownloader <> nil then
      Result := NestedDownloader.Prepare;
end;

function TNestedDownloader.Download: boolean;
begin
  inherited Download;
  Result := False;
  if NestedDownloader <> nil then
    begin
    NestedDownloader.Options := Options;
    NestedDownloader.OnProgress := OnProgress;
    NestedDownloader.OnFileNameValidate := NestedFileNameValidate;
    Result := NestedDownloader.ValidateFileName and NestedDownloader.Download;
    end;
end;

{$IFDEF MULTIDOWNLOADS}
function TNestedDownloader.First: boolean;
begin
  if NestedDownloader <> nil then
    Result := NestedDownloader.First
  else
    Result := False;
  FirstItem := Result;
end;

function TNestedDownloader.Next: boolean;
begin
  if NestedDownloader <> nil then
    Result := NestedDownloader.Next
  else
    Result := False;
  FirstItem := False;
end;
{$ENDIF}

function TNestedDownloader.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
var ID, Url: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO));
  if (NestedIDRegExp <> nil) and GetRegExpVar(NestedIDRegExp, Page, 'ID', ID) and (ID <> '') and CreateNestedDownloaderFromID(ID) then
    begin
    if NestedUrlRegExp <> nil then
      if GetRegExpVar(NestedUrlRegExp, Page, 'URL', Url) then
        MovieURL := Url;
    SetPrepared(True);
    Result := True;
    end
  else if (NestedUrlRegExp <> nil) and GetRegExpVar(NestedUrlRegExp, Page, 'URL', Url) and (Url <> '') and CreateNestedDownloaderFromURL(Url) then
    begin
    MovieUrl := Url;
    SetPrepared(True);
    Result := True;
    end;
end;

procedure TNestedDownloader.NestedFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean);
begin
  FileName := GetThisFileName;
  Valid := ValidateFileName(FileName);
end;

end.
