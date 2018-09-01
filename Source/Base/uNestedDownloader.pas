unit uNestedDownloader;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend, blcksock, janXmlParser2,
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
      NestedIDRegExp: IRegEx;
      NestedUrlRegExp: IRegEx;
    protected
      function GetFileName: string; override;
      procedure SetNestedDownloader(Value: TDownloader); virtual;
      procedure CreateNestedDownloader(const MovieID: string); virtual;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
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
  uMessages;
  
{ TNestedDownloader }

constructor TNestedDownloader.Create(const AMovieID: string);
begin
  inherited;
  fNestedDownloader := nil;
end;

destructor TNestedDownloader.Destroy;
begin
  FreeAndNil(fNestedDownloader);
  NestedIDRegExp := nil;
  NestedUrlRegExp := nil;
  inherited;
end;

procedure TNestedDownloader.SetNestedDownloader(Value: TDownloader);
begin
  FreeAndNil(fNestedDownloader);
  fNestedDownloader := Value;
end;

procedure TNestedDownloader.CreateNestedDownloader(const MovieID: string);
begin
end;

function TNestedDownloader.GetFileName: string;
var NestedFN: string;
begin
  Result := inherited GetFileName;
  if NestedDownloader <> nil then
    begin
    NestedFN := NestedDownloader.FileName;
    {$IFDEF MULTIDOWNLOADS}
    if not FirstItem then
      Result := NestedDownloader.FileName
    else
    {$ENDIF}
      Result := ChangeFileExt(Result, '') + ExtractFileExt(NestedFN);
    end;
end;

function TNestedDownloader.Prepare: boolean;
begin
  NestedDownloader := nil;
  Result := False;
  if inherited Prepare then
    if NestedDownloader <> nil then
      begin
      Result := NestedDownloader.Prepare;
      end;
end;

function TNestedDownloader.Download: boolean;
begin
  inherited Download;
  Result := False;
  if NestedDownloader <> nil then
    begin
    NestedDownloader.InitOptions(Options);
    NestedDownloader.DestinationPath := DestinationPath;
    NestedDownloader.OnProgress := OnProgress;
    NestedDownloader.OnFileNameValidate := OnFileNameValidate;
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
  SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO);
  if NestedIDRegExp <> nil then
    if GetRegExpVar(NestedIDRegExp, Page, 'ID', ID) then
      begin
      CreateNestedDownloader(ID);
      if NestedUrlRegExp <> nil then
        if GetRegExpVar(NestedUrlRegExp, Page, 'URL', URL) then
          MovieURL := URL;
      SetPrepared(True);
      Result := True;
      end;
end;

end.
