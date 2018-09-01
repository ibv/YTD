unit uDownloadList;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Dialogs,
  uDownloadClassifier, uDownloader,
  uPlaylistDownloader, listHTML, listHTMLfile,
  uDownloadListItem, uDownloadThread,
  uOptions, uLanguages;

type
  TDownloadList = class;

  TDownloadListNotifyEvent = procedure(Sender: TDownloadList; Item: TDownloadListItem) of object;

  TDownloadList = class
    private
      fDownloadClassifier: TDownloadClassifier;
      fList: TStringList;
      fDownloadingList: TList;
      fOnListChange: TNotifyEvent;
      fOnStateChange: TDownloadListNotifyEvent;
      fOnDownloadProgress: TDownloadListNotifyEvent;
      fOnError: TDownloadListNotifyEvent;
      fOnFinished: TDownloadListNotifyEvent;
      {$IFDEF CONVERTERS}
      fOnConverted: TDownloadListNotifyEvent;
      {$ENDIF}
      fOptions: TYTDOptions;
    protected
      function GetCount: integer; virtual;
      function GetItem(Index: integer): TDownloadListItem; virtual;
      function GetUrl(Index: integer): string; virtual;
      function GetDownloadingCount: integer; virtual;
      function GetDownloadingItem(Index: integer): TDownloadListItem; virtual;
      procedure NotifyList; virtual;
      procedure Notify(Event: TDownloadListNotifyEvent; Item: TDownloadListItem); virtual;
      procedure DownloadItemStateChange(Sender: TObject); virtual;
      procedure DownloadItemDownloadProgress(Sender: TObject); virtual;
      procedure DownloadItemFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      procedure DownloadItemError(Sender: TObject); virtual;
      procedure DownloadItemThreadFinished(Sender: TObject); virtual;
      {$IFDEF CONVERTERS}
      procedure DownloadItemConvertThreadFinished(Sender: TObject); virtual;
      {$ENDIF}
      function AddNewItem(const Source: string; Downloader: TDownloader): integer; virtual;
      property List: TStringList read fList;
      property DownloadingList: TList read fDownloadingList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear; virtual;
      procedure StartAll; virtual;
      procedure StopAll; virtual;
      procedure PauseAll; virtual;
      procedure Start(Item: TDownloadListItem); virtual;
      procedure Stop(Item: TDownloadListItem); virtual;
      procedure Pause(Item: TDownloadListItem); virtual;
      {$IFDEF CONVERTERS}
      procedure ConvertAll; virtual;
      {$ENDIF}
      function Add(const Url: string): integer; virtual;
      function AddFromHTML(const Source: string): integer; virtual;
      function IndexOf(Item: TDownloadListItem): integer; virtual;
      procedure Delete(Item: TDownloadListItem); overload; virtual;
      procedure Delete(Index: integer); overload; virtual;
      procedure LoadFromOptions; virtual;
      procedure SaveToOptions; virtual;
      property Count: integer read GetCount;
      property Items[Index: integer]: TDownloadListItem read GetItem; default;
      property Urls[Index: integer]: string read GetUrl;
      property DownloadingCount: integer read GetDownloadingCount;
      property DownloadingItems[Index: integer]: TDownloadListItem read GetDownloadingItem;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
    published
      property Options: TYTDOptions read fOptions write fOptions;
      property OnStateChange: TDownloadListNotifyEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TDownloadListNotifyEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnError: TDownloadListNotifyEvent read fOnError write fOnError;
      property OnFinished: TDownloadListNotifyEvent read fOnFinished write fOnFinished;
      {$IFDEF CONVERTERS}
      property OnConverted: TDownloadListNotifyEvent read fOnConverted write fOnConverted;
      {$ENDIF}
      property OnListChange: TNotifyEvent read fOnListChange write fOnListChange;
    end;

implementation

{ TDownloadList }

constructor TDownloadList.Create;
begin
  inherited Create;
  fDownloadClassifier := TDownloadClassifier.Create;
  fDownloadClassifier.OwnsDownloader := False;
  fList := TStringList.Create;
  fDownloadingList := TList.Create;
end;

destructor TDownloadList.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  FreeAndNil(fDownloadingList);
  FreeAndNil(fDownloadClassifier);
  inherited;
end;

procedure TDownloadList.Clear;
var i: integer;
    AutoSt: boolean;
begin
  AutoSt := Options.AutoStartDownloads;
  try
    Options.AutoStartDownloads := False;
    StopAll;
    for i := 0 to Pred(Count) do
      Items[i].Free;
    List.Clear;
    DownloadingList.Clear;
  finally
    Options.AutoStartDownloads := AutoSt;
    end;
end;

function TDownloadList.GetCount: integer;
begin
  Result := List.Count;
end;

function TDownloadList.GetItem(Index: integer): TDownloadListItem;
begin
  Result := TDownloadListItem(List.Objects[Index]);
end;

function TDownloadList.GetUrl(Index: integer): string;
begin
  Result := List[Index];
end;

function TDownloadList.GetDownloadingCount: integer;
begin
  Result := DownloadingList.Count;
end;

function TDownloadList.GetDownloadingItem(Index: integer): TDownloadListItem;
begin
  Result := TDownloadListItem(DownloadingList[Index]);
end;

function TDownloadList.AddNewItem(const Source: string; Downloader: TDownloader): integer; 
var Item: TDownloadListItem;
begin
  Item := TDownloadListItem.Create(Downloader, True);
  Item.Options := Options;
  Item.OnStateChange := DownloadItemStateChange;
  Item.OnDownloadProgress := DownloadItemDownloadProgress;
  Item.OnFileNameValidate := DownloadItemFileNameValidate;
  Item.OnError := DownloadItemError;
  Item.OnThreadFinished := DownloadItemThreadFinished;
  {$IFDEF CONVERTERS}
  Item.OnConvertThreadFinished := DownloadItemConvertThreadFinished;
  {$ENDIF}
  Result := List.AddObject(Source, Item);
  NotifyList;
  if Options.AutoStartDownloads then
    StartAll;
end;

function TDownloadList.Add(const Url: string): integer;
begin
  DownloadClassifier.Url := Url;
  if DownloadClassifier.Downloader <> nil then
    Result := AddNewItem(Url, DownloadClassifier.Downloader)
  else
    Result := -1;
end;

function TDownloadList.AddFromHTML(const Source: string): integer;
const HTTP_PREFIX = 'http://';
      HTTPS_PREFIX = 'https://';
var Downloader: TPlaylist_HTML;
begin
  if (AnsiCompareText(Copy(Source, 1, Length(HTTP_PREFIX)), HTTP_PREFIX) = 0) or (AnsiCompareText(Copy(Source, 1, Length(HTTPS_PREFIX)), HTTPS_PREFIX) = 0) then
    Downloader := TPlaylist_HTML.Create(Source)
  else
    Downloader := TPlaylist_HTMLfile.Create(Source);
  Result := AddNewItem(Source, Downloader);
end;

function TDownloadList.IndexOf(Item: TDownloadListItem): integer;
begin
  Result := List.IndexOfObject(Item);
end;

procedure TDownloadList.Delete(Item: TDownloadListItem);
begin
  Delete(IndexOf(Item));
end;

procedure TDownloadList.Delete(Index: integer);
begin
  Stop(Items[Index]);
  List.Objects[Index].Free;
  List.Delete(Index);
  NotifyList;
end;

procedure TDownloadList.DownloadItemDownloadProgress(Sender: TObject);
begin
  Notify(OnDownloadProgress, TDownloadListItem(Sender));
end;

procedure TDownloadList.DownloadItemFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean);

    function AutoRename(var FileName: string): boolean;
      var FileNameBase, FileNameExt: string;
          Index: integer;
      begin
        Index := 1;
        FileNameExt := ExtractFileExt(FileName);
        FileNameBase := ChangeFileExt(FileName, '');
        repeat
          FileName := Format('%s%s.%d%s', [Options.DestinationPath, FileNameBase, Index, FileNameExt]);
          Inc(Index);
        until not FileExists(FileName);
        Result := True;
      end;

var D: TSaveDialog;
begin
  if FileExists(Options.DestinationPath + FileName) then
    case Options.OverwriteMode of
      omAlways:
        Valid := True;
      omNever:
        Valid := False;
      omRename:
        Valid := AutoRename(FileName);
      else
        begin
        D := TSaveDialog.Create(nil);
        try
          D.DefaultExt := Copy(ExtractFileExt(FileName), 2, MaxInt);
          D.FileName := FileName;
          D.InitialDir := ExcludeTrailingBackslash(Options.DestinationPath);
          if D.InitialDir = '' then
            D.InitialDir := GetCurrentDir;
          D.Options := D.Options + [ofOverwritePrompt, ofNoChangeDir, ofNoReadOnlyReturn] - [ofReadOnly];
          D.Title := _('File already exists.'); // GUI: Filename already exists. User is being asked to provide a new filename or confirm the existing one.
          Valid := D.Execute;
          if Valid then
            FileName := ExtractFileName(D.FileName);
        finally
          D.Free;
          end;
        end;
      end;
  if Valid and FileExists(Options.DestinationPath + FileName) then
    DeleteFile(Options.DestinationPath + FileName);
end;

procedure TDownloadList.DownloadItemError(Sender: TObject);
begin
  Notify(OnError, TDownloadListItem(Sender));
end;

procedure TDownloadList.DownloadItemStateChange(Sender: TObject);
begin
  Notify(OnStateChange, TDownloadListItem(Sender));
end;

procedure TDownloadList.DownloadItemThreadFinished(Sender: TObject);
var Item: TDownloadListItem;
    Playlist: TPlaylistDownloader;
    i: integer;
begin
  Item := TDownloadListItem(Sender);
  Notify(OnFinished, Item);
  Stop(Item);
  if (Item.Downloader is TPlaylistDownloader) and (Item.State = dtsFinished) then
    begin
    Playlist := TPlaylistDownloader(Item.Downloader);
    for i := 0 to Pred(Playlist.Count) do
      Add(Playlist[i]);
    end;
  if Options.AutoStartDownloads then
    StartAll;
end;

procedure TDownloadList.NotifyList;
begin
  if @OnListChange <> nil then
    OnListChange(Self);
end;

procedure TDownloadList.Notify(Event: TDownloadListNotifyEvent; Item: TDownloadListItem);
begin
  if @Event <> nil then
    Event(Self, Item);
end;

procedure TDownloadList.Start(Item: TDownloadListItem);
var i: integer;
begin
  i := Pred(DownloadingCount);
  while i >= 0 do
    if DownloadingItems[i] = Item then
      Break
    else
      Dec(i);
  if i < 0 then
    {i :=} DownloadingList.Add(Item);
  Item.Start;
end;

procedure TDownloadList.Stop(Item: TDownloadListItem);
var i: integer;
begin
  for i := 0 to Pred(DownloadingCount) do
    if DownloadingItems[i] = Item then
      begin
      DownloadingList.Delete(i);
      Item.Stop;
      Break;
      end;
end;

procedure TDownloadList.Pause(Item: TDownloadListItem);
var i: integer;
begin
  for i := 0 to Pred(DownloadingCount) do
    if DownloadingItems[i] = Item then
      begin
      Item.Pause;
      Break;
      end;
end;

procedure TDownloadList.StartAll;
var Item: TDownloadListItem;
    i, j: integer;
    CanStart: boolean;
begin
  for i := 0 to Pred(DownloadingCount) do
    if DownloadingItems[i].Paused then
      DownloadingItems[i].Start;
  for i := 0 to Pred(Count) do
    begin
    Item := Items[i];
    CanStart := False;
    if Item.Waiting then
      begin
      CanStart := True;
      for j := 0 to Pred(DownloadingCount) do
        if Item.Downloader.UltimateProvider = DownloadingItems[j].Downloader.UltimateProvider then
          begin
          CanStart := False;
          Break;
          end;
      end;
    if CanStart then
      Start(Item);
    end;
  {$IFDEF CONVERTERS}
  ConvertAll;
  {$ENDIF}
end;

{$IFDEF CONVERTERS}
procedure TDownloadList.ConvertAll;
var i, ConvertThreadCount, MaxConvertThreadCount: integer;
begin
  if Options.SelectedConverterID <> '' then
    begin
    ConvertThreadCount := 0;
    for i := 0 to Pred(Count) do
      if Items[i].ConvertState = ctsConverting then
        Inc(ConvertThreadCount);
    MaxConvertThreadCount := Options.MaxConversionThreads;
    i := 0;
    while (ConvertThreadCount < MaxConvertThreadCount) and (i < Count) do
      begin
      if Items[i].Convert then
        Inc(ConvertThreadCount);
      Inc(i);
      end;
    end;
end;

procedure TDownloadList.DownloadItemConvertThreadFinished(Sender: TObject);
var Item: TDownloadListItem;
begin
  Item := TDownloadListItem(Sender);
  Notify(OnConverted, Item);
  ConvertAll;
end;
{$ENDIF}

procedure TDownloadList.StopAll;
begin
  while DownloadingCount > 0 do
    Stop(DownloadingItems[0]);
end;

procedure TDownloadList.PauseAll;
var i: integer;
begin
  for i := 0 to Pred(DownloadingCount) do
    Pause(DownloadingItems[i]);
end;

procedure TDownloadList.LoadFromOptions;
var L: TStringList;
    i: integer;
begin
  L := TStringList.Create;
  try
    Options.ReadUrlList(L);
    for i := 0 to Pred(L.Count) do
      if L[i] <> '' then
        Add(L[i]);
  finally
    L.Free;
    end;
end;

procedure TDownloadList.SaveToOptions;
var L: TStringList;
    i: integer;
begin
  L := TStringList.Create;
  try
    for i := 0 to Pred(Count) do
      if Items[i].State <> dtsFinished then
        L.Add(Urls[i]);
    Options.WriteUrlList(L);
  finally
    L.Free;
    end;
end;

end.
