unit uDownloadList;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Dialogs,
  uDownloadClassifier, uDownloader,
  uPlaylistDownloader, uPlaylist_HTML, uPlaylist_HTMLfile,
  uDownloadListItem, uDownloadThread;

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
      fAutoStart: boolean;
      fAutoOverwrite: boolean;
      fDestinationPath: string;
    protected
      function GetCount: integer; virtual;
      function GetItem(Index: integer): TDownloadListItem; virtual;
      function GetUrl(Index: integer): string; virtual;
      function GetDownloadingCount: integer; virtual;
      function GetDownloadingItem(Index: integer): TDownloadListItem; virtual;
      procedure SetDestinationPath(const Value: string); virtual;
      procedure NotifyList; virtual;
      procedure Notify(Event: TDownloadListNotifyEvent; Item: TDownloadListItem); virtual;
      procedure DownloadItemStateChange(Sender: TObject); virtual;
      procedure DownloadItemDownloadProgress(Sender: TObject); virtual;
      procedure DownloadItemFileNameValidate(Sender: TObject; var FileName: string; var Valid: boolean); virtual;
      procedure DownloadItemError(Sender: TObject); virtual;
      procedure DownloadItemThreadFinished(Sender: TObject); virtual;
      property DownloadClassifier: TDownloadClassifier read fDownloadClassifier;
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
      function Add(const Url: string): integer; virtual;
      function AddFromHTML(const Source: string): integer; virtual;
      function IndexOf(Item: TDownloadListItem): integer; virtual;
      procedure Delete(Item: TDownloadListItem); overload; virtual;
      procedure Delete(Index: integer); overload; virtual;
      property Count: integer read GetCount;
      property Items[Index: integer]: TDownloadListItem read GetItem; default;
      property Urls[Index: integer]: string read GetUrl;
      property DownloadingCount: integer read GetDownloadingCount;
      property DownloadingItems[Index: integer]: TDownloadListItem read GetDownloadingItem;
    published
      property AutoStart: boolean read fAutoStart write fAutoStart;
      property AutoOverwrite: boolean read fAutoOverwrite write fAutoOverwrite;
      property DestinationPath: string read fDestinationPath write SetDestinationPath;
      property OnStateChange: TDownloadListNotifyEvent read fOnStateChange write fOnStateChange;
      property OnDownloadProgress: TDownloadListNotifyEvent read fOnDownloadProgress write fOnDownloadProgress;
      property OnError: TDownloadListNotifyEvent read fOnError write fOnError;
      property OnFinished: TDownloadListNotifyEvent read fOnFinished write fOnFinished;
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
  fAutoStart := True;
  fAutoOverwrite := False;
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
  AutoSt := AutoStart;
  try
    AutoStart := False;
    StopAll;
    for i := 0 to Pred(Count) do
      Items[i].Free;
    List.Clear;
    DownloadingList.Clear;
  finally
    AutoStart := AutoSt;
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

procedure TDownloadList.SetDestinationPath(const Value: string);
var i: integer;
begin
  fDestinationPath := Value;
  for i := 0 to Pred(Count) do
    if Items[i].Waiting then
      Items[i].Downloader.DestinationPath := Value;
end;

function TDownloadList.Add(const Url: string): integer;
var Item: TDownloadListItem;
begin
  DownloadClassifier.Url := Url;
  if DownloadClassifier.Downloader <> nil then
    begin
    Item := TDownloadListItem.Create(DownloadClassifier.Downloader, True);
    Item.Downloader.DestinationPath := DestinationPath;
    Item.OnStateChange := DownloadItemStateChange;
    Item.OnDownloadProgress := DownloadItemDownloadProgress;
    Item.OnFileNameValidate := DownloadItemFileNameValidate;
    Item.OnError := DownloadItemError;
    Item.OnThreadFinished := DownloadItemThreadFinished;
    Result := List.AddObject(Url, Item);
    NotifyList;
    if AutoStart then
      StartAll;
    end
  else
    Result := -1;
end;

function TDownloadList.AddFromHTML(const Source: string): integer;
const HTTP_PREFIX = 'http://';
      HTTPS_PREFIX = 'https://';
var Downloader: TPlaylist_HTML;
    Item: TDownloadListItem;
begin
  if (AnsiCompareText(Copy(Source, 1, Length(HTTP_PREFIX)), HTTP_PREFIX) = 0) or (AnsiCompareText(Copy(Source, 1, Length(HTTPS_PREFIX)), HTTPS_PREFIX) = 0) then
    Downloader := TPlaylist_HTML.Create(Source)
  else
    Downloader := TPlaylist_HTMLfile.Create(Source);
  Item := TDownloadListItem.Create(Downloader, True);
  Item.Downloader.DestinationPath := DestinationPath;
  Item.OnStateChange := DownloadItemStateChange;
  Item.OnDownloadProgress := DownloadItemDownloadProgress;
  Item.OnFileNameValidate := DownloadItemFileNameValidate;
  Item.OnError := DownloadItemError;
  Item.OnThreadFinished := DownloadItemThreadFinished;
  Result := List.AddObject(Source, Item);
  NotifyList;
  if AutoStart then
    StartAll;
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
var D: TSaveDialog;
begin
  if FileExists(DestinationPath + FileName) then
    if AutoOverwrite then
      Valid := True
    else
      begin
      D := TSaveDialog.Create(nil);
      try
        D.DefaultExt := Copy(ExtractFileExt(FileName), 2, MaxInt);
        D.FileName := FileName;
        D.InitialDir := DestinationPath;
        if D.InitialDir = '' then
          D.InitialDir := GetCurrentDir;
        D.Options := D.Options + [ofOverwritePrompt, ofNoChangeDir, ofNoReadOnlyReturn] - [ofReadOnly];
        D.Title := 'File already exists.';
        Valid := D.Execute;
        if Valid then
          FileName := ExtractFileName(D.FileName);
      finally
        D.Free;
        end;
      end;
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
  if AutoStart then
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
        if Item.Downloader.Provider = DownloadingItems[j].Downloader.Provider then
          begin
          CanStart := False;
          Break;
          end;
      end;
    if CanStart then
      Start(Item);
    end;
end;

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

end.
