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

unit uDownloadList;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  {$IFDEF GUI_WINAPI} uDialogs, {$ELSE} Dialogs, {$ENDIF}
  uCompatibility, uDownloadListItem, uDownloadThread,
  uDownloadClassifier, uDownloader,
  uPlaylistDownloader, listHTML, listHTMLfile,
  uOptions, uLanguages, uFunctions;

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
      fAutoTryHtmlParserTemporarilyDisabled: boolean;
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
      function AddNewItem(const Source, Title: string; Downloader: TDownloader): integer; virtual;
      function AutoTryHtmlParser: boolean;
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
      function Add(const Url: string; const Title: string = ''): integer; virtual;
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
      property AutoTryHtmlParserTemporarilyDisabled: boolean read fAutoTryHtmlParserTemporarilyDisabled write fAutoTryHtmlParserTemporarilyDisabled;
    public
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

function TDownloadList.AddNewItem(const Source, Title: string; Downloader: TDownloader): integer; 
var Item: TDownloadListItem;
begin
  Item := TDownloadListItem.Create(Downloader, True);
  Item.Title := Title;
  Item.RetryCount := Options.DownloadRetryCount;
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

function TDownloadList.Add(const Url, Title: string): integer;
begin
  DownloadClassifier.Url := Url;
  if DownloadClassifier.Downloader <> nil then
    Result := AddNewItem(Url, Title, DownloadClassifier.Downloader)
  else if AutoTryHtmlParser then
    Result := AddFromHTML(Url)
  else
    Result := -1;
end;

function TDownloadList.AddFromHTML(const Source: string): integer;
var
  Downloader: TPlaylist_HTML;
begin
  if IsHttpProtocol(Source) then
    Downloader := TPlaylist_HTML.Create(Source)
  else
    Downloader := TPlaylist_HTMLfile.Create(Source);
  Result := AddNewItem(Source, '', Downloader);
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
  if Index >= 0 then
    begin
    Stop(Items[Index]);
    List.Objects[Index].Free;
    List.Delete(Index);
    NotifyList;
    end;
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
          FileName := Format('%s.%d%s', [FileNameBase, Index, FileNameExt]);
          Inc(Index);
        until not FileExists(FileName);
        Result := True;
      end;

var
  InitialDir: string;
{$IFNDEF GUI_WINAPI}
var
  D: TSaveDialog;
{$ENDIF}
begin
  if FileExists(FileName) then
    case Options.OverwriteMode of
      omAlways:
        Valid := True;
      omNever:
        Valid := False;
      omRename:
        Valid := AutoRename(FileName);
      else
        begin
        InitialDir := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
        if InitialDir = '' then
          InitialDir := GetCurrentDir;
        {$IFDEF GUI_WINAPI}
        Valid := SaveDialog(FileName, InitialDir, PChar(_('File already exists.'))); // GUI: Filename already exists. User is being asked to provide a new filename or confirm the existing one.
        {$ELSE}
        D := TSaveDialog.Create(nil);
        try
          D.DefaultExt := Copy(ExtractFileExt(FileName), 2, MaxInt);
          D.FileName := FileName;
          D.InitialDir := InitialDir;
          D.Options := D.Options + [ofOverwritePrompt, ofNoChangeDir, ofNoReadOnlyReturn] - [ofReadOnly];
          D.Title := _('File already exists.'); // GUI: Filename already exists. User is being asked to provide a new filename or confirm the existing one.
          Valid := D.Execute;
          if Valid then
            FileName := D.FileName;
        finally
          FreeAndNil(D);
          end;
        {$ENDIF}
        end;
      end;
  if Valid and FileExists(FileName) then
    SysUtils.DeleteFile(FileName);
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
      Add(Playlist[i], Playlist.Names[i]);
    end;
  if Options.AutoStartDownloads and (Item.State <> dtsAborted) then
    StartAll;
  if Options.AutoDeleteFinishedDownloads and (Item.State = dtsFinished) then
    Delete(Item);
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
  for i := Pred(DownloadingCount) downto 0 do
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

  function StartIfPermitted(Item: TDownloadListItem): boolean;
    var
      i: integer;
    begin
      Result := True;
      for i := 0 to Pred(DownloadingCount) do
        if Item.Downloader.Provider = DownloadingItems[i].Downloader.Provider then
          begin
          Result := False;
          Break;
          end;
      if Result then
        Start(Item);
    end;

var i: integer;
begin
  for i := 0 to Pred(DownloadingCount) do
    if DownloadingItems[i].Paused then
      DownloadingItems[i].Start;
  for i := 0 to Pred(Count) do
    if Items[i].Waiting then
      StartIfPermitted(Items[i]);
  for i := 0 to Pred(Count) do
    if Items[i].Failed and (Items[i].RetryCount > 0) then
      if StartIfPermitted(Items[i]) then
        Items[i].RetryCount := Pred(Items[i].RetryCount);
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
    FreeAndNil(L);
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
    FreeAndNil(L);
    end;
end;

function TDownloadList.AutoTryHtmlParser: boolean;
begin
  Result := (not AutoTryHtmlParserTemporarilyDisabled) and Options.AutoTryHtmlParser;
end;

end.
