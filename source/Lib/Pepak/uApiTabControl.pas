unit uApiTabControl;

interface
{$INCLUDE 'uApi.inc'}

uses
  SysUtils, Classes, Windows, CommCtrl,
  {$IFDEF DELPHI2009_UP} RtlConsts, {$ELSE} Consts, {$ENDIF}
  uApiCommon, uApiFunctions, uApiForm;

type
  TApiTabSheet = class(TApiForm);

  TApiTabControl = class
    private
      fHandle: THandle;
      fOwnsPages: boolean;
      fActivePageIndex: integer;
      function GetCount: integer;
      function GetPage(Index: integer): TApiTabSheet;
      procedure SetActivePageIndex(Value: integer);
      function GetRealActivePageIndex: integer;
    protected
    public
      constructor Create(AHandle: THandle; AOwnsPages: boolean = False); virtual;
      destructor Destroy; override;
      procedure Clear;
      function Add(Page: TApiTabSheet; const Caption: string): integer;
      function Insert(Index: integer; Page: TApiTabSheet; const Caption: string): integer;
      procedure Delete(Index: integer);
      procedure ResizeTabs;
      property Handle: THandle read fHandle;
      property OwnsPages: boolean read fOwnsPages write fOwnsPages;
      property Count: integer read GetCount;
      property Pages[Index: integer]: TApiTabSheet read GetPage; default;
      property ActivePageIndex: integer read fActivePageIndex write SetActivePageIndex;
      property RealActivePageIndex: integer read GetRealActivePageIndex;
    end;

implementation

{ TApiTabControl }

constructor TApiTabControl.Create(AHandle: THandle; AOwnsPages: boolean);
begin
  inherited Create;
  fHandle := AHandle;
  fOwnsPages := AOwnsPages;
  fActivePageIndex := -1;
end;

destructor TApiTabControl.Destroy;
begin
  Clear;
  inherited;
end;

function TApiTabControl.GetCount: integer;
begin
  Result := SendMessage(Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TApiTabControl.GetPage(Index: integer): TApiTabSheet;
var Item: TTCITEM;
begin
  if (Index < 0) or (Index >= Count) then
    Raise EListError.Create(SListIndexError);
  FillChar(Item, Sizeof(Item), 0);
  Item.mask := TCIF_PARAM;
  ShowApiError(SendMessage(Handle, TCM_GETITEM, Index, LPARAM(@Item)) = 0);
  Result := TApiTabSheet(Item.lParam);
end;

function TApiTabControl.Add(Page: TApiTabSheet; const Caption: string): integer;
begin
  Result := Insert(Count, Page, Caption);
end;

function TApiTabControl.Insert(Index: integer; Page: TApiTabSheet; const Caption: string): integer;
var Item: TTCITEM;
    TabOrderPredecessor: THandle;
begin
  FillChar(Item, Sizeof(Item), 0);
  Item.mask := TCIF_TEXT or TCIF_PARAM;
  Item.pszText := PChar(Caption);
  Item.lParam := LPARAM(Page);
  Result := SendMessage(Handle, TCM_INSERTITEM, Index, LPARAM(@Item));
  ShowApiError(Result = -1);
  if Page.Handle <> 0 then
    begin
    if (Index > 0) and (Index <= Count) then
      TabOrderPredecessor := Pages[Pred(Index)].Handle
    else
      TabOrderPredecessor := Handle;
    SetWindowPos(Page.Handle, TabOrderPredecessor, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    end;
end;

procedure TApiTabControl.Delete(Index: integer);
var Page: TApiTabSheet;
begin
  Page := Pages[Index];
  ShowApiError(SendMessage(Handle, TCM_DELETEITEM, Index, 0) = 0);
  if Index = fActivePageIndex then
    ActivePageIndex := Pred(Index) // Use property because I need to actually change the displayed page
  else if Index < fActivePageIndex then
    fActivePageIndex := Pred(fActivePageIndex); // Use variable because the page remains unchanged
  if OwnsPages and (Page <> nil) then
    Page.Free;
end;

procedure TApiTabControl.Clear;
var i: integer;
begin
  for i := Pred(Count) downto 0 do
    Delete(i);
end;

procedure TApiTabControl.ResizeTabs;
var Rect: TRect;
    i: integer;
begin
  ShowApiError(not GetClientRect(Handle, Rect));
  SendMessage(Handle, TCM_ADJUSTRECT, 0, LPARAM(@Rect));
  for i := 0 to Pred(Count) do
    if Pages[i] <> nil then
      MoveWindow(Pages[i].Handle, Rect.Left, Rect.Top, Rect.Right-Rect.Left, Rect.Bottom-Rect.Top, True);
end;

procedure TApiTabControl.SetActivePageIndex(Value: integer);
begin
  // Hide the last page
  if (fActivePageIndex >= 0) and (fActivePageIndex < Count) then
    if Pages[fActivePageIndex] <> nil then
      ShowWindow(Pages[fActivePageIndex].Handle, SW_HIDE);
  // Change the selection
  SendMessage(Handle, TCM_SETCURSEL, Value, 0);
  fActivePageIndex := Value;
  // Show the new page
  if (fActivePageIndex >= 0) and (fActivePageIndex < Count) then
    if Pages[fActivePageIndex] <> nil then
      ShowWindow(Pages[fActivePageIndex].Handle, SW_SHOWDEFAULT);
end;

function TApiTabControl.GetRealActivePageIndex: integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

end.
