unit uApiForm;

interface
{.DEFINE USE_PROP}
{$DEFINE ANCHORS} // Allows easy resizing of forms

uses
  SysUtils, Windows, Messages, CommCtrl,
  {$IFDEF ANCHORS} Contnrs, {$ENDIF}
  uApiGraphics;

type
  TApiForm = class;

  EApiError = class(Exception);

  TAlignment = (alLeft, alRight, alCenter);

  {$IFDEF ANCHORS}
  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;

  TAnchorDesc = class
    Handle: THandle;
    Anchors: TAnchors;
    Rect: TRect;
    Margins: TRect;
    end;
  {$ENDIF}

  // Returns TRUE if the message was handled (should not be passed to the parent window procedure)
  TApiFormSubclassFn = function (Handle: THandle; Form: TApiForm; var Msg: TMessage): boolean; stdcall;

  TApiForm = class
    private
      fDialogResourceName: string;
      fHandle: HWND;
      fModalResult: integer;
      {$IFDEF ANCHORS}
      fAnchorList: TObjectList;
      {$ENDIF}
    protected // Message handlers
      // Message handler for all messages
      function DialogProc(var Msg: TMessage): boolean; virtual;
      // Message handler for WM_INITDIALOG - form is being created
      function DoInitDialog: boolean; virtual;
      // Message handler for WM_CLOSE - form is being destroyed
      function DoClose: boolean; virtual;
      // Message handler for WM_COMMAND - shortcuts, menus, control-specific codes...
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; virtual;
      // Message handler for WM_NOTIFY - something happens to a control
      function DoNotify(Control: THandle; ControlID: DWORD; Code, WParam, LParam: integer; out NotifyResult: integer): boolean; virtual;
      // Message handler for WM_SIZE - form was resized
      function DoSize(ResizeType, NewWidth, NewHeight: integer): boolean; virtual;
    protected // Support for message handlers
      function CanClose: boolean; virtual;
    protected // Support functions for descendants
      // Add your own message handler to the specified window. Note: Each window may only be subclassed once.
      procedure SubClassAWindow(AHandle: THandle; AWndProc: TApiFormSubclassFn);
    protected // ListView functions
      // Add a column to a listview.
      function ListViewInsertColumn(ListView: THandle; Index, Subitem: integer; Alignment: TAlignment; Width: integer; const Title: string): integer; virtual;
    protected // Properties
      procedure SetModalResult(const Value: integer);
      property DialogResourceName: string read fDialogResourceName;
      {$IFDEF ANCHORS}
      function FindControlAnchorIndex(Control: THandle): integer;
      function FindControlAnchor(Control: THandle): TAnchorDesc;
      property AnchorList: TObjectList read fAnchorList;
      {$ENDIF}
      class function DefaultResourceName: string; virtual;
    public
      constructor Create(const ADialogResourceName: string); overload; virtual;
      constructor Create; overload; virtual;
      destructor Destroy; override;
      procedure Show; virtual;
      function ShowModal: integer; virtual;
      {$IFDEF ANCHORS}
        // Simulate anchors from VCL, to achieve easy resizing. If no anchor is defined
        // for a control, it behaves as if [akLeft, akTop] was assigned to it.
        procedure SetControlAnchors(Control: THandle; Anchors: TAnchors);
        procedure ClearControlAnchors(Control: THandle);
        procedure RepositionAnchoredControl(Control: THandle);
        function GetControlAnchors(Control: THandle; out Anchors: TAnchors): boolean;
      {$ENDIF}
      property ModalResult: integer read fModalResult;
      property Handle: HWND read fHandle;
    end;

procedure ShowApiError(IsError: boolean); overload;
procedure ShowApiError(LastError: DWORD); overload;

implementation

procedure ShowApiError(IsError: boolean);
begin
  if IsError then
    ShowApiError(GetLastError);
end;

procedure ShowApiError(LastError: DWORD);
var Buf: array[0..32768] of char;
    n: DWORD;
    Msg: string;
begin
  if LastError <> NO_ERROR then
    begin
    n := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, Buf, Sizeof(Buf), nil);
    if n = 0 then
      Msg := Format('Unknown error (%u, %08.8x).', [LastError, LastError])
    else
      begin
      Buf[n] := #0;
      Msg := string(Buf);
      end;
    Raise EApiError.Create(Msg);
    end;
end;

{$IFDEF USE_PROP}
const
  APIFORM_SELF_PROPERTY = '9AF510CC-C253-4F9E-9ACE-C39E533AD206';
{$ELSE}
const
  SELF_INDEX = DWL_USER;
{$ENDIF}

const
  APIFORM_SUBCLASS_OLDWNDPROC = '1634CAB6-8ECA-4ADF-890F-CA0C57A2B3E5';
  APIFORM_SUBCLASS_APIFORM = '19DD5893-5D7D-4251-A2A0-C792B70ED578';
  APIFORM_SUBCLASS_HANDLER = '22B34AE3-EE7E-4107-899D-D770E8D9D664';

function ApiFormDialogProc(Handle: HWND; Msg: Cardinal; wParam, lParam: integer): integer; stdcall;
var F: TApiForm;
    M: TMessage;
    Handled: boolean;
begin
  M.Msg := Msg;
  M.wParam := wParam;
  M.lParam := lParam;
  M.Result := 0;
  // Ulozim si ukazatel na Self pro pozdejsi pouziti
  if Msg = WM_INITDIALOG then
    begin
    {$IFDEF USE_PROP}
    SetProp(Handle, APIFORM_SELF_PROPERTY, lParam);
    {$ELSE}
    SetWindowLong(Handle, SELF_INDEX, lParam);
    {$ENDIF}
    F := TApiForm(lParam);
    F.fHandle := Handle;
    F.DialogProc(M);
    Result := 1;
    end
  else
    begin
    {$IFDEF USE_PROP}
    F := TApiForm(GetProp(Handle, APIFORM_SELF_PROPERTY));
    {$ELSE}
    F := TApiForm(GetWindowLong(Handle, SELF_INDEX));
    {$ENDIF}
    if F = nil then
      Result := 0
    else
      begin
      Handled := F.DialogProc(M);
      if Handled then
        begin
        if False
           or (Msg = WM_CHARTOITEM) or (Msg = WM_COMPAREITEM) or (Msg = WM_CTLCOLORBTN) or (Msg = WM_CTLCOLORDLG)
           or (Msg = WM_CTLCOLOREDIT) or (Msg = WM_CTLCOLORLISTBOX) or (Msg = WM_CTLCOLORSCROLLBAR) or (Msg = WM_CTLCOLORSTATIC)
           or (Msg = WM_INITDIALOG) or (Msg = WM_QUERYDRAGICON) or (Msg = WM_CTLCOLORBTN) or (Msg = WM_VKEYTOITEM)
        then
          Result := M.Result
        else
          begin
          SetWindowLong(Handle, DWL_MSGRESULT, M.Result);
          if Msg = WM_CLOSE then
            begin
            F.fHandle := 0;
            {$IFDEF USE_PROP}
            SetProp(Handle, APIFORM_SELF_PROPERTY, 0);
            {$ELSE}
            SetWindowLong(Handle, SELF_INDEX, 0);
            {$ENDIF}
            end;
          Result := 1;
          end;
        end
      else
        Result := 0;
      end;
    end;
end;

function ApiFormSubclassWndProc(Handle: HWND; Msg: Cardinal; wParam, lParam: integer): integer; stdcall;
var OldWndProc: TFNWndProc;
    Form: TApiForm;
    Handler: TApiFormSubclassFn;
    M: TMessage;
begin
  OldWndProc := TFNWndProc(GetProp(Handle, APIFORM_SUBCLASS_OLDWNDPROC));
  if OldWndProc = nil then
    Result := DefWindowProc(Handle, Msg, wParam, lParam)
  else
    begin
    Form := TApiForm(GetProp(Handle, APIFORM_SUBCLASS_APIFORM));
    Handler := TApiFormSubclassFn(GetProp(Handle, APIFORM_SUBCLASS_HANDLER));
    if Assigned(Handler) then
      begin
      M.Msg := Msg;
      M.WParam := wParam;
      M.LParam := lParam;
      M.Result := 0;
      if Handler(Handle, Form, M) then
        begin
        Result := M.Result;
        Exit;
        end;
      end;
    Result := CallWindowProc(OldWndProc, Handle, Msg, wParam, lParam);
    end;
end;

class function TApiForm.DefaultResourceName: string;
begin
  Result := ClassName;
end;

constructor TApiForm.Create(const ADialogResourceName: string);
begin
  inherited Create;
  fDialogResourceName := ADialogResourceName;
  fHandle := 0;
  {$IFDEF ANCHORS}
  fAnchorList := TObjectList.Create(True);
  {$ENDIF}
end;

constructor TApiForm.Create;
begin
  Create(DefaultResourceName);
end;

destructor TApiForm.Destroy;
begin
  if fHandle <> 0 then
    SendMessage(Handle, WM_CLOSE, 0, 0);
  fHandle := 0;
  {$IFDEF ANCHORS}
  FreeAndNil(fAnchorList);
  {$ENDIF}
  inherited Destroy;
end;

function TApiForm.DialogProc(var Msg: TMessage): boolean;
begin
  Result := False;
  Msg.Result := 0;
  case Msg.Msg of
    WM_INITDIALOG:
      Result := DoInitDialog;
    WM_CLOSE:
      Result := DoClose;
    WM_COMMAND:
      Result := DoCommand(Msg.WParamHi, Msg.WParamLo, Msg.LParam);
    WM_NOTIFY:
      Result := DoNotify(PNMHdr(Msg.LParam)^.hwndFrom, PNMHdr(Msg.LParam)^.idFrom, PNMHdr(Msg.LParam)^.code, Msg.WParam, Msg.LParam, Msg.Result);
    WM_SIZE:
      Result := DoSize(Msg.wParam, Msg.lParam and $ffff, Msg.lParam shr 16); 
    end;
end;

function TApiForm.DoInitDialog: boolean;
begin
  Result := True;
end;

function TApiForm.DoClose: boolean;
begin
  if CanClose then
    EndDialog(Handle, ModalResult);
  Result := True;
end;

function TApiForm.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
end;

function TApiForm.DoNotify(Control: THandle; ControlID: DWORD; Code, WParam, LParam: integer; out NotifyResult: integer): boolean;
begin
  Result := False;
  NotifyResult := 0;
end;

function TApiForm.DoSize(ResizeType, NewWidth, NewHeight: integer): boolean;
{$IFDEF ANCHORS}
var i, NewX, NewY, NewW, NewH: integer;
    AnchorItem: TAnchorDesc;
    OwnerRect: TRect;
{$ENDIF}
begin
  Result := False;
  {$IFDEF ANCHORS}
  if GetClientRect(Self.Handle, OwnerRect) then
    for i := 0 to Pred(AnchorList.Count) do
      if AnchorList[i] <> nil then
        if AnchorList[i] is TAnchorDesc then
          begin
          AnchorItem := TAnchorDesc(AnchorList[i]);
          // Horizontal anchors:
            // Left and Right anchor - position and resize
            if (AnchorItem.Anchors * [akLeft, akRight]) = [akLeft, akRight] then
              begin
              NewW := (OwnerRect.Right - OwnerRect.Left) - AnchorItem.Margins.Left - AnchorItem.Margins.Right;
              NewX := AnchorItem.Margins.Left;
              end
            // Left anchor - just position, no sizing
            else if akLeft in AnchorItem.Anchors then
              begin
              NewW := AnchorItem.Rect.Right - AnchorItem.Rect.Left;
              NewX := AnchorItem.Margins.Left;
              end
            // right anchor - just position, no sizing
            else if akRight in AnchorItem.Anchors then
              begin
              NewW := AnchorItem.Rect.Right - AnchorItem.Rect.Left;
              NewX := (OwnerRect.Right - OwnerRect.Left) - AnchorItem.Margins.Right - NewW;
              end
            else
              begin
              NewW := AnchorItem.Rect.Right - AnchorItem.Rect.Left;
              NewX := AnchorItem.Rect.Left - OwnerRect.Left;
              end;
          // Vertical anchors:
            // Top and Bottom anchor - position and resize
            if (AnchorItem.Anchors * [akTop, akBottom]) = [akTop, akBottom] then
              begin
              NewH := (OwnerRect.Bottom - OwnerRect.Top) - AnchorItem.Margins.Top - AnchorItem.Margins.Bottom;
              NewY := AnchorItem.Margins.Top;
              end
            // Left anchor - just position, no sizing
            else if akTop in AnchorItem.Anchors then
              begin
              NewH := AnchorItem.Rect.Bottom - AnchorItem.Rect.Top;
              NewY := AnchorItem.Margins.Top;
              end
            // right anchor - just position, no sizing
            else if akBottom in AnchorItem.Anchors then
              begin
              NewH := AnchorItem.Rect.Bottom - AnchorItem.Rect.Top;
              NewY := (OwnerRect.Bottom - OwnerRect.Top) - AnchorItem.Margins.Bottom - NewH;
              end
            else
              begin
              NewH := AnchorItem.Rect.Bottom - AnchorItem.Rect.Top;
              NewY := AnchorItem.Rect.Top - OwnerRect.Top;
              end;
          MoveWindow(AnchorItem.Handle, NewX, NewY, NewW, NewH, True);
          end;
  {$ENDIF}
end;

function TApiForm.CanClose: boolean;
begin
  Result := True;
end;

procedure TApiForm.SetModalResult(const Value: integer);
begin
  fModalResult := Value;
end;

function TApiForm.ShowModal: integer;
begin
  SetModalResult(0);
  //fHandle := CreateDialogParam(hInstance, PChar(DialogResourceName), 0, @ApiFormDialogProc, Integer(Self));
  Result := DialogBoxParam(hInstance, PChar(DialogResourceName), 0, @ApiFormDialogProc, Integer(Self));
  ShowApiError(Result = -1);
  SetModalResult(Result);
end;

procedure TApiForm.Show;
begin
  SetModalResult(0);
  ShowApiError(CreateDialogParam(hInstance, PChar(DialogResourceName), 0, @ApiFormDialogProc, Integer(Self)) = 0);
end;

procedure TApiForm.SubClassAWindow(AHandle: THandle; AWndProc: TApiFormSubclassFn);
begin
  // If the window is already subclasses, just replace the window procedure
  if (GetProp(AHandle, APIFORM_SUBCLASS_OLDWNDPROC) <> 0) or (GetProp(AHandle, APIFORM_SUBCLASS_APIFORM) <> 0) then
    SetProp(AHandle, APIFORM_SUBCLASS_HANDLER, Integer(@AWndProc))
  // Otherwise register a new subclass
  else
    begin
    SetProp(AHandle, APIFORM_SUBCLASS_APIFORM, Integer(Self));
    SetProp(AHandle, APIFORM_SUBCLASS_OLDWNDPROC, GetWindowLong(AHandle, GWL_WNDPROC));
    SetProp(AHandle, APIFORM_SUBCLASS_HANDLER, Integer(@AWndProc));
    SetWindowLong(AHandle, GWL_WNDPROC, Integer(@ApiFormSubclassWndProc));
    end;
end;

{$IFDEF ANCHORS}
function TApiForm.FindControlAnchorIndex(Control: THandle): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Pred(AnchorList.Count) do
    if AnchorList[i] <> nil then
      if AnchorList[i] is TAnchorDesc then
        if TAnchorDesc(AnchorList[i]).Handle = Control then
          begin
          Result := i;
          Break;
          end;
end;

function TApiForm.FindControlAnchor(Control: THandle): TAnchorDesc;
var Index: integer;
begin
  Index := FindControlAnchorIndex(Control);
  if Index >= 0 then
    Result := TAnchorDesc(AnchorList[Index])
  else
    Result := nil;
end;

procedure TApiForm.ClearControlAnchors(Control: THandle);
var Index: integer;
begin
  Index := FindControlAnchorIndex(Control);
  if Index >= 0 then
    AnchorList.Delete(Index);
end;

function TApiForm.GetControlAnchors(Control: THandle; out Anchors: TAnchors): boolean;
var Item: TAnchorDesc;
begin
  Item := FindControlAnchor(Control);
  if Item = nil then
    Result := False
  else
    begin
    Result := True;
    Anchors := Item.Anchors;
    end;
end;

procedure TApiForm.SetControlAnchors(Control: THandle; Anchors: TAnchors);
var Item: TAnchorDesc;
    Rect, OwnerRect: TRect;
    ControlPlacement: TWindowPlacement;
begin
  Item := FindControlAnchor(Control);
  if Item = nil then
    begin
    ControlPlacement.length := Sizeof(ControlPlacement);
    ShowApiError(GetWindowPlacement(Control, @ControlPlacement));
    ShowApiError(GetWindowRect(Control, Rect));
    ShowApiError(GetClientRect(Self.Handle, OwnerRect));
    Item := TAnchorDesc.Create;
    Item.Handle := Control;
    Item.Anchors := Anchors;
    Item.Rect := Rect;
    Item.Margins.Left := ControlPlacement.rcNormalPosition.Left;
    Item.Margins.Top := ControlPlacement.rcNormalPosition.Top;
    Item.Margins.Right := OwnerRect.Right - ControlPlacement.rcNormalPosition.Right;
    Item.Margins.Bottom := OwnerRect.Bottom - ControlPlacement.rcNormalPosition.Bottom;
    AnchorList.Add(Item);
    end
  else
    Item.Anchors := Anchors;
end;

procedure TApiForm.RepositionAnchoredControl(Control: THandle);
var Item: TAnchorDesc;
    Anchors: TAnchors;
begin
  Item := FindControlAnchor(Control);
  if Item <> nil then
    begin
    Anchors := Item.Anchors;
    ClearControlAnchors(Control);
    SetControlAnchors(Control, Anchors);
    end;
end;
{$ENDIF}

function TApiForm.ListViewInsertColumn(ListView: THandle; Index, Subitem: integer; Alignment: TAlignment; Width: integer; const Title: string): integer;
const Alignments: array[TAlignment] of integer = (LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_CENTER);
var Column: LV_COLUMN;
begin
  Column.mask := LVCF_FMT or LVCF_TEXT;
  if Width > 0 then
    Column.mask := Column.mask or LVCF_WIDTH;
  if Subitem > 0 then
    Column.mask := Column.mask or LVCF_SUBITEM;
  Column.fmt := Alignments[Alignment];
  Column.cx := Width;
  Column.pszText := PChar(Title);
  Column.cchTextMax := 0;
  Result := SendMessage(ListView, LVM_INSERTCOLUMN, Index, integer(@Column));
end;

end.
