unit uApiForm;

interface
{$INCLUDE 'uApi.inc'}

uses
  SysUtils, Classes, Windows, Messages, CommCtrl,
  {$IFDEF APIFORM_ANCHORS} Contnrs, {$ENDIF}
  uApiCommon, uApiFunctions, uApiGraphics, uCompatibility;

type
  TApiForm = class;

  TApiFormSubclassFn = function (Handle: THandle; Form: TApiForm; var Msg: TMessage): boolean; stdcall;
    // Returns TRUE if the message was handled (should not be passed to the parent window procedure)
    
  TApiForm = class
    private
      fDialogResourceName: string;
      fHandle: HWND;
      fModalResult: integer;
      {$IFDEF APIFORM_ANCHORS}
      fAnchorList: TObjectList;
      {$ENDIF}
      {$IFDEF APIFORM_ACCELERATORS}
      fAccelerators: HACCEL;
      {$ENDIF}
      fOwner: TApiForm;
      fMenu: HMENU;
      fPopupMenu: HMENU;
      fIcon: THandle;
      procedure SetMenu(Value: HMENU);
    protected // Message handlers
      // Message handler for all messages
      function DialogProc(var Msg: TMessage): boolean; virtual;
      // Message handler for WM_INITDIALOG - form is being created
      function DoInitDialog: boolean; virtual;
      // Message handler for WM_CLOSE - form is being destroyed
      function DoClose: boolean; virtual;
//      // Message handler for WM_SHOWWINDOW
//      function DoShowWindow: boolean; virtual;
      // Message handler for WM_COMMAND - shortcuts, menus, control-specific codes...
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; virtual;
      // Message handler for WM_NOTIFY - something happens to a control
      function DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: integer): boolean; virtual;
      // Message handler for WM_SIZE - form was resized
      function DoSize(ResizeType, NewWidth, NewHeight: integer): boolean; virtual;
      // Message handler for WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN
      function DoMouseButtonDown(Button: TMouseButton; Keys: WPARAM; Point: TPoint): boolean; virtual;
      // Message handled for WM_CONTEXTMENU
      function DoContextMenu(Control: THandle; Point: TPoint): boolean; virtual;
    protected // Support for message handlers
      function CanClose: boolean; virtual;
      function AfterInitDialog: boolean; virtual;
    protected // Support functions for descendants
      // Add your own message handler to the specified window. Note: Each window may only be subclassed once.
      procedure SubClassAWindow(AHandle: THandle; AWndProc: TApiFormSubclassFn);
      {$IFDEF APIFORM_TRANSLATIONS}
      function TranslateControl(Control: THandle): boolean; virtual;
      function TranslateMenu(Control: THandle): boolean; virtual;
      function Translate: boolean; virtual;
      {$ENDIF}
      function GetControlText(ControlID: integer): string;
      function GetOwnerHandle: HWND;
      procedure ErrorMessageBox(const Msg, Title: string); virtual;
    protected // Properties
      procedure SetModalResult(const Value: integer);
      property DialogResourceName: string read fDialogResourceName;
      {$IFDEF APIFORM_ANCHORS}
      function FindControlAnchorIndex(Control: THandle): integer;
      function FindControlAnchor(Control: THandle): TAnchorDesc;
      property AnchorList: TObjectList read fAnchorList;
      {$ENDIF}
      {$IFDEF APIFORM_ACCELERATORS}
      property Accelerators: HACCEL read fAccelerators write fAccelerators;
      {$ENDIF}
      property Menu: HMENU read fMenu write SetMenu;
      property PopupMenu: HMENU read fPopupMenu write fPopupMenu;
      property Icon: THandle read fIcon write fIcon;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); overload; virtual;
      constructor Create(AOwner: TApiForm); overload; virtual;
      constructor Create; overload; virtual;
      destructor Destroy; override;
      function ShowModal: integer; virtual;
      procedure Show; virtual;
      function Close: boolean; overload; virtual;
      function Close(ModalResult: integer): boolean; overload; virtual;
      {$IFDEF APIFORM_ANCHORS}
        // Simulate anchors from VCL, to achieve easy resizing. If no anchor is defined
        // for a control, it behaves as if [akLeft, akTop] was assigned to it.
        procedure SetControlAnchors(Control: THandle; Anchors: TAnchors);
        procedure ClearControlAnchors(Control: THandle);
        procedure RepositionAnchoredControl(Control: THandle);
        function GetControlAnchors(Control: THandle; out Anchors: TAnchors): boolean;
      {$ENDIF}
      property ModalResult: integer read fModalResult;
      property Handle: HWND read fHandle;
      property OwnerHandle: HWND read GetOwnerHandle;
      property Owner: TApiForm read fOwner write fOwner;
    end;

{$IFDEF APIFORM_TRANSLATIONS}
type
  TTranslateFunction = function (const Msg: WideString): string;

var
  ApiFormTranslateFunction: TTranslateFunction = nil;
{$ENDIF}

implementation

// Use SetProp instead of SetWindowLong(DWL_USER) to link dialog with ApiForm. It's slow
{.DEFINE USE_PROP}

// Storing ApiForm's address in Dialog's data
const
  APIFORM_SELF_PROPERTY = '9AF510CC-C253-4F9E-9ACE-C39E533AD206';
  SELF_INDEX = DWL_USER;

// Storing subclassing information
const
  APIFORM_SUBCLASS_OLDWNDPROC = '1634CAB6-8ECA-4ADF-890F-CA0C57A2B3E5';
  APIFORM_SUBCLASS_APIFORM = '19DD5893-5D7D-4251-A2A0-C792B70ED578';
  APIFORM_SUBCLASS_HANDLER = '22B34AE3-EE7E-4107-899D-D770E8D9D664';

{$IFDEF APIFORM_ACCELERATORS}
var
  ApiFormKeyboardMessageHook: HHOOK = 0;

function ApiFormKeyboardMessageHookProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var Handled: boolean;
    Msg: PMsg;
    Handle: HWND;
    F: TApiForm;
begin
  Handled := False;
  Result := 0;
  // Is msg. intended for a dialog?
  if nCode = MSGF_DIALOGBOX then
    begin
    Msg := PMsg(lParam);
    // Is msg. a keyboard event?
    if (Msg.message >= WM_KEYFIRST) and (Msg.message <= WM_KEYLAST) then
      begin
      // Is it my dialog?
      Handle := Msg.hwnd;
      while Handle <> 0 do
        begin
        F := TApiForm(GetProp(Handle, APIFORM_SELF_PROPERTY));
        if F <> nil then
          // Are there any accelerators?
          if F.Accelerators <> 0 then
            // Try to perform the translation
            if TranslateAccelerator(F.Handle, F.Accelerators, Msg^) <> 0 then
              begin
              Handled := True;
              Result := 1;
              Break;
              end;
        Handle := GetParent(Handle);
        end;
      end;
    end;
  if not Handled then
    Result := CallNextHookEx(ApiFormKeyboardMessageHook, nCode, wParam, lParam);
end;
{$ENDIF}

function ApiFormDialogProc(Handle: HWND; Msg: Cardinal; wParam: WPARAM; lParam: LPARAM): integer; stdcall;
var F: TApiForm;
    M: TMessage;
    Handled: boolean;
begin
  try
    M.Msg := Msg;
    M.wParam := wParam;
    M.lParam := lParam;
    M.Result := 0;
    // Ulozim si ukazatel na Self pro pozdejsi pouziti
    if Msg = WM_INITDIALOG then
      begin
      SetProp(Handle, APIFORM_SELF_PROPERTY, lParam);
      SetWindowLong(Handle, SELF_INDEX, lParam);
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
              SetProp(Handle, APIFORM_SELF_PROPERTY, 0);
              SetWindowLong(Handle, SELF_INDEX, 0);
              end;
            Result := 1;
            end;
          end
        else
          Result := 0;
        end;
      end;
  except
    SysUtils.ShowException(ExceptObject, ExceptAddr);
    Result := 0;
    end;
end;

function ApiFormSubclassWndProc(Handle: HWND; Msg: Cardinal; wParam: WPARAM; lParam: LPARAM): integer; stdcall;
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

constructor TApiForm.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited Create;
  fDialogResourceName := ADialogResourceName;
  fOwner := AOwner;
  fHandle := 0;
  {$IFDEF APIFORM_ANCHORS}
  fAnchorList := TObjectList.Create(True);
  {$ENDIF}
  fIcon := LoadIcon(hInstance, 'MAINICON');
end;

constructor TApiForm.Create(AOwner: TApiForm);
begin
  Create(AOwner, ClassName);
end;

constructor TApiForm.Create;
begin
  Create(nil);
end;

destructor TApiForm.Destroy;
begin
  FreeGDIObject(fIcon);
  if fHandle <> 0 then
    SendMessage(Handle, WM_CLOSE, 0, 0);
  fHandle := 0;
  {$IFDEF APIFORM_ANCHORS}
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
      Result := DoInitDialog and AfterInitDialog;
    WM_CLOSE:
      Result := DoClose;
    WM_COMMAND:
      Result := DoCommand(Msg.WParamHi, Msg.WParamLo, Msg.LParam);
    WM_NOTIFY:
      Result := DoNotify(PNMHdr(Msg.LParam)^.hwndFrom, PNMHdr(Msg.LParam)^.idFrom, PNMHdr(Msg.LParam)^.code, Msg.WParam, Msg.LParam, Msg.Result);
    WM_SIZE:
      Result := DoSize(Msg.wParam, Msg.lParam and $ffff, Msg.lParam shr 16);
    WM_LBUTTONDOWN:
      Result := DoMouseButtonDown(mbLeft, Msg.wParam and $ffff, MakePoints(Msg.lParam));
    WM_RBUTTONDOWN:
      Result := DoMouseButtonDown(mbRight, Msg.wParam and $ffff, MakePoints(Msg.lParam));
    WM_MBUTTONDOWN:
      Result := DoMouseButtonDown(mbMiddle, Msg.wParam and $ffff, MakePoints(Msg.lParam));
    WM_XBUTTONDOWN:
      if (Msg.wParam shr 16) = XBUTTON1 then
        Result := DoMouseButtonDown(mbExtra1, Msg.wParam and $ffff, MakePoints(Msg.lParam))
      else if (Msg.wParam shr 16) = XBUTTON2 then
        Result := DoMouseButtonDown(mbExtra2, Msg.wParam and $ffff, MakePoints(Msg.lParam));
    WM_CONTEXTMENU:
      Result := DoContextMenu(THandle(Msg.wParam), MakePoints(Msg.lParam));
    end;
end;

function TApiForm.DoInitDialog: boolean;
begin
  Result := True;
  // Ikona
  if Icon <> 0 then
    SendMessage(Self.Handle, WM_SETICON, 0, Icon);
end;

function TApiForm.AfterInitDialog: boolean;
begin
  Result := True;
end;

function TApiForm.DoClose: boolean;
begin
  Result := CanClose;
  if Result then
    EndDialog(Handle, ModalResult);
end;

function TApiForm.CanClose: boolean;
begin
  Result := True;
end;

function TApiForm.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
end;

function TApiForm.DoNotify(Control: THandle; ControlID: DWORD; Code: integer; wParam: WPARAM; lParam: LPARAM; out NotifyResult: integer): boolean;
begin
  Result := False;
  NotifyResult := 0;
end;

function TApiForm.DoSize(ResizeType, NewWidth, NewHeight: integer): boolean;
{$IFDEF APIFORM_ANCHORS}
var i, NewX, NewY, NewW, NewH: integer;
    AnchorItem: TAnchorDesc;
    OwnerRect: TRect;
{$ENDIF}
begin
  Result := False;
  {$IFDEF APIFORM_ANCHORS}
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

function TApiForm.DoMouseButtonDown(Button: TMouseButton; Keys: WPARAM; Point: TPoint): boolean;
begin
  Result := False;
end;

function TApiForm.DoContextMenu(Control: THandle; Point: TPoint): boolean;
var Menu: HMENU;
begin
  Result := False;
  if PopupMenu <> 0 then
    begin
    Menu := GetSubMenu(PopupMenu, 0);
    if Menu <> 0 then
      if TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON, Point.x, Point.y, 0, Self.Handle, nil) then
        Result := True;
    end;
end;

{$IFDEF APIFORM_TRANSLATIONS}
function TranslateEnumFunc(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var F: TApiForm;
begin
  Result := False;
  F := TApiForm(lParam);
  if F <> nil then
    Result := F.TranslateControl(hwnd);
end;

function TApiForm.TranslateMenu(Control: THandle): boolean;
var i, n, TitleLen, TabStart: integer;
    Item: TMenuItemInfo;
    OldTitle, Title: string;
    TitleBuf: array of char;
    OK: boolean;
begin
  Result := False;
  n := GetMenuItemCount(Control);
  if n >= 0 then
    begin
    Result := True;
    for i := 0 to Pred(n) do
      begin
      OK := False;
      FillChar(Item, Sizeof(Item), 0);
      Item.cbSize := Sizeof(Item);
      Item.fMask := MIIM_STRING or MIIM_SUBMENU;
      Item.dwTypeData := nil;
      if GetMenuItemInfo(Control, i, True, Item) then
        begin
        TitleLen := Item.cch;
        SetLength(TitleBuf, Succ(TitleLen));
        Item.dwTypeData := PChar(TitleBuf);
        Item.cch := Succ(TitleLen);
        if GetMenuItemInfo(Control, i, True, Item) then
          begin
          if TitleLen > 0 then
            begin
            SetString(OldTitle, PChar(TitleBuf), TitleLen);
            Title := ApiFormTranslateFunction(WideString(OldTitle));
            if OldTitle = Title then
              begin
              TabStart := Pos(#9, OldTitle);
              if TabStart > 0 then
                Title := ApiFormTranslateFunction(WideString(Copy(OldTitle, 1, Pred(TabStart)))) + Copy(OldTitle, TabStart, MaxInt);
              end;
            if OldTitle = Title then
              OK := True
            else
              begin
              TitleLen := Length(Title);
              SetLength(TitleBuf, Succ(TitleLen));
              StrPCopy(PChar(TitleBuf), Title);
              Item.fMask := MIIM_STRING;
              Item.dwTypeData := PChar(TitleBuf);
              Item.cch := Succ(TitleLen);
              if SetMenuItemInfo(Control, i, True, Item) then
                OK := True;
              end;
            end;
          if Item.hSubMenu <> 0 then
            if not TranslateMenu(Item.hSubMenu) then
              OK := False;
          end;
        end;
      if not OK then
        Result := False;
      end;
    end;
end;

function TApiForm.TranslateControl(Control: THandle): boolean;
var Buf: array of char;
    n: integer;
    s: string;
begin
  Result := False;
  if Control <> 0 then
    begin
    n := GetWindowTextLength(Control);
    if n > 0 then
      begin
      Inc(n);
      SetLength(Buf, n);
      n := GetWindowText(Control, PChar(Buf), n);
      if n > 0 then
        begin
        SetString(s, PChar(Buf), n);
        s := ApiFormTranslateFunction(WideString(s));
        SetWindowText(Control, PChar(s));
        end;
      end;
    Result := True;
    end;
end;

function TApiForm.Translate: boolean;
begin
  Result := False;
  if Assigned(ApiFormTranslateFunction) then
    begin
    TranslateEnumFunc(Self.Handle, Integer(Self));
    EnumChildWindows(Self.Handle, @TranslateEnumFunc, Integer(Self));
    if Menu <> 0 then
      TranslateMenu(Menu);
    if PopupMenu <> 0 then
      TranslateMenu(PopupMenu);
    end;
end;
{$ENDIF}

procedure TApiForm.SetModalResult(const Value: integer);
begin
  fModalResult := Value;
end;

function TApiForm.ShowModal: integer;
begin
  SetModalResult(0);
  //fHandle := CreateDialogParam(hInstance, PChar(DialogResourceName), 0, @ApiFormDialogProc, Integer(Self));
  Result := DialogBoxParam(hInstance, PChar(DialogResourceName), Self.OwnerHandle, @ApiFormDialogProc, Integer(Self));
  ShowApiError(Result = -1);
  SetModalResult(Result);
end;

procedure TApiForm.Show;
begin
  SetModalResult(0);
  ShowApiError(CreateDialogParam(hInstance, PChar(DialogResourceName), Self.OwnerHandle, @ApiFormDialogProc, Integer(Self)) = 0);
end;

function TApiForm.Close: boolean;
begin
  Result := SendMessage(Self.Handle, WM_CLOSE, 0, 0) <> 0;
end;

function TApiForm.Close(ModalResult: integer): boolean;
begin
  SetModalResult(ModalResult);
  Result := Close;
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

{$IFDEF APIFORM_ANCHORS}
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

function TApiForm.GetControlText(ControlID: integer): string;
begin
  Result := GetWindowTextAsString(GetDlgItem(Self.Handle, ControlID));
end;

function TApiForm.GetOwnerHandle: HWND;
begin
  if Owner = nil then
    Result := 0
  else
    Result := Owner.Handle;
end;

procedure TApiForm.ErrorMessageBox(const Msg, Title: string);
begin
  MessageBox(Self.Handle, PChar(Msg), PChar(Title), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

procedure TApiForm.SetMenu(Value: HMENU);
begin
  Windows.SetMenu(Self.Handle, Value);
  fMenu := Value;
end;

initialization
  {$IFDEF APIFORM_ACCELERATORS}
  ApiFormKeyboardMessageHook := SetWindowsHookEx(WH_MSGFILTER, @ApiFormKeyboardMessageHookProc, 0, GetCurrentThreadId);
  {$ENDIF}

finalization
  {$IFDEF APIFORM_ACCELERATORS}
  if ApiFormKeyboardMessageHook <> 0 then
    begin
    UnhookWindowsHookEx(ApiFormKeyboardMessageHook);
    ApiFormKeyboardMessageHook := 0;
    end;
  {$ENDIF}

end.
