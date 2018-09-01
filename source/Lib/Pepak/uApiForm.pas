unit uApiForm;

interface
{.DEFINE USE_PROP}

uses
  SysUtils, Windows, Messages,
  uApiGraphics;

type
  TApiForm = class;

  // Returns TRUE if the message was handled (should not be passed to the parent window procedure)
  TApiFormSubclassFn = function (Handle: THandle; Form: TApiForm; var Msg: TMessage): boolean; stdcall;

  TApiForm = class
    private
      fDialogResourceName: string;
      fHandle: HWND;
      fModalResult: integer;
    protected // Message handlers
      // Message handler for all messages
      function DialogProc(var Msg: TMessage): boolean; virtual;
      // Message handler for WM_INITDIALOG - form is being created
      function DoInitDialog: boolean; virtual;
      // Message handler for WM_CLOSE - form is being destroyed
      function DoClose: boolean; virtual;
      // Message handled for WM_COMMAND - shortcuts, menus, control-specific codes...
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; virtual;
    protected // Support for message handlers
      function CanClose: boolean; virtual;
    protected // Support functions for descendants
      // Add your own message handler to the specified window. Note: Each window may only be subclassed once.
      procedure SubClassAWindow(AHandle: THandle; AWndProc: TApiFormSubclassFn);
    protected // Properties
      procedure SetModalResult(const Value: integer);
      property DialogResourceName: string read fDialogResourceName;
    public
      constructor Create(const ADialogResourceName: string); virtual;
      destructor Destroy; override;
      procedure Show; virtual;
      function ShowModal: integer; virtual;
      property ModalResult: integer read fModalResult;
      property Handle: HWND read fHandle;
    end;

implementation

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

constructor TApiForm.Create(const ADialogResourceName: string);
begin
  inherited Create;
  fDialogResourceName := ADialogResourceName;
  fHandle := 0;
end;

destructor TApiForm.Destroy;
begin
  if fHandle <> 0 then
    SendMessage(Handle, WM_CLOSE, 0, 0);
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
  SetModalResult(Result);
end;

procedure TApiForm.Show;
begin
  SetModalResult(0);
  CreateDialogParam(hInstance, PChar(DialogResourceName), 0, @ApiFormDialogProc, Integer(Self));
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

end.
