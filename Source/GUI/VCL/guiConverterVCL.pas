unit guiConverterVCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls,
  uOptions, uLanguages, guiConsts;

type
  TFormSelectConverter = class(TForm)
    LabelConverter: TLabel;
    comboConverter: TComboBox;
    Button1: TButton;
    Button2: TButton;
    ActionList: TActionList;
    actOK: TAction;
    actCancel: TAction;
    procedure FormCreate(Sender: TObject);
  private
  protected
  public
  end;

function SelectConverter(Options: TYTDOptions; var SelectedID: string; Owner: TComponent = nil; const Caption: string = ''): boolean;
procedure PrepareConverterComboBox(Combo: TCustomComboBox; Options: TYTDOptions; const SelectedID: string = '');
function DecodeConverterComboBox(Combo: TCustomComboBox; Options: TYTDOptions; out SelectedID: string): boolean;

implementation

{$R *.DFM}

procedure PrepareConverterComboBox(Combo: TCustomComboBox; Options: TYTDOptions; const SelectedID: string);
var L: TStringList;
    i, Index: integer;
    Converter: TConverter;
begin
  L := TStringList.Create;
  try
    Options.ReadConverterIDList(L);
    Combo.Items.Clear;
    Combo.Items.AddObject(CONVERTERS_NOCONVERTER, TObject(-1));
    Index := 0;
    for i := 0 to Pred(L.Count) do
      if Options.ReadConverter(L[i], Converter) then
        begin
        Combo.Items.AddObject(Converter.Title, TObject(i));
        if (Index <= 0) and (SelectedID <> '') and (SelectedID = L[i]) then
          Index := Succ(i);
        end;
    Combo.ItemIndex := Index;
  finally
    L.Free;
    end;
end;

function DecodeConverterComboBox(Combo: TCustomComboBox; Options: TYTDOptions; out SelectedID: string): boolean;
var L: TStringList;
    Index: integer;
begin
  Result := Combo.ItemIndex >= 0;
  if Combo.ItemIndex <= 0 then
    SelectedID := ''
  else
    begin
    L := TStringList.Create;
    try
      Options.ReadConverterIDList(L);
      Index := integer(Combo.Items.Objects[Combo.ItemIndex]);
      SelectedID := L[Index];
    finally
      L.Free;
      end;
    end;
end;

function SelectConverter(Options: TYTDOptions; var SelectedID: string; Owner: TComponent = nil; const Caption: string = ''): boolean;
var F: TFormSelectConverter;
    NewID: string;
begin
  Result := False;
  F := TFormSelectConverter.Create(Owner);
  try
    if Caption <> '' then
      F.Caption := Caption;
    PrepareConverterComboBox(F.comboConverter, Options, SelectedID);
    if F.ShowModal = mrOK then
      if DecodeConverterComboBox(F.comboConverter, Options, NewID) then
        begin
        SelectedID := NewID;
        Result := True;
        end;
  finally
    F.Free;
    end;
end;

procedure TFormSelectConverter.FormCreate(Sender: TObject);
begin
  {$IFDEF GETTEXT}
  TranslateProperties(self);
  {$ENDIF}
end;

end.
