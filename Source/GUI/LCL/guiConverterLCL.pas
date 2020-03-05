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

unit guiConverterLCL;
{$INCLUDE 'ytd.inc'}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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

{$R *.dfm}

function SelectConverter(Options: TYTDOptions; var SelectedID: string; Owner: TComponent = nil; const Caption: string = ''): boolean;
var F: TFormSelectConverter;
    NewID: string;
begin
  Result := False;
  F := TFormSelectConverter.Create(Owner);

  F.Button1.Enabled := true;
  F.Button2.Enabled := true;

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
    FreeAndNil(F);
    end;
end;

procedure PrepareConverterComboBox(Combo: TCustomComboBox; Options: TYTDOptions; const SelectedID: string);
var L: TStringList;
    i, Index: integer;
    Converter: TConverter;
begin
  L := TStringList.Create;
  try
    Options.ReadConverterIDList(L);
    Combo.Items.Clear;
    Combo.Items.AddObject(_(CONVERTERS_NOCONVERTER), TObject(-1));
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
    FreeAndNil(L);
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
      FreeAndNil(L);
      end;
    end;
end;

procedure TFormSelectConverter.FormCreate(Sender: TObject);
begin
  {$IFDEF GETTEXT}
  TranslateProperties(self);
  {$ENDIF}
end;

end.
