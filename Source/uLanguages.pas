unit uLanguages;
{$INCLUDE 'YTD.inc'}

interface

procedure UseLanguage(const LanguageCode: string);
function _(const Msg: WideString): string;
procedure TranslateProperties(AnObject: TObject; const TextDomain: string = '');

implementation

{$IFDEF GETTEXT}
uses
  gnugettext;
{$ENDIF}

procedure UseLanguage(const LanguageCode: string);
begin
  {$IFDEF GETTEXT}
  gnugettext.UseLanguage(LanguageCode);
  {$ENDIF}
end;

function _(const Msg: WideString): string;
begin
  {$IFDEF GETTEXT}
  Result := gnugettext._(Msg);
  {$ELSE}
  Result := Msg;
  {$ENDIF}
end;

procedure TranslateProperties(AnObject: TObject; const TextDomain: string = '');
begin
  {$IFDEF GETTEXT}
  gnugettext.DefaultInstance.TranslateProperties(AnObject, TextDomain);
  {$ENDIF}
end;

initialization
  {$IFDEF GETTEXT}
    AddDomainForResourceString('delphi');
    //TP_GlobalIgnoreClassProperty(TFont, 'Name');
    //TP_GlobalIgnoreClassProperty(TToolButton, 'Caption');
    //TP_GlobalIgnoreClassProperty(TAction, 'Category');
    //TP_GlobalIgnoreClassProperty(TControl, 'HelpKeyword');
  {$ENDIF}

end.
