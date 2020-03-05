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

unit uLanguages;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$INCLUDE 'ytd.inc'}

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
  Result := {$IFDEF FPC} string {$ENDIF} (Msg);
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
