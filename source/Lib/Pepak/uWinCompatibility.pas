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

unit uWinCompatibility;
{$INCLUDE 'jedi.inc'}

interface

uses
  SysUtils, Classes, Windows;

procedure FixFormatSettingsForWindows8; overload;
  // Needed because Delphi up to XE2 (at least) have a faulty GetLocaleChar
  // implementation which causes long locale chars (e.g. DateSeparator under
  // Windows 8) to remain at their default value.
{$IFDEF DELPHI2009_UP}
procedure FixFormatSettingsForWindows8(const Locale: LCID; var FormatSettings: TFormatSettings); overload;
{$ENDIF}

implementation

function GetLocaleChar(Locale, LocaleType: integer; Default: Char): Char;
var
  i, n: Integer;
  Buffer: array[0..255] of Char;
begin
  Result := Default;
  n := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  for i := 0 to Pred(n) do
    if Buffer[i] <> ' ' then
      begin
      Result := Buffer[i];
      Break;
      end;
end;

{$IFDEF DELPHI2009_UP}
procedure FixFormatSettingsForWindows8(const Locale: LCID; var FormatSettings: TFormatSettings);
begin
  FormatSettings.ThousandSeparator := GetLocaleChar(Locale, LOCALE_STHOUSAND, FormatSettings.ThousandSeparator);
  FormatSettings.DecimalSeparator := GetLocaleChar(Locale, LOCALE_SDECIMAL, FormatSettings.DecimalSeparator);
  FormatSettings.DateSeparator := GetLocaleChar(Locale, LOCALE_SDATE, FormatSettings.DateSeparator);
  FormatSettings.TimeSeparator := GetLocaleChar(Locale, LOCALE_STIME, FormatSettings.TimeSeparator);
  FormatSettings.ListSeparator := GetLocaleChar(Locale, LOCALE_SLIST, FormatSettings.ListSeparator);
end;
{$ENDIF}

procedure FixFormatSettingsForWindows8;
var
  Locale: LCID;
begin
  Locale := GetThreadLocale;
  SysUtils.ThousandSeparator := GetLocaleChar(Locale, LOCALE_STHOUSAND, SysUtils.ThousandSeparator);
  SysUtils.DecimalSeparator := GetLocaleChar(Locale, LOCALE_SDECIMAL, SysUtils.DecimalSeparator);
  SysUtils.DateSeparator := GetLocaleChar(Locale, LOCALE_SDATE, SysUtils.DateSeparator);
  SysUtils.TimeSeparator := GetLocaleChar(Locale, LOCALE_STIME, SysUtils.TimeSeparator);
  SysUtils.ListSeparator := GetLocaleChar(Locale, LOCALE_SLIST, SysUtils.ListSeparator);
end;

initialization
  FixFormatSettingsForWindows8;
  
end.
