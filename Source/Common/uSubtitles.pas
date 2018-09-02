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

unit uSubtitles;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes;

function SubtitlesToSrt(var Index: integer; const TimeStart, TimeEnd: TDateTime; const Subtitle: string): string; overload;
function SubtitlesToSrt(var Index: integer; const TimeStart, TimeEnd: string; const Subtitle: string): string; overload;
function SubtitlesToSrt(var Index: integer; const TimeStart: TDateTime; const Subtitle: string): string; overload;

implementation

function SubtitlesToSrt(var Index: integer; const TimeStart: TDateTime; const Subtitle: string): string;
const
  ONE_SECOND = 1/(24*60*60);
var
  Time: TDateTime;
begin
  Time := (Length(Subtitle) / 14)*ONE_SECOND;
  if Time < (2*ONE_SECOND) then
    Time := 2*ONE_SECOND;
  Result := SubtitlesToSrt(Index, TimeStart, TimeStart + Time, Subtitle);
end;

function SubtitlesToSrt(var Index: integer; const TimeStart, TimeEnd: TDateTime; const Subtitle: string): string;
begin
  Result := SubtitlesToSrt(Index, FormatDateTime('hh":"nn":"ss","zzz', TimeStart), FormatDateTime('hh":"nn":"ss","zzz', TimeEnd), Subtitle);
end;

function SubtitlesToSrt(var Index: integer; const TimeStart, TimeEnd: string; const Subtitle: string): string;
begin
  Inc(Index);
  Result := Format('%d'#13#10'%s --> %s'#13#10'%s'#13#10#13#10, [Index, TimeStart, TimeEnd, Subtitle]);
end;

end.
