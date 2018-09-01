(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit uPCRE;
{$INCLUDE 'ytd.inc'}

interface

uses
  PerlRegEx;

type
  TRegExp = class(TPerlRegEx)
    public
      function Match(const Subject: PCREString): boolean; overload; virtual;
      function SubexpressionByName(const Name: PCREString): PCREString; overload; virtual;
      function SubexpressionByName(const Name: PCREString; out Value: string): boolean; overload; virtual;
    end;

  TRegExpMatch = TRegExp;

  TRegExpOptions = set of (rcoIgnoreCase, rcoMultiLine, rcoSingleLine, rcoIgnorePatternWhitespace, rcoAnchored, rcoUngreedy, rcoNoAutoCapture);

function RegExCreate(const Pattern: PCREString; Options: TRegExpOptions = []): TRegExp; overload;
procedure RegExFree(RegExp: TRegExp);
procedure RegExFreeAndNil(var RegExp: TRegExp);

implementation

{$HINTS OFF}
function RegExCreate(const Pattern: PCREString; Options: TRegExpOptions): TRegExp;
var PerlRegExpOptions: TPerlRegExOptions;
begin
  PerlRegExpOptions := [];
  if rcoIgnoreCase              in Options then PerlRegExpOptions := PerlRegExpOptions + [preCaseLess];
  if rcoMultiLine               in Options then PerlRegExpOptions := PerlRegExpOptions + [preMultiLine];
  if rcoSingleLine              in Options then PerlRegExpOptions := PerlRegExpOptions + [preSingleLine];
  if rcoIgnorePatternWhitespace in Options then PerlRegExpOptions := PerlRegExpOptions + [preExtended];
  if rcoAnchored                in Options then PerlRegExpOptions := PerlRegExpOptions + [preAnchored];
  if rcoUngreedy                in Options then PerlRegExpOptions := PerlRegExpOptions + [preUnGreedy];
  if rcoNoAutoCapture           in Options then PerlRegExpOptions := PerlRegExpOptions + [preNoAutoCapture];
  Result := TRegExp.Create(nil);
  try
    Result.Options := PerlRegExpOptions;
    Result.RegEx := Pattern;
  except
    Result.Free;
    Result := nil;
    Raise;
    end;
end;
{$HINTS ON}

procedure RegExFree(RegExp: TRegExp);
begin
  if RegExp <> nil then
    RegExp.Free;
end;

procedure RegExFreeAndNil(var RegExp: TRegExp);
begin
  RegExFree(RegExp);
  RegExp := nil;
end;

{ TRegExp }

function TRegExp.Match(const Subject: PCREString): boolean;
begin
  Self.Subject := Subject;
  Result := Self.Match;
end;

function TRegExp.SubexpressionByName(const Name: PCREString): PCREString;
begin
  Self.SubexpressionByName(Name, Result);
end;

function TRegExp.SubexpressionByName(const Name: PCREString; out Value: string): boolean;
var ix: integer;
begin
  ix := Self.NamedSubExpression(Name);
  Result := ix >= 0;
  if Result then
    Value := Self.SubExpressions[ix]
  else
    Value := '';
end;

end.
