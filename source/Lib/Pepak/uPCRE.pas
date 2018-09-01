unit uPCRE;

interface

uses
  PerlRegEx;

type
  TRegExp = class(TPerlRegEx)
    public
      function Match(const Subject: PCREString): boolean; overload; virtual;
      function SubexpressionByName(const Name: PCREString): PCREString; overload; virtual;
      function SubexpressionByName(const Name: PCREString; out Value: PCREString): boolean; overload; virtual;
      {$IFDEF UNICODE}
      function Match(const Subject: string): boolean; overload; virtual;
      function SubexpressionByName(const Name: string): string; overload; virtual;
      function SubexpressionByName(const Name: string; out Value: string): boolean; overload; virtual;
      {$ENDIF}
    end;

  TRegExpMatch = TRegExp;

  TRegExpOptions = set of (rcoIgnoreCase, rcoMultiLine, rcoSingleLine, rcoIgnorePatternWhitespace, rcoAnchored, rcoUngreedy, rcoNoAutoCapture);

function RegExCreate(const Pattern: PCREString; Options: TRegExpOptions = []): TRegExp; overload;
{$IFDEF UNICODE}
function RegExCreate(const Pattern: string; Options: TRegExpOptions = []): TRegExp; overload;
{$ENDIF}
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

{$IFDEF UNICODE}
function RegExCreate(const Pattern: string; Options: TRegExpOptions): TRegExp;
begin
  Result := RegExCreate(PCREString(Pattern), Options);
end;
{$ENDIF}

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

function TRegExp.SubexpressionByName(const Name: PCREString; out Value: PCREString): boolean;
var ix: integer;
begin
  ix := Self.NamedSubExpression(Name);
  Result := ix >= 0;
  if Result then
    Value := Self.SubExpressions[ix]
  else
    Value := '';
end;

{$IFDEF UNICODE}
function TRegExp.Match(const Subject: string): boolean;
begin
  Result := Self.Match(PCREString(Subject));
end;

function TRegExp.SubexpressionByName(const Name: string): string;
begin
  Self.SubexpressionByName(Name, Result);
end;

function TRegExp.SubexpressionByName(const Name: string; out Value: string): boolean;
var s: PCREString;
begin
  Result := Self.SubexpressionByName(PCREString(Name), s);
  Value := string(s);
end;
{$ENDIF}

end.
