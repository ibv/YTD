unit superdate;

{$INCLUDE 'super.inc'}

interface

uses
  supertypes,
  {$ifdef mswindows}
  supertimezone
  {$else}
  linuxtzone
  {$endif}

  ;

function JavaToDelphiDateTime(const dt: Int64; const TimeZone: SOString = ''): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime; const TimeZone: SOString = ''): Int64;
function JavaDateTimeToISO8601Date(const dt: Int64; const TimeZone: SOString = ''): SOString;
function DelphiDateTimeToISO8601Date(const dt: TDateTime; const TimeZone: SOString = ''): SOString;
function ISO8601DateToJavaDateTime(const str: SOString; var ms: Int64; const TimeZone: SOString = ''): Boolean;
function ISO8601DateToDelphiDateTime(const str: SOString; var dt: TDateTime; const TimeZone: SOString = ''): Boolean;

implementation

function JavaToDelphiDateTime(const dt: Int64; const TimeZone: SOString = ''): TDateTime;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.JavaToDelphi(dt);
  {$else}
  Result := JavaToDelphi(dt);
  {$endif}
end;


function DelphiToJavaDateTime(const dt: TDateTime; const TimeZone: SOString = ''): Int64;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.DelphiToJava(dt);
  {$else}
  Result := DelphiToJava(dt);
  {$endif}
end;

function JavaDateTimeToISO8601Date(const dt: Int64; const TimeZone: SOString = ''): SOString;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.JavaToISO8601(dt);
  {$else}
  Result := JavaToISO8601(dt);
  {$endif}
end;

function DelphiDateTimeToISO8601Date(const dt: TDateTime; const TimeZone: SOString = ''): SOString;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.DelphiToISO8601(dt);
  {$else}
  Result := DelphiToISO8601(dt);
  {$endif}
end;

function ISO8601DateToJavaDateTime(const str: SOString; var ms: Int64; const TimeZone: SOString = ''): Boolean;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.ISO8601ToJava(str, ms);
  {$else}
  Result := ISO8601ToJava(str, ms);
  {$endif}
end;

function ISO8601DateToDelphiDateTime(const str: SOString; var dt: TDateTime; const TimeZone: SOString = ''): Boolean;
begin
  {$ifdef mswindows}
  Result := TSuperTimeZone.Zone{$ifdef LEGACYVERSION}(TimeZone){$else}[TimeZone]{$endif}.ISO8601ToDelphi(str, dt);
  {$else}
  Result := ISO8601ToDelphi(str, dt);
  {$endif}
end;

end.
