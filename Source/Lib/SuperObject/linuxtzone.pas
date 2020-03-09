unit linuxtzone;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math,
  supertypes,
  UnixUtil, unix, baseunix
  ;


function JavaToDelphi(const JavaDateTime: Int64): TDateTime;
function DelphiToJava(const DelphiDateTime: TDateTime): Int64;

function JavaToISO8601(JavaDateTime: Int64): SOString;
function DelphiToISO8601(DelphiDateTime: TDateTime): SOString;

function ISO8601ToJava(const ISO8601Date: SOString; var JavaDateTime: Int64): Boolean;
function ISO8601ToDelphi(const ISO8601Date: SOString; var DelphiDateTime: TDateTime): Boolean;


implementation


{ ISO8601 formatted date Parser }
class function ParseISO8601Date(const ISO8601Date: SOString;
  var st: TSystemTime; var dayofyear: Integer; var week: Word; var bias: Integer;
  var havetz, havedate: Boolean): Boolean; forward;



const
  UnixStartDate = 25569.0;

function DateTimeToUnixTime(const ADateTime: TDateTime): Cardinal;
begin
  Result := Round(ADateTime - UnixStartDate) * 86400;
end;

function UnixTimeToDateTime(const UnixDate: Cardinal): TDateTime;
begin
  Result := UnixDate / 86400 + UnixStartDate;
end;



function JavaToDelphi(const JavaDateTime: Int64): TDateTime;
begin
  Result := UnixTimeToDateTime(JavaDateTime div 1000);
end;


function DelphiToJava(const DelphiDateTime: TDateTime): Int64;
begin
  Result := DateTimeToUnixTime(DelphiDateTime) * 1000;
end;

function JavaToISO8601(JavaDateTime: Int64): SOString;
begin
  Result := DelphiToISO8601(JavaToDelphi(JavaDateTime));
end;

function DelphiToISO8601(DelphiDateTime: TDateTime): SOString;
const
  ISO_Fmt = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%d';
  TZ_Fmt  = '%s%.2d:%.2d';
var
  local:  TSystemTime;
  bias: integer;
  h, m: Word;
  iso: SOString;
begin
  result:='';
  DateTimeToSystemTime(DelphiDateTime, local);
  iso := Format(ISO_Fmt, [
    local.Year, local.Month, local.Day,
    local.Hour, local.Minute, local.Second, local.Millisecond]);

  bias := TZSeconds div 60;
  h := abs(bias) div 60;
  m := abs(bias) mod 60;

  case Sign(bias) of
  -1: Result := iso + Format(TZ_Fmt, [ '-', h, m ]);
   0: Result := iso + 'Z';
  +1: Result := iso + Format(TZ_Fmt, [ '+', h, m ]);
  end;
  {
  if GetTimeZoneInformation(local.wYear, tzi) and TzSpecificLocalTimeToSystemTime(@tzi, local, utc) then
  begin
    bias := SystemTimeToDateTime(local) - SystemTimeToDateTime(utc);
    DecodeTime(bias, h, m, d, d);
    case Sign(bias) of
    -1: Result := iso + Format(TZ_Fmt, [ '-', h, m ]);
     0: Result := iso + 'Z';
    +1: Result := iso + Format(TZ_Fmt, [ '+', h, m ]);
    end;
  end
  else
    Result := iso;

  }
end;


function ISO8601ToJava(const ISO8601Date: SOString; var JavaDateTime: Int64): Boolean;
var
  st: TSystemTime;
  dayofyear: Integer;
  week: Word;
  bias: Integer;
  havetz, havedate: Boolean;

  tzi: TTimeZone;
  utc: TSystemTime;
  m: Word;
  DayTable: PDayTable;
begin
  if ParseISO8601Date(ISO8601Date, st, dayofyear, week, bias, havetz, havedate) then
  begin
    ///if (not havetz) and GetTimeZoneInformation(st.Year, tzi) and TzSpecificLocalTimeToSystemTime(@tzi, st, utc) then
    if (not havetz) then
      bias := Trunc((SystemTimeToDateTime(st) - SystemTimeToDateTime(utc)) * MinsPerDay);
    JavaDateTime := st.Millisecond + st.Second * 1000 + (st.Minute + bias) * 60000 + st.Hour * 3600000;
    if havedate then
    begin
      DayTable := @MonthDays[IsLeapYear(st.Year)];
      if st.Month <> 0 then
      begin
        if not (st.Month in [1..12]) or (DayTable^[st.Month] < st.Day) then
        begin
          Result := False;
          Exit;
        end;
        for m := 1 to st.Month - 1 do
          Inc(JavaDateTime, Int64(DayTable^[m]) * 86400000);
      end;
      Dec(st.Year);
      Inc(JavaDateTime, Int64(
        (st.Year * 365) + (st.Year div 4) - (st.Year div 100) +
        (st.Year div 400) + st.Day + dayofyear - 719163) * 86400000);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function ISO8601ToDelphi(const ISO8601Date: SOString; var DelphiDateTime: TDateTime): Boolean;
var
  JavaDateTime: Int64;
begin
  Result := ISO8601ToJava(ISO8601Date, JavaDateTime);
  if Result then
    DelphiDateTime := JavaToDelphi(JavaDateTime);
end;




class function ParseISO8601Date(const ISO8601Date: SOString;
  var st: TSystemTime; var dayofyear: Integer; var week: Word;
  var bias: Integer; var havetz, havedate: Boolean): Boolean;

  function get(var v: Word; c: SOChar): Boolean; {$IFDEF HAVE_INLINE} inline; {$ENDIF}
  begin
    if (c < #256) and (AnsiChar(c) in ['0' .. '9']) then
    begin
      Result := True;
      v := v * 10 + Ord(c) - Ord('0');
    end
    else
      Result := False;
  end;

type
  TState = (stStart, stYear, stMonth, stWeek, stWeekDay, stDay, stDayOfYear,
    stHour, stMin, stSec, stMs, stUTC, stGMTH, stGMTM, stGMTend, stEnd);
  TPerhaps = (yes, no, perhaps);
var
  p: PSOChar;
  sep: TPerhaps;
  state: TState;
  pos, v: Word;
  inctz: Boolean;
label
  error;
begin
  p := PSOChar(ISO8601Date);
  sep := perhaps;
  state := stStart;
  pos := 0;
  inctz := False;

  FillChar(st, SizeOf(st), 0);
  dayofyear := 0;
  week := 0;
  bias := 0;
  havedate := True;
  havetz := False;

  while True do
    case state of
      stStart:
        case p^ of
          '0' .. '9':
            state := stYear;
          'T', 't':
            begin
              state := stHour;
              pos := 0;
              Inc(p);
              havedate := False;
            end;
        else
          goto error;
        end;
      stYear:
        case pos of
          0 .. 1, 3:
            if get(st.Year, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          2:
            case p^ of
              '0' .. '9':
                begin
                  st.Year := st.Year * 10 + Ord(p^) - Ord('0');
                  Inc(pos);
                  Inc(p);
                end;
              ':':
                begin
                  havedate := False;
                  st.Hour := st.Year;
                  st.Year := 0;
                  Inc(p);
                  pos := 0;
                  state := stMin;
                  sep := yes;
                end;
            else
              goto error;
            end;
          4:
            case p^ of
              '-':
                begin
                  pos := 0;
                  Inc(p);
                  sep := yes;
                  state := stMonth;
                end;
              '0' .. '9':
                begin
                  sep := no;
                  pos := 0;
                  state := stMonth;
                end;
              'W', 'w':
                begin
                  pos := 0;
                  Inc(p);
                  state := stWeek;
                end;
              'T', 't', ' ':
                begin
                  state := stHour;
                  pos := 0;
                  Inc(p);
                  st.Month := 1;
                  st.Day := 1;
                end;
              #0:
                begin
                  st.Month := 1;
                  st.Day := 1;
                  state := stEnd;
                end;
            else
              goto error;
            end;
        end;
      stMonth:
        case pos of
          0:
            case p^ of
              '0' .. '9':
                begin
                  st.Month := Ord(p^) - Ord('0');
                  Inc(pos);
                  Inc(p);
                end;
              'W', 'w':
                begin
                  pos := 0;
                  Inc(p);
                  state := stWeek;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Month, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          2:
            case p^ of
              '-':
                if (sep in [yes, perhaps]) then
                begin
                  pos := 0;
                  Inc(p);
                  state := stDay;
                  sep := yes;
                end
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stDay;
                  sep := no;
                end
                else
                begin
                  dayofyear := st.Month * 10 + Ord(p^) - Ord('0');
                  st.Month := 0;
                  Inc(p);
                  pos := 3;
                  state := stDayOfYear;
                end;
              'T', 't', ' ':
                begin
                  state := stHour;
                  pos := 0;
                  Inc(p);
                  st.Day := 1;
                end;
              #0:
                begin
                  st.Day := 1;
                  state := stEnd;
                end;
            else
              goto error;
            end;
        end;
      stDay:
        case pos of
          0:
            if get(st.Day, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          1:
            if get(st.Day, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else if sep in [no, perhaps] then
            begin
              dayofyear := st.Month * 10 + st.Day;
              st.Day := 0;
              st.Month := 0;
              state := stDayOfYear;
            end
            else
              goto error;
          2:
            case p^ of
              'T', 't', ' ':
                begin
                  pos := 0;
                  Inc(p);
                  state := stHour;
                end;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stDayOfYear:
        begin
          if (dayofyear <= 0) then
            goto error;
          case p^ of
            'T', 't', ' ':
              begin
                pos := 0;
                Inc(p);
                state := stHour;
              end;
            #0:
              state := stEnd;
          else
            goto error;
          end;
        end;
      stWeek:
        begin
          case pos of
            0 .. 1:
              if get(week, p^) then
              begin
                Inc(pos);
                Inc(p);
              end
              else
                goto error;
            2:
              case p^ of
                '-':
                  if (sep in [yes, perhaps]) then
                  begin
                    Inc(p);
                    state := stWeekDay;
                    sep := yes;
                  end
                  else
                    goto error;
                '1' .. '7':
                  if sep in [no, perhaps] then
                  begin
                    state := stWeekDay;
                    sep := no;
                  end
                  else
                    goto error;
              else
                goto error;
              end;
          end;
        end;
      stWeekDay:
        begin
          if (week > 0) and get(st.DayOfWeek, p^) then
          begin
            Inc(p);
            v := st.Year - 1;
            v := ((v * 365) + (v div 4) - (v div 100) + (v div 400)) mod 7 + 1;
            dayofyear := (st.DayOfWeek - v) + ((week) * 7) + 1;
            if v <= 4 then
              Dec(dayofyear, 7);
            case p^ of
              'T', 't', ' ':
                begin
                  pos := 0;
                  Inc(p);
                  state := stHour;
                end;
              #0:
                state := stEnd;
            else
              goto error;
            end;
          end
          else
            goto error;
        end;
      stHour:
        case pos of
          0:
            case p^ of
              '0' .. '9':
                if get(st.Hour, p^) then
                begin
                  Inc(pos);
                  Inc(p);
                end
                else
                  goto error;
              '-':
                begin
                  Inc(p);
                  state := stMin;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Hour, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          2:
            case p^ of
              ':':
                if sep in [yes, perhaps] then
                begin
                  sep := yes;
                  pos := 0;
                  Inc(p);
                  state := stMin;
                end
                else
                  goto error;
              ',', '.':
                begin
                  Inc(p);
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stMin;
                  sep := no;
                end
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stMin:
        case pos of
          0:
            case p^ of
              '0' .. '9':
                if get(st.Minute, p^) then
                begin
                  Inc(pos);
                  Inc(p);
                end
                else
                  goto error;
              '-':
                begin
                  Inc(p);
                  state := stSec;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Minute, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          2:
            case p^ of
              ':':
                if sep in [yes, perhaps] then
                begin
                  pos := 0;
                  Inc(p);
                  state := stSec;
                  sep := yes;
                end
                else
                  goto error;
              ',', '.':
                begin
                  Inc(p);
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stSec;
                end
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stSec:
        case pos of
          0 .. 1:
            if get(st.Second, p^) then
            begin
              Inc(pos);
              Inc(p);
            end
            else
              goto error;
          2:
            case p^ of
              ',', '.':
                begin
                  Inc(p);
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Inc(p);
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stMs:
        case p^ of
          '0' .. '9':
            begin
              st.Millisecond := st.Millisecond * 10 + Ord(p^) - Ord('0');
              Inc(p);
            end;
          '+':
            if havedate then
            begin
              state := stGMTH;
              pos := 0;
              v := 0;
              Inc(p);
            end
            else
              goto error;
          '-':
            if havedate then
            begin
              state := stGMTH;
              pos := 0;
              v := 0;
              Inc(p);
              inctz := True;
            end
            else
              goto error;
          'Z', 'z':
            if havedate then
              state := stUTC
            else
              goto error;
          #0:
            state := stEnd;
        else
          goto error;
        end;
      stUTC: // = GMT 0
        begin
          havetz := True;
          Inc(p);
          if p^ = #0 then
            Break
          else
            goto error;
        end;
      stGMTH:
        begin
          havetz := True;
          case pos of
            0 .. 1:
              if get(v, p^) then
              begin
                Inc(p);
                Inc(pos);
              end
              else
                goto error;
            2:
              begin
                bias := v * 60;
                case p^ of
                  ':': // if sep in [yes, perhaps] then
                    begin
                      state := stGMTM;
                      Inc(p);
                      pos := 0;
                      v := 0;
                      sep := yes;
                    end; // else goto error;
                  '0' .. '9':
                    // if sep in [no, perhaps] then
                    begin
                      state := stGMTM;
                      pos := 1;
                      sep := no;
                      Inc(p);
                      v := Ord(p^) - Ord('0');
                    end; // else goto error;
                  #0:
                    state := stGMTend;
                else
                  goto error;
                end;

              end;
          end;
        end;
      stGMTM:
        case pos of
          0 .. 1:
            if get(v, p^) then
            begin
              Inc(p);
              Inc(pos);
            end
            else
              goto error;
          2:
            case p^ of
              #0:
                begin
                  state := stGMTend;
                  Inc(bias, v);
                end;
            else
              goto error;
            end;
        end;
      stGMTend:
        begin
          if not inctz then
            bias := -bias;
          Break;
        end;
      stEnd:
        begin

          Break;
        end;
    end;

  if (st.Hour >= 24) or (st.Minute >= 60) or (st.Second >= 60) or
    (st.Millisecond >= 1000) or (week > 53) then
    goto error;

  Result := True;
  Exit;
error:
  Result := False;
end;




end.

