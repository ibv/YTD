
{*******************************************************}
{                                                       }
{       Borland Delphi Runtime Library                  }
{       System Utilities Unit                           }
{                                                       }
{       Copyright (C) 1995,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit SysUtils;

{$H+}

interface

uses Windows, SysConst, kol;

const

{ File open modes }

  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

{ File attribute constants }

  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faAnyFile   = $0000003F;

{ File mode magic numbers }

  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

{ Seconds and milliseconds per day }

  SecsPerDay = 24 * 60 * 60;
  MSecsPerDay = SecsPerDay * 1000;

{ Days between 1/1/0001 and 12/31/1899 }

  DateDelta = 693594;

type

{ Standard Character set type }

  TSysCharSet = set of Char;

{ Set access to an integer }

  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

{ Type conversion records }

  WordRec = packed record
    Lo, Hi: Byte;
  end;

  LongRec = packed record
    Lo, Hi: Word;
  end;

  Int64Rec = packed record
    Lo, Hi: DWORD;
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

{ General arrays }

  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

{ Generic procedure pointer }

  TProcedure = procedure;

{ Generic filename type }

  TFileName = type string;

{ Search record used by FindFirst, FindNext, and FindClose }

  TSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: TFileName;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindData;
  end;

{ Typed-file and untyped-file record }

  TFileRec = packed record (* must match the size the compiler generates: 332 bytes *)
    Handle: Integer;
    Mode: Integer;
    RecSize: Cardinal;
    Private: array[1..28] of Byte;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of Char;
  end;

{ Text file record structure used for Text files }

  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of Char;
  TTextRec = packed record (* must match the size the compiler generates: 460 bytes *)
    Handle: Integer;
    Mode: Integer;
    BufSize: Cardinal;
    BufPos: Cardinal;
    BufEnd: Cardinal;
    BufPtr: PChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of Char;
    Buffer: TTextBuf;
  end;

{ FloatToText, FloatToTextFmt, TextToFloat, and FloatToDecimal type codes }

  TFloatValue = (fvExtended, fvCurrency);

{ FloatToText format codes }

  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);

{ FloatToDecimal result record }

  TFloatRec = packed record
    Exponent: Smallint;
    Negative: Boolean;
    Digits: array[0..20] of Char;
  end;

{ Date and time record }

  TTimeStamp = record
    Time: Integer;      { Number of milliseconds since midnight }
    Date: Integer;      { One plus number of days since 1/1/0001 }
  end;

{ MultiByte Character Set (MBCS) byte type }
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

{ System Locale information record }
  TSysLocale = packed record
    DefaultLCID: LCID;
    PriLangID: LANGID;
    SubLangID: LANGID;
    FarEast: Boolean;
    MiddleEast: Boolean;
  end;

{ This is used by TLanguages }
  TLangRec = packed record
    FName: string;
    FLCID: LCID;
    FExt: string;
  end;

{ This stores the langauges that the system supports }
  TLanguages = class
  private
    FSysLangs: array of TLangRec;
    function LocalesCallback(LocaleID: PChar): Integer; stdcall;
    function GetExt(Index: Integer): string;
    function GetID(Index: Integer): string;
    function GetLCID(Index: Integer): LCID;
    function GetName(Index: Integer): string;
    function GetNameFromLocaleID(ID: LCID): string;
    function GetNameFromLCID(const ID: string): string;
    function GetCount: integer;
  public
    constructor Create;
    function IndexOf(ID: LCID): Integer;
    property Count: Integer read GetCount;
    property Name[Index: Integer]: string read GetName;
    property NameFromLocaleID[ID: LCID]: string read GetNameFromLocaleID;
    property NameFromLCID[const ID: string]: string read GetNameFromLCID;
    property ID[Index: Integer]: string read GetID;
    property LocaleID[Index: Integer]: LCID read GetLCID;
    property Ext[Index: Integer]: string read GetExt;
  end;

{ Exceptions }

  Exception = class(TObject)
  private
    FMessage: string;
    FHelpContext: Integer;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: Integer; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
    constructor CreateHelp(const Msg: string; AHelpContext: Integer);
    constructor CreateFmtHelp(const Msg: string; const Args: array of const;
      AHelpContext: Integer);
    constructor CreateResHelp(Ident: Integer; AHelpContext: Integer); overload;
    constructor CreateResHelp(ResStringRec: PResStringRec; AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(ResStringRec: PResStringRec; const Args: array of const;
      AHelpContext: Integer); overload;
    constructor CreateResFmtHelp(Ident: Integer; const Args: array of const;
      AHelpContext: Integer); overload;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property Message: string read FMessage write FMessage;
  end;

  ExceptClass = class of Exception;

  EAbort = class(Exception);

  EHeapException = class(Exception)
  private
    AllowFree: Boolean;
  public
    procedure FreeInstance; override;
  end;

  EOutOfMemory = class(EHeapException);

  EInOutError = class(Exception)
  public
    ErrorCode: Integer;
  end;

  EExternal = class(Exception)
  public
    ExceptionRecord: PExceptionRecord;
  end;

  EExternalException = class(EExternal);

  EIntError = class(EExternal);
  EDivByZero = class(EIntError);
  ERangeError = class(EIntError);
  EIntOverflow = class(EIntError);

  EMathError = class(EExternal);
  EInvalidOp = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow = class(EMathError);
  EUnderflow = class(EMathError);

  EInvalidPointer = class(EHeapException);

  EInvalidCast = class(Exception);

  EConvertError = class(Exception);

  EAccessViolation = class(EExternal);
  EPrivilege = class(EExternal);
  EStackOverflow = class(EExternal);
  EControlC = class(EExternal);

  EVariantError = class(Exception);

  EPropReadOnly = class(Exception);
  EPropWriteOnly = class(Exception);

  EAssertionFailed = class(Exception);

  EAbstractError = class(Exception);

  EIntfCastError = class(Exception);

  EInvalidContainer = class(Exception);
  EInvalidInsert = class(Exception);

  EPackageError = class(Exception);

  EWin32Error = class(Exception)
  public
    ErrorCode: DWORD;
  end;

  ESafecallException = class(Exception);

var

{ Empty string and null string pointer. These constants are provided for
  backwards compatibility only.  }

  EmptyStr: string = '';
  NullStr: PString = @EmptyStr;

{ Win32 platform identifier.  This will be one of the following values:

    VER_PLATFORM_WIN32s
    VER_PLATFORM_WIN32_WINDOWS
    VER_PLATFORM_WIN32_NT

  See WINDOWS.PAS for the numerical values. }

  Win32Platform: Integer = 0;

{ Win32 OS version information -

  see TOSVersionInfo.dwMajorVersion/dwMinorVersion/dwBuildNumber }

  Win32MajorVersion: Integer = 0;
  Win32MinorVersion: Integer = 0;
  Win32BuildNumber: Integer = 0;

{ Win32 OS extra version info string -

  see TOSVersionInfo.szCSDVersion }

  Win32CSDVersion: string = '';

{ Currency and date/time formatting options

  The initial values of these variables are fetched from the system registry
  using the GetLocaleInfo function in the Win32 API. The description of each
  variable specifies the LOCALE_XXXX constant used to fetch the initial
  value.

  CurrencyString - Defines the currency symbol used in floating-point to
  decimal conversions. The initial value is fetched from LOCALE_SCURRENCY.

  CurrencyFormat - Defines the currency symbol placement and separation
  used in floating-point to decimal conversions. Possible values are:

    0 = '$1'
    1 = '1$'
    2 = '$ 1'
    3 = '1 $'

  The initial value is fetched from LOCALE_ICURRENCY.

  NegCurrFormat - Defines the currency format for used in floating-point to
  decimal conversions of negative numbers. Possible values are:

    0 = '($1)'      4 = '(1$)'      8 = '-1 $'      12 = '$ -1'
    1 = '-$1'       5 = '-1$'       9 = '-$ 1'      13 = '1- $'
    2 = '$-1'       6 = '1-$'      10 = '1 $-'      14 = '($ 1)'
    3 = '$1-'       7 = '1$-'      11 = '$ 1-'      15 = '(1 $)'

  The initial value is fetched from LOCALE_INEGCURR.

  ThousandSeparator - The character used to separate thousands in numbers
  with more than three digits to the left of the decimal separator. The
  initial value is fetched from LOCALE_STHOUSAND.

  DecimalSeparator - The character used to separate the integer part from
  the fractional part of a number. The initial value is fetched from
  LOCALE_SDECIMAL.

  CurrencyDecimals - The number of digits to the right of the decimal point
  in a currency amount. The initial value is fetched from LOCALE_ICURRDIGITS.

  DateSeparator - The character used to separate the year, month, and day
  parts of a date value. The initial value is fetched from LOCATE_SDATE.

  ShortDateFormat - The format string used to convert a date value to a
  short string suitable for editing. For a complete description of date and
  time format strings, refer to the documentation for the FormatDate
  function. The short date format should only use the date separator
  character and the  m, mm, d, dd, yy, and yyyy format specifiers. The
  initial value is fetched from LOCALE_SSHORTDATE.

  LongDateFormat - The format string used to convert a date value to a long
  string suitable for display but not for editing. For a complete description
  of date and time format strings, refer to the documentation for the
  FormatDate function. The initial value is fetched from LOCALE_SLONGDATE.

  TimeSeparator - The character used to separate the hour, minute, and
  second parts of a time value. The initial value is fetched from
  LOCALE_STIME.

  TimeAMString - The suffix string used for time values between 00:00 and
  11:59 in 12-hour clock format. The initial value is fetched from
  LOCALE_S1159.

  TimePMString - The suffix string used for time values between 12:00 and
  23:59 in 12-hour clock format. The initial value is fetched from
  LOCALE_S2359.

  ShortTimeFormat - The format string used to convert a time value to a
  short string with only hours and minutes. The default value is computed
  from LOCALE_ITIME and LOCALE_ITLZERO.

  LongTimeFormat - The format string used to convert a time value to a long
  string with hours, minutes, and seconds. The default value is computed
  from LOCALE_ITIME and LOCALE_ITLZERO.

  ShortMonthNames - Array of strings containing short month names. The mmm
  format specifier in a format string passed to FormatDate causes a short
  month name to be substituted. The default values are fecthed from the
  LOCALE_SABBREVMONTHNAME system locale entries.

  LongMonthNames - Array of strings containing long month names. The mmmm
  format specifier in a format string passed to FormatDate causes a long
  month name to be substituted. The default values are fecthed from the
  LOCALE_SMONTHNAME system locale entries.

  ShortDayNames - Array of strings containing short day names. The ddd
  format specifier in a format string passed to FormatDate causes a short
  day name to be substituted. The default values are fecthed from the
  LOCALE_SABBREVDAYNAME system locale entries.

  LongDayNames - Array of strings containing long day names. The dddd
  format specifier in a format string passed to FormatDate causes a long
  day name to be substituted. The default values are fecthed from the
  LOCALE_SDAYNAME system locale entries.

  ListSeparator - The character used to separate items in a list.  The
  initial value is fetched from LOCALE_SLIST.

  TwoDigitYearCenturyWindow - Determines what century is added to two
  digit years when converting string dates to numeric dates.  This value
  is subtracted from the current year before extracting the century.
  This can be used to extend the lifetime of existing applications that
  are inextricably tied to 2 digit year data entry.  The best solution
  to Year 2000 (Y2k) issues is not to accept 2 digit years at all - require
  4 digit years in data entry to eliminate century ambiguities.

  Examples:

  Current TwoDigitCenturyWindow  Century  StrToDate() of:
  Year    Value                  Pivot    '01/01/03' '01/01/68' '01/01/50'
  -------------------------------------------------------------------------
  1998    0                      1900     1903       1968       1950
  2002    0                      2000     2003       2068       2050
  1998    50 (default)           1948     2003       1968       1950
  2002    50 (default)           1952     2003       1968       2050
  2020    50 (default)           1970     2003       2068       2050
 }

var
  CurrencyString: string;
  CurrencyFormat: Byte;
  NegCurrFormat: Byte;
  ThousandSeparator: Char;
  DecimalSeparator: Char;
  CurrencyDecimals: Byte;
  DateSeparator: Char;
  ShortDateFormat: string;
  LongDateFormat: string;
  TimeSeparator: Char;
  TimeAMString: string;
  TimePMString: string;
  ShortTimeFormat: string;
  LongTimeFormat: string;
  ShortMonthNames: array[1..12] of string;
  LongMonthNames: array[1..12] of string;
  ShortDayNames: array[1..7] of string;
  LongDayNames: array[1..7] of string;
  SysLocale: TSysLocale;
  EraNames: array[1..7] of string;
  EraYearOffsets: array[1..7] of Integer;
  TwoDigitYearCenturyWindow: Word = 50;
  ListSeparator: Char;

function Languages: TLanguages;

{ Memory management routines }

{ AllocMem allocates a block of the given size on the heap. Each byte in
  the allocated buffer is set to zero. To dispose the buffer, use the
  FreeMem standard procedure. }

function AllocMem(Size: Cardinal): Pointer;

{ Exit procedure handling }

{ AddExitProc adds the given procedure to the run-time library's exit
  procedure list. When an application terminates, its exit procedures are
  executed in reverse order of definition, i.e. the last procedure passed
  to AddExitProc is the first one to get executed upon termination. }

procedure AddExitProc(Proc: TProcedure);

{ String handling routines }

{ NewStr allocates a string on the heap. NewStr is provided for backwards
  compatibility only. }

function NewStr(const S: string): PString;

{ DisposeStr disposes a string pointer that was previously allocated using
  NewStr. DisposeStr is provided for backwards compatibility only. }

procedure DisposeStr(P: PString);

{ AssignStr assigns a new dynamically allocated string to the given string
  pointer. AssignStr is provided for backwards compatibility only. }

procedure AssignStr(var P: PString; const S: string);

{ AppendStr appends S to the end of Dest. AppendStr is provided for
  backwards compatibility only. Use "Dest := Dest + S" instead. }

procedure AppendStr(var Dest: string; const S: string);

{ UpperCase converts all ASCII characters in the given string to upper case.
  The conversion affects only 7-bit ASCII characters between 'a' and 'z'. To
  convert 8-bit international characters, use AnsiUpperCase. }

function UpperCase(const S: string): string;

{ LowerCase converts all ASCII characters in the given string to lower case.
  The conversion affects only 7-bit ASCII characters between 'A' and 'Z'. To
  convert 8-bit international characters, use AnsiLowerCase. }

function LowerCase(const S: string): string;

{ CompareStr compares S1 to S2, with case-sensitivity. The return value is
  less than 0 if S1 < S2, 0 if S1 = S2, or greater than 0 if S1 > S2. The
  compare operation is based on the 8-bit ordinal value of each character
  and is not affected by the current Windows locale. }

function CompareStr(const S1, S2: string): Integer;

{ CompareMem performs a binary compare of Length bytes of memory referenced
  by P1 to that of P2.  CompareMem returns True if the memory referenced by
  P1 is identical to that of P2. }

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;

{ CompareText compares S1 to S2, without case-sensitivity. The return value
  is the same as for CompareStr. The compare operation is based on the 8-bit
  ordinal value of each character, after converting 'a'..'z' to 'A'..'Z',
  and is not affected by the current Windows locale. }

function CompareText(const S1, S2: string): Integer;

{ SameText compares S1 to S2, without case-sensitivity. Returns true if
  S1 and S2 are the equal, that is, if CompareText would return 0. SameText
  has the same 8-bit limitations as CompareText }

function SameText(const S1, S2: string): Boolean;

{ AnsiUpperCase converts all characters in the given string to upper case.
  The conversion uses the current Windows locale. }

function AnsiUpperCase(const S: string): string;

{ AnsiLowerCase converts all characters in the given string to lower case.
  The conversion uses the current Windows locale. }

function AnsiLowerCase(const S: string): string;

{ AnsiCompareStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is the same as for CompareStr. }

function AnsiCompareStr(const S1, S2: string): Integer;

{ AnsiSameStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is True if AnsiCompareStr would have returned 0. }

function AnsiSameStr(const S1, S2: string): Boolean;

{ AnsiCompareText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is the same as for CompareStr. }

function AnsiCompareText(const S1, S2: string): Integer;

{ AnsiSameText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is True if AnsiCompareText would have returned 0. }

function AnsiSameText(const S1, S2: string): Boolean;

{ AnsiStrComp compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is the same as for CompareStr. }

function AnsiStrComp(S1, S2: PChar): Integer;

{ AnsiStrIComp compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current Windows locale. The return value
  is the same as for CompareStr. }

function AnsiStrIComp(S1, S2: PChar): Integer;

{ AnsiStrLComp compares S1 to S2, with case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current Windows locale. The return value is the same as for CompareStr. }

function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer;

{ AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current Windows locale. The return value is the same as for CompareStr. }

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;

{ AnsiStrLower converts all characters in the given string to lower case.
  The conversion uses the current Windows locale. }

function AnsiStrLower(Str: PChar): PChar;

{ AnsiStrUpper converts all characters in the given string to upper case.
  The conversion uses the current Windows locale. }

function AnsiStrUpper(Str: PChar): PChar;

{ AnsiLastChar returns a pointer to the last full character in the string.
  This function supports multibyte characters  }

function AnsiLastChar(const S: string): PChar;

{ AnsiStrLastChar returns a pointer to the last full character in the string.
  This function supports multibyte characters.  }

function AnsiStrLastChar(P: PChar): PChar;

{ Trim trims leading and trailing spaces and control characters from the
  given string. }

function Trim(const S: string): string;

{ TrimLeft trims leading spaces and control characters from the given
  string. }

function TrimLeft(const S: string): string;

{ TrimRight trims trailing spaces and control characters from the given
  string. }

function TrimRight(const S: string): string;

{ QuotedStr returns the given string as a quoted string. A single quote
  character is inserted at the beginning and the end of the string, and
  for each single quote character in the string, another one is added. }

function QuotedStr(const S: string): string;

{ AnsiQuotedStr returns the given string as a quoted string, using the
  provided Quote character.  A Quote character is inserted at the beginning
  and end of thestring, and each Quote character in the string is doubled.
  This function supports multibyte character strings (MBCS). }

function AnsiQuotedStr(const S: string; Quote: Char): string;

{ AnsiExtractQuotedStr removes the Quote characters from the beginning and end
  of a quoted string, and reduces pairs of Quote characters within the quoted
  string to a single character. If the first character in Src is not the Quote
  character, the function returns an empty string.  The function copies
  characters from the Src to the result string until the second solitary
  Quote character or the first null character in Src. The Src parameter is
  updated to point to the first character following the quoted string.  If
  the Src string does not contain a matching end Quote character, the Src
  parameter is updated to point to the terminating null character in Src.
  This function supports multibyte character strings (MBCS).  }

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;

{ AdjustLineBreaks adjusts all line breaks in the given string to be true
  CR/LF sequences. The function changes any CR characters not followed by
  a LF and any LF characters not preceded by a CR into CR/LF pairs. }

function AdjustLineBreaks(const S: string): string;

{ IsValidIdent returns true if the given string is a valid identifier. An
  identifier is defined as a character from the set ['A'..'Z', 'a'..'z', '_']
  followed by zero or more characters from the set ['A'..'Z', 'a'..'z',
  '0..'9', '_']. }

function IsValidIdent(const Ident: string): Boolean;

{ IntToStr converts the given value to its decimal string representation. }

function IntToStr(Value: Integer): string; overload;
function IntToStr(Value: Int64): string; overload;

{ IntToHex converts the given value to a hexadecimal string representation
  with the minimum number of digits specified. }

function IntToHex(Value: Integer; Digits: Integer): string; overload;
function IntToHex(Value: Int64; Digits: Integer): string; overload;

{ StrToInt converts the given string to an integer value. If the string
  doesn't contain a valid value, an EConvertError exception is raised. }

function StrToInt(const S: string): Integer;
function StrToInt64(const S: string): Int64;

{ StrToIntDef converts the given string to an integer value. If the string
  doesn't contain a valid value, the value given by Default is returned. }

function StrToIntDef(const S: string; Default: Integer): Integer;
function StrToInt64Def(const S: string; Default: Int64): Int64;

{ LoadStr loads the string resource given by Ident from the application's
  executable file. If the string resource does not exist, an empty string
  is returned. }

function LoadStr(Ident: Integer): string;

{ LoadStr loads the string resource given by Ident from the application's
  executable file, and uses it as the format string in a call to the
  Format function with the given arguments. }

function FmtLoadStr(Ident: Integer; const Args: array of const): string;

{ File management routines }

{ FileOpen opens the specified file using the specified access mode. The
  access mode value is constructed by OR-ing one of the fmOpenXXXX constants
  with one of the fmShareXXXX constants. If the return value is positive,
  the function was successful and the value is the file handle of the opened
  file. A return value of -1 indicates that an error occurred. }

function FileOpen(const FileName: string; Mode: LongWord): Integer;

{ FileCreate creates a new file by the specified name. If the return value
  is positive, the function was successful and the value is the file handle
  of the new file. A return value of -1 indicates that an error occurred. }

function FileCreate(const FileName: string): Integer;

{ FileRead reads Count bytes from the file given by Handle into the buffer
  specified by Buffer. The return value is the number of bytes actually
  read; it is less than Count if the end of the file was reached. The return
  value is -1 if an error occurred. }

function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;

{ FileWrite writes Count bytes to the file given by Handle from the buffer
  specified by Buffer. The return value is the number of bytes actually
  written, or -1 if an error occurred. }

function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;

{ FileSeek changes the current position of the file given by Handle to be
  Offset bytes relative to the point given by Origin. Origin = 0 means that
  Offset is relative to the beginning of the file, Origin = 1 means that
  Offset is relative to the current position, and Origin = 2 means that
  Offset is relative to the end of the file. The return value is the new
  current position, relative to the beginning of the file, or -1 if an error
  occurred. }

function FileSeek(Handle, Offset, Origin: Integer): Integer; overload;
function FileSeek(Handle: Integer; const Offset: Int64; Origin: Integer): Int64; overload;

{ FileClose closes the specified file. }

procedure FileClose(Handle: Integer);

{ FileAge returns the date-and-time stamp of the specified file. The return
  value can be converted to a TDateTime value using the FileDateToDateTime
  function. The return value is -1 if the file does not exist. }

function FileAge(const FileName: string): Integer;

{ FileExists returns a boolean value that indicates whether the specified
  file exists. }

function FileExists(const FileName: string): Boolean;

{ FindFirst searches the directory given by Path for the first entry that
  matches the filename given by Path and the attributes given by Attr. The
  result is returned in the search record given by SearchRec. The return
  value is zero if the function was successful. Otherwise the return value
  is a Windows error code. FindFirst is typically used in conjunction with
  FindNext and FindClose as follows:

    Result := FindFirst(Path, Attr, SearchRec);
    while Result = 0 do
    begin
      ProcessSearchRec(SearchRec);
      Result := FindNext(SearchRec);
    end;
    FindClose(SearchRec);

  where ProcessSearchRec represents user-defined code that processes the
  information in a search record. }

function FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;

{ FindNext returs the next entry that matches the name and attributes
  specified in a previous call to FindFirst. The search record must be one
  that was passed to FindFirst. The return value is zero if the function was
  successful. Otherwise the return value is a Windows error code. }

function FindNext(var F: TSearchRec): Integer;

{ FindClose terminates a FindFirst/FindNext sequence. FindClose does nothing
  in the 16-bit version of Windows, but is required in the 32-bit version,
  so for maximum portability every FindFirst/FindNext sequence should end
  with a call to FindClose. }

procedure FindClose(var F: TSearchRec);

{ FileGetDate returns the DOS date-and-time stamp of the file given by
  Handle. The return value is -1 if the handle is invalid. The
  FileDateToDateTime function can be used to convert the returned value to
  a TDateTime value. }

function FileGetDate(Handle: Integer): Integer;

{ FileSetDate sets the DOS date-and-time stamp of the file given by Handle
  to the value given by Age. The DateTimeToFileDate function can be used to
  convert a TDateTime value to a DOS date-and-time stamp. The return value
  is zero if the function was successful. Otherwise the return value is a
  Windows error code. }

function FileSetDate(Handle: Integer; Age: Integer): Integer;

{ FileGetAttr returns the file attributes of the file given by FileName. The
  attributes can be examined by AND-ing with the faXXXX constants defined
  above. A return value of -1 indicates that an error occurred. }

function FileGetAttr(const FileName: string): Integer;

{ FileSetAttr sets the file attributes of the file given by FileName to the
  value given by Attr. The attribute value is formed by OR-ing the
  appropriate faXXXX constants. The return value is zero if the function was
  successful. Otherwise the return value is a Windows error code. }

function FileSetAttr(const FileName: string; Attr: Integer): Integer;

{ DeleteFile deletes the file given by FileName. The return value is True if
  the file was successfully deleted, or False if an error occurred. }

function DeleteFile(const FileName: string): Boolean;

{ RenameFile renames the file given by OldName to the name given by NewName.
  The return value is True if the file was successfully renamed, or False if
  an error occurred. }

function RenameFile(const OldName, NewName: string): Boolean;

{ ChangeFileExt changes the extension of a filename. FileName specifies a
  filename with or without an extension, and Extension specifies the new
  extension for the filename. The new extension can be a an empty string or
  a period followed by up to three characters. }

function ChangeFileExt(const FileName, Extension: string): string;

{ ExtractFilePath extracts the drive and directory parts of the given
  filename. The resulting string is the leftmost characters of FileName,
  up to and including the colon or backslash that separates the path
  information from the name and extension. The resulting string is empty
  if FileName contains no drive and directory parts. }

function ExtractFilePath(const FileName: string): string;

{ ExtractFileDir extracts the drive and directory parts of the given
  filename. The resulting string is a directory name suitable for passing
  to SetCurrentDir, CreateDir, etc. The resulting string is empty if
  FileName contains no drive and directory parts. }

function ExtractFileDir(const FileName: string): string;

{ ExtractFileDrive extracts the drive part of the given filename.  For
  filenames with drive letters, the resulting string is '<drive>:'.
  For filenames with a UNC path, the resulting string is in the form
  '\\<servername>\<sharename>'.  If the given path contains neither
  style of filename, the result is an empty string. }

function ExtractFileDrive(const FileName: string): string;

{ ExtractFileName extracts the name and extension parts of the given
  filename. The resulting string is the leftmost characters of FileName,
  starting with the first character after the colon or backslash that
  separates the path information from the name and extension. The resulting
  string is equal to FileName if FileName contains no drive and directory
  parts. }

function ExtractFileName(const FileName: string): string;

{ ExtractFileExt extracts the extension part of the given filename. The
  resulting string includes the period character that separates the name
  and extension parts. The resulting string is empty if the given filename
  has no extension. }

function ExtractFileExt(const FileName: string): string;

{ ExpandFileName expands the given filename to a fully qualified filename.
  The resulting string consists of a drive letter, a colon, a root relative
  directory path, and a filename. Embedded '.' and '..' directory references
  are removed. }

function ExpandFileName(const FileName: string): string;

{ ExpandUNCFileName expands the given filename to a fully qualified filename.
  This function is the same as ExpandFileName except that it will return the
  drive portion of the filename in the format '\\<servername>\<sharename> if
  that drive is actually a network resource instead of a local resource.
  Like ExpandFileName, embedded '.' and '..' directory references are
  removed. }

function ExpandUNCFileName(const FileName: string): string;

{ ExtractRelativePath will return a file path name relative to the given
  BaseName.  It strips the common path dirs and adds '..\' for each level
  up from the BaseName path. }

function ExtractRelativePath(const BaseName, DestName: string): string;

{ ExtractShortPathName will convert the given filename to the short form
  by calling the GetShortPathName API.  Will return an empty string if
  the file or directory specified does not exist }

function ExtractShortPathName(const FileName: string): string;

{ FileSearch searches for the file given by Name in the list of directories
  given by DirList. The directory paths in DirList must be separated by
  semicolons. The search always starts with the current directory of the
  current drive. The returned value is a concatenation of one of the
  directory paths and the filename, or an empty string if the file could not
  be located. }

function FileSearch(const Name, DirList: string): string;

{ DiskFree returns the number of free bytes on the specified drive number,
  where 0 = Current, 1 = A, 2 = B, etc. DiskFree returns -1 if the drive
  number is invalid. }

function DiskFree(Drive: Byte): Int64;

{ DiskSize returns the size in bytes of the specified drive number, where
  0 = Current, 1 = A, 2 = B, etc. DiskSize returns -1 if the drive number
  is invalid. }

function DiskSize(Drive: Byte): Int64;

{ FileDateToDateTime converts a DOS date-and-time value to a TDateTime
  value. The FileAge, FileGetDate, and FileSetDate routines operate on DOS
  date-and-time values, and the Time field of a TSearchRec used by the
  FindFirst and FindNext functions contains a DOS date-and-time value. }

function FileDateToDateTime(FileDate: Integer): TDateTime;

{ DateTimeToFileDate converts a TDateTime value to a DOS date-and-time
  value. The FileAge, FileGetDate, and FileSetDate routines operate on DOS
  date-and-time values, and the Time field of a TSearchRec used by the
  FindFirst and FindNext functions contains a DOS date-and-time value. }

function DateTimeToFileDate(DateTime: TDateTime): Integer;

{ GetCurrentDir returns the current directory. }

function GetCurrentDir: string;

{ SetCurrentDir sets the current directory. The return value is True if
  the current directory was successfully changed, or False if an error
  occurred. }

function SetCurrentDir(const Dir: string): Boolean;

{ CreateDir creates a new directory. The return value is True if a new
  directory was successfully created, or False if an error occurred. }

function CreateDir(const Dir: string): Boolean;

{ RemoveDir deletes an existing empty directory. The return value is
  True if the directory was successfully deleted, or False if an error
  occurred. }

function RemoveDir(const Dir: string): Boolean;

{ PChar routines }
{ const params help simplify C++ code.  No effect on pascal code }

{ StrLen returns the number of characters in Str, not counting the null
  terminator. }

function StrLen(const Str: PChar): Cardinal;

{ StrEnd returns a pointer to the null character that terminates Str. }

function StrEnd(const Str: PChar): PChar;

{ StrMove copies exactly Count characters from Source to Dest and returns
  Dest. Source and Dest may overlap. }

function StrMove(Dest: PChar; const Source: PChar; Count: Cardinal): PChar;

{ StrCopy copies Source to Dest and returns Dest. }

function StrCopy(Dest: PChar; const Source: PChar): PChar;

{ StrECopy copies Source to Dest and returns StrEnd(Dest). }

function StrECopy(Dest:PChar; const Source: PChar): PChar;

{ StrLCopy copies at most MaxLen characters from Source to Dest and
  returns Dest. }

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

{ StrPCopy copies the Pascal style string Source into Dest and
  returns Dest. }

function StrPCopy(Dest: PChar; const Source: string): PChar;

{ StrPLCopy copies at most MaxLen characters from the Pascal style string
  Source into Dest and returns Dest. }

function StrPLCopy(Dest: PChar; const Source: string;
  MaxLen: Cardinal): PChar;

{ StrCat appends a copy of Source to the end of Dest and returns Dest. }

function StrCat(Dest: PChar; const Source: PChar): PChar;

{ StrLCat appends at most MaxLen - StrLen(Dest) characters from Source to
  the end of Dest, and returns Dest. }

function StrLCat(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

{ StrComp compares Str1 to Str2. The return value is less than 0 if
  Str1 < Str2, 0 if Str1 = Str2, or greater than 0 if Str1 > Str2. }

function StrComp(const Str1, Str2: PChar): Integer;

{ StrIComp compares Str1 to Str2, without case sensitivity. The return
  value is the same as StrComp. }

function StrIComp(const Str1, Str2: PChar): Integer;

{ StrLComp compares Str1 to Str2, for a maximum length of MaxLen
  characters. The return value is the same as StrComp. }

function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;

{ StrLIComp compares Str1 to Str2, for a maximum length of MaxLen
  characters, without case sensitivity. The return value is the same
  as StrComp. }

function StrLIComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;

{ StrScan returns a pointer to the first occurrence of Chr in Str. If Chr
  does not occur in Str, StrScan returns NIL. The null terminator is
  considered to be part of the string. }

function StrScan(const Str: PChar; Chr: Char): PChar;

{ StrRScan returns a pointer to the last occurrence of Chr in Str. If Chr
  does not occur in Str, StrRScan returns NIL. The null terminator is
  considered to be part of the string. }

function StrRScan(const Str: PChar; Chr: Char): PChar;

{ StrPos returns a pointer to the first occurrence of Str2 in Str1. If
  Str2 does not occur in Str1, StrPos returns NIL. }

function StrPos(const Str1, Str2: PChar): PChar;

{ StrUpper converts Str to upper case and returns Str. }

function StrUpper(Str: PChar): PChar;

{ StrLower converts Str to lower case and returns Str. }

function StrLower(Str: PChar): PChar;

{ StrPas converts Str to a Pascal style string. This function is provided
  for backwards compatibility only. To convert a null terminated string to
  a Pascal style string, use a string type cast or an assignment. }

function StrPas(const Str: PChar): string;

{ StrAlloc allocates a buffer of the given size on the heap. The size of
  the allocated buffer is encoded in a four byte header that immediately
  preceeds the buffer. To dispose the buffer, use StrDispose. }

function StrAlloc(Size: Cardinal): PChar;

{ StrBufSize returns the allocated size of the given buffer, not including
  the two byte header. }

function StrBufSize(const Str: PChar): Cardinal;

{ StrNew allocates a copy of Str on the heap. If Str is NIL, StrNew returns
  NIL and doesn't allocate any heap space. Otherwise, StrNew makes a
  duplicate of Str, obtaining space with a call to the StrAlloc function,
  and returns a pointer to the duplicated string. To dispose the string,
  use StrDispose. }

function StrNew(const Str: PChar): PChar;

{ StrDispose disposes a string that was previously allocated with StrAlloc
  or StrNew. If Str is NIL, StrDispose does nothing. }

procedure StrDispose(Str: PChar);

{ String formatting routines }

{ The Format routine formats the argument list given by the Args parameter
  using the format string given by the Format parameter.

  Format strings contain two types of objects--plain characters and format
  specifiers. Plain characters are copied verbatim to the resulting string.
  Format specifiers fetch arguments from the argument list and apply
  formatting to them.

  Format specifiers have the following form:

    "%" [index ":"] ["-"] [width] ["." prec] type

  A format specifier begins with a % character. After the % come the
  following, in this order:

  -  an optional argument index specifier, [index ":"]
  -  an optional left-justification indicator, ["-"]
  -  an optional width specifier, [width]
  -  an optional precision specifier, ["." prec]
  -  the conversion type character, type

  The following conversion characters are supported:

  d  Decimal. The argument must be an integer value. The value is converted
     to a string of decimal digits. If the format string contains a precision
     specifier, it indicates that the resulting string must contain at least
     the specified number of digits; if the value has less digits, the
     resulting string is left-padded with zeros.

  u  Unsigned decimal.  Similar to 'd' but no sign is output.

  e  Scientific. The argument must be a floating-point value. The value is
     converted to a string of the form "-d.ddd...E+ddd". The resulting
     string starts with a minus sign if the number is negative, and one digit
     always precedes the decimal point. The total number of digits in the
     resulting string (including the one before the decimal point) is given
     by the precision specifer in the format string--a default precision of
     15 is assumed if no precision specifer is present. The "E" exponent
     character in the resulting string is always followed by a plus or minus
     sign and at least three digits.

  f  Fixed. The argument must be a floating-point value. The value is
     converted to a string of the form "-ddd.ddd...". The resulting string
     starts with a minus sign if the number is negative. The number of digits
     after the decimal point is given by the precision specifier in the
     format string--a default of 2 decimal digits is assumed if no precision
     specifier is present.

  g  General. The argument must be a floating-point value. The value is
     converted to the shortest possible decimal string using fixed or
     scientific format. The number of significant digits in the resulting
     string is given by the precision specifier in the format string--a
     default precision of 15 is assumed if no precision specifier is present.
     Trailing zeros are removed from the resulting string, and a decimal
     point appears only if necessary. The resulting string uses fixed point
     format if the number of digits to the left of the decimal point in the
     value is less than or equal to the specified precision, and if the
     value is greater than or equal to 0.00001. Otherwise the resulting
     string uses scientific format.

  n  Number. The argument must be a floating-point value. The value is
     converted to a string of the form "-d,ddd,ddd.ddd...". The "n" format
     corresponds to the "f" format, except that the resulting string
     contains thousand separators.

  m  Money. The argument must be a floating-point value. The value is
     converted to a string that represents a currency amount. The conversion
     is controlled by the CurrencyString, CurrencyFormat, NegCurrFormat,
     ThousandSeparator, DecimalSeparator, and CurrencyDecimals global
     variables, all of which are initialized from the Currency Format in
     the International section of the Windows Control Panel. If the format
     string contains a precision specifier, it overrides the value given
     by the CurrencyDecimals global variable.

  p  Pointer. The argument must be a pointer value. The value is converted
     to a string of the form "XXXX:YYYY" where XXXX and YYYY are the
     segment and offset parts of the pointer expressed as four hexadecimal
     digits.

  s  String. The argument must be a character, a string, or a PChar value.
     The string or character is inserted in place of the format specifier.
     The precision specifier, if present in the format string, specifies the
     maximum length of the resulting string. If the argument is a string
     that is longer than this maximum, the string is truncated.

  x  Hexadecimal. The argument must be an integer value. The value is
     converted to a string of hexadecimal digits. If the format string
     contains a precision specifier, it indicates that the resulting string
     must contain at least the specified number of digits; if the value has
     less digits, the resulting string is left-padded with zeros.

  Conversion characters may be specified in upper case as well as in lower
  case--both produce the same results.

  For all floating-point formats, the actual characters used as decimal and
  thousand separators are obtained from the DecimalSeparator and
  ThousandSeparator global variables.

  Index, width, and precision specifiers can be specified directly using
  decimal digit string (for example "%10d"), or indirectly using an asterisk
  charcater (for example "%*.*f"). When using an asterisk, the next argument
  in the argument list (which must be an integer value) becomes the value
  that is actually used. For example "Format('%*.*f', [8, 2, 123.456])" is
  the same as "Format('%8.2f', [123.456])".

  A width specifier sets the minimum field width for a conversion. If the
  resulting string is shorter than the minimum field width, it is padded
  with blanks to increase the field width. The default is to right-justify
  the result by adding blanks in front of the value, but if the format
  specifier contains a left-justification indicator (a "-" character
  preceding the width specifier), the result is left-justified by adding
  blanks after the value.

  An index specifier sets the current argument list index to the specified
  value. The index of the first argument in the argument list is 0. Using
  index specifiers, it is possible to format the same argument multiple
  times. For example "Format('%d %d %0:d %d', [10, 20])" produces the string
  '10 20 10 20'.

  The Format function can be combined with other formatting functions. For
  example

    S := Format('Your total was %s on %s', [
      FormatFloat('$#,##0.00;;zero', Total),
      FormatDateTime('mm/dd/yy', Date)]);

  which uses the FormatFloat and FormatDateTime functions to customize the
  format beyond what is possible with Format. }

function Format(const Format: string; const Args: array of const): string;

{ FmtStr formats the argument list given by Args using the format string
  given by Format into the string variable given by Result. For further
  details, see the description of the Format function. }

procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const);

{ StrFmt formats the argument list given by Args using the format string
  given by Format into the buffer given by Buffer. It is up to the caller to
  ensure that Buffer is large enough for the resulting string. The returned
  value is Buffer. For further details, see the description of the Format
  function. }

function StrFmt(Buffer, Format: PChar; const Args: array of const): PChar;

{ StrFmt formats the argument list given by Args using the format string
  given by Format into the buffer given by Buffer. The resulting string will
  contain no more than MaxLen characters, not including the null terminator.
  The returned value is Buffer. For further details, see the description of
  the Format function. }

function StrLFmt(Buffer: PChar; MaxLen: Cardinal; Format: PChar;
  const Args: array of const): PChar;

{ FormatBuf formats the argument list given by Args using the format string
  given by Format and FmtLen into the buffer given by Buffer and BufLen.
  The Format parameter is a reference to a buffer containing FmtLen
  characters, and the Buffer parameter is a reference to a buffer of BufLen
  characters. The returned value is the number of characters actually stored
  in Buffer. The returned value is always less than or equal to BufLen. For
  further details, see the description of the Format function. }

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;

{ Floating point conversion routines }

{ FloatToStr converts the floating-point value given by Value to its string
  representation. The conversion uses general number format with 15
  significant digits. For further details, see the description of the
  FloatToStrF function. }

function FloatToStr(Value: Extended): string;

{ CurrToStr converts the currency value given by Value to its string
  representation. The conversion uses general number format. For further
  details, see the description of the CurrToStrF function. }

function CurrToStr(Value: Currency): string;

{ FloatToStrF converts the floating-point value given by Value to its string
  representation. The Format parameter controls the format of the resulting
  string. The Precision parameter specifies the precision of the given value.
  It should be 7 or less for values of type Single, 15 or less for values of
  type Double, and 18 or less for values of type Extended. The meaning of the
  Digits parameter depends on the particular format selected.

  The possible values of the Format parameter, and the meaning of each, are
  described below.

  ffGeneral - General number format. The value is converted to the shortest
  possible decimal string using fixed or scientific format. Trailing zeros
  are removed from the resulting string, and a decimal point appears only
  if necessary. The resulting string uses fixed point format if the number
  of digits to the left of the decimal point in the value is less than or
  equal to the specified precision, and if the value is greater than or
  equal to 0.00001. Otherwise the resulting string uses scientific format,
  and the Digits parameter specifies the minimum number of digits in the
  exponent (between 0 and 4).

  ffExponent - Scientific format. The value is converted to a string of the
  form "-d.ddd...E+dddd". The resulting string starts with a minus sign if
  the number is negative, and one digit always precedes the decimal point.
  The total number of digits in the resulting string (including the one
  before the decimal point) is given by the Precision parameter. The "E"
  exponent character in the resulting string is always followed by a plus
  or minus sign and up to four digits. The Digits parameter specifies the
  minimum number of digits in the exponent (between 0 and 4).

  ffFixed - Fixed point format. The value is converted to a string of the
  form "-ddd.ddd...". The resulting string starts with a minus sign if the
  number is negative, and at least one digit always precedes the decimal
  point. The number of digits after the decimal point is given by the Digits
  parameter--it must be between 0 and 18. If the number of digits to the
  left of the decimal point is greater than the specified precision, the
  resulting value will use scientific format.

  ffNumber - Number format. The value is converted to a string of the form
  "-d,ddd,ddd.ddd...". The ffNumber format corresponds to the ffFixed format,
  except that the resulting string contains thousand separators.

  ffCurrency - Currency format. The value is converted to a string that
  represents a currency amount. The conversion is controlled by the
  CurrencyString, CurrencyFormat, NegCurrFormat, ThousandSeparator, and
  DecimalSeparator global variables, all of which are initialized from the
  Currency Format in the International section of the Windows Control Panel.
  The number of digits after the decimal point is given by the Digits
  parameter--it must be between 0 and 18.

  For all formats, the actual characters used as decimal and thousand
  separators are obtained from the DecimalSeparator and ThousandSeparator
  global variables.

  If the given value is a NAN (not-a-number), the resulting string is 'NAN'.
  If the given value is positive infinity, the resulting string is 'INF'. If
  the given value is negative infinity, the resulting string is '-INF'. }

function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer): string;

{ CurrToStrF converts the currency value given by Value to its string
  representation. A call to CurrToStrF corresponds to a call to
  FloatToStrF with an implied precision of 19 digits. }

function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): string;

{ FloatToText converts the given floating-point value to its decimal
  representation using the specified format, precision, and digits. The
  Value parameter must be a variable of type Extended or Currency, as
  indicated by the ValueType parameter. The resulting string of characters
  is stored in the given buffer, and the returned value is the number of
  characters stored. The resulting string is not null-terminated. For
  further details, see the description of the FloatToStrF function. }

function FloatToText(Buffer: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer): Integer;

{ FormatFloat formats the floating-point value given by Value using the
  format string given by Format. The following format specifiers are
  supported in the format string:

  0     Digit placeholder. If the value being formatted has a digit in the
        position where the '0' appears in the format string, then that digit
        is copied to the output string. Otherwise, a '0' is stored in that
        position in the output string.

  #     Digit placeholder. If the value being formatted has a digit in the
        position where the '#' appears in the format string, then that digit
        is copied to the output string. Otherwise, nothing is stored in that
        position in the output string.

  .     Decimal point. The first '.' character in the format string
        determines the location of the decimal separator in the formatted
        value; any additional '.' characters are ignored. The actual
        character used as a the decimal separator in the output string is
        determined by the DecimalSeparator global variable. The default value
        of DecimalSeparator is specified in the Number Format of the
        International section in the Windows Control Panel.

  ,     Thousand separator. If the format string contains one or more ','
        characters, the output will have thousand separators inserted between
        each group of three digits to the left of the decimal point. The
        placement and number of ',' characters in the format string does not
        affect the output, except to indicate that thousand separators are
        wanted. The actual character used as a the thousand separator in the
        output is determined by the ThousandSeparator global variable. The
        default value of ThousandSeparator is specified in the Number Format
        of the International section in the Windows Control Panel.

  E+    Scientific notation. If any of the strings 'E+', 'E-', 'e+', or 'e-'
  E-    are contained in the format string, the number is formatted using
  e+    scientific notation. A group of up to four '0' characters can
  e-    immediately follow the 'E+', 'E-', 'e+', or 'e-' to determine the
        minimum number of digits in the exponent. The 'E+' and 'e+' formats
        cause a plus sign to be output for positive exponents and a minus
        sign to be output for negative exponents. The 'E-' and 'e-' formats
        output a sign character only for negative exponents.

  'xx'  Characters enclosed in single or double quotes are output as-is, and
  "xx"  do not affect formatting.

  ;     Separates sections for positive, negative, and zero numbers in the
        format string.

  The locations of the leftmost '0' before the decimal point in the format
  string and the rightmost '0' after the decimal point in the format string
  determine the range of digits that are always present in the output string.

  The number being formatted is always rounded to as many decimal places as
  there are digit placeholders ('0' or '#') to the right of the decimal
  point. If the format string contains no decimal point, the value being
  formatted is rounded to the nearest whole number.

  If the number being formatted has more digits to the left of the decimal
  separator than there are digit placeholders to the left of the '.'
  character in the format string, the extra digits are output before the
  first digit placeholder.

  To allow different formats for positive, negative, and zero values, the
  format string can contain between one and three sections separated by
  semicolons.

  One section - The format string applies to all values.

  Two sections - The first section applies to positive values and zeros, and
  the second section applies to negative values.

  Three sections - The first section applies to positive values, the second
  applies to negative values, and the third applies to zeros.

  If the section for negative values or the section for zero values is empty,
  that is if there is nothing between the semicolons that delimit the
  section, the section for positive values is used instead.

  If the section for positive values is empty, or if the entire format string
  is empty, the value is formatted using general floating-point formatting
  with 15 significant digits, corresponding to a call to FloatToStrF with
  the ffGeneral format. General floating-point formatting is also used if
  the value has more than 18 digits to the left of the decimal point and
  the format string does not specify scientific notation.

  The table below shows some sample formats and the results produced when
  the formats are applied to different values:

  Format string          1234        -1234       0.5         0
  -----------------------------------------------------------------------
                         1234        -1234       0.5         0
  0                      1234        -1234       1           0
  0.00                   1234.00     -1234.00    0.50        0.00
  #.##                   1234        -1234       .5
  #,##0.00               1,234.00    -1,234.00   0.50        0.00
  #,##0.00;(#,##0.00)    1,234.00    (1,234.00)  0.50        0.00
  #,##0.00;;Zero         1,234.00    -1,234.00   0.50        Zero
  0.000E+00              1.234E+03   -1.234E+03  5.000E-01   0.000E+00
  #.###E-0               1.234E3     -1.234E3    5E-1        0E0
  ----------------------------------------------------------------------- }

function FormatFloat(const Format: string; Value: Extended): string;

{ FormatCurr formats the currency value given by Value using the format
  string given by Format. For further details, see the description of the
  FormatFloat function. }

function FormatCurr(const Format: string; Value: Currency): string;

{ FloatToTextFmt converts the given floating-point value to its decimal
  representation using the specified format. The Value parameter must be a
  variable of type Extended or Currency, as indicated by the ValueType
  parameter. The resulting string of characters is stored in the given
  buffer, and the returned value is the number of characters stored. The
  resulting string is not null-terminated. For further details, see the
  description of the FormatFloat function. }

function FloatToTextFmt(Buffer: PChar; const Value; ValueType: TFloatValue;
  Format: PChar): Integer;

{ StrToFloat converts the given string to a floating-point value. The string
  must consist of an optional sign (+ or -), a string of digits with an
  optional decimal point, and an optional 'E' or 'e' followed by a signed
  integer. Leading and trailing blanks in the string are ignored. The
  DecimalSeparator global variable defines the character that must be used
  as a decimal point. Thousand separators and currency symbols are not
  allowed in the string. If the string doesn't contain a valid value, an
  EConvertError exception is raised. }

function StrToFloat(const S: string): Extended;

{ StrToCurr converts the given string to a currency value. For further
  details, see the description of the StrToFloat function. }

function StrToCurr(const S: string): Currency;

{ TextToFloat converts the null-terminated string given by Buffer to a
  floating-point value which is returned in the variable given by Value.
  The Value parameter must be a variable of type Extended or Currency, as
  indicated by the ValueType parameter. The return value is True if the
  conversion was successful, or False if the string is not a valid
  floating-point value. For further details, see the description of the
  StrToFloat function. }

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue): Boolean;

{ FloatToDecimal converts a floating-point value to a decimal representation
  that is suited for further formatting. The Value parameter must be a
  variable of type Extended or Currency, as indicated by the ValueType
  parameter. For values of type Extended, the Precision parameter specifies
  the requested number of significant digits in the result--the allowed range
  is 1..18. For values of type Currency, the Precision parameter is ignored,
  and the implied precision of the conversion is 19 digits. The Decimals
  parameter specifies the requested maximum number of digits to the left of
  the decimal point in the result. Precision and Decimals together control
  how the result is rounded. To produce a result that always has a given
  number of significant digits regardless of the magnitude of the number,
  specify 9999 for the Decimals parameter. The result of the conversion is
  stored in the specified TFloatRec record as follows:

  Exponent - Contains the magnitude of the number, i.e. the number of
  significant digits to the right of the decimal point. The Exponent field
  is negative if the absolute value of the number is less than one. If the
  number is a NAN (not-a-number), Exponent is set to -32768. If the number
  is INF or -INF (positive or negative infinity), Exponent is set to 32767.

  Negative - True if the number is negative, False if the number is zero
  or positive.

  Digits - Contains up to 18 (for type Extended) or 19 (for type Currency)
  significant digits followed by a null terminator. The implied decimal
  point (if any) is not stored in Digits. Trailing zeros are removed, and
  if the resulting number is zero, NAN, or INF, Digits contains nothing but
  the null terminator. }

procedure FloatToDecimal(var Result: TFloatRec; const Value;
  ValueType: TFloatValue; Precision, Decimals: Integer);

{ Date/time support routines }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp;

{ EncodeDate encodes the given year, month, and day into a TDateTime value.
  The year must be between 1 and 9999, the month must be between 1 and 12,
  and the day must be between 1 and N, where N is the number of days in the
  specified month. If the specified values are not within range, an
  EConvertError exception is raised. The resulting value is the number of
  days between 12/30/1899 and the given date. }

function EncodeDate(Year, Month, Day: Word): TDateTime;

{ EncodeTime encodes the given hour, minute, second, and millisecond into a
  TDateTime value. The hour must be between 0 and 23, the minute must be
  between 0 and 59, the second must be between 0 and 59, and the millisecond
  must be between 0 and 999. If the specified values are not within range, an
  EConvertError exception is raised. The resulting value is a number between
  0 (inclusive) and 1 (not inclusive) that indicates the fractional part of
  a day given by the specified time. The value 0 corresponds to midnight,
  0.5 corresponds to noon, 0.75 corresponds to 6:00 pm, etc. }

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;

{ DecodeDate decodes the integral (date) part of the given TDateTime value
  into its corresponding year, month, and day. If the given TDateTime value
  is less than or equal to zero, the year, month, and day return parameters
  are all set to zero. }

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);

{ DecodeTime decodes the fractional (time) part of the given TDateTime value
  into its corresponding hour, minute, second, and millisecond. }

procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);

{ DateTimeToSystemTime converts a date and time from Delphi's TDateTime
  format into the Win32 API's TSystemTime format. }

procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);

{ SystemTimeToDateTime converts a date and time from the Win32 API's
  TSystemTime format into Delphi's TDateTime format. }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;

{ DayOfWeek returns the day of the week of the given date. The result is an
  integer between 1 and 7, corresponding to Sunday through Saturday. }

function DayOfWeek(Date: TDateTime): Integer;

{ Date returns the current date. }

function Date: TDateTime;

{ Time returns the current time. }

function Time: TDateTime;

{ Now returns the current date and time, corresponding to Date + Time. }

function Now: TDateTime;

{ IncMonth returns Date shifted by the specified number of months.
  NumberOfMonths parameter can be negative, to return a date N months ago.
  If the input day of month is greater than the last day of the resulting
  month, the day is set to the last day of the resulting month.
  Input time of day is copied to the DateTime result.  }

function IncMonth(const Date: TDateTime; NumberOfMonths: Integer): TDateTime;

{ ReplaceTime replaces the time portion of the DateTime parameter with the given
  time value, adjusting the signs as needed if the date is prior to 1900
  (Date value less than zero)  }

procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);

{ ReplaceDate replaces the date portion of the DateTime parameter with the given
  date value, adjusting as needed for negative dates }

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);

{ IsLeapYear determines whether the given year is a leap year. }

function IsLeapYear(Year: Word): Boolean;

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;

{ The MonthDays array can be used to quickly find the number of
  days in a month:  MonthDays[IsLeapYear(Y), M]      }

const
  MonthDays: array [Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

{ DateToStr converts the date part of the given TDateTime value to a string.
  The conversion uses the format specified by the ShortDateFormat global
  variable. }

function DateToStr(Date: TDateTime): string;

{ TimeToStr converts the time part of the given TDateTime value to a string.
  The conversion uses the format specified by the LongTimeFormat global
  variable. }

function TimeToStr(Time: TDateTime): string;

{ DateTimeToStr converts the given date and time to a string. The resulting
  string consists of a date and time formatted using the ShortDateFormat and
  LongTimeFormat global variables. Time information is included in the
  resulting string only if the fractional part of the given date and time
  value is non-zero. }

function DateTimeToStr(DateTime: TDateTime): string;

{ StrToDate converts the given string to a date value. The string must
  consist of two or three numbers, separated by the character defined by
  the DateSeparator global variable. The order for month, day, and year is
  determined by the ShortDateFormat global variable--possible combinations
  are m/d/y, d/m/y, and y/m/d. If the string contains only two numbers, it
  is interpreted as a date (m/d or d/m) in the current year. Year values
  between 0 and 99 are assumed to be in the current century. If the given
  string does not contain a valid date, an EConvertError exception is
  raised. }

function StrToDate(const S: string): TDateTime;

{ StrToTime converts the given string to a time value. The string must
  consist of two or three numbers, separated by the character defined by
  the TimeSeparator global variable, optionally followed by an AM or PM
  indicator. The numbers represent hour, minute, and (optionally) second,
  in that order. If the time is followed by AM or PM, it is assumed to be
  in 12-hour clock format. If no AM or PM indicator is included, the time
  is assumed to be in 24-hour clock format. If the given string does not
  contain a valid time, an EConvertError exception is raised. }

function StrToTime(const S: string): TDateTime;

{ StrToDateTime converts the given string to a date and time value. The
  string must contain a date optionally followed by a time. The date and
  time parts of the string must follow the formats described for the
  StrToDate and StrToTime functions. }

function StrToDateTime(const S: string): TDateTime;

{ FormatDateTime formats the date-and-time value given by DateTime using the
  format given by Format. The following format specifiers are supported:

  c       Displays the date using the format given by the ShortDateFormat
          global variable, followed by the time using the format given by
          the LongTimeFormat global variable. The time is not displayed if
          the fractional part of the DateTime value is zero.

  d       Displays the day as a number without a leading zero (1-31).

  dd      Displays the day as a number with a leading zero (01-31).

  ddd     Displays the day as an abbreviation (Sun-Sat) using the strings
          given by the ShortDayNames global variable.

  dddd    Displays the day as a full name (Sunday-Saturday) using the strings
          given by the LongDayNames global variable.

  ddddd   Displays the date using the format given by the ShortDateFormat
          global variable.

  dddddd  Displays the date using the format given by the LongDateFormat
          global variable.

  g       Displays the period/era as an abbreviation (Japanese and
          Taiwanese locales only).

  gg      Displays the period/era as a full name.

  e       Displays the year in the current period/era as a number without
          a leading zero (Japanese, Korean and Taiwanese locales only).

  ee      Displays the year in the current period/era as a number with
          a leading zero (Japanese, Korean and Taiwanese locales only).

  m       Displays the month as a number without a leading zero (1-12). If
          the m specifier immediately follows an h or hh specifier, the
          minute rather than the month is displayed.

  mm      Displays the month as a number with a leading zero (01-12). If
          the mm specifier immediately follows an h or hh specifier, the
          minute rather than the month is displayed.

  mmm     Displays the month as an abbreviation (Jan-Dec) using the strings
          given by the ShortMonthNames global variable.

  mmmm    Displays the month as a full name (January-December) using the
          strings given by the LongMonthNames global variable.

  yy      Displays the year as a two-digit number (00-99).

  yyyy    Displays the year as a four-digit number (0000-9999).

  h       Displays the hour without a leading zero (0-23).

  hh      Displays the hour with a leading zero (00-23).

  n       Displays the minute without a leading zero (0-59).

  nn      Displays the minute with a leading zero (00-59).

  s       Displays the second without a leading zero (0-59).

  ss      Displays the second with a leading zero (00-59).

  z       Displays the millisecond without a leading zero (0-999).

  zzz     Displays the millisecond with a leading zero (000-999).

  t       Displays the time using the format given by the ShortTimeFormat
          global variable.

  tt      Displays the time using the format given by the LongTimeFormat
          global variable.

  am/pm   Uses the 12-hour clock for the preceding h or hh specifier, and
          displays 'am' for any hour before noon, and 'pm' for any hour
          after noon. The am/pm specifier can use lower, upper, or mixed
          case, and the result is displayed accordingly.

  a/p     Uses the 12-hour clock for the preceding h or hh specifier, and
          displays 'a' for any hour before noon, and 'p' for any hour after
          noon. The a/p specifier can use lower, upper, or mixed case, and
          the result is displayed accordingly.

  ampm    Uses the 12-hour clock for the preceding h or hh specifier, and
          displays the contents of the TimeAMString global variable for any
          hour before noon, and the contents of the TimePMString global
          variable for any hour after noon.

  /       Displays the date separator character given by the DateSeparator
          global variable.

  :       Displays the time separator character given by the TimeSeparator
          global variable.

  'xx'    Characters enclosed in single or double quotes are displayed as-is,
  "xx"    and do not affect formatting.

  Format specifiers may be written in upper case as well as in lower case
  letters--both produce the same result.

  If the string given by the Format parameter is empty, the date and time
  value is formatted as if a 'c' format specifier had been given.

  The following example:

    S := FormatDateTime('"The meeting is on" dddd, mmmm d, yyyy, ' +
      '"at" hh:mm AM/PM', StrToDateTime('2/15/95 10:30am'));

  assigns 'The meeting is on Wednesday, February 15, 1995 at 10:30 AM' to
  the string variable S. }

function FormatDateTime(const Format: string; DateTime: TDateTime): string;

{ DateTimeToString converts the date and time value given by DateTime using
  the format string given by Format into the string variable given by Result.
  For further details, see the description of the FormatDateTime function. }

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime);

{ System error messages }

function SysErrorMessage(ErrorCode: Integer): string;

{ Initialization file support }

function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;

{ GetFormatSettings resets all date and number format variables to their
  default values. }

procedure GetFormatSettings;

{ Exception handling routines }

function ExceptObject: TObject;
function ExceptAddr: Pointer;

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PChar; Size: Integer): Integer;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);

procedure Abort;

procedure OutOfMemoryError;

procedure Beep;

{ MBCS functions }

{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
var
  LeadBytes: set of Char = [];
(*$EXTERNALSYM LeadBytes*)
(*$HPPEMIT 'namespace Sysutils {'*)
(*$HPPEMIT 'extern PACKAGE System::Set<Byte, 0, 255>  LeadBytes;'*)
(*$HPPEMIT '} // namespace Sysutils'*)

{ ByteType indicates what kind of byte exists at the Index'th byte in S.
  Western locales always return mbSingleByte.  Far East multibyte locales
  may also return mbLeadByte, indicating the byte is the first in a multibyte
  character sequence, and mbTrailByte, indicating that the byte is the second
  in a multibyte character sequence.  Parameters are assumed to be valid. }

function ByteType(const S: string; Index: Integer): TMbcsByteType;

{ StrByteType works the same as ByteType, but on null-terminated PChar strings }

function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;

{ ByteToCharLen returns the character length of a MBCS string, scanning the
  string for up to MaxLen bytes.  In multibyte character sets, the number of
  characters in a string may be less than the number of bytes.  }

function ByteToCharLen(const S: string; MaxLen: Integer): Integer;

{ CharToByteLen returns the byte length of a MBCS string, scanning the string
  for up to MaxLen characters. }

function CharToByteLen(const S: string; MaxLen: Integer): Integer;

{ ByteToCharIndex returns the 1-based character index of the Index'th byte in
  a MBCS string.  Returns zero if Index is out of range:
  (Index <= 0) or (Index > Length(S)) }

function ByteToCharIndex(const S: string; Index: Integer): Integer;

{ CharToByteIndex returns the 1-based byte index of the Index'th character
  in a MBCS string.  Returns zero if Index or Result are out of range:
  (Index <= 0) or (Index > Length(S)) or (Result would be > Length(S)) }

function CharToByteIndex(const S: string; Index: Integer): Integer;

{ IsPathDelimiter returns True if the character at byte S[Index]
  is '\', and it is not a MBCS lead or trail byte. }

function IsPathDelimiter(const S: string; Index: Integer): Boolean;

{ IsDelimiter returns True if the character at byte S[Index] matches any
  character in the Delimiters string, and the character is not a MBCS lead or
  trail byte.  S may contain multibyte characters; Delimiters must contain
  only single byte characters. }

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;

{ IncludeTrailingBackslash returns the path with a '\' at the end.
  This function is MBCS enabled. }

function IncludeTrailingBackslash(const S: string): string;

{ ExcludeTrailingBackslash returns the path without a '\' at the end.
  This function is MBCS enabled. }

function ExcludeTrailingBackslash(const S: string): string;

{ LastDelimiter returns the byte index in S of the rightmost whole
  character that matches any character in Delimiters (except null (#0)).
  S may contain multibyte characters; Delimiters must contain only single
  byte non-null characters.
  Example: LastDelimiter('\.:', 'c:\filename.ext') returns 12. }

function LastDelimiter(const Delimiters, S: string): Integer;

{ AnsiCompareFileName supports DOS file name comparison idiosyncracies
  in Far East locales (Zenkaku).  In non-MBCS locales, AnsiCompareFileName
  is identical to AnsiCompareText.  For general purpose file name comparisions,
  you should use this function instead of AnsiCompareText. }

function AnsiCompareFileName(const S1, S2: string): Integer;

{ AnsiLowerCaseFileName supports lowercase conversion idiosyncracies of
  DOS file names in Far East locales (Zenkaku).  In non-MBCS locales,
  AnsiLowerCaseFileName is identical to AnsiLowerCase. }

function AnsiLowerCaseFileName(const S: string): string;

{ AnsiUpperCaseFileName supports uppercase conversion idiosyncracies of
  DOS file names in Far East locales (Zenkaku).  In non-MBCS locales,
  AnsiUpperCaseFileName is identical to AnsiUpperCase. }

function AnsiUpperCaseFileName(const S: string): string;

{ AnsiPos:  Same as Pos but supports MBCS strings }

function AnsiPos(const Substr, S: string): Integer;

{ AnsiStrPos: Same as StrPos but supports MBCS strings }

function AnsiStrPos(Str, SubStr: PChar): PChar;

{ AnsiStrRScan: Same as StrRScan but supports MBCS strings }

function AnsiStrRScan(Str: PChar; Chr: Char): PChar;

{ AnsiStrScan: Same as StrScan but supports MBCS strings }

function AnsiStrScan(Str: PChar; Chr: Char): PChar;

{ StringReplace replaces occurances of <oldpattern> with <newpattern> in a
  given string.  Assumes the string may contain Multibyte characters }

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;

{ WrapText will scan a string for BreakChars and insert the BreakStr at the
  last BreakChar position before MaxCol.  Will not insert a break into an
  embedded quoted string (both ''' and '"' supported) }

function WrapText(const Line, BreakStr: string; BreakChars: TSysCharSet;
  MaxCol: Integer): string; overload;
function WrapText(const Line: string; MaxCol: Integer = 45): string; overload;

{ FindCmdLineSwitch determines whether the string in the Switch parameter
  was passed as a command line argument to the application.  SwitchChars
  identifies valid argument-delimiter characters (i.e., "-" and "/" are
  common delimiters). The IgnoreCase paramter controls whether a
  case-sensistive or case-insensitive search is performed. }

function FindCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet;
  IgnoreCase: Boolean): Boolean;

{ FreeAndNil frees the given TObject instance and sets the variable reference
  to nil.  Be careful to only pass TObjects to this routine. }

procedure FreeAndNil(var Obj);

{ Interface support routines }

function Supports(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean; overload;
function Supports(Instance: TObject; const Intf: TGUID; out Inst): Boolean; overload;

{ Package support routines }

{ Package Info flags }

const
  pfNeverBuild = $00000001;
  pfDesignOnly = $00000002;
  pfRunOnly = $00000004;
  pfIgnoreDupUnits = $00000008;
  pfModuleTypeMask = $C0000000;
  pfExeModule = $00000000;
  pfPackageModule = $40000000;
  pfProducerMask = $0C000000;
  pfV3Produced =  $00000000;
  pfProducerUndefined = $04000000;
  pfBCB4Produced = $08000000;
  pfDelphi4Produced = $0C000000;
  pfLibraryModule = $80000000;

{ Unit info flags }

const
  ufMainUnit = $01;
  ufPackageUnit = $02;
  ufWeakUnit = $04;
  ufOrgWeakUnit = $08;
  ufImplicitUnit = $10;

  ufWeakPackageUnit = ufPackageUnit or ufWeakUnit;

{ Procedure type of the callback given to GetPackageInfo.  Name is the actual
  name of the package element.  If IsUnit is True then Name is the name of
  a contained unit; a required package if False.  Param is the value passed
  to GetPackageInfo }

type
  TNameType = (ntContainsUnit, ntRequiresPackage);

  TPackageInfoProc = procedure (const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);

{ LoadPackage loads a given package DLL, checks for duplicate units and
  calls the initialization blocks of all the contained units }

function LoadPackage(const Name: string): HMODULE;

{ UnloadPackage does the opposite of LoadPackage by calling the finalization
  blocks of all contained units, then unloading the package DLL }

procedure UnloadPackage(Module: HMODULE);

{ GetPackageInfo accesses the given package's info table and enumerates
  all the contained units and required packages }

procedure GetPackageInfo(Module: HMODULE; Param: Pointer; var Flags: Integer;
  InfoProc: TPackageInfoProc);

{ GetPackageDescription loads the description resource from the package
  library. If the description resource does not exist,
  an empty string is returned. }
function GetPackageDescription(ModuleName: PChar): string;

{ InitializePackage Validates and initializes the given package DLL }

procedure InitializePackage(Module: HMODULE);

{ FinalizePackage finalizes the given package DLL }

procedure FinalizePackage(Module: HMODULE);

{ RaiseLastWin32Error calls the GetLastError API to retrieve the code for }
{ the last occuring Win32 error.  If GetLastError returns an error code,  }
{ RaiseLastWin32Error then raises an exception with the error code and    }
{ message associated with with error. }

procedure RaiseLastWin32Error;

{ Win32Check is used to check the return value of a Win32 API function     }
{ which returns a BOOL to indicate success.  If the Win32 API function     }
{ returns False (indicating failure), Win32Check calls RaiseLastWin32Error }
{ to raise an exception.  If the Win32 API function returns True,          }
{ Win32Check returns True. }

function Win32Check(RetVal: BOOL): BOOL;

{ Termination procedure support }

type
  TTerminateProc = function: Boolean;

{ Call AddTerminateProc to add a terminate procedure to the system list of }
{ termination procedures.  Delphi will call all of the function in the     }
{ termination procedure list before an application terminates.  The user-  }
{ defined TermProc function should return True if the application can      }
{ safely terminate or False if the application cannot safely terminate.    }
{ If one of the functions in the termination procedure list returns False, }
{ the application will not terminate. }

procedure AddTerminateProc(TermProc: TTerminateProc);

{ CallTerminateProcs is called by VCL when an application is about to }
{ terminate.  It returns True only if all of the functions in the     }
{ system's terminate procedure list return True.  This function is    }
{ intended only to be called by Delphi, and it should not be called   }
{ directly. }

function CallTerminateProcs: Boolean;

function GDAL: LongWord;
procedure RCS;
procedure RPR;


{ HexDisplayPrefix contains the prefix to display on hexadecimal
  values - '$' for Pascal syntax, '0x' for C++ syntax.  This is
  for display only - this does not affect the string-to-integer
  conversion routines. }
var
  HexDisplayPrefix: string = '$';

{ The GetDiskFreeSpace Win32 API does not support partitions larger than 2GB
  under Win95.  A new Win32 function, GetDiskFreeSpaceEx, supports partitions
  larger than 2GB but only exists on Win NT 4.0 and Win95 OSR2.
  The GetDiskFreeSpaceEx function pointer variable below will be initialized
  at startup to point to either the actual OS API function if it exists on
  the system, or to an internal Delphi function if it does not.  When running
  on Win95 pre-OSR2, the output of this function will still be limited to
  the 2GB range reported by Win95, but at least you don't have to worry
  about which API function to call in code you write.  }

var
  GetDiskFreeSpaceEx: function (Directory: PChar; var FreeAvailable,
    TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool stdcall = nil;

{ SafeLoadLibrary calls LoadLibrary, disabling normal Win32 error message
  popup dialogs if the requested file can't be loaded.  SafeLoadLibrary also
  preserves the current FPU control word (precision, exception masks) across
  the LoadLibrary call (in case the DLL you're loading hammers the FPU control
  word in its initialization, as many MS DLLs do)}

function SafeLoadLibrary(const Filename: string;
  ErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE;

{ Thread synchronization }

{ TMultiReadExclusiveWriteSynchronizer minimizes thread serialization to gain
  read access to a resource shared among threads while still providing complete
  exclusivity to callers needing write access to the shared resource.
  (multithread shared reads, single thread exclusive write)
  Reading is allowed while owning a write lock.
  Read locks can be promoted to write locks.}

type
  TActiveThreadRecord = record
    ThreadID: Integer;
    RecursionCount: Integer;
  end;
  TActiveThreadArray = array of TActiveThreadRecord;

  TMultiReadExclusiveWriteSynchronizer = class
  private
    FLock: TRTLCriticalSection;
    FReadExit: THandle;
    FCount: Integer;
    FSaveReadCount: Integer;
    FActiveThreads: TActiveThreadArray;
    FWriteRequestorID: Integer;
    FReallocFlag: Integer;
    FWriting: Boolean;
    function WriterIsOnlyReader: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
  end;

implementation

{ Utility routines }

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

procedure ConvertError(const Ident: string);
begin
  raise EConvertError.Create(Ident);
end;
//!!!
procedure ConvertErrorFmt(const ResString: string; const Args: array of const);
begin
  raise EConvertError.CreateFmt(ResString, Args);
end;

{ Memory management routines }

function AllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

{ Exit procedure handling }

type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = record
    Next: PExitProcInfo;
    SaveExit: Pointer;
    Proc: TProcedure;
  end;

const
  ExitProcList: PExitProcInfo = nil;

procedure DoExitProc;
var
  P: PExitProcInfo;
  Proc: TProcedure;
begin
  P := ExitProcList;
  ExitProcList := P^.Next;
  ExitProc := P^.SaveExit;
  Proc := P^.Proc;
  Dispose(P);
  Proc;
end;

procedure AddExitProc(Proc: TProcedure);
var
  P: PExitProcInfo;
begin
  New(P);
  P^.Next := ExitProcList;
  P^.SaveExit := ExitProc;
  P^.Proc := Proc;
  ExitProcList := P;
  ExitProc := @DoExitProc;
end;

{ String handling routines }

function NewStr(const S: string): PString;
begin
  if S = '' then Result := NullStr else
  begin
    New(Result);
    Result^ := S;
  end;
end;

procedure DisposeStr(P: PString);
begin
  if (P <> nil) and (P^ <> '') then Dispose(P);
end;

procedure AssignStr(var P: PString; const S: string);
var
  Temp: PString;
begin
  Temp := P;
  P := NewStr(S);
  DisposeStr(Temp);
end;

procedure AppendStr(var Dest: string; const S: string);
begin
  Dest := Dest + S;
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function CompareStr(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    CMP     ECX,ECX
        REPE    CMPSB
        JE      @@4
        MOVZX   EAX,BYTE PTR [ESI-1]
        MOVZX   EDX,BYTE PTR [EDI-1]
@@4:    SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

function CompareText(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@0
        MOV     EAX,[EAX-4]
@@0:    OR      EDX,EDX
        JE      @@1
        MOV     EDX,[EDX-4]
@@1:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@2
        MOV     ECX,EDX
@@2:    CMP     ECX,ECX
@@3:    REPE    CMPSB
        JE      @@6
        MOV     BL,BYTE PTR [ESI-1]
        CMP     BL,'a'
        JB      @@4
        CMP     BL,'z'
        JA      @@4
        SUB     BL,20H
@@4:    MOV     BH,BYTE PTR [EDI-1]
        CMP     BH,'a'
        JB      @@5
        CMP     BH,'z'
        JA      @@5
        SUB     BH,20H
@@5:    CMP     BL,BH
        JE      @@3
        MOVZX   EAX,BL
        MOVZX   EDX,BH
@@6:    SUB     EAX,EDX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function SameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
        CALL    CompareText
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;

function AnsiUpperCase(const S: string): string;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(S), Len);
  if Len > 0 then CharUpperBuff(Pointer(Result), Len);
end;

function AnsiLowerCase(const S: string): string;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(S), Len);
  if Len > 0 then CharLowerBuff(Pointer(Result), Len);
end;

function AnsiCompareStr(const S1, S2: string): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
end;

function AnsiSameStr(const S1, S2: string): Boolean;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) = 2;
end;

function AnsiCompareText(const S1, S2: string): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
end;

function AnsiSameText(const S1, S2: string): Boolean;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) = 2;
end;

function AnsiStrComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0, S1, -1, S2, -1) - 2;
end;

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, 0,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrLower(Str: PChar): PChar;
begin
  CharLower(Str);
  Result := Str;
end;

function AnsiStrUpper(Str: PChar): PChar;
begin
  CharUpper(Str);
  Result := Str;
end;

function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function QuotedStr(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '''' then Insert('''', Result, I);
  Result := '''' + Result + '''';
end;

function AnsiQuotedStr(const S: string; Quote: Char): string;
var
  P, Src, Dest: PChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := AnsiStrScan(PChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := AnsiStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := AnsiStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := AnsiStrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
var
  P, Dest: PChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := AnsiStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := AnsiStrScan(Src, Quote);
  end;
  if Src = nil then Src := StrEnd(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PChar(Result);
    Src := AnsiStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := AnsiStrScan(Src, Quote);
    end;
    if Src = nil then Src := StrEnd(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function AdjustLineBreaks(const S: string): string;
var
  Source, SourceEnd, Dest: PChar;
  Extra: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  Extra := 0;
  while Source < SourceEnd do
  begin
    case Source^ of
      #10:
        Inc(Extra);
      #13:
        if Source[1] = #10 then Inc(Source) else Inc(Extra);
    else
      if Source^ in LeadBytes then
        Inc(Source)
    end;
    Inc(Source);
  end;
  if Extra = 0 then Result := S else
  begin
    Source := Pointer(S);
    SetString(Result, nil, SourceEnd - Source + Extra);
    Dest := Pointer(Result);
    while Source < SourceEnd do
      case Source^ of
        #10:
          begin
            Dest^ := #13;
            Inc(Dest);
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
          end;
        #13:
          begin
            Dest^ := #13;
            Inc(Dest);
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
            if Source^ = #10 then Inc(Source);
          end;
      else
        if Source^ in LeadBytes then
        begin
          Dest^ := Source^;
          Inc(Dest);
          Inc(Source);
        end;
        Dest^ := Source^;
        Inc(Dest);
        Inc(Source);
      end;
  end;
end;

function IsValidIdent(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

function IntToStr(Value: Integer): string;
begin
  FmtStr(Result, '%d', [Value]);
end;

function IntToStr(Value: Int64): string;
begin
  FmtStr(Result, '%d', [Value]);
end;

function IntToHex(Value: Integer; Digits: Integer): string;
begin
  FmtStr(Result, '%.*x', [Digits, Value]);
end;

function IntToHex(Value: Int64; Digits: Integer): string;
begin
  FmtStr(Result, '%.*x', [Digits, Value]);
end;

function StrToInt(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then ConvertErrorFmt(SInvalidInteger, [S]);
end;

function StrToInt64(const S: string): Int64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then ConvertErrorFmt(SInvalidInteger, [S]);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function StrToInt64Def(const S: string; Default: Int64): Int64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Buffer: PChar;
    BufSize: Integer;
    nChars: Integer;
  end;

function EnumStringModules(Instance: Longint; Data: Pointer): Boolean;
begin
  with PStrData(Data)^ do
  begin
    nChars := LoadString(Instance, Ident, Buffer, BufSize);
    Result := nChars = 0;
  end;
end;

function FindStringResource(Ident: Integer; Buffer: PChar; BufSize: Integer): Integer;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Buffer := Buffer;
  StrData.BufSize := BufSize;
  StrData.nChars := 0;
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.nChars;
end;

function LoadStr(Ident: Integer): string;
var
  Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, FindStringResource(Ident, Buffer, SizeOf(Buffer)));
end;

function FmtLoadStr(Ident: Integer; const Args: array of const): string;
begin
  FmtStr(Result, LoadStr(Ident), Args);
end;

{ File management routines }

function FileOpen(const FileName: string; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(CreateFile(PChar(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0));
end;

function FileCreate(const FileName: string): Integer;
begin
  Result := Integer(CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;
begin
  if not ReadFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
end;

function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;
begin
  if not WriteFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
end;

function FileSeek(Handle, Offset, Origin: Integer): Integer;
begin
  Result := SetFilePointer(THandle(Handle), Offset, nil, Origin);
end;

function FileSeek(Handle: Integer; const Offset: Int64; Origin: Integer): Int64;
begin
  Result := Offset;
  Int64Rec(Result).Lo := SetFilePointer(THandle(Handle), Int64Rec(Result).Lo,
    @Int64Rec(Result).Hi, Origin);
end;

procedure FileClose(Handle: Integer);
begin
  CloseHandle(THandle(Handle));
end;

function FileAge(const FileName: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;

function FileExists(const FileName: string): Boolean;
begin
  Result := FileAge(FileName) <> -1;
end;

function FileGetDate(Handle: Integer): Integer;
var
  FileTime, LocalFileTime: TFileTime;
begin
  if GetFileTime(THandle(Handle), nil, nil, @FileTime) and
    FileTimeToLocalFileTime(FileTime, LocalFileTime) and
    FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
      LongRec(Result).Lo) then Exit;
  Result := -1;
end;

function FileSetDate(Handle: Integer; Age: Integer): Integer;
var
  LocalFileTime, FileTime: TFileTime;
begin
  Result := 0;
  if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
    LocalFileTimeToFileTime(LocalFileTime, FileTime) and
    SetFileTime(Handle, nil, nil, @FileTime) then Exit;
  Result := GetLastError;
end;

function FileGetAttr(const FileName: string): Integer;
begin
  Result := GetFileAttributes(PChar(FileName));
end;

function FileSetAttr(const FileName: string; Attr: Integer): Integer;
begin
  Result := 0;
  if not SetFileAttributes(PChar(FileName), Attr) then
    Result := GetLastError;
end;

function FindMatchingFile(var F: TSearchRec): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFile(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

function FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFile(PChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFile(F);
    if Result <> 0 then FindClose(F);
  end else
    Result := GetLastError;
end;

function FindNext(var F: TSearchRec): Integer;
begin
  if FindNextFile(F.FindHandle, F.FindData) then
    Result := FindMatchingFile(F) else
    Result := GetLastError;
end;

procedure FindClose(var F: TSearchRec);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function DeleteFile(const FileName: string): Boolean;
begin
  Result := Windows.DeleteFile(PChar(FileName));
end;

function RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := MoveFile(PChar(OldName), PChar(NewName));
end;

function AnsiStrLastChar(P: PChar): PChar;
var
  LastByte: Integer;
begin
  LastByte := StrLen(P) - 1;
  Result := @P[LastByte];
  if StrByteType(P, LastByte) = mbTrailByte then Dec(Result);
end;

function AnsiLastChar(const S: string): PChar;
var
  LastByte: Integer;
begin
  LastByte := Length(S);
  if LastByte <> 0 then
  begin
    Result := @S[LastByte];
    if ByteType(S, LastByte) = mbTrailByte then Dec(Result);
  end
  else
    Result := nil;
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Dec(Result)
      else
        Exit;
    Dec(Result);
  end;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDir(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:',Filename);
  if (I > 1) and (FileName[I] = '\') and
    (not (FileName[I - 1] in ['\', ':']) or
    (ByteType(FileName, I-1) = mbTrailByte)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: string): string;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = ':') then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = '\') and
    (FileName[2] = '\') then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = '\' then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = '\' then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

function ExtractFileName(const FileName: string): string;
begin
  Result:=kol.ExtractFileName(FileName);
end;

function ExtractFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function ExpandFileName(const FileName: string): string;
var
  FName: PChar;
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer, GetFullPathName(PChar(FileName), SizeOf(Buffer),
    Buffer, FName));
end;

function GetUniversalName(const FileName: string): string;
type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..MaxInt div SizeOf(TNetResource) - 1] of TNetResource;
var
  I, BufSize, NetResult: Integer;
  Count, Size: LongWord;
  Drive: Char;
  NetHandle: THandle;
  NetResources: PNetResourceArray;
  RemoteNameInfo: array[0..1023] of Byte;
begin
  Result := FileName;
  if (Win32Platform <> VER_PLATFORM_WIN32_WINDOWS) or (Win32MajorVersion > 4) then
  begin
    Size := SizeOf(RemoteNameInfo);
    if WNetGetUniversalName(PChar(FileName), UNIVERSAL_NAME_INFO_LEVEL,
      @RemoteNameInfo, Size) <> NO_ERROR then Exit;
    Result := PRemoteNameInfo(@RemoteNameInfo).lpUniversalName;
  end else
  begin
  { The following works around a bug in WNetGetUniversalName under Windows 95 }
    Drive := UpCase(FileName[1]);
    if (Drive < 'A') or (Drive > 'Z') or (Length(FileName) < 3) or
      (FileName[2] <> ':') or (FileName[3] <> '\') then
      Exit;
    if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil,
      NetHandle) <> NO_ERROR then Exit;
    try
      BufSize := 50 * SizeOf(TNetResource);
      GetMem(NetResources, BufSize);
      try
        while True do
        begin
          Count := $FFFFFFFF;
          Size := BufSize;
          NetResult := WNetEnumResource(NetHandle, Count, NetResources, Size);
          if NetResult = ERROR_MORE_DATA then
          begin
            BufSize := Size;
            ReallocMem(NetResources, BufSize);
            Continue;
          end;
          if NetResult <> NO_ERROR then Exit;
          for I := 0 to Count - 1 do
            with NetResources^[I] do
              if (lpLocalName <> nil) and (Drive = UpCase(lpLocalName[0])) then
              begin
                Result := lpRemoteName + Copy(FileName, 3, Length(FileName) - 2);
                Exit;
              end;
        end;
      finally
        FreeMem(NetResources, BufSize);
      end;
    finally
      WNetCloseEnum(NetHandle);
    end;
  end;
end;

function ExpandUNCFileName(const FileName: string): string;
begin
  { First get the local resource version of the file name }
  Result := ExpandFileName(FileName);
  if (Length(Result) >= 3) and (Result[2] = ':') and (Upcase(Result[1]) >= 'A')
    and (Upcase(Result[1]) <= 'Z') then
    Result := GetUniversalName(Result);
end;

function ExtractRelativePath(const BaseName, DestName: string): string;
var
  BasePath, DestPath: string;
  BaseDirs, DestDirs: array[0..129] of PChar;
  BaseDirCount, DestDirCount: Integer;
  I, J: Integer;

  function ExtractFilePathNoDrive(const FileName: string): string;
  begin
    Result := ExtractFilePath(FileName);
    Result := Copy(Result, Length(ExtractFileDrive(FileName)) + 1, 32767);
  end;

  procedure SplitDirs(var Path: string; var Dirs: array of PChar;
    var DirCount: Integer);
  var
    I, J: Integer;
  begin
    I := 1;
    J := 0;
    while I <= Length(Path) do
    begin
      if Path[I] in LeadBytes then Inc(I)
      else if Path[I] = '\' then             { Do not localize }
      begin
        Path[I] := #0;
        Dirs[J] := @Path[I + 1];
        Inc(J);
      end;
      Inc(I);
    end;
    DirCount := J - 1;
  end;

begin
  if AnsiCompareText(ExtractFileDrive(BaseName), ExtractFileDrive(DestName)) = 0 then
  begin
    BasePath := ExtractFilePathNoDrive(BaseName);
    DestPath := ExtractFilePathNoDrive(DestName);
    SplitDirs(BasePath, BaseDirs, BaseDirCount);
    SplitDirs(DestPath, DestDirs, DestDirCount);
    I := 0;
    while (I < BaseDirCount) and (I < DestDirCount) do
    begin
      if AnsiStrIComp(BaseDirs[I], DestDirs[I]) = 0 then
        Inc(I)
      else Break;
    end;
    Result := '';
    for J := I to BaseDirCount - 1 do
      Result := Result + '..\';              { Do not localize }
    for J := I to DestDirCount - 1 do
      Result := Result + DestDirs[J] + '\';  { Do not localize }
    Result := Result + ExtractFileName(DestName);
  end else Result := DestName;
end;

function ExtractShortPathName(const FileName: string): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer,
    GetShortPathName(PChar(FileName), Buffer, SizeOf(Buffer)));
end;

function FileSearch(const Name, DirList: string): string;
var
  I, P, L: Integer;
begin
  Result := Name;
  P := 1;
  L := Length(DirList);
  while True do
  begin
    if FileExists(Result) then Exit;
    while (P <= L) and (DirList[P] = ';') do Inc(P);
    if P > L then Break;
    I := P;
    while (P <= L) and (DirList[P] <> ';') do
    begin
      if DirList[P] in LeadBytes then Inc(P);
      Inc(P);
    end;
    Result := Copy(DirList, I, P - I);
    if not (AnsiLastChar(Result)^ in [':', '\']) then Result := Result + '\';
    Result := Result + Name;
  end;
  Result := '';
end;

// This function is used if the OS doesn't support GetDiskFreeSpaceEx
function BackfillGetDiskFreeSpaceEx(Directory: PChar; var FreeAvailable,
    TotalSpace: TLargeInteger; TotalFree: PLargeInteger): Bool; stdcall;
var
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: LongWord;
  Temp: Int64;
  Dir: PChar;
begin
  if Directory <> nil then
    Dir := Directory
  else
    Dir := nil;
  Result := GetDiskFreeSpaceA(Dir, SectorsPerCluster, BytesPerSector,
    FreeClusters, TotalClusters);
  Temp := SectorsPerCluster * BytesPerSector;
  FreeAvailable := Temp * FreeClusters;
  TotalSpace := Temp * TotalClusters;
end;

function InternalGetDiskSpace(Drive: Byte;
  var TotalSpace, FreeSpaceAvailable: Int64): Bool;
var
  RootPath: array[0..4] of Char;
  RootPtr: PChar;
begin
  RootPtr := nil;
  if Drive > 0 then
  begin
    RootPath[0] := Char(Drive + $40);
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
  end;
  Result := GetDiskFreeSpaceEx(RootPtr, FreeSpaceAvailable, TotalSpace, nil);
end;

function DiskFree(Drive: Byte): Int64;
var
  TotalSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, TotalSpace, Result) then
    Result := -1;
end;

function DiskSize(Drive: Byte): Int64;
var
  FreeSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, Result, FreeSpace) then
    Result := -1;
end;

function FileDateToDateTime(FileDate: Integer): TDateTime;
begin
  Result :=
    EncodeDate(
      LongRec(FileDate).Hi shr 9 + 1980,
      LongRec(FileDate).Hi shr 5 and 15,
      LongRec(FileDate).Hi and 31) +
    EncodeTime(
      LongRec(FileDate).Lo shr 11,
      LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0);
end;

function DateTimeToFileDate(DateTime: TDateTime): Integer;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2099) then Result := 0 else
  begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(Result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;

function GetCurrentDir: string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  SetString(Result, Buffer, GetCurrentDirectory(SizeOf(Buffer), Buffer));
end;

function SetCurrentDir(const Dir: string): Boolean;
begin
  Result := SetCurrentDirectory(PChar(Dir));
end;

function CreateDir(const Dir: string): Boolean;
begin
  Result := CreateDirectory(PChar(Dir), nil);
end;

function RemoveDir(const Dir: string): Boolean;
begin
  Result := RemoveDirectory(PChar(Dir));
end;

{ PChar routines }

function StrLen(const Str: PChar): Cardinal;
begin
  Result:=kol.StrLen(Str);
end;

function StrEnd(const Str: PChar): PChar; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        LEA     EAX,[EDI-1]
        MOV     EDI,EDX
end;

function StrMove(Dest: PChar; const Source: PChar; Count: Cardinal): PChar; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX
        MOV     EDI,EAX
        MOV     EDX,ECX
        CMP     EDI,ESI
        JA      @@1
        JE      @@2
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        JMP     @@2
@@1:    LEA     ESI,[ESI+ECX-1]
        LEA     EDI,[EDI+ECX-1]
        AND     ECX,3
        STD
        REP     MOVSB
        SUB     ESI,3
        SUB     EDI,3
        MOV     ECX,EDX
        SHR     ECX,2
        REP     MOVSD
        CLD
@@2:    POP     EDI
        POP     ESI
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
end;

function StrECopy(Dest: PChar; const Source: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        LEA     EAX,[EDI-1]
        POP     ESI
        POP     EDI
end;

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;
begin
  Result:=kol.StrLCopy(Dest, Source, MaxLen);
end;

function StrPCopy(Dest: PChar; const Source: string): PChar;
begin
  Result := StrLCopy(Dest, PChar(Source), Length(Source));
end;

function StrPLCopy(Dest: PChar; const Source: string;
  MaxLen: Cardinal): PChar;
begin
  Result := StrLCopy(Dest, PChar(Source), MaxLen);
end;

function StrCat(Dest: PChar; const Source: PChar): PChar;
begin
  StrCopy(StrEnd(Dest), Source);
  Result := Dest;
end;

function StrLCat(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,Dest
        MOV     ESI,Source
        MOV     EBX,MaxLen
        CALL    StrEnd
        MOV     ECX,EDI
        ADD     ECX,EBX
        SUB     ECX,EAX
        JBE     @@1
        MOV     EDX,ESI
        CALL    StrLCopy
@@1:    MOV     EAX,EDI
        POP     EBX
        POP     ESI
        POP     EDI
end;

function StrComp(const Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSB
        MOV     AL,[ESI-1]
        MOV     DL,[EDI-1]
        SUB     EAX,EDX
        POP     ESI
        POP     EDI
end;

function StrIComp(const Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,[ESI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,[EDI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     EAX,EDX
        JE      @@1
@@4:    POP     ESI
        POP     EDI
end;

function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@1
        REPNE   SCASB
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
        XOR     EDX,EDX
        REPE    CMPSB
        MOV     AL,[ESI-1]
        MOV     DL,[EDI-1]
        SUB     EAX,EDX
@@1:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrLIComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@4
        REPNE   SCASB
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
        XOR     EDX,EDX
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,[ESI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,[EDI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     EAX,EDX
        JE      @@1
@@4:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrScan(const Str: PChar; Chr: Char): PChar;
begin
  Result:=kol.StrScan(Str, Chr);
end;

function StrRScan(const Str: PChar; Chr: Char): PChar; assembler;
asm
        PUSH    EDI
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        STD
        DEC     EDI
        MOV     AL,Chr
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        INC     EAX
@@1:    CLD
        POP     EDI
end;

function StrPos(const Str1, Str2: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX
        JE      @@2
        OR      EDX,EDX
        JE      @@2
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     AL,AL
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        DEC     ECX
        JE      @@2
        MOV     ESI,ECX
        MOV     EDI,EBX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        SUB     ECX,ESI
        JBE     @@2
        MOV     EDI,EBX
        LEA     EBX,[ESI-1]
@@1:    MOV     ESI,EDX
        LODSB
        REPNE   SCASB
        JNE     @@2
        MOV     EAX,ECX
        PUSH    EDI
        MOV     ECX,EBX
        REPE    CMPSB
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrUpper(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'a'
        JB      @@1
        CMP     AL,'z'
        JA      @@1
        SUB     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;

function StrLower(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'A'
        JB      @@1
        CMP     AL,'Z'
        JA      @@1
        ADD     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end;

function StrAlloc(Size: Cardinal): PChar;
begin
  Inc(Size, SizeOf(Cardinal));
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(Cardinal));
end;

function StrBufSize(const Str: PChar): Cardinal;
var
  P: PChar;
begin
  P := Str;
  Dec(P, SizeOf(Cardinal));
  Result := Cardinal(Pointer(P)^) - SizeOf(Cardinal);
end;

function StrNew(const Str: PChar): PChar;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := StrLen(Str) + 1;
    Result := StrMove(StrAlloc(Size), Str, Size);
  end;
end;

procedure StrDispose(Str: PChar);
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

{ String formatting routines }

procedure FormatError(ErrorCode: Integer; Format: PChar; FmtLen: Cardinal);
const
  FormatErrorStrs: array[0..1] of string = (
    SInvalidFormat, SArgumentMissing);
var
  Buffer: array[0..31] of Char;
begin
  if FmtLen > 31 then FmtLen := 31;
  if StrByteType(Format, FmtLen-1) = mbLeadByte then Dec(FmtLen);
  StrMove(Buffer, Format, FmtLen);
  Buffer[FmtLen] := #0;
  ConvertErrorFmt(FormatErrorStrs[ErrorCode], [PChar(@Buffer)]);
end;

procedure FormatVarToStr(var S: string; const V: Variant);
begin
  S := V;
end;

procedure FormatClearStr(var S: string);
begin
  S := '';
end;
{
function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
var
    ElsArray, El: PDWORD;
    I : Integer;
    P : PDWORD;
begin
  ElsArray := nil;
  if High( Args ) >= 0 then
    GetMem( ElsArray, (High( Args ) + 1) * sizeof( Pointer ) );
  El := ElsArray;
  for I := 0 to High( Args ) do
  begin
    P := @Args[ I ];
    P := Pointer( P^ );
    El^ := DWORD( P );
    Inc( El );
  end;
  Result:=wvsprintf( @Buffer, PChar( @Format ), PChar( ElsArray ) );
  if ElsArray <> nil then
     FreeMem( ElsArray );
end;
}
function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
const
  C10000: Single = 10000;
var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr, TempStr: PChar;
  JustFlag: Byte;
  StrBuf: array[0..64] of Char;
  TempAnsiStr: string;
  TempInt64 : int64;
asm
				{ in: eax <-> Buffer }
				{ in: edx <-> BufLen }
				{ in: ecx <-> Format }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,ECX
        ADD     ECX,FmtLen
        MOV     BufferOrg,EDI
        XOR     EAX,EAX
        MOV     ArgIndex,EAX
        MOV     TempStr,EAX
        MOV     TempAnsiStr,EAX

@Loop:
        OR      EDX,EDX
        JE      @Done

@NextChar:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @Format

@StoreChar:
        STOSB
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX,EDI
        SUB     EAX,BufferOrg
        JMP     @Exit

@Format:
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CMP     AL,'%'
        JE      @StoreChar
        LEA     EBX,[ESI-2]
        MOV     FormatOrg,EBX
@A0:    MOV     JustFlag,AL
        CMP     AL,'-'
        JNE     @A1
        CMP     ESI,ECX
        JE      @Done
        LODSB
@A1:    CALL    @Specifier
        CMP     AL,':'
        JNE     @A2
        MOV     ArgIndex,EBX
        CMP     ESI,ECX
        JE      @Done
        LODSB
        JMP     @A0

@A2:    MOV     Width,EBX
        MOV     EBX,-1
        CMP     AL,'.'
        JNE     @A3
        CMP     ESI,ECX
        JE      @Done
        LODSB
        CALL    @Specifier
@A3:    MOV     Prec,EBX
        MOV     FormatPtr,ESI
        PUSH    ECX
        PUSH    EDX

        CALL    @Convert

        POP     EDX
        MOV     EBX,Width
        SUB     EBX,ECX                   (* ECX <=> number of characters output *)
        JAE     @A4                       (*         jump -> output smaller than width *)
        XOR     EBX,EBX

@A4:    CMP     JustFlag,'-'
        JNE     @A6
        SUB     EDX,ECX
        JAE     @A5
        ADD     ECX,EDX
        XOR     EDX,EDX

@A5:    REP     MOVSB

@A6:    XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A7
        ADD     ECX,EDX
        XOR     EDX,EDX
@A7:    MOV     AL,' '
        REP     STOSB
        XCHG    EBX,ECX
        SUB     EDX,ECX
        JAE     @A8
        ADD     ECX,EDX
        XOR     EDX,EDX
@A8:    REP     MOVSB
        CMP     TempStr,0
        JE      @A9
        PUSH    EDX
        LEA     EAX,TempStr
        CALL    FormatClearStr
        POP     EDX
@A9:    POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX,EBX
        CMP     AL,'*'
        JE      @B3
@B1:    CMP     AL,'0'
        JB      @B5
        CMP     AL,'9'
        JA      @B5
        IMUL    EBX,EBX,10
        SUB     AL,'0'
        MOVZX   EAX,AL
        ADD     EBX,EAX
        CMP     ESI,ECX
        JE      @B2
        LODSB
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX,ArgIndex
        CMP     EAX,Args.Integer[-4]
        JA      @B4
        INC     ArgIndex
        MOV     EBX,Args
        CMP     [EBX+EAX*8].Byte[4],vtInteger
        MOV     EBX,[EBX+EAX*8]
        JE      @B4
        XOR     EBX,EBX
@B4:    CMP     ESI,ECX
        JE      @B2
        LODSB
@B5:    RET

@Convert:
        AND     AL,0DFH
        MOV     CL,AL
        MOV     EAX,1
        MOV     EBX,ArgIndex
        CMP     EBX,Args.Integer[-4]
        JA      @ErrorExit
        INC     ArgIndex
        MOV     ESI,Args
        LEA     ESI,[ESI+EBX*8]
        MOV     EAX,[ESI].Integer[0]       // TVarRec.data
        MOVZX   EBX,[ESI].Byte[4]          // TVarRec.VType
        JMP     @CvtVector.Pointer[EBX*4]

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtWideChar:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
        CALL    @ClearTmpAnsiStr
        MOV     EDX,FormatOrg
        MOV     ECX,FormatPtr
        SUB     ECX,EDX
        CALL    FormatError
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     EBX, TempInt64       // (input is array of const; save original)
        MOV     EDX, [EAX]
        MOV     [EBX], EDX
        MOV     EDX, [EAX + 4]
        MOV     [EBX + 4], EDX

        // EBX <= address of TempInt64

        CMP     CL,'D'
        JE      @DecI64
        CMP     CL,'U'
        JE      @DecI64_2
        CMP     CL,'X'
        JNE     @CvtError

@HexI64:
        MOV     ECX,16               // hex divisor
        JMP     @CvtI64

@DecI64:
        TEST    DWORD PTR [EBX + 4], $80000000      // sign bit set?
        JE      @DecI64_2            //   no -> bypass '-' output

        NEG     DWORD PTR [EBX]      // negate lo-order, then hi-order
        ADC     DWORD PTR [EBX+4], 0
        NEG     DWORD PTR [EBX+4]

        CALL    @DecI64_2

        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET

@DecI64_2:                           // unsigned int64 output
        MOV     ECX,10               // decimal divisor

@CvtI64:
        LEA     ESI,StrBuf[32]

@CvtI64_1:
        PUSH    ECX                  // save radix
        PUSH    0
        PUSH    ECX                  // radix divisor (10 or 16 only)
        MOV     EAX, [EBX]
        MOV     EDX, [EBX + 4]
        CALL    System.@_llumod
        POP     ECX                  // saved radix

        XCHG    EAX, EDX             // lo-value to EDX for character output
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @CvtI64_2

        ADD     DL,'A'-'0'-10

@CvtI64_2:
        DEC     ESI
        MOV     [ESI],DL

        PUSH    ECX                  // save radix
        PUSH    0
        PUSH    ECX                  // radix divisor (10 or 16 only)
        MOV     EAX, [EBX]           // value := value DIV radix
        MOV     EDX, [EBX + 4]
        CALL    System.@_lludiv
        POP     ECX                  // saved radix
        MOV     [EBX], EAX
        MOV     [EBX + 4], EDX
        OR      EAX,EDX              // anything left to output?
        JNE     @CvtI64_1            //   no jump => EDX:EAX = 0

        LEA     ECX,StrBuf[32]
        SUB     ECX,ESI
        MOV     EDX,Prec
        CMP     EDX,16
        JBE     @CvtI64_3
        RET

@CvtI64_3:
        SUB     EDX,ECX
        JBE     @CvtI64_5
        ADD     ECX,EDX
        MOV     AL,'0'

@CvtI64_4:
        DEC     ESI
        MOV     [ESI],AL
        DEC     EDX
        JNE     @CvtI64_4

@CvtI64_5:
        RET
////////////////////////////////////////////////

@CvtInteger:
        CMP     CL,'D'
        JE      @C1
        CMP     CL,'U'
        JE      @C2
        CMP     CL,'X'
        JNE     @CvtError
        MOV     ECX,16
        JMP     @CvtLong
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLong:
        LEA     ESI,StrBuf[16]
@D1:    XOR     EDX,EDX
        DIV     ECX
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,'A'-'0'-10
@D2:    DEC     ESI
        MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        LEA     ECX,StrBuf[16]
        SUB     ECX,ESI
        MOV     EDX,Prec
        CMP     EDX,16
        JBE     @D3
        RET
@D3:    SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
@D4:    DEC     ESI
        MOV     [ESI],AL
        DEC     EDX
        JNE     @D4
@D5:    RET

@CvtChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ECX,1
        RET

@CvtVariant:
        CMP     CL,'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType,varNull
        JBE     @CvtEmptyStr
        MOV     EDX,EAX
        LEA     EAX,TempStr
        CALL    FormatVarToStr
        MOV     ESI,TempStr
        JMP     @CvtStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        LODSB
        MOVZX   ECX,AL
        JMP     @CvtStrLen

@CvtPWideChar:
        MOV    ESI,OFFSET System.@LStrFromPWChar
        JMP    @CvtWideThing

@CvtWideString:
        MOV    ESI,OFFSET System.@LStrFromWStr

@CvtWideThing:
        CMP    CL,'S'
        JNE    @CvtError
        MOV    EDX,EAX
        LEA    EAX,TempAnsiStr
        CALL   ESI
        MOV    ESI,TempAnsiStr
        MOV    EAX,ESI
        JMP    @CvtStrRef

@CvtAnsiStr:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX

@CvtStrRef:
        OR      ESI,ESI
        JE      @CvtEmptyStr
        MOV     ECX,[ESI-4]

@CvtStrLen:
        CMP     ECX,Prec
        JA      @E1
        RET
@E1:    MOV     ECX,Prec
        RET

@CvtPChar:
        CMP     CL,'S'
        JNE     @CvtError
        MOV     ESI,EAX
        PUSH    EDI
        MOV     EDI,EAX
        XOR     AL,AL
        MOV     ECX,Prec
        JECXZ   @F1
        REPNE   SCASB
        JNE     @F1
        DEC     EDI
@F1:    MOV     ECX,EDI
        SUB     ECX,ESI
        POP     EDI
        RET

@CvtPointer:
        CMP     CL,'P'
        JNE     @CvtError
        MOV     Prec,8
        MOV     ECX,16
        JMP     @CvtLong

@CvtCurrency:
        MOV     BH,fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH,fvExtended

@CvtFloat:
        MOV     ESI,EAX
        MOV     BL,ffGeneral
        CMP     CL,'G'
        JE      @G2
        MOV     BL,ffExponent
        CMP     CL,'E'
        JE      @G2
        MOV     BL,ffFixed
        CMP     CL,'F'
        JE      @G1
        MOV     BL,ffNumber
        CMP     CL,'N'
        JE      @G1
        CMP     CL,'M'
        JNE     @CvtError
        MOV     BL,ffCurrency
@G1:    MOV     EAX,18
        MOV     EDX,Prec
        CMP     EDX,EAX
        JBE     @G3
        MOV     EDX,2
        CMP     CL,'M'
        JNE     @G3
        MOVZX   EDX,CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX,Prec
        MOV     EDX,3
        CMP     EAX,18
        JBE     @G3
        MOV     EAX,15
@G3:    PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,StrBuf
        MOV     EDX,ESI
        MOVZX   ECX,BH
        CALL    FloatToText
        MOV     ECX,EAX
        LEA     ESI,StrBuf
        RET

@ClearTmpAnsiStr:
        PUSH    EAX
        LEA     EAX,TempAnsiStr
        CALL    System.@LStrClr
        POP     EAX
        RET

@Exit:
        CALL    @ClearTmpAnsiStr
        POP     EDI
        POP     ESI
        POP     EBX
end;

function StrFmt(Buffer, Format: PChar; const Args: array of const): PChar;
begin
  Buffer[FormatBuf(Buffer^, MaxInt, Format^, StrLen(Format), Args)] := #0;
  Result := Buffer;
end;

function StrLFmt(Buffer: PChar; MaxLen: Cardinal; Format: PChar;
  const Args: array of const): PChar;
begin
  Buffer[FormatBuf(Buffer^, MaxLen, Format^, StrLen(Format), Args)] := #0;
  Result := Buffer;
end;

function Format(const Format: string; const Args: array of const): string;
begin
  FmtStr(Result, Format, Args);
end;

procedure FmtStr(var Result: string; const Format: string;
  const Args: array of const);
var
  Len, BufLen: Integer;
  Buffer: array[0..4097] of Char;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (BufLen - (BufLen div 4)) then
    Len := FormatBuf(Buffer, BufLen - 1, Pointer(Format)^, Length(Format), Args)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := FormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

{ Floating point conversion routines }

{$L FFMT.OBJ}

procedure FloatToDecimal(var Result: TFloatRec; const Value;
  ValueType: TFloatValue; Precision, Decimals: Integer); external;

function FloatToText(Buffer: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer): Integer; external;

function FloatToTextFmt(Buffer: PChar; const Value; ValueType: TFloatValue;
  Format: PChar): Integer; external;

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue): Boolean; external;

function FloatToStr(Value: Extended): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0));
end;

function CurrToStr(Value: Currency): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    ffGeneral, 0, 0));
end;

function FloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    Format, Precision, Digits));
end;

function CurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvCurrency,
    Format, 0, Digits));
end;

function FormatFloat(const Format: string; Value: Extended): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvExtended,
    PChar(Format)));
end;

function FormatCurr(const Format: string; Value: Currency): string;
var
  Buffer: array[0..255] of Char;
begin
  if Length(Format) > SizeOf(Buffer) - 32 then ConvertError(SFormatTooLong);
  SetString(Result, Buffer, FloatToTextFmt(Buffer, Value, fvCurrency,
    PChar(Format)));
end;

function StrToFloat(const S: string): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    ConvertErrorFmt(SInvalidFloat, [S]);
end;

function StrToCurr(const S: string): Currency;
begin
  if not TextToFloat(PChar(S), Result, fvCurrency) then
    ConvertErrorFmt(SInvalidFloat, [S]);
end;

{ Date/time support routines }

const
  FMSecsPerDay: Single = MSecsPerDay;
  IMSecsPerDay: Integer = MSecsPerDay;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
asm
        MOV     ECX,EAX
        FLD     DateTime
        FMUL    FMSecsPerDay
        SUB     ESP,8
        FISTP   QWORD PTR [ESP]
        FWAIT
        POP     EAX
        POP     EDX
        OR      EDX,EDX
        JNS     @@1
        NEG     EDX
        NEG     EAX
        SBB     EDX,0
        DIV     IMSecsPerDay
        NEG     EAX
        JMP     @@2
@@1:    DIV     IMSecsPerDay
@@2:    ADD     EAX,DateDelta
        MOV     [ECX].TTimeStamp.Time,EDX
        MOV     [ECX].TTimeStamp.Date,EAX
end;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
asm
        MOV     ECX,[EAX].TTimeStamp.Time
        MOV     EAX,[EAX].TTimeStamp.Date
        SUB     EAX,DateDelta
        IMUL    IMSecsPerDay
        OR      EDX,EDX
        JNS     @@1
        SUB     EAX,ECX
        SBB     EDX,0
        JMP     @@2
@@1:    ADD     EAX,ECX
        ADC     EDX,0
@@2:    PUSH    EDX
        PUSH    EAX
        FILD    QWORD PTR [ESP]
        FDIV    FMSecsPerDay
        ADD     ESP,8
end;

function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
asm
        MOV     ECX,EAX
        MOV     EAX,MSecs.Integer[0]
        MOV     EDX,MSecs.Integer[4]
        DIV     IMSecsPerDay
        MOV     [ECX].TTimeStamp.Time,EDX
        MOV     [ECX].TTimeStamp.Date,EAX
end;

function TimeStampToMSecs(const TimeStamp: TTimeStamp): Comp;
asm
        FILD    [EAX].TTimeStamp.Date
        FMUL    FMSecsPerDay
        FIADD   [EAX].TTimeStamp.Time
end;

{ Time encoding and decoding }

function DoEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  if not DoEncodeTime(Hour, Min, Sec, MSec, Result) then
    ConvertError(STimeEncodeError);
end;

procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
var
  MinCount, MSecCount: Word;
begin
  DivMod(DateTimeToTimeStamp(Time).Time, 60000, MinCount, MSecCount);
  DivMod(MinCount, 60, Hour, Min);
  DivMod(MSecCount, 1000, Sec, MSec);
end;

{ Date encoding and decoding }

function IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  if not DoEncodeDate(Year, Month, Day, Result) then
    ConvertError(SDateEncodeError);
end;

procedure InternalDecodeDate(Date: TDateTime; var Year, Month, Day, DOW: Word);
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(Date).Date;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
  end else
  begin
    DOW := T mod 7;
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    DayTable := @MonthDays[IsLeapYear(Y)];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
var
  Dummy: Word;
begin
  InternalDecodeDate(Date, Year, Month, Day, Dummy);
end;

procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
begin
  with SystemTime do
  begin
    InternalDecodeDate(DateTime, wYear, wMonth, wDay, wDayOfWeek);
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
end;

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  with SystemTime do
  begin
    Result := EncodeDate(wYear, wMonth, wDay);
    if Result >= 0 then
      Result := Result + EncodeTime(wHour, wMinute, wSecond, wMilliSeconds)
    else
      Result := Result - EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
  end;
end;

function DayOfWeek(Date: TDateTime): Integer;
begin
  Result := DateTimeToTimeStamp(Date).Date mod 7 + 1;
end;

function Date: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do Result := EncodeDate(wYear, wMonth, wDay);
end;

function Time: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
end;

function Now: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

function IncMonth(const Date: TDateTime; NumberOfMonths: Integer): TDateTime;
var
  DayTable: PDayTable;
  Year, Month, Day: Word;
  Sign: Integer;
begin
  if NumberOfMonths >= 0 then Sign := 1 else Sign := -1;
  DecodeDate(Date, Year, Month, Day);
  Year := Year + (NumberOfMonths div 12);
  NumberOfMonths := NumberOfMonths mod 12;
  Inc(Month, NumberOfMonths);
  if Word(Month-1) > 11 then    // if Month <= 0, word(Month-1) > 11)
  begin
    Inc(Year, Sign);
    Inc(Month, -12 * Sign);
  end;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if Day > DayTable^[Month] then Day := DayTable^[Month];
  Result := EncodeDate(Year, Month, Day);
  ReplaceTime(Result, Date);
end;

procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
begin
  DateTime := Trunc(DateTime);
  if DateTime >= 0 then
    DateTime := DateTime + system.Abs(Frac(NewTime))
  else
    DateTime := DateTime - system.Abs(Frac(NewTime));
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
var
  Temp: TDateTime;
begin
  Temp := NewDate;
  ReplaceTime(Temp, DateTime);
  DateTime := Temp;
end;

function CurrentYear: Word;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;

{ Date/time to string conversions }

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := SizeOf(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N);
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, SizeOf(NumBuf), Format,
      SizeOf(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        Inc(Format);
        if Starter in LeadBytes then
        begin
          if Format^ = #0 then Break;
          Inc(Format);
          LastToken := ' ';
          Continue;
        end;
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(ShortMonthNames[Month]);
              else
                AppendString(LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(ShortDayNames[DayOfWeek(DateTime)]);
                4: AppendString(LongDayNames[DayOfWeek(DateTime)]);
                5: AppendFormat(Pointer(ShortDateFormat));
              else
                AppendFormat(Pointer(LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if P^ in LeadBytes then
                begin
                  Inc(P);
                  if P^ = #0 then Break;
                  Inc(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(ShortTimeFormat)) else
                AppendFormat(Pointer(LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(TimeAMString) else
                  AppendString(TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(LongDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(ShortDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(LongTimeFormat));
              end;
            end;
          '/':
            AppendChars(@DateSeparator, 1);
          ':':
            AppendChars(@TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if Format^ in LeadBytes then
                begin
                  Inc(Format);
                  if Format^ = #0 then Break;
                end;
                Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  SetString(Result, Buffer, BufPos);
end;

function DateToStr(Date: TDateTime): string;
begin
  DateTimeToString(Result, ShortDateFormat, Date);
end;

function TimeToStr(Time: TDateTime): string;
begin
  DateTimeToString(Result, LongTimeFormat, Time);
end;

function DateTimeToStr(DateTime: TDateTime): string;
begin
  DateTimeToString(Result, '', DateTime);
end;

function FormatDateTime(const Format: string; DateTime: TDateTime): string;
begin
  DateTimeToString(Result, Format, DateTime);
end;

{ String to date/time conversions }

type
  TDateOrder = (doMDY, doDMY, doYMD);

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; var Pos: Integer;
  var Number: Word; var CharCount: Byte): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanString(const S: string; var Pos: Integer;
  const Symbol: string): Boolean;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ScanBlanks(S, Pos);
    if AnsiCompareText(Symbol, Copy(S, Pos, Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := doMDY;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
  Result := doMDY;
end;

procedure ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not (S[Pos] in ['0'..'9']) do
  begin
    if S[Pos] in LeadBytes then Inc(Pos);
    Inc(Pos);
  end;
end;

function GetEraYearOffset(const Name: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EraNames) to High(EraNames) do
  begin
    if EraNames[I] = '' then Break;
    if AnsiStrPos(PChar(EraNames[I]), PChar(Name)) <> nil then
    begin
      Result := EraYearOffsets[I];
      Exit;
    end;
  end;
end;

function ScanDate(const S: string; var Pos: Integer;
  var Date: TDateTime): Boolean;
var
  DateOrder: TDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  EraName : string;
  EraYearOffset: Integer;
  CenturyBase: Integer;

  function EraToYear(Year: Integer): Integer;
  begin
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
    Result := Year + EraYearOffset;
  end;

begin
  Y := 0;
  M := 0;
  D := 0;
  YearLen := 0;
  Result := False;
  DateOrder := GetDateOrder(ShortDateFormat);
  EraYearOffset := 0;
  if ShortDateFormat[1] = 'g' then  // skip over prefix text
  begin
    ScanToNumber(S, Pos);
    EraName := Trim(Copy(S, 1, Pos-1));
    EraYearOffset := GetEraYearOffset(EraName);
  end
  else
    if AnsiPos('e', ShortDateFormat) > 0 then
      EraYearOffset := EraYearOffsets[1];
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, DateSeparator) and
    ScanNumber(S, Pos, N2, L2)) then Exit;
  if ScanChar(S, Pos, DateSeparator) then
  begin
    if not ScanNumber(S, Pos, N3, L3) then Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      doYMD: begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else if (YearLen <= 2) then
    begin
      CenturyBase := CurrentYear - TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1; M := N2;
    end else
    begin
      M := N1; D := N2;
    end;
  end;
  ScanChar(S, Pos, DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', ShortDateFormat) <> 0) then
  begin     // ignore trailing text
    if ShortTimeFormat[1] in ['0'..'9'] then  // stop at time digit
      ScanToNumber(S, Pos)
    else  // stop at time prefix
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do Inc(Pos);
        ScanBlanks(S, Pos);
      until (Pos > Length(S)) or
        (AnsiCompareText(TimeAMString, Copy(S, Pos, Length(TimeAMString))) = 0) or
        (AnsiCompareText(TimePMString, Copy(S, Pos, Length(TimePMString))) = 0);
  end;
  Result := DoEncodeDate(Y, M, D, Date);
end;

function ScanTime(const S: string; var Pos: Integer;
  var Time: TDateTime): Boolean;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
begin
  Result := False;
  BaseHour := -1;
  if ScanString(S, Pos, TimeAMString) or ScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ScanString(S, Pos, TimePMString) or ScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ScanBlanks(S, Pos);
  if not ScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  if ScanChar(S, Pos, TimeSeparator) then
    if not ScanNumber(S, Pos, Min, Junk) then Exit;
  Sec := 0;
  if ScanChar(S, Pos, TimeSeparator) then
    if not ScanNumber(S, Pos, Sec, Junk) then Exit;
  MSec := 0;
  if ScanChar(S, Pos, DecimalSeparator) then
    if not ScanNumber(S, Pos, MSec, Junk) then Exit;
  if BaseHour < 0 then
    if ScanString(S, Pos, TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ScanString(S, Pos, TimePMString) or ScanString(S, Pos, 'PM') then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ScanBlanks(S, Pos);
  Result := DoEncodeTime(Hour, Min, Sec, MSec, Time);
end;

function StrToDate(const S: string): TDateTime;
var
  Pos: Integer;
begin
  Pos := 1;
  if not ScanDate(S, Pos, Result) or (Pos <= Length(S)) then
    ConvertErrorFmt(SInvalidDate, [S]);
end;

function StrToTime(const S: string): TDateTime;
var
  Pos: Integer;
begin
  Pos := 1;
  if not ScanTime(S, Pos, Result) or (Pos <= Length(S)) then
    ConvertErrorFmt(SInvalidTime, [S]);
end;

function StrToDateTime(const S: string): TDateTime;
var
  Pos: Integer;
  Date, Time: TDateTime;
begin
  Pos := 1;
  Time := 0;
  if not ScanDate(S, Pos, Date) or not ((Pos > Length(S)) or
    ScanTime(S, Pos, Time)) then
  begin   // Try time only
    Pos := 1;
    if not ScanTime(S, Pos, Result) or (Pos <= Length(S)) then
      ConvertErrorFmt(SInvalidDateTime, [S]);
  end else
    if Date >= 0 then
      Result := Date + Time else
      Result := Date - Time;
end;

{ System error messages }

function SysErrorMessage(ErrorCode: Integer): string;
var
  Len: Integer;
  Buffer: array[0..255] of Char;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
    SizeOf(Buffer), nil);
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do Dec(Len);
  SetString(Result, Buffer, Len);
end;

{ Initialization file support }

function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
var
  L: Integer;
  Buffer: array[0..255] of Char;
begin
  L := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  if L > 0 then SetString(Result, Buffer, L - 1) else Result := Default;
end;

function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
var
  Buffer: array[0..1] of Char;
begin
  if GetLocaleInfo(Locale, LocaleType, Buffer, 2) > 0 then
    Result := Buffer[0] else
    Result := Default;
end;

procedure GetMonthDayNames;
{
const
  DefShortMonthNames: array[1..12] of string = (SShortMonthNameJan,
    SShortMonthNameFeb, SShortMonthNameMar, SShortMonthNameApr,
    SShortMonthNameMay, SShortMonthNameJun, SShortMonthNameJul,
    SShortMonthNameAug, SShortMonthNameSep, SShortMonthNameOct,
    SShortMonthNameNov, SShortMonthNameDec);

  DefLongMonthNames: array[1..12] of string = (SLongMonthNameJan,
    SLongMonthNameFeb, SLongMonthNameMar, SLongMonthNameApr,
    SLongMonthNameMay, SLongMonthNameJun, SLongMonthNameJul,
    SLongMonthNameAug, SLongMonthNameSep, SLongMonthNameOct,
    SLongMonthNameNov, SLongMonthNameDec);

  DefShortDayNames: array[1..7] of string = (SShortDayNameSun,
    SShortDayNameMon, SShortDayNameTue, SShortDayNameWed,
    SShortDayNameThu, SShortDayNameFri, SShortDayNameSat);

  DefLongDayNames: array[1..7] of string = (SLongDayNameSun,
    SLongDayNameMon, SLongDayNameTue, SLongDayNameWed,
    SLongDayNameThu, SLongDayNameFri, SLongDayNameSat);
}

var
  I, Day: Integer;
  DefaultLCID: LCID;

  function LocalGetLocaleStr(LocaleType: Integer): string;
  begin
    Result := GetLocaleStr(DefaultLCID, LocaleType, '');
    if Result = '' then
      Result := GetLocaleStr($409, LocaleType, '');
  end;

begin
  DefaultLCID := GetThreadLocale;
  for I := 1 to 12 do
  begin
    ShortMonthNames[I] := LocalGetLocaleStr(LOCALE_SABBREVMONTHNAME1 + I - 1);
    LongMonthNames[I] := LocalGetLocaleStr(LOCALE_SMONTHNAME1 + I - 1);
  end;
  for I := 1 to 7 do
  begin
    Day := (I + 5) mod 7;
    ShortDayNames[I] := LocalGetLocaleStr(LOCALE_SABBREVDAYNAME1 + Day);
    LongDayNames[I] := LocalGetLocaleStr(LOCALE_SDAYNAME1 + Day);
  end;
end;

function EnumEraNames(Names: PChar): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  I := Low(EraNames);
  while EraNames[I] <> '' do
    if (I = High(EraNames)) then
      Exit
    else Inc(I);
  EraNames[I] := Names;
  Result := 1;
end;

function EnumEraYearOffsets(YearOffsets: PChar): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  I := Low(EraYearOffsets);
  while EraYearOffsets[I] <> -1 do
    if (I = High(EraYearOffsets)) then
      Exit
    else Inc(I);
  EraYearOffsets[I] := StrToIntDef(YearOffsets, 0);
  Result := 1;
end;

procedure GetEraNamesAndYearOffsets;
var
  J: Integer;
  CalendarType: CALTYPE;
begin
  CalendarType := StrToIntDef(GetLocaleStr(GetThreadLocale,
    LOCALE_IOPTIONALCALENDAR, '1'), 1);
  if CalendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA] then
  begin
    EnumCalendarInfoA(@EnumEraNames, GetThreadLocale, CalendarType,
      CAL_SERASTRING);
    for J := Low(EraYearOffsets) to High(EraYearOffsets) do
      EraYearOffsets[J] := -1;
    EnumCalendarInfoA(@EnumEraYearOffsets, GetThreadLocale, CalendarType,
      CAL_IYEAROFFSETRANGE);
  end;
end;

function TranslateDateFormat(const FormatStr: string): string;
var
  I: Integer;
  CalendarType: CALTYPE;
  RemoveEra: Boolean;
begin
  I := 1;
  Result := '';
  CalendarType := StrToIntDef(GetLocaleStr(GetThreadLocale,
    LOCALE_ICALENDARTYPE, '1'), 1);
  if not (CalendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA]) then
  begin
    RemoveEra := SysLocale.PriLangID in [LANG_JAPANESE, LANG_CHINESE, LANG_KOREAN];
    if RemoveEra then
    begin
      While I <= Length(FormatStr) do
      begin
        if not (FormatStr[I] in ['g', 'G']) then
          Result := Result + FormatStr[I];
        Inc(I);
      end;
    end
    else
      Result := FormatStr;
    Exit;
  end;

  while I <= Length(FormatStr) do
  begin
    if FormatStr[I] in LeadBytes then
    begin
      Result := Result + Copy(FormatStr, I, 2);
      Inc(I, 2);
    end else
    begin
      if StrLIComp(@FormatStr[I], 'gg', 2) = 0 then
      begin
        Result := Result + 'ggg';
        Inc(I, 1);
      end
      else if StrLIComp(@FormatStr[I], 'yyyy', 4) = 0 then
      begin
        Result := Result + 'eeee';
        Inc(I, 4-1);
      end
      else if StrLIComp(@FormatStr[I], 'yy', 2) = 0 then
      begin
        Result := Result + 'ee';
        Inc(I, 2-1);
      end
      else if FormatStr[I] in ['y', 'Y'] then
        Result := Result + 'e'
      else
        Result := Result + FormatStr[I];
      Inc(I);
    end;
  end;
end;

{ Exception handling routines }

var
  OutOfMemory: EOutOfMemory;
  InvalidPointer: EInvalidPointer;

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

{ Return current exception object }

function ExceptObject: TObject;
begin
  if RaiseList <> nil then
    Result := PRaiseFrame(RaiseList)^.ExceptObject else
    Result := nil;
end;

{ Return current exception address }

function ExceptAddr: Pointer;
begin
  if RaiseList <> nil then
    Result := PRaiseFrame(RaiseList)^.ExceptAddr else
    Result := nil;
end;

{ Convert physical address to logical address }

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX         { Always convert nil to nil }
        JE      @@1
        SUB     EAX, $1000      { offset from code start; code start set by linker to $1000 }
@@1:
end;

{ Format and return an exception error message }

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PChar; Size: Integer): Integer;
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  ModuleName: array[0..MAX_PATH] of Char;
  Temp: array[0..MAX_PATH] of Char;
  Info: TMemoryBasicInformation;
  ConvertedAddress: Pointer;
begin
  VirtualQuery(ExceptAddr, Info, sizeof(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    ConvertedAddress := ConvertAddr(ExceptAddr);
  end
  else
    Integer(ConvertedAddress) := Integer(ExceptAddr) - Integer(Info.AllocationBase);
  StrLCopy(ModuleName, AnsiStrRScan(Temp, '\') + 1, SizeOf(ModuleName) - 1);
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  StrPCopy(Buffer, kol.Format(SException, [ExceptObject.ClassName, ModuleName,
                                  ConvertedAddress, MsgPtr, MsgEnd]) );
  Result := StrLen(Buffer);
end;

{ Display exception message box }

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
var
  Buffer: array[0..1023] of Char;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
  if IsConsole then
    WriteLn(Buffer)
  else
  begin
    MessageBox(0, Buffer, SExceptTitle, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
  end;
end;

{ Raise abort exception }

procedure Abort;

  function ReturnAddr: Pointer;
  asm
//          MOV     EAX,[ESP + 4] !!! codegen dependant
          MOV     EAX,[EBP - 4]
  end;

begin
  raise EAbort.Create(SOperationAborted) at ReturnAddr;
end;

{ Raise out of memory exception }

procedure OutOfMemoryError;
begin
  raise OutOfMemory;
end;

{ Exception class }

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
end;

constructor Exception.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  FMessage := kol.Format(Msg, Args);
end;

constructor Exception.CreateRes(Ident: Integer);
begin
  FMessage := LoadStr(Ident);
end;

constructor Exception.CreateRes(ResStringRec: PResStringRec);
begin
  FMessage := LoadResString(ResStringRec);
end;

constructor Exception.CreateResFmt(Ident: Integer;
  const Args: array of const);
begin
  FMessage := kol.Format(LoadStr(Ident), Args);
end;

constructor Exception.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  FMessage := kol.Format(LoadResString(ResStringRec), Args);
end;

constructor Exception.CreateHelp(const Msg: string; AHelpContext: Integer);
begin
  FMessage := Msg;
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateFmtHelp(const Msg: string; const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := kol.Format(Msg, Args);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResHelp(Ident: Integer; AHelpContext: Integer);
begin
  FMessage := LoadStr(Ident);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResHelp(ResStringRec: PResStringRec;
  AHelpContext: Integer);
begin
  FMessage := LoadResString(ResStringRec);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResFmtHelp(Ident: Integer;
  const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := kol.Format(LoadStr(Ident), Args);
  FHelpContext := AHelpContext;
end;

constructor Exception.CreateResFmtHelp(ResStringRec: PResStringRec;
  const Args: array of const;
  AHelpContext: Integer);
begin
  FMessage := kol.Format(LoadResString(ResStringRec), Args);
  FHelpContext := AHelpContext;
end;

{ EHeapException class }

procedure EHeapException.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

{ Create I/O exception }

function CreateInOutError: EInOutError;
type
  TErrorRec = record
    Code: Integer;
    Ident: string;
  end;
const
  ErrorMap: array[0..6] of TErrorRec = (
    (Code: 2; Ident: SFileNotFound),
    (Code: 3; Ident: SInvalidFilename),
    (Code: 4; Ident: STooManyOpenFiles),
    (Code: 5; Ident: SAccessDenied),
    (Code: 100; Ident: SEndOfFile),
    (Code: 101; Ident: SDiskFull),
    (Code: 106; Ident: SInvalidInput));
var
  I: Integer;
  InOutRes: Integer;
begin
  I := Low(ErrorMap);
  InOutRes := IOResult;  // resets IOResult to zero
  while (I <= High(ErrorMap)) and (ErrorMap[I].Code <> InOutRes) do Inc(I);
  if I <= High(ErrorMap) then
    Result := EInOutError.Create(ErrorMap[I].Ident) else
    Result := EInOutError.CreateFmt(SInOutError, [InOutRes]);
  Result.ErrorCode := InOutRes;
end;

{ RTL error handler }

type
  TExceptRec = record
    EClass: ExceptClass;
    EIdent: string;
  end;

const
  ExceptMap: array[3..24] of TExceptRec = (
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: EAccessViolation; EIdent: SAccessViolation),
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlC; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: EVariantError; EIdent: SInvalidVarCast),
    (EClass: EVariantError; EIdent: SInvalidVarOp),
    (EClass: EVariantError; EIdent: SDispatchError),
    (EClass: EVariantError; EIdent: SVarArrayCreate),
    (EClass: EVariantError; EIdent: SVarNotArray),
    (EClass: EVariantError; EIdent: SVarArrayBounds),
    (EClass: EAssertionFailed; EIdent: SAssertionFailed),
    (EClass: EExternalException; EIdent: SExternalException),
    (EClass: EIntfCastError; EIdent: SIntfCastError),
    (EClass: ESafecallException; EIdent: SSafecallException));

procedure ErrorHandler(ErrorCode: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case ErrorCode of
    1: E := OutOfMemory;
    2: E := InvalidPointer;
    3..24: with ExceptMap[ErrorCode] do E := EClass.Create(EIdent);
  else
    E := CreateInOutError;
  end;
  raise E at ErrorAddr;
end;

{ Assertion error handler }

{ This is complicated by the desire to make it look like the exception     }
{ happened in the user routine, so the debugger can give a decent stack    }
{ trace. To make that feasible, AssertErrorHandler calls a helper function }
{ to create the exception object, so that AssertErrorHandler itself does   }
{ not need any temps. After the exception object is created, the asm       }
{ routine RaiseAssertException sets up the registers just as if the user   }
{ code itself had raised the exception.                                    }

function CreateAssertException(const Message, Filename: string;
  LineNumber: Integer): Exception;
var
  S: string;
begin
  if Message <> '' then S := Message else S := SAssertionFailed;
  Result := EAssertionFailed.CreateFmt(SAssertError,
         [S, Filename, LineNumber]);
end;

{ This code is based on the following assumptions:                         }
{  - Our direct caller (AssertErrorHandler) has an EBP frame               }
{  - ErrorStack points to where the return address would be if the         }
{    user program had called System.@RaiseExcept directly                  }
procedure RaiseAssertException(const E: Exception; const ErrorAddr, ErrorStack: Pointer);
asm
        MOV     ESP,ECX
        MOV     [ESP],EDX
        MOV     EBP,[EBP]
        JMP     System.@RaiseExcept
end;

{ If you change this procedure, make sure it does not have any local variables }
{ or temps that need cleanup - they won't get cleaned up due to the way        }
{ RaiseAssertException frame works. Also, it can not have an exception frame.  }
procedure AssertErrorHandler(const Message, Filename: string;
  LineNumber: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
   E := CreateAssertException(Message, Filename, LineNumber);
   RaiseAssertException(E, ErrorAddr, PChar(@ErrorAddr)+4);
end;

{ Abstract method invoke error handler }

procedure AbstractErrorHandler;
begin
  raise EAbstractError.CreateFmt(SAbstractError, ['']);
end;

function MapException(P: PExceptionRecord): Byte;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:
      Result := 3;
    STATUS_ARRAY_BOUNDS_EXCEEDED:
      Result := 4;
    STATUS_INTEGER_OVERFLOW:
      Result := 5;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:
      Result := 6;
    STATUS_FLOAT_DIVIDE_BY_ZERO:
      Result := 7;
    STATUS_FLOAT_OVERFLOW:
      Result := 8;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:
      Result := 9;
    STATUS_ACCESS_VIOLATION:
      Result := 11;
    STATUS_PRIVILEGED_INSTRUCTION:
      Result := 12;
    STATUS_CONTROL_C_EXIT:
      Result := 13;
    STATUS_STACK_OVERFLOW:
      Result := 14;
  else
    Result := 22; { must match System.reExternalException }
  end;
end;

function GetExceptionClass(P: PExceptionRecord): ExceptClass;
var
  ErrorCode: Byte;
begin
  ErrorCode := MapException(P);
  Result := ExceptMap[ErrorCode].EClass;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  ErrorCode: Integer;

  function CreateAVObject: Exception;
  var
    AccessOp: string; // string ID indicating the access type READ or WRITE
    AccessAddress: Pointer;
    MemInfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of Char;
  begin
    with P^ do
    begin
      if ExceptionInformation[0] = 0 then
        AccessOp := SReadAccess else
        AccessOp := SWriteAccess;
      AccessAddress := Pointer(ExceptionInformation[1]);
      VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
      if (MemInfo.State = MEM_COMMIT) and (GetModuleFileName(THandle(MemInfo.AllocationBase),
        ModName, SizeOf(ModName)) <> 0) then
        Result := EAccessViolation.CreateFmt(sModuleAccessViolation,
          [ExceptionAddress, ExtractFileName(ModName), AccessOp,
          AccessAddress])
      else Result := EAccessViolation.CreateFmt(sAccessViolation,
          [ExceptionAddress, AccessOp, AccessAddress]);
    end;
  end;

begin
  ErrorCode := MapException(P);
  case ErrorCode of
    3..10, 12..21:
      with ExceptMap[ErrorCode] do Result := EClass.Create(EIdent);
    11: Result := CreateAVObject;
  else
    Result := EExternalException.CreateFmt(SExternalException, [P.ExceptionCode]);
  end;
  if Result is EExternal then
  begin
    EExternal(Result).ExceptionRecord := P;
    if P.ExceptionCode = $0EEFFACE then
      Result.FMessage := 'C++ Exception';  // do not localize
  end;    
end;

{ RTL exception handler }

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
begin
  ShowException(ExceptObject, ExceptAddr);
  Halt(1);
end;

procedure InitExceptions;
begin
  OutOfMemory := EOutOfMemory.Create(SOutOfMemory);
  InvalidPointer := EInvalidPointer.Create(SInvalidPointer);
  ErrorProc := @ErrorHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception;
  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
  AssertErrorProc := @AssertErrorHandler;
  AbstractErrorProc := @AbstractErrorHandler;
end;

procedure DoneExceptions;
begin
  OutOfMemory.AllowFree := True;
  OutOfMemory.FreeInstance;
  OutOfMemory := nil;
  InvalidPointer.AllowFree := True;
  InvalidPointer.Free;
  InvalidPointer := nil;
  ErrorProc := nil;
  ExceptProc := nil;
  ExceptionClass := nil;
  ExceptClsProc := nil;
  ExceptObjProc := nil;
  AssertErrorProc := nil;
end;

procedure InitPlatformId;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    with OSVersionInfo do
    begin
      Win32Platform := dwPlatformId;
      Win32MajorVersion := dwMajorVersion;
      Win32MinorVersion := dwMinorVersion;
      Win32BuildNumber := dwBuildNumber;
      Win32CSDVersion := szCSDVersion;
    end;
end;

procedure Beep;
begin
  MessageBeep(0);
end;

{ MBCS functions }

function ByteTypeTest(P: PChar; Index: Integer): TMbcsByteType;
var
  I: Integer;
begin
  Result := mbSingleByte;
  if (P = nil) or (P[Index] = #$0) then Exit;
  if (Index = 0) then
  begin
    if P[0] in LeadBytes then Result := mbLeadByte;
  end
  else
  begin
    I := Index - 1;
    while (I >= 0) and (P[I] in LeadBytes) do Dec(I);
    if ((Index - I) mod 2) = 0 then Result := mbTrailByte
    else if P[Index] in LeadBytes then Result := mbLeadByte;
  end;
end;

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  if SysLocale.FarEast then
    Result := ByteTypeTest(PChar(S), Index-1);
end;

function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
begin
  Result := mbSingleByte;
  if SysLocale.FarEast then
    Result := ByteTypeTest(Str, Index);
end;

function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
begin
  if Length(S) < MaxLen then MaxLen := Length(S);
  Result := ByteToCharIndex(S, MaxLen);
end;

function ByteToCharIndex(const S: string; Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Index <= 0) or (Index > Length(S)) then Exit;
  Result := Index;
  if not SysLocale.FarEast then Exit;
  I := 1;
  Result := 0;
  while I <= Index do
  begin
    if S[I] in LeadBytes then Inc(I);
    Inc(I);
    Inc(Result);
  end;
end;

procedure CountChars(const S: string; MaxChars: Integer; var CharCount, ByteCount: Integer);
var
  C, L, B: Integer;
begin
  L := Length(S);
  C := 1;
  B := 1;
  while (B < L) and (C < MaxChars) do
  begin
    Inc(C);
    if S[B] in LeadBytes then Inc(B);
    Inc(B);
  end;
  if (C = MaxChars) and (B < L) and (S[B] in LeadBytes) then Inc(B);
  CharCount := C;
  ByteCount := B;
end;

function CharToByteIndex(const S: string; Index: Integer): Integer;
var
  Chars: Integer;
begin
  Result := 0;
  if (Index <= 0) or (Index > Length(S)) then Exit;
  if (Index > 1) and SysLocale.FarEast then
  begin
    CountChars(S, Index-1, Chars, Result);
    if (Chars < (Index-1)) or (Result >= Length(S)) then
      Result := 0  // Char index out of range
    else
      Inc(Result);
  end
  else
    Result := Index;
end;

function CharToByteLen(const S: string; MaxLen: Integer): Integer;
var
  Chars: Integer;
begin
  Result := 0;
  if MaxLen <= 0 then Exit;
  if MaxLen > Length(S) then MaxLen := Length(S);
  if SysLocale.FarEast then
  begin
    CountChars(S, MaxLen, Chars, Result);
    if Result > Length(S) then
      Result := Length(S);
  end
  else
    Result := MaxLen;
end;

function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = '\')
    and (ByteType(S, Index) = mbSingleByte);
end;

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Length(S)) or (ByteType(S, Index) <> mbSingleByte) then exit;
  Result := StrScan(PChar(Delimiters), S[Index]) <> nil;
end;

function IncludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then Result := Result + '\';
end;

function ExcludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function AnsiPos(const Substr, S: string): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := AnsiStrPos(PChar(S), PChar(SubStr));
  if P <> nil then
    Result := Integer(P) - Integer(PChar(S)) + 1;
end;

function AnsiCompareFileName(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(AnsiLowerCaseFileName(S1), AnsiLowerCaseFileName(S2));
end;

function AnsiLowerCaseFileName(const S: string): string;
var
  I,L: Integer;
begin
  if SysLocale.FarEast then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['A'..'Z'] then Inc(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;

function AnsiUpperCaseFileName(const S: string): string;
var
  I,L: Integer;
begin
  if SysLocale.FarEast then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['a'..'z'] then Dec(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiUpperCase(S);
end;

function AnsiStrPos(Str, SubStr: PChar): PChar;
var
  L1, L2: Cardinal;
  ByteType : TMbcsByteType;
begin
  Result := nil;
  if (Str = nil) or (Str^ = #0) or (SubStr = nil) or (SubStr^ = #0) then Exit;
  L1 := StrLen(Str);
  L2 := StrLen(SubStr);
  Result := StrPos(Str, SubStr);
  while (Result <> nil) and ((L1 - Cardinal(Result - Str)) >= L2) do
  begin
    ByteType := StrByteType(Str, Integer(Result-Str));
    if (ByteType <> mbTrailByte) and
      (CompareString(LOCALE_USER_DEFAULT, 0, Result, L2, SubStr, L2) = 2) then Exit;
    if (ByteType = mbLeadByte) then Inc(Result);
    Inc(Result);
    Result := StrPos(Result, SubStr);
  end;
  Result := nil;
end;

function AnsiStrRScan(Str: PChar; Chr: Char): PChar;
begin
  Str := AnsiStrScan(Str, Chr);
  Result := Str;
  if Chr <> #$0 then
  begin
    while Str <> nil do
    begin
      Result := Str;
      Inc(Str);
      Str := AnsiStrScan(Str, Chr);
    end;
  end
end;

function AnsiStrScan(Str: PChar; Chr: Char): PChar;
begin
  Result := StrScan(Str, Chr);
  while Result <> nil do
  begin
    case StrByteType(Str, Integer(Result-Str)) of
      mbSingleByte: Exit;
      mbLeadByte: Inc(Result);
    end;
    Inc(Result);
    Result := StrScan(Result, Chr);
  end;
end;

procedure InitSysLocale;
var
  DefaultLCID: LCID;
  DefaultLangID: LANGID;
  AnsiCPInfo: TCPInfo;
  I: Integer;
  J: Byte;
begin
  { Set default to English (US). }
  SysLocale.DefaultLCID := $0409;
  SysLocale.PriLangID := LANG_ENGLISH;
  SysLocale.SubLangID := SUBLANG_ENGLISH_US;

  DefaultLCID := GetThreadLocale;
  if DefaultLCID <> 0 then SysLocale.DefaultLCID := DefaultLCID;

  DefaultLangID := Word(DefaultLCID);
  if DefaultLangID <> 0 then
  begin
    SysLocale.PriLangID := DefaultLangID and $3ff;
    SysLocale.SubLangID := DefaultLangID shr 10;
  end;

  SysLocale.MiddleEast := GetSystemMetrics(SM_MIDEASTENABLED) <> 0;
  SysLocale.FarEast := GetSystemMetrics(SM_DBCSENABLED) <> 0;
  if not SysLocale.FarEast then Exit;

  GetCPInfo(CP_ACP, AnsiCPInfo);
  with AnsiCPInfo do
  begin
    I := 0;
    while (I < MAX_LEADBYTES) and ((LeadByte[I] or LeadByte[I+1]) <> 0) do
    begin
      for J := LeadByte[I] to LeadByte[I+1] do
        Include(LeadBytes, Char(J));
      Inc(I,2);
    end;
  end;
end;

procedure GetFormatSettings;
var
  HourFormat, TimePrefix, TimePostfix: string;
  DefaultLCID: LCID;
begin
  InitSysLocale;
  GetMonthDayNames;
  if SysLocale.FarEast then GetEraNamesAndYearOffsets;
  DefaultLCID := GetThreadLocale;
  CurrencyString := GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '');
  CurrencyFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, '0'), 0);
  NegCurrFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, '0'), 0);
  ThousandSeparator := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
  DecimalSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
  CurrencyDecimals := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, '0'), 0);
  DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
  ShortDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy'));
  LongDateFormat := TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy'));
  TimeSeparator := GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
  TimeAMString := GetLocaleStr(DefaultLCID, LOCALE_S1159, 'am');
  TimePMString := GetLocaleStr(DefaultLCID, LOCALE_S2359, 'pm');
  TimePrefix := '';
  TimePostfix := '';
  if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0'), 0) = 0 then
    HourFormat := 'h' else
    HourFormat := 'hh';
  if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0'), 0) = 0 then
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
      TimePostfix := ' AMPM'
    else
      TimePrefix := 'AMPM ';
  ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  ListSeparator := GetLocaleChar(DefaultLCID, LOCALE_SLIST, ',');
end;

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function WrapText(const Line, BreakStr: string; BreakChars: TSysCharSet;
  MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := ' ';
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar in LeadBytes then
    begin
      Inc(Pos);
      Inc(Col);
    end else
      if CurChar = BreakStr[1] then
      begin
        if QuoteChar = ' ' then
        begin
          ExistingBreak := CompareText(BreakStr, Copy(Line, Pos, BreakLen)) = 0;
          if ExistingBreak then
          begin
            Inc(Pos, BreakLen-1);
            BreakPos := Pos;
          end;
        end
      end
      else if CurChar in BreakChars then
      begin
        if QuoteChar = ' ' then BreakPos := Pos
      end
      else if CurChar in QuoteChars then
        if CurChar = QuoteChar then
          QuoteChar := ' '
        else if QuoteChar = ' ' then
          QuoteChar := CurChar;
    Inc(Pos);
    Inc(Col);
    if not (QuoteChar in QuoteChars) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := Pos - BreakPos;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (CurChar in QuoteChars) then
        while (Pos <= LineLen) and (Line[Pos] in BreakChars + [#13, #10]) do Inc(Pos);
      if not ExistingBreak and (Pos < LineLen) then
        Result := Result + BreakStr;
      Inc(BreakPos);
      LinePos := BreakPos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

function WrapText(const Line: string; MaxCol: Integer): string;
begin
  Result := WrapText(Line, #13#10, [' ', '-', #9], MaxCol); { do not localize }
end;

function FindCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet;
  IgnoreCase: Boolean): Boolean;
var
  I: Integer;
  S: string;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (SwitchChars = []) or (S[1] in SwitchChars) then
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end
      else begin
        if (AnsiCompareStr(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end;
  end;
  Result := False;
end;

{ Package info structures }

type
  PPkgName = ^TPkgName;
  TPkgName = packed record
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }
  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

  { Package flags:
    bit     meaning
    -----------------------------------------------------------------------------------------
    0     | 1: never-build                  0: always build
    1     | 1: design-time only             0: not design-time only      on => bit 2 = off
    2     | 1: run-time only                0: not run-time only         on => bit 1 = off
    3     | 1: do not check for dup units   0: perform normal dup unit check
    4..25 | reserved
    26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
    28..29| reserved
    30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
  }
  PPackageInfoHeader = ^TPackageInfoHeader;
  TPackageInfoHeader = packed record
    Flags: DWORD;
    RequiresCount: Integer;
    {Requires: array[0..9999] of TPkgName;
    ContainsCount: Integer;
    Contains: array[0..9999] of TUnitName;}
  end;

function PackageInfoTable(Module: HMODULE): PPackageInfoHeader;
var
  ResInfo: HRSRC;
  Data: THandle;
begin
  Result := nil;
  ResInfo := FindResource(Module, 'PACKAGEINFO', RT_RCDATA);
  if ResInfo <> 0 then
  begin
    Data := LoadResource(Module, ResInfo);
    if Data <> 0 then
    try
      Result := LockResource(Data);
      UnlockResource(Data);
    finally
      FreeResource(Data);
    end;
  end;
end;

function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Windows.GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;

var
  Reserved: Integer;

procedure CheckForDuplicateUnits(Module: HMODULE);
var
  ModuleFlags: DWORD;

  function IsUnitPresent(HC: Byte; UnitName: PChar; Module: HMODULE;
    const ModuleName: string; var UnitPackage: string): Boolean;
  var
    I: Integer;
    InfoTable: PPackageInfoHeader;
    LibModule: PLibModule;
    PkgName: PPkgName;
    UName : PUnitName;
    Count: Integer;
  begin
    Result := True;
    if (StrIComp(UnitName, 'SysInit') <> 0) and
      (StrIComp(UnitName, PChar(ModuleName)) <> 0) then
    begin
      LibModule := LibModuleList;
      while LibModule <> nil do
      begin
        if LibModule.Instance <> Module then
        begin
          InfoTable := PackageInfoTable(HMODULE(LibModule.Instance));
          if (InfoTable <> nil) and (InfoTable.Flags and pfModuleTypeMask = pfPackageModule) and
            ((InfoTable.Flags and pfIgnoreDupUnits) = (ModuleFlags and pfIgnoreDupUnits)) then
          begin
            PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
            Count := InfoTable.RequiresCount;
            { Skip the Requires list }
            for I := 0 to Count - 1 do Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
            Count := Integer(Pointer(PkgName)^);
            UName := PUnitName(Integer(PkgName) + 4);
            for I := 0 to Count - 1 do
            begin
              with UName^ do
                // Test Flags to ignore weak package units
                if ((HashCode = HC) or (HashCode = 0) or (HC = 0)) and
                  ((Flags and $06) = 0) and (StrIComp(UnitName, Name) = 0) then
                begin
                  UnitPackage := ChangeFileExt(ExtractFileName(
                    GetModuleName(HMODULE(LibModule.Instance))), '');
                  Exit;
                end;
              Inc(Integer(UName), StrLen(UName.Name) + 3);
            end;
          end;
        end;
        LibModule := LibModule.Next;
      end;
    end;
    Result := False;
  end;

  function FindLibModule(Module: HModule): PLibModule;
  begin
    Result := LibModuleList;
    while Result <> nil do
    begin
      if Result.Instance = Module then Exit;
      Result := Result.Next;
    end;
  end;

  procedure InternalUnitCheck(Module: HModule);
  var
    I: Integer;
    InfoTable: PPackageInfoHeader;
    UnitPackage: string;
    ModuleName: string;
    PkgName: PPkgName;
    UName: PUnitName;
    Count: Integer;
    LibModule: PLibModule;
  begin
    InfoTable := PackageInfoTable(Module);
    if (InfoTable <> nil) and (InfoTable.Flags and pfModuleTypeMask = pfPackageModule) then
    begin
      if ModuleFlags = 0 then ModuleFlags := InfoTable.Flags;
      ModuleName := ChangeFileExt(ExtractFileName(GetModuleName(Module)), '');
      PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
      Count := InfoTable.RequiresCount;
      for I := 0 to Count - 1 do
      begin
        with PkgName^ do
          InternalUnitCheck(GetModuleHandle(PChar(ChangeFileExt(Name, '.bpl'))));
        Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
      end;
      LibModule := FindLibModule(Module);
      if (LibModule = nil) or ((LibModule <> nil) and (LibModule.Reserved <> Reserved)) then
      begin
        if LibModule <> nil then LibModule.Reserved := Reserved;
        Count := Integer(Pointer(PkgName)^);
        UName := PUnitName(Integer(PkgName) + 4);
        for I := 0 to Count - 1 do
        begin
          with UName^ do
            // Test Flags to ignore weak package units
            if ((Flags and ufWeakPackageUnit) = 0 ) and
              IsUnitPresent(HashCode, Name, Module, ModuleName, UnitPackage) then
              raise EPackageError.CreateFmt(SDuplicatePackageUnit,
                [ModuleName, Name, UnitPackage]);
          Inc(Integer(UName), StrLen(UName.Name) + 3);
        end;
      end;
    end;
  end;

begin
  Inc(Reserved);
  ModuleFlags := 0;
  InternalUnitCheck(Module);
end;

{ InitializePackage }

procedure InitializePackage(Module: HMODULE);
type
  TPackageLoad = procedure;
var
  PackageLoad: TPackageLoad;
begin
  CheckForDuplicateUnits(Module);
  @PackageLoad := GetProcAddress(Module, 'Initialize'); //Do not localize
  if Assigned(PackageLoad) then
    PackageLoad else
    raise Exception.CreateFmt(sInvalidPackageFile, [GetModuleName(Module)]);
end;

{ FinalizePackage }

procedure FinalizePackage(Module: HMODULE);
type
  TPackageUnload = procedure;
var
  PackageUnload: TPackageUnload;
begin
  @PackageUnload := GetProcAddress(Module, 'Finalize'); //Do not localize
  if Assigned(PackageUnload) then
    PackageUnload else
    raise EPackageError.Create(sInvalidPackageHandle);
end;

{ LoadPackage }

function LoadPackage(const Name: string): HMODULE;
begin
  Result := SafeLoadLibrary(Name);
  if Result = 0 then
    raise EPackageError.CreateFmt(sErrorLoadingPackage,
      [Name, SysErrorMessage(GetLastError)]);
  try
    InitializePackage(Result);
  except
    FreeLibrary(Result);
    raise;
  end;
end;

{ UnloadPackage }

procedure UnloadPackage(Module: HMODULE);
begin
  FinalizePackage(Module);
  FreeLibrary(Module);
end;

{ GetPackageInfo }

procedure GetPackageInfo(Module: HMODULE; Param: Pointer; var Flags: Integer;
  InfoProc: TPackageInfoProc);
var
  InfoTable: PPackageInfoHeader;
  I: Integer;
  PkgName: PPkgName;
  UName: PUnitName;
  Count: Integer;
begin
  InfoTable := PackageInfoTable(Module);
  if not Assigned(InfoTable) then
    raise Exception.CreateFmt(SCannotReadPackageInfo,
      [ExtractFileName(GetModuleName(Module))]);
  Flags := InfoTable.Flags;
  with InfoTable^ do
  begin
    PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
    Count := RequiresCount;
    for I := 0 to Count - 1 do
    begin
      InfoProc(PkgName.Name, ntRequiresPackage, 0, Param);
      Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
    end;
    Count := Integer(Pointer(PkgName)^);
    UName := PUnitName(Integer(PkgName) + 4);
    for I := 0 to Count - 1 do
    begin
      InfoProc(UName.Name, ntContainsUnit, UName.Flags, Param);
      Inc(Integer(UName), StrLen(UName.Name) + 3);
    end;
  end;
end;

function GetPackageDescription(ModuleName: PChar): string;
var
  ResModule: HModule;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
begin
  Result := '';
  ResModule := LoadResourceModule(ModuleName);
  if ResModule = 0 then
  begin
    ResModule := LoadLibraryEx(ModuleName, 0, LOAD_LIBRARY_AS_DATAFILE);
    if ResModule = 0 then
      raise EPackageError.CreateFmt(sErrorLoadingPackage,
        [ModuleName, SysErrorMessage(GetLastError)]);
  end;
  try
    ResInfo := FindResource(ResModule, 'DESCRIPTION', RT_RCDATA);
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        Result := PWideChar(LockResource(ResData));
        UnlockResource(ResData);
      finally
        FreeResource(ResData);
      end;
    end;
  finally
    FreeLibrary(ResModule);
  end;
end;

{ RaiseLastWin32Error }

procedure RaiseLastWin32Error;
var
  LastError: DWORD;
  Error: EWin32Error;
begin
  LastError := GetLastError;
  if LastError <> ERROR_SUCCESS then
    Error := EWin32Error.CreateFmt(SWin32Error, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EWin32Error.Create(SUnkWin32Error);
  Error.ErrorCode := LastError;
  raise Error;
end;

{ Win32Check }

function Win32Check(RetVal: BOOL): BOOL;
begin
  if not RetVal then RaiseLastWin32Error;
  Result := RetVal;
end;

type
  PTerminateProcInfo = ^TTerminateProcInfo;
  TTerminateProcInfo = record
    Next: PTerminateProcInfo;
    Proc: TTerminateProc;
  end;

var
  TerminateProcList: PTerminateProcInfo = nil;

procedure AddTerminateProc(TermProc: TTerminateProc);
var
  P: PTerminateProcInfo;
begin
  New(P);
  P^.Next := TerminateProcList;
  P^.Proc := TermProc;
  TerminateProcList := P;
end;

function CallTerminateProcs: Boolean;
var
  PI: PTerminateProcInfo;
begin
  Result := True;
  PI := TerminateProcList;
  while Result and (PI <> nil) do
  begin
    Result := PI^.Proc;
    PI := PI^.Next;
  end;
end;

procedure FreeTerminateProcs;
var
  PI: PTerminateProcInfo;
begin
  while TerminateProcList <> nil do
  begin
    PI := TerminateProcList;
    TerminateProcList := PI^.Next;
    Dispose(PI);
  end;
end;

{ --- }

function AL1(const P): LongWord;
asm
        MOV     EDX,DWORD PTR [P]
        XOR     EDX,DWORD PTR [P+4]
        XOR     EDX,DWORD PTR [P+8]
        XOR     EDX,DWORD PTR [P+12]
        MOV     EAX,EDX
end;

function AL2(const P): LongWord;
asm
        MOV     EDX,DWORD PTR [P]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+4]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+8]
        ROR     EDX,5
        XOR     EDX,DWORD PTR [P+12]
        MOV     EAX,EDX
end;

const
  AL1s: array[0..2] of LongWord = ($FFFFFFF0, $FFFFEBF0, 0);
  AL2s: array[0..2] of LongWord = ($42C3ECEF, $20F7AEB6, $D1C2F74E);

procedure ALV;
begin
  raise Exception.Create(SNL);
end;

function ALR: Pointer;
var
  LibModule: PLibModule;
begin
  if MainInstance <> 0 then
    Result := Pointer(LoadResource(MainInstance, FindResource(MainInstance, 'DVCLAL',
      RT_RCDATA)))
  else
  begin
    Result := nil;
    LibModule := LibModuleList;
    while LibModule <> nil do
    begin
      with LibModule^ do
      begin
        Result := Pointer(LoadResource(Instance, FindResource(Instance, 'DVCLAL',
          RT_RCDATA)));
        if Result <> nil then Break;
      end;
      LibModule := LibModule.Next;
    end;
  end;
  if Result = nil then ALV;
end;

function GDAL: LongWord;
type
  TDVCLAL = array[0..3] of LongWord;
  PDVCLAL = ^TDVCLAL;
var
  P: Pointer;
  A1, A2: LongWord;
  PAL1s, PAL2s: PDVCLAL;
  ALOK: Boolean;
begin
  P := ALR;
  A1 := AL1(P^);
  A2 := AL2(P^);
  Result := A1;
  PAL1s := @AL1s;
  PAL2s := @AL2s;
  ALOK := ((A1 = PAL1s[0]) and (A2 = PAL2s[0])) or
          ((A1 = PAL1s[1]) and (A2 = PAL2s[1])) or
          ((A1 = PAL1s[2]) and (A2 = PAL2s[2]));
  FreeResource(Integer(P));
  if not ALOK then ALV;
end;

procedure RCS;
var
  P: Pointer;
  ALOK: Boolean;
begin
  P := ALR;
  ALOK := (AL1(P^) = AL1s[2]) and (AL2(P^) = AL2s[2]);
  FreeResource(Integer(P));
  if not ALOK then ALV;
end;

procedure RPR;
var
  AL: LongWord;
begin
  AL := GDAL;
  if (AL <> AL1s[1]) and (AL <> AL1s[2]) then ALV;
end;

procedure InitDriveSpacePtr;
var
  Kernel: THandle;
begin
  Kernel := GetModuleHandle(Windows.Kernel32);
  if Kernel <> 0 then
    @GetDiskFreeSpaceEx := GetProcAddress(Kernel, 'GetDiskFreeSpaceExA');
  if not Assigned(GetDiskFreeSpaceEx) then
    GetDiskFreeSpaceEx := @BackfillGetDiskFreeSpaceEx;
end;

{ TMultiReadExclusiveWriteSynchronizer }

constructor TMultiReadExclusiveWriteSynchronizer.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FReadExit := CreateEvent(nil, True, True, nil);  // manual reset, start signaled
  SetLength(FActiveThreads, 4);
end;

destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  BeginWrite;
  inherited Destroy;
  CloseHandle(FReadExit);
  DeleteCriticalSection(FLock);
end;

function TMultiReadExclusiveWriteSynchronizer.WriterIsOnlyReader: Boolean;
var
  I, Len: Integer;
begin
  Result := False;
  if FWriteRequestorID = 0 then Exit;
  // We know a writer is waiting for entry with the FLock locked,
  // so FActiveThreads is stable - no BeginRead could be resizing it now
  I := 0;
  Len := High(FActiveThreads);
  while (I < Len) and
    ((FActiveThreads[I].ThreadID = 0) or (FActiveThreads[I].ThreadID = FWriteRequestorID)) do
    Inc(I);
  Result := I >= Len;
end;

procedure TMultiReadExclusiveWriteSynchronizer.BeginWrite;
begin
  EnterCriticalSection(FLock);  // Block new read or write ops from starting
  if not FWriting then
  begin
    FWriteRequestorID := GetCurrentThreadID;   // Indicate that writer is waiting for entry
    if not WriterIsOnlyReader then              // See if any other thread is reading
      WaitForSingleObject(FReadExit, INFINITE); // Wait for current readers to finish
    FSaveReadCount := FCount;  // record prior read recursions for this thread
    FCount := 0;
    FWriteRequestorID := 0;
    FWriting := True;
  end;
  Inc(FCount);  // allow read recursions during write without signalling FReadExit event
end;

procedure TMultiReadExclusiveWriteSynchronizer.EndWrite;
begin
  Dec(FCount);
  if FCount = 0 then
  begin
    FCount := FSaveReadCount;  // restore read recursion count
    FSaveReadCount := 0;
    FWriting := False;
  end;
  LeaveCriticalSection(FLock);
end;

procedure TMultiReadExclusiveWriteSynchronizer.BeginRead;
var
  I: Integer;
  ThreadID: Integer;
  ZeroSlot: Integer;
  AlreadyInRead: Boolean;
begin
  ThreadID := GetCurrentThreadID;
  // First, do a lightweight check to see if this thread already has a read lock
  while InterlockedExchange(FReallocFlag, ThreadID) <> 0 do  Sleep(0);
  try    // FActiveThreads array is now stable
    I := 0;
    while (I < High(FActiveThreads)) and (FActiveThreads[I].ThreadID <> ThreadID) do
      Inc(I);
    AlreadyInRead := I < High(FActiveThreads);
    if AlreadyInRead then  // This thread already has a read lock
    begin                   // Don't grab FLock, since that could deadlock with
      if not FWriting then  // a waiting BeginWrite
      begin                 // Bump up ref counts and exit
        InterlockedIncrement(FCount);
        Inc(FActiveThreads[I].RecursionCount); // thread safe = unique to threadid
      end;
    end
  finally
    FReallocFlag := 0;
  end;
  if not AlreadyInRead then
  begin   // Ok, we don't already have a lock, so do the hard work of making one
    EnterCriticalSection(FLock);
    try
      if not FWriting then
      begin
        // This will call ResetEvent more than necessary on win95, but still work
        if InterlockedIncrement(FCount) = 1 then
          ResetEvent(FReadExit); // Make writer wait until all readers are finished.
        I := 0;  // scan for empty slot in activethreads list
        ZeroSlot := -1;
        while (I < High(FActiveThreads)) and (FActiveThreads[I].ThreadID <> ThreadID) do
        begin
          if (FActiveThreads[I].ThreadID = 0) and (ZeroSlot < 0) then ZeroSlot := I;
          Inc(I);
        end;
        if I >= High(FActiveThreads) then  // didn't find our threadid slot
        begin
          if ZeroSlot < 0 then  // no slots available.  Grow array to make room
          begin   // spin loop.  wait for EndRead to put zero back into FReallocFlag
            while InterlockedExchange(FReallocFlag, ThreadID) <> 0 do  Sleep(0);
            try
              SetLength(FActiveThreads, High(FActiveThreads) + 3);
            finally
              FReallocFlag := 0;
            end;
          end
          else  // use an empty slot
            I := ZeroSlot;
          // no concurrency issue here.  We're the only thread interested in this record.
          FActiveThreads[I].ThreadID := ThreadID;
          FActiveThreads[I].RecursionCount := 1;
        end
        else  // found our threadid slot.
          Inc(FActiveThreads[I].RecursionCount); // thread safe = unique to threadid
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
  end;
end;

procedure TMultiReadExclusiveWriteSynchronizer.EndRead;
var
  I, ThreadID, Len: Integer;
begin
  if not FWriting then
  begin
    // Remove our threadid from the list of active threads
    I := 0;
    ThreadID := GetCurrentThreadID;
    // wait for BeginRead to finish any pending realloc of FActiveThreads
    while InterlockedExchange(FReallocFlag, ThreadID) <> 0 do  Sleep(0);
    try
      Len := High(FActiveThreads);
      while (I < Len) and (FActiveThreads[I].ThreadID <> ThreadID) do Inc(I);
      assert(I < Len);
      // no concurrency issues here.  We're the only thread interested in this record.
      Dec(FActiveThreads[I].RecursionCount); // threadsafe = unique to threadid
      if FActiveThreads[I].RecursionCount = 0 then
        FActiveThreads[I].ThreadID := 0; // must do this last!
    finally
      FReallocFlag := 0;
    end;
    if (InterlockedDecrement(FCount) = 0) or WriterIsOnlyReader then
      SetEvent(FReadExit);     // release next writer
  end;
end;

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

{ Interface support routines }

function Supports(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(Intf, Inst) = 0);
end;

function Supports(Instance: TObject; const Intf: TGUID; out Inst): Boolean;
var
  Unk: IUnknown;
begin
  Result := (Instance <> nil) and Instance.GetInterface(IUnknown, Unk) and
    Supports(Unk, Intf, Inst);
end;

{ TLanguages }

{ Query the OS for information for a specified locale. Unicode version. Works correctly on Asian WinNT. }
function GetLocaleDataW(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of WideChar;
begin
  Buffer[0] := #0;
  GetLocaleInfoW(ID, Flag, Buffer, SizeOf(Buffer) div 2);
  Result := Buffer;
end;

{ Query the OS for information for a specified locale. ANSI Version. Works correctly on Asian Win95. }
function GetLocaleDataA(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of Char;
begin
  Buffer[0] := #0;
  SetString(Result, Buffer, GetLocaleInfoA(ID, Flag, Buffer, SizeOf(Buffer)) - 1);
end;

{ Called for each supported locale. }
function TLanguages.LocalesCallback(LocaleID: PChar): Integer; stdcall;
var
  AID: LCID;
  ShortLangName: string;
  GetLocaleDataProc: function (ID: LCID; Flag: DWORD): string;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    GetLocaleDataProc := @GetLocaleDataW
  else
    GetLocaleDataProc := @GetLocaleDataA;
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  ShortLangName := GetLocaleDataProc(AID, LOCALE_SABBREVLANGNAME);
  if ShortLangName <> '' then
  begin
    SetLength(FSysLangs, Length(FSysLangs) + 1);
    with FSysLangs[High(FSysLangs)] do
    begin
      FName := GetLocaleDataProc(AID, LOCALE_SLANGUAGE);
      FLCID := AID;
      FExt := ShortLangName;
    end;
  end;
  Result := 1;
end;

constructor TLanguages.Create;
type
  TCallbackThunk = packed record
    POPEDX: Byte;
    MOVEAX: Byte;
    SelfPtr: Pointer;
    PUSHEAX: Byte;
    PUSHEDX: Byte;
    JMP: Byte;
    JmpOffset: Integer;
  end;
var
  Callback: TCallbackThunk;
begin
  inherited Create;
  Callback.POPEDX := $5A;
  Callback.MOVEAX := $B8;
  Callback.SelfPtr := Self;
  Callback.PUSHEAX := $50;
  Callback.PUSHEDX := $52;
  Callback.JMP     := $E9;
  Callback.JmpOffset := Integer(@TLanguages.LocalesCallback) - Integer(@Callback.JMP) - 5;
  EnumSystemLocales(TFNLocaleEnumProc(@Callback), LCID_SUPPORTED);
end;

function TLanguages.GetCount: Integer;
begin
  Result := High(FSysLangs) + 1;
end;

function TLanguages.GetExt(Index: Integer): string;
begin
  Result := FSysLangs[Index].FExt;
end;

function TLanguages.GetID(Index: Integer): string;
begin
  Result := HexDisplayPrefix + IntToHex(FSysLangs[Index].FLCID, 8);
end;

function TLanguages.GetLCID(Index: Integer): LCID;
begin
  Result := FSysLangs[Index].FLCID;
end;

function TLanguages.GetName(Index: Integer): string;
begin
  Result := FSysLangs[Index].FName;
end;

function TLanguages.GetNameFromLocaleID(ID: LCID): string;
var
  Index: Integer;
begin
  Index := IndexOf(ID);
  if Index <> - 1 then Result := Name[Index];
  if Result = '' then Result := sUnknown;
end;

function TLanguages.GetNameFromLCID(const ID: string): string;
begin
  Result := NameFromLocaleID[StrToIntDef(ID, 0)];
end;

function TLanguages.IndexOf(ID: LCID): Integer;
begin
  for Result := Low(FSysLangs) to High(FSysLangs) do
    if FSysLangs[Result].FLCID = ID then Exit;
  Result := -1;
end;

var
  FLanguages: TLanguages;

function Languages: TLanguages;
begin
  if FLanguages = nil then
    FLanguages := TLanguages.Create;
  Result := FLanguages;
end;

function SafeLoadLibrary(const Filename: string; ErrorMode: UINT): HMODULE;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

initialization
  if ModuleIsCpp then HexDisplayPrefix := '0x';
  InitExceptions;
  GetFormatSettings;
  InitPlatformId;
  InitDriveSpacePtr;

finalization
  FreeAndNil(FLanguages);
  FreeTerminateProcs;
  DoneExceptions;

end.
