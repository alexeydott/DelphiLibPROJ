unit PROJ.Crtl;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

uses Winapi.Windows;

const
  msvcrt = 'msvcrt.dll';
  {$EXTERNALSYM msvcrt}

type
{$IFDEF NEXTGEN}
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^PAnsiChar;
{$ENDIF}

  va_list = Pointer;
  {$EXTERNALSYM va_list}

  qsort_compare_func = function(P1, P2: Pointer): Integer; cdecl;
  {$EXTERNALSYM qsort_compare_func}

  time_t = {$IFDEF Win32} Integer {$ENDIF}
           {$IFDEF Win64} Int64 {$ENDIF};
  {$EXTERNALSYM time_t}
  Ptime_t = ^time_t;
  {$EXTERNALSYM Ptime_t}
  _time64_t = Int64;
  {$EXTERNALSYM _time64_t}
  P_time64_t = ^_time64_t;
  {$EXTERNALSYM P_time64_t}
  tm = packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
  end;
  {$EXTERNALSYM tm}
  Ptm = ^tm;
  {$EXTERNALSYM Ptm}

  PFILE = Pointer;

  plconv = ^lconv;
  {$EXTERNALSYM lconv}
  lconv = record
    decimal_point: PChar;
    thousands_sep: PChar;
    grouping: PChar;
    int_curr_symbol: PChar;
    currency_symbol: PChar;
    mon_decimal_point: PChar;
    mon_thousands_sep: PChar;
    mon_grouping: PChar;
    positive_sign: PChar;
    negative_sign: PChar;
    int_frac_digits: Char;
    frac_digits: Char;
    p_cs_precedes: Char;
    p_sep_by_space: Char;
    n_cs_precedes: Char;
    n_sep_by_space: Char;
    p_sign_posn: Char;
    n_sign_posn: Char;
  end;

{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function  malloc(size: size_t): Pointer; cdecl;
{$EXTERNALSYM malloc}

function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
{$EXTERNALSYM realloc}

procedure  free(pBlock: Pointer); cdecl;
{$EXTERNALSYM free}

function calloc(nelem, size: size_t): Pointer; cdecl;
{$EXTERNALSYM calloc}

{$IFDEF WIN32}
function _calloc(nelem, size: size_t): Pointer; cdecl;
{$EXTERNALSYM _calloc}
{$ENDIF}


{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr(s: Pointer; c: Integer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM memchr}

function  memcmp(buf1: Pointer; buf2: Pointer; n: size_t): Integer; cdecl;
{$EXTERNALSYM memcmp}

function  memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memcpy}

function  memmove(dest, src: Pointer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memmove}

function  memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memset}

function  strcat(dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strcat}

function  strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strcpy}

function  strncpy(dest, src: PAnsiChar; n: size_t): PAnsiChar; cdecl;
{$EXTERNALSYM strncpy}

function  strcmp(s1: PAnsiChar; s2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM strcmp}

function  strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; cdecl;
{$EXTERNALSYM strncmp}

function  strlen(s: PAnsiChar): size_t; cdecl;
{$EXTERNALSYM strlen}

function  strnlen(s: PAnsiChar; n: size_t): size_t; cdecl;
{$EXTERNALSYM strnlen}

function  strchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strchr}

function  strrchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strrchr}

function  _strrchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM _strrchr}

function  strerror(__errnum: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strerror}

function strcspn(const str1, str2: PAnsiChar): size_t; cdecl;
{$EXTERNALSYM strcspn}

function stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM stricmp}

function _stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM _stricmp}

function strncat(s1: PAnsiChar; const s2: PAnsiChar; n: size_t): PAnsiChar; cdecl;
{$EXTERNALSYM strncat}

function _strncat(s1: PAnsiChar; const s2: PAnsiChar; n: size_t): PAnsiChar; cdecl;
{$EXTERNALSYM _strncat}

function _mbscspn(const str, strCharSet: PWideChar): size_t; cdecl;
{$EXTERNALSYM _mbscspn}

function mbstowcs(pwcs: PWideChar; const s: PWideChar;n: size_t): size_t; cdecl;
{$EXTERNALSYM mbstowcs}

function wcslen(str: PWideChar): size_t; cdecl;
{$EXTERNALSYM wcslen}

function wcsnlen(str: PWideChar; n: size_t): size_t; cdecl;
{$EXTERNALSYM wcsnlen}

function wcstombs(s:Pointer; const pwcs:Pointer; n:Integer):Integer; cdecl;
{$EXTERNALSYM wcstombs}

function strstr(const str1, str2: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strstr}

function wcscpy(dest, src: PWideChar): PWideChar; cdecl;
{$EXTERNALSYM wcscpy}

function _wcscpy(dest, src: PWideChar): PWideChar; cdecl;
{$EXTERNALSYM _wcscpy}

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM tolower}

function  toupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM toupper}

function  towlower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM towlower}

function  towupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM towupper}

function  isalnum(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isalnum}

function  isalpha(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isalpha}

function  iscntrl(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM iscntrl}

function  isdigit(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isdigit}

function  isgraph(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isgraph}

function  islower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM islower}

function  isprint(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isprint}

function  ispunct(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM ispunct}

function  isspace(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isspace}

function  isupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isupper}

function  isxdigit(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isxdigit}

function _ismbblead(c: Cardinal): Integer; cdecl;
{$EXTERNALSYM _ismbblead}


{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM _open}

function _wopen(const __path: PChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM _wopen}

function _close(__handle: Integer): Integer; cdecl;
{$EXTERNALSYM _close}

function _lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
{$EXTERNALSYM _lseek}

function _read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM _read}

function _write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM _write}

function open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM open}
function close(__handle: Integer): Integer; cdecl;
{$EXTERNALSYM close}
function lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
{$EXTERNALSYM lseek}
function read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM read}
function write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM write}
function rename(const __oldname, __newname: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM rename}

function fclose(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM fclose}
function fopen(filename, mode: PAnsiChar): PFILE; cdecl;
{$EXTERNALSYM fopen}
function fread(ptr: Pointer; size, nelem: size_t; stream: PFILE): size_t; cdecl;
{$EXTERNALSYM fread}
function fseek(stream: PFILE; offset: Longint; mode: Integer): Integer; cdecl;
{$EXTERNALSYM fseek}
function ftell(stream: PFILE): Longint; cdecl;
{$EXTERNALSYM ftell}
function _fclose(stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fclose}
function _fopen(filename, mode: PAnsiChar): PFILE; cdecl;
{$EXTERNALSYM _fopen}
function _fread(ptr: Pointer; size, nelem: size_t; stream: PFILE): size_t; cdecl;
{$EXTERNALSYM _fread}
function _fseek(stream: PFILE; offset: Longint; mode: Integer): Integer; cdecl;
{$EXTERNALSYM _fseek}
function _ftell(stream: PFILE): Longint; cdecl;
{$EXTERNALSYM _ftell}
function fwrite(ptr: Pointer; size, nelem: size_t; stream: PFILE): size_t; cdecl;
{$EXTERNALSYM fwrite}
function _fwrite(ptr: Pointer; size, nelem: size_t; stream: PFILE): size_t; cdecl;
{$EXTERNALSYM _fwrite}

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf(format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM printf}

function  fprintf(fHandle: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM fprintf}

function  sprintf(buf: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM sprintf}

function snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM snprintf}

function _snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM _snprintf}

function vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM vsnprintf}

function _vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM _vsnprintf}

function vfprintf(stream: PFILE; const format: PAnsiChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM vfprintf}

function _vfprintf(stream: PFILE; const format: PAnsiChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM _vfprintf}

function vsprintf(s: PAnsiChar; const format: PAnsiChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM vsprintf}

function _vsprintf(s: PAnsiChar; const format: PAnsiChar; ap: va_list): Integer; cdecl;
{$EXTERNALSYM _vsprintf}

{$IFDEF WIN64}
function fputc(c: Integer; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM fputc}
function _fputc(c: Integer; f: PFILE): Integer; cdecl;
{$EXTERNALSYM _fputc}
function fputs(const s: PAnsiChar; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM fputs}
{$ELSE}
function _fputc(c: Integer; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fputc}
function __fputc(c: Integer; f: PFILE): Integer; cdecl;
{$EXTERNALSYM __fputc}
function _fputs(const s: PAnsiChar; stream: PFILE): Integer; cdecl;
{$EXTERNALSYM _fputs}
{$ENDIF}

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM _itoa}

function itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM itoa}

function _i64toa(value: Int64; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM _i64toa}

function _atoi64(const str: PAnsiChar): Int64; cdecl;
{$EXTERNALSYM _atoi64}

function atoi(const str: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM atoi}

function atof(value: PAnsiChar): Double; cdecl;
{$EXTERNALSYM atof}

function atol(const str: PAnsiChar): LongInt; cdecl;
{$EXTERNALSYM atol}

function strtod(value: PAnsiChar; endPtr: Pointer): Double; cdecl;
{$EXTERNALSYM strtod}
function _strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
{$EXTERNALSYM _strtod}

function gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM gcvt}
function _gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM _gcvt}

function localeconv: plconv; cdecl;
{$EXTERNALSYM localeconv}

function _localeconv: plconv; cdecl;
{$EXTERNALSYM _localeconv}

const
  _fltused: Integer = $9875;  // from stubs.c in MS crtl
  {$EXTERNALSYM _fltused}

  _streams: array [0..2] of NativeInt = (0, 1, 2);
  {$EXTERNALSYM _streams}
  __streams: array [0..2] of NativeInt = (0, 1, 2);
  {$EXTERNALSYM __streams}

{ ----------------------------------------------------- }
{       Errors                                          }
{ ----------------------------------------------------- }


var
  errno: Integer = 0;
  {$EXTERNALSYM errno}

var  __turboFloat: Integer = 0; // Win32
  {$EXTERNALSYM __turboFloat}
function __errno(): PInteger;
{$EXTERNALSYM __errno}
function ___errno: PInteger;
{$EXTERNALSYM ___errno}

procedure _assert(cond: boolean); cdecl;
{$EXTERNALSYM _assert}
procedure __assert(cond: boolean); cdecl;
{$EXTERNALSYM __assert}

procedure _exit(status: integer); cdecl;
{$EXTERNALSYM _exit}


{ ----------------------------------------------------- }
{       Misc                                            }
{ ----------------------------------------------------- }

procedure _mbctype; // Not a function, pointer to data
{$EXTERNALSYM _mbctype}
{$IFDEF WIN64}
procedure _purecall; cdecl;
function _lseeki64(__handle: Integer; __offset: Int64; __fromwhere: Integer): Int64; cdecl;
{$EXTERNALSYM _lseeki64}
{$ENDIF}
{$IFDEF WIN32}
procedure __pure_error_;
{$EXTERNALSYM __pure_error_}
function GetMem2(Size: NativeInt): Pointer;
{$EXTERNALSYM GetMem2}
function SysFreeMem2(p: Pointer): Integer;
{$EXTERNALSYM SysFreeMem2}
function _malloc(size: size_t): Pointer; cdecl;
{$EXTERNALSYM _malloc}
function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
{$EXTERNALSYM _realloc}
procedure _free(pBlock: Pointer); cdecl;
{$EXTERNALSYM _free}

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
{$EXTERNALSYM __atold}
procedure _ftol; cdecl; external;
{$EXTERNALSYM _ftol}
procedure __ftol; cdecl; external; {$L ftol.obj}
{$EXTERNALSYM __ftol}
procedure _ftoul; cdecl;
{$EXTERNALSYM _ftoul}
procedure __ftoul; cdecl; external; {$L _ftoul.obj}
{$EXTERNALSYM __ftoul}
{$ENDIF WIN32}

procedure __mbctype; // Not a function, pointer to data
{$EXTERNALSYM __mbctype}
function  _ltolower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM _ltolower}
function  _ltoupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM _ltoupper}
function _ltowlower(c:Integer):Integer; cdecl;
{$EXTERNALSYM _ltowlower}
function _ltowupper(c:Integer):Integer; cdecl;
{$EXTERNALSYM _ltowupper}
procedure __ltolower; cdecl;
{$EXTERNALSYM __ltolower}
procedure __ltoupper; cdecl;
{$EXTERNALSYM __ltoupper}
procedure __ltowlower; cdecl;
{$EXTERNALSYM __ltowlower}
procedure __ltowupper; cdecl;
{$EXTERNALSYM __ltowupper}

{$IFDEF WIN32}
procedure _atof; cdecl;
{$EXTERNALSYM _atof}
procedure _atol; cdecl;
{$EXTERNALSYM _atol}
procedure _strcspn; cdecl;
{$EXTERNALSYM _strcspn}
procedure _strcat; cdecl;
{$EXTERNALSYM _strcat}
procedure _strcmp; cdecl;
{$EXTERNALSYM _strcmp}
procedure _strncmp; cdecl;
{$EXTERNALSYM _strncmp}
procedure _strcpy; cdecl;
{$EXTERNALSYM _strcpy}
procedure _strncpy; cdecl;
{$EXTERNALSYM _strncpy}
procedure _memmove; cdecl;
{$EXTERNALSYM _memmove}
procedure _memset; cdecl;
{$EXTERNALSYM _memset}
procedure _memcpy; cdecl;
{$EXTERNALSYM _memcpy}
procedure _memcmp; cdecl;
{$EXTERNALSYM _memcmp}
procedure _memchr; cdecl;
{$EXTERNALSYM _memchr}
procedure _strlen; cdecl;
{$EXTERNALSYM _strlen}
procedure _islower; cdecl;
{$EXTERNALSYM _islower}
procedure _isdigit; cdecl;
{$EXTERNALSYM _isdigit}
procedure _isupper; cdecl;
{$EXTERNALSYM _isupper}
procedure _isalnum; cdecl;
{$EXTERNALSYM _isalnum}
procedure _isspace; cdecl;
{$EXTERNALSYM _isspace}
procedure _isxdigit; cdecl;
{$EXTERNALSYM _isxdigit}
procedure _isgraph; cdecl;
{$EXTERNALSYM _isgraph}
procedure _isprint; cdecl;
{$EXTERNALSYM _isprint}
procedure _ispunct; cdecl;
{$EXTERNALSYM _ispunct}
procedure _iscntrl; cdecl;
{$EXTERNALSYM _iscntrl}
procedure _isalpha; cdecl;
{$EXTERNALSYM _isalpha}
procedure _strchr; cdecl;
{$EXTERNALSYM _strchr}
procedure _strnlen; cdecl;
{$EXTERNALSYM _strnlen}
procedure _wcslen; cdecl;
{$EXTERNALSYM _wcslen}
procedure _wcsnlen; cdecl;
{$EXTERNALSYM _wcsnlen}
procedure _printf; cdecl;
{$EXTERNALSYM _printf}
procedure _fprintf; cdecl;
{$EXTERNALSYM _fprintf}
procedure _sprintf; cdecl;
{$EXTERNALSYM _sprintf}
procedure __vsnprintf; cdecl;
{$EXTERNALSYM __vsnprintf}
procedure _tolower; cdecl;
{$EXTERNALSYM _tolower}
procedure _toupper; cdecl;
{$EXTERNALSYM _toupper}
procedure __mbscspn; cdecl;
{$EXTERNALSYM __mbscspn}
procedure __i64toa; cdecl;
{$EXTERNALSYM __i64toa}
procedure __atoi64; cdecl;
{$EXTERNALSYM __atoi64}
procedure _strstr; cdecl;
{$EXTERNALSYM _strstr}
procedure _mbstowcs; cdecl;
{$EXTERNALSYM _mbstowcs}
procedure _wcstombs; cdecl;
{$EXTERNALSYM _wcstombs}
procedure _strerror; cdecl;
{$EXTERNALSYM _strerror}
procedure _llmod; cdecl;
{$EXTERNALSYM _llmod}
procedure _lldiv; cdecl;
{$EXTERNALSYM _lldiv}
procedure _lludiv; cdecl;
{$EXTERNALSYM _lludiv}
procedure _llmul; cdecl;
{$EXTERNALSYM _llmul}
procedure _llumod; cdecl;
{$EXTERNALSYM _llumod}
procedure _llshl; cdecl;
{$EXTERNALSYM _llshl}
procedure _llshr; cdecl;
{$EXTERNALSYM _llshr}
procedure _llushr; cdecl;
{$EXTERNALSYM _llushr}

{$ENDIF WIN32}

procedure qsort(baseP: PByte; NElem, Width: size_t; comparF: qsort_compare_func); cdecl;
{$EXTERNALSYM qsort}
function localtime(t: Ptime_t): Ptm; cdecl;
{$EXTERNALSYM localtime}

{$IFDEF  WIN32}
function _time(tod: ptime_t): time_t; cdecl;
{$EXTERNALSYM _time}
function _localtime(t: Ptime_t): Ptm; cdecl;
{$EXTERNALSYM _localtime}
{$ENDIF WIN32}

function _beginthreadex(security_attr: Pointer; stksize: LongWord;
  start: Pointer; arg: Pointer; create_flags: LongWord;
  var thread_id: LongWord): LongWord; cdecl;
{$EXTERNALSYM _beginthreadex}
procedure _endthreadex(thread_retval: LongWord);
{$EXTERNALSYM _endthreadex}

function log(__x: Double): Double; cdecl;
{$EXTERNALSYM log}
function getenv(const name: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM getenv}


{$IFDEF  WIN32}
function _log(__x: Double): Double; cdecl;
{$EXTERNALSYM _log}
function _getenv(const name: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM _getenv}
{$ENDIF WIN32}



{ ----------------------------------------------------- }
{       Math                                            }
{ ----------------------------------------------------- }
const __ieee_64_p_inf = $7FF0000000000000;
{$EXTERNALSYM __ieee_64_p_inf}
const __ieee_32_p_nanq = $7FC00000;
{$EXTERNALSYM __ieee_64_p_inf}

//var _Inf: UInt64 = $7FF0000000000000;
//{$EXTERNALSYM _Inf}


function ___ieee_32_p_nanq(): UInt32; cdecl;
{$EXTERNALSYM ___ieee_32_p_nanq}

function asin(const x: Double): Double; cdecl;
{$EXTERNALSYM asin}
function atan(const x: Double): Double; cdecl;
{$EXTERNALSYM atan}
function atan2(const x, y: Double): Double; cdecl;
{$EXTERNALSYM atan2}
function acos(const x: Double): Double; cdecl;
{$EXTERNALSYM acos}
function pow(const x, y: Double): Double; cdecl;
{$EXTERNALSYM pow}
function sqrt(const x: Double): Double;
{$EXTERNALSYM sqrt}
function abs(const x: double): Double; cdecl;
{$EXTERNALSYM abs}

{$IFDEF WIN32}

function __huge_dble: Double; cdecl;
{$EXTERNALSYM __huge_dble}

function _asin(const x: Double): Double; cdecl;
{$EXTERNALSYM _asin}
function _atan(const x: Double): Double; cdecl;
{$EXTERNALSYM _atan}
function _atan2(const x, y: Double): Double; cdecl;
{$EXTERNALSYM _atan2}
function _acos(const x: Double): Double; cdecl;
{$EXTERNALSYM _acos}
function _pow(const x, y: Double): Double; cdecl;
{$EXTERNALSYM _pow}
function _sin(const x: double): Double; cdecl;
{$EXTERNALSYM _sin}
function _cos(const x: double): Double; cdecl;
{$EXTERNALSYM _cos}
function _sqrt(const x: double): Double; cdecl;
{$EXTERNALSYM _sqrt}
function _tan(const x: double): Double; cdecl;
{$EXTERNALSYM _tan}
function _fmod(const n, d: Double): Double; cdecl;
{$EXTERNALSYM _fmod}
function _abs(const x: double): Double; cdecl;
{$EXTERNALSYM _abs}
function _exp(const x: double): Double; cdecl;
{$EXTERNALSYM _exp}
function _sinh(const x: double): Double; cdecl;
{$EXTERNALSYM _sinh}
function _cosh(const x: double): Double; cdecl;
{$EXTERNALSYM _cosh}
function _floor(const x: double): Integer; cdecl;
{$EXTERNALSYM _floor}
function _ceil(const x: double): Integer; cdecl;
{$EXTERNALSYM _ceil}

// c99
function _hypot(const a,b: double): Double; cdecl;
{$EXTERNALSYM _hypot}
function _log1p(const x: double): Double; cdecl;
{$EXTERNALSYM _log1p}
function _lround(const x: double): Integer; cdecl;
{$EXTERNALSYM _lround}
function _asinh(const x: double): double; cdecl;
{$EXTERNALSYM _asinh}
function __Dclass(const x: double): Integer; cdecl;
{$EXTERNALSYM _asinh}
function _fpclass(const x: double): Integer; external msvcrt name '_fpclass';
{$EXTERNALSYM _fpclass}
function _atanh(const x: double): double; cdecl;
{$EXTERNALSYM _atanh}
function _remquo(const x,y: double; var c: Integer): double; cdecl;
{$EXTERNALSYM _remquo}
function _remainder(const x,y: double): double; cdecl;
{$EXTERNALSYM _remainder}
function _cbrt(const x: double): double; cdecl;
{$EXTERNALSYM _cbrt}
function _isnan(const x: double): Integer;cdecl;
{$EXTERNALSYM _isnan}
function _copysign(const x,y: double): double;cdecl;
{$EXTERNALSYM _copysign}
{$ENDIF}

implementation

uses System.SysUtils, System.Character, Math;

{ ----------------------------------------------------- }
{       Errors                                          }
{ ----------------------------------------------------- }

function  __errno(): PInteger;
begin
{$J+}
  Result := @errno;
{$J-}
end;

function ___errno(): PInteger;
begin
  Result := @errno;
end;

procedure _assert(cond: boolean);
begin
	assert(cond);
end;

procedure __assert(cond: boolean);
begin
	_assert(cond);
end;

procedure _exit; external msvcrt name 'exit';

{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function internalAllocMem(size: size_t): pointer; cdecl;
var
  mm: TMemoryManagerEx;
begin
  Result := nil;
  if size > 0 then
  begin
    if IsMemoryManagerSet then
    begin
      GetMemoryManager(mm);

      Result := mm.AllocMem(size);
    end
    else
    begin
      Result := SysAllocMem(size);
    end;
  end;
end;

function  malloc(size: size_t): Pointer;
begin
  Result := internalAllocMem(size);
end;

function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  ReallocMem(P, Newsize);
  Result := P;
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;


function calloc(nelem, size: size_t): Pointer;
begin
  Result := internalAllocMem(nelem * size);
end;

{$IFDEF WIN32}
function _calloc(nelem, size: size_t): Pointer;
begin
  Result := internalAllocMem(nelem * size);
end;
{$ENDIF}

{$IFDEF WIN64}
procedure _purecall; cdecl;
asm
  jmp System.@AbstractError
end;

function _lseeki64; external msvcrt;

{$ENDIF}

{$IFDEF WIN32}
procedure _llmod; cdecl;
asm
  jmp System.@_llmod;
end;

procedure _lldiv; cdecl;
asm
  jmp System.@_lldiv
end;

procedure _lludiv; cdecl;
asm
  jmp System.@_lludiv
end;

procedure _llmul; cdecl;
asm
  jmp System.@_llmul
end;

procedure _llumod; cdecl;
asm
  jmp System.@_llumod
end;

procedure _llshl; cdecl;
asm
  jmp System.@_llshl
end;

procedure _llshr; cdecl;
asm
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshr@below32
        MOV   EAX, EDX
        CDQ
        SAR   EAX,CL
        RET

@__llshr@below32:
        SHRD  EAX, EDX, CL
        SAR   EDX, CL
        RET
end;

procedure _llushr; cdecl;
asm
  jmp System.@_llushr
end;

function _malloc(size: size_t): Pointer; cdecl;
begin
  try
    Result := AllocMem(size);
  except
    Result := nil;
  end;
end;

function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  try
    ReallocMem(P, Newsize);
    Result := P;
  except
    Result := nil;
  end;
end;

procedure _free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

procedure __pure_error_;
asm
  JMP  System.@AbstractError
end;

// C++'s alloc allocates 1 byte if size is 0.
function GetMem2(Size: NativeInt): Pointer;
begin
  if Size = 0 then Inc(Size);
  GetMem(Result, Size);
end;

// C++'s free allow NULL pointer.
function SysFreeMem2(p: Pointer): Integer;
begin
  result := 0;
  if (p <> NIL) then result := FreeMemory(p);
end;

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
var
  s: string;
begin
  s := string(Value);
  if endPtr <> nil then
    endPtr^ := value;
  if not TryStrToFloat(s, Result) then
    Result := 0
  else if endPtr <> nil then
    endPtr^ := PAnsiChar(PByte(Value) + Length(s));
end;

procedure _ftoul; cdecl;
asm
  JMP  System.@Trunc
end;
{$ENDIF WIN32}

{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr; external msvcrt;

function  memcmp; external msvcrt;

function  memcpy; external msvcrt;

function  memmove; external msvcrt;

function  memset; external msvcrt;

function  strcat; external msvcrt;

function  strcpy; external msvcrt;

function  strncpy; external msvcrt;

function  strcmp; external msvcrt;

function  strncmp; external msvcrt;

function  strlen; external msvcrt;

function  strnlen; external msvcrt;

function  strchr; external msvcrt;

function  strrchr; external msvcrt;

function  strerror; external msvcrt;

function strcspn; external msvcrt;

function stricmp; external msvcrt name '_stricmp';

function _stricmp; external msvcrt;

function strncat; external msvcrt name 'strncat';
function _strncat; external msvcrt name 'strncat';

function _mbscspn; external msvcrt;

function mbstowcs; external msvcrt;

function wcslen; external msvcrt;

function wcsnlen; external msvcrt;

function wcstombs; external msvcrt;

function strstr; external msvcrt;

function wcscpy; external msvcrt;
function _wcscpy; external msvcrt name 'wcscpy';

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower; external msvcrt;

function  toupper; external msvcrt;

function  towlower; external msvcrt;

function  towupper; external msvcrt;

function  isalnum; external msvcrt;

function  isalpha; external msvcrt;

function  iscntrl; external msvcrt;

function  isdigit; external msvcrt;

function  isgraph; external msvcrt;

function  islower; external msvcrt;

function  isprint; external msvcrt;

function  ispunct; external msvcrt;

function  isspace; external msvcrt;

function  isupper; external msvcrt;

function  isxdigit; external msvcrt;

function _ismbblead; external msvcrt;


{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _wopen; external msvcrt;

function _open; external msvcrt;

function _close; external msvcrt;

function _lseek; external msvcrt;

function _read; external msvcrt;

function _write; external msvcrt;

function open; external msvcrt name '_open';

function close; external msvcrt name '_close';

function lseek; external msvcrt name '_lseek';

function read; external msvcrt name '_read';

function write; external msvcrt name '_write';

function rename; external msvcrt;

function fclose; external msvcrt;

function fopen; external msvcrt;

function fread; external msvcrt;

function fseek; external msvcrt;

function ftell; external msvcrt;

function _fclose; external msvcrt name 'fclose';

function _fopen; external msvcrt name 'fopen';

function _fread; external msvcrt name 'fread';

function _fseek; external msvcrt name 'fseek';

function _ftell; external msvcrt name 'ftell';

function fwrite; external msvcrt;

function _fwrite; external msvcrt name 'fwrite';

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf; external msvcrt;

function  fprintf; external msvcrt;

function  sprintf; external msvcrt;

function snprintf; external msvcrt name '_snprintf';

function _snprintf; external msvcrt;

function vsnprintf; external msvcrt name '_vsnprintf';

function _vsnprintf; external msvcrt;

function vfprintf; external msvcrt;

function _vfprintf; external msvcrt;

function vsprintf; external msvcrt name 'vsprintf';
function _vsprintf; external msvcrt name 'vsprintf';

{$IFDEF WIN64}
function fputc; external msvcrt name 'fputc';
function fputs; external msvcrt name 'fputs';

function _fputc(c: Integer; f: PFILE): Integer;
{$ELSE}
function _fputc; external msvcrt name 'fputc';
function _fputs; external msvcrt name 'fputs';
function __fputc(c: Integer; f: PFILE): Integer;
{$ENDIF}
begin
  if f = @_streams[1] then
  begin
		System.Write(Output, AnsiChar(C));
		Result := 0;
	end
	else
	if (f = @_streams[2]) then
	begin
		System.Write(ErrOutput, AnsiChar(C));
		Result := 0;
  end
  else
  {$IFDEF WIN64}
    Result := fputc(c, f);
  {$ELSE}
    Result := _fputc(c, f);
  {$ENDIF}
end;

{$IFDEF WIN64}

{$ELSE}

{$ENDIF}

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa; external msvcrt;

function itoa; external msvcrt name '_itoa';

function _i64toa; external msvcrt;

function _atoi64; external msvcrt;

function atoi; external msvcrt;

function atof; external msvcrt;

function atol; external msvcrt;

function strtod; external msvcrt;
function _strtod; external msvcrt name 'strtod';

function gcvt; external msvcrt name '_gcvt';

function _gcvt; external msvcrt;

function localeconv; external msvcrt name 'localeconv';

function _localeconv; external msvcrt name 'localeconv';

procedure _mbctype; external msvcrt; // Not a function, pointer to data

procedure __mbctype; external msvcrt name '_mbctype'; // Not a function, pointer to data

function  _ltolower; external msvcrt name 'tolower';

function  _ltoupper; external msvcrt name 'toupper';

function _ltowlower; external msvcrt name 'towlower';

function _ltowupper; external msvcrt name 'towupper';

procedure __ltolower; external msvcrt name 'tolower';

procedure __ltoupper; external msvcrt name 'toupper';

procedure __ltowlower; external msvcrt name 'towlower';

procedure __ltowupper; external msvcrt name 'towupper';

procedure _atof; external msvcrt name 'atof';

procedure _atol; external msvcrt name 'atol';

procedure _strcspn; external msvcrt name 'strcspn';

procedure _strcat; external msvcrt name 'strcat';

procedure _strcmp; external msvcrt name 'strcmp';

procedure _strncmp; external msvcrt name 'strncmp';

procedure _strcpy; external msvcrt name 'strcpy';

procedure _strncpy; external msvcrt name 'strncpy';

procedure _memmove; external msvcrt name 'memmove';

procedure _memset; external msvcrt name 'memset';

procedure _memcpy; external msvcrt name 'memcpy';

procedure _memcmp; external msvcrt name 'memcmp';

procedure _memchr; external msvcrt name 'memchr';

procedure _strlen; external msvcrt name 'strlen';

procedure _islower; external msvcrt name 'islower';

procedure _isdigit; external msvcrt name 'isdigit';

procedure _isupper; external msvcrt name 'isupper';

procedure _isalnum; external msvcrt name 'isalnum';

procedure _isspace; external msvcrt name 'isspace';

procedure _isxdigit; external msvcrt name 'isxdigit';

procedure _isgraph; external msvcrt name 'isgraph';

procedure _isprint; external msvcrt name 'isprint';

procedure _ispunct; external msvcrt name 'ispunct';

procedure _iscntrl; external msvcrt name 'iscntrl';

procedure _isalpha; external msvcrt name 'isalpha';

procedure _strchr; external msvcrt name 'strchr';

function _strrchr; external msvcrt name 'strrchr';

procedure _strnlen; external msvcrt name 'strnlen';

procedure _wcslen; external msvcrt name 'wcslen';

procedure _wcsnlen; external msvcrt name 'wcsnlen';

procedure _printf; external msvcrt name 'printf';

procedure _fprintf; external msvcrt name 'fprintf';

procedure _sprintf; external msvcrt name 'sprintf';

procedure __vsnprintf; external msvcrt name '_vsnprintf';

procedure _tolower; external msvcrt name 'tolower';

procedure _toupper; external msvcrt name 'toupper';

procedure __mbscspn; external msvcrt name '_mbscspn';

procedure __i64toa; external msvcrt name '_i64toa';

procedure __atoi64; external msvcrt name '_atoi64';

procedure _strstr; external msvcrt name 'strstr';

procedure _mbstowcs; external msvcrt name 'mbstowcs';

procedure _wcstombs; external msvcrt name 'wcstombs';

procedure _strerror; external msvcrt name 'strerror';

procedure qsort; external msvcrt;

function localtime; external msvcrt;

{$IFDEF WIN32}
function _time; external msvcrt name 'time';
function _localtime; external msvcrt name 'localtime';

{$ENDIF WIN32}

function _beginthreadex; external msvcrt;

procedure _endthreadex; external msvcrt;

function log; external msvcrt;
function getenv; external msvcrt;

{$IFDEF WIN32}
function _log; external msvcrt name 'log';
function _getenv; external msvcrt name 'getenv';
{$ENDIF WIN32}




{ ----------------------------------------------------- }
{       Math                                            }
{ ----------------------------------------------------- }

function _huge_dble: double;
begin
  Result := Infinity;// chuge_dble[0];
end;

//function __ieee_64_p_nanq: UInt64;
//begin
//  Result := $7FF8F00000000000;
//end;

//function __inf_value(const v: double): PByte;
//var
//  b: array[0..7] of byte absolute v;
//begin
//  Result := PByte(Addr(b[0]));
//end;

function ___ieee_32_p_nanq: UInt32;
begin
  Result := __ieee_32_p_nanq;
end;

// function abs; external msvcrt name 'abs';
function abs(const x: double): Double;
begin
  result := System.Abs(x);
end;

//function asin; external msvcrt name 'asin';

function asin(const x: Double): Double;
begin
  Result :=  Math.ArcSin(x);
end;

//function atan; external msvcrt name 'atan';
function atan(const x: Double): Double;
begin
  Result := System.ArcTan(x);
end;

function atan2(const x, y: Double): Double;
begin
  Result := Math.ArcTan2(x,y);
end;
//function acos; external msvcrt name 'acos';
function acos(const x: Double): Double;
begin
  Result := Math.ArcCos(x);
end;
//function pow; external msvcrt name 'pow';
function pow(const x, y: Double): Double;
begin
  Result := Math.Power(x,y);
end;

// function sqrt; external msvcrt name 'sqrt';
function sqrt(const x: Double): Double;
begin
  if x < 0 then
    Result := Infinity
  else
    Result := System.Sqrt(x);
end;


{$IFDEF WIN32}
function __huge_dble: Double;
begin
  Result := _huge_dble;
end;

function _asin(const x: Double): Double;
begin
  Result :=  Math.ArcSin(x);
end;

function _atan(const x: Double): Double;
begin
  Result := System.ArcTan(x);
end;

function _atan2(const x, y: Double): Double;
begin
//  Result := Math.ArcTan2(x,y);
  result := atan2(x,y);
end;

function _acos(const x: Double): Double;
begin
  Result := Math.ArcCos(x);
end;

function _pow(const x, y: Double): Double;
begin
  Result := Math.Power(x,y);
end;

function _sin(const x: double): Double;
begin
  Result := System.Sin(x);
end;

function _cos(const x: double): Double;
begin
  Result := System.Cos(x);
end;

function _sqrt(const x: double): Double;
begin
  Result := sqrt(x);
end;

function _tan(const x: double): double;
begin
  Result := System.Tangent(x);
end;

function _fmod(const n, d: Double): Double;
begin
  Result := Math.FMod(n,d);
end;

function _abs(const x: double): Double;
begin
  Result := System.Abs(x);
end;

function _exp(const x: double): Double;
begin
  Result := System.Exp(x);
end;

function _sinh(const x: double): Double;
begin
  Result := Math.Sinh(x);
end;

function _cosh(const x: double): Double;
begin
  Result := Math.Cosh(x);
end;

function _floor(const x: double): Integer;
begin
  Result := Math.Floor(x);
end;

function _ceil(const x: double): Integer;
begin
  Result := Math.Ceil(x);
end;

function _hypot(const a,b: Double): Double;
begin
  Result := Math.Hypot(a,b);
end;

function _log1p(const x: double): Double;
begin
//  if x < -1 then
//    Result := Math.NaN
//  else
//  if x = -1 then
//    Result := Math.NegInfinity
//  else
//    Result := System.Ln(x); // System.LnXPlus1(x);
    Result := Math.LnXP1(x);
end;

function _lround(const x: double): Integer;
begin
  Result := System.Round(x);
end;

function _asinh(const x: double): Double;
begin
  Result := Math.ArcSinh(x);
end;

function __Dclass(const x: double): Integer;
begin
  Result := _fpclass(x);
end;

function _atanh(const x: double): double;
begin
  Result := Math.ArcTanh(x);
end;

function _remquo(const x,y: double; var c: Integer): double;
begin
  if y = 0 then
    Result := Math.NaN
  else
  begin
    c := Trunc(x / y);
    Result := x - c * y;
  end;
end;

function _remainder(const x,y: double): double;
var
  c: Integer;
begin
  Result := _remquo(x,y,c); //Math.FMod(x,y);
end;

function _cbrt(const x: double): double;
const cbExp = double(1/3);
begin
  Result := Math.Power(x,cbExp);
end;

function _isnan(const x: double): Integer;
begin
  Result := Ord(Math.IsNan(x));
end;

function _copysign(const x,y: double): double;
begin
//  return := abs(x) * (y < 0 || (y == 0 && 1/y < 0) ? -1 : 1);
  result := abs(x) * IfThen(y=0,0,IfThen(1/y<0,-1,1));
end;
{$ENDIF}



end.
