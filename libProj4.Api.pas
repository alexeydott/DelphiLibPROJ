unit libProj4.Api;

interface
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.Classes, SysUtils, System.Math, System.Sensors,
  libProj4.Types
;

{$REGION 'proj4 related types'}
type
  TPJInfoHelper = record helper for TPJInfo
    function Release: string;
    function Version: string;
    function SearchPath: string;
    function Paths: TArray<string>;
  end;

  TPJProjInfoHelper = record helper for TPJProjInfo
    function Id: string;
    function Description: string;
    function Definition: string;
  end;

  TPJhelper = record helper for {$ifndef _proj_api_deepdive}TPJ_t{$else}TPJconsts_t{$endif}
  public
    function Id: string;
    function Description: string;
    function Definition: string;
    function IsValid: Boolean;
    function IsGeographic: LongBool;
    function IsGeocentric: LongBool;
    function GetToMetersFactor: Double;
    function GetSpheroidInfo(var a, e2: Double): Boolean;
  end;

  TGeodesicPolygonHelper = record helper for TGeodesicPolygon
  public
    class function Create(g: TGeodesic; const yLats, xLons: TArray<Double>; AsPolyline: Boolean): TGeodesicPolygon; static;
    /// <summary>
    ///   Initialize a geod_polygon object.
    /// </summary>
    /// <param name="AsPolyline">
    ///   true if a polyline instead of a polygon.
    /// </param>
    procedure Init(AsPolyline: Boolean);
    /// <summary>
    ///   Clear the polygon, allowing a new polygon to be started.
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Add a point to the polygon or polyline.
    /// </summary>
    /// <param name="g">
    ///   pointer to the geod_geodesic object specifying ellipsoid.
    /// </param>
    /// <param name="yLat">
    ///   latitude of the point (degrees).
    /// </param>
    /// <param name="xLon">
    ///   longitude of the point (degrees).
    /// </param>
    procedure AddPoint(const g: TGeodesic; const yLat,xLon: Double);
    /// <summary>
    ///   Add an edge to the polygon or polyline.
    /// </summary>
    /// <param name="g">
    ///   pointer to the geod_geodesic object specifying the ellipsoid.
    /// </param>
    /// <param name="Azi">
    ///   azi azimuth at current point (degrees).
    /// </param>
    /// <param name="DistToNxt">
    ///   distance from current point to next point (meters).
    /// </param>
    procedure AddEdge(const g: TGeodesic; const Azi, DistToNxt: Double);
    /// <param name="g">
    ///   pointer to the geod_geodesic object specifying the ellipsoid.
    /// </param>
    /// <param name="reverse">
    ///   reverse if true then clockwise (instead of counter-clockwise) traversal counts as a positive area.
    /// </param>
    /// <param name="area">
    ///   area of the polygon in meters
    /// </param>
    /// <param name="perimeter">
    ///   perimeter of the polygon or length of the polyline in meters.
    /// </param>
    /// <returns>
    ///   Return the results for a polygon.
    /// </returns>
    function Compute(const g: TGeodesic; reversed, signed: boolean; var area, perimeter: double): Cardinal;
    /// <summary>
    ///   Return the results assuming a tentative final test point is added.
    /// </summary>
    /// <param name="g">
    ///   pointer to the geod_geodesic object specifying the ellipsoid.
    /// </param>
    /// <param name="yLat">
    ///   latitude of the test point in degrees. <i>Should be in the range -90..+90.</i>
    /// </param>
    /// <param name="xLon">
    ///   longitude of the test point in degrees.
    /// </param>
    /// <param name="reversed">
    ///   if true then clockwise (instead of counter-clockwise) traversal counts as a positive area.
    /// </param>
    /// <param name="signed">
    ///   if true then return a signed result for the area if the polygon is traversed in the "wrong" direction
    ///   instead of returning the area for the rest of the earth.
    /// </param>
    /// <param name="area">
    ///   area of the polygon in meters
    /// </param>
    /// <param name="perimeter">
    ///   perimeter of the polygon or length of the polyline in meters.
    /// </param>
    /// <returns>
    ///   the number of points.
    /// </returns>
    /// <remarks>
    ///   However, the data for the test point isnot saved. This lets you report a running result for the perimeter and
    ///   area as the user moves the mousecursor. Ordinary floating point arithmetic is used to accumulate the data for
    ///   the test point; thus the areaand perimeter returned are less accurate than if geod_polygon_addpoint() and
    ///   geod_polygon_compute() are used.
    /// </remarks>
    function TestPoint(const g: TGeodesic; const yLat, xLon: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
    /// <summary>
    ///   Return the results assuming a tentative final test point is added.
    /// </summary>
    /// <param name="g">
    ///   pointer to the geod_geodesic object specifying the ellipsoid.
    /// </param>
    /// <param name="azi">
    ///   azimuth at current point in degrees.
    /// </param>
    /// <param name="distToNxt">
    ///   distance from current point to final test point in meter
    /// </param>
    /// <param name="reversed">
    ///   reverse if true then clockwise (instead of counter-clockwise) traversal counts as a positive area.
    /// </param>
    /// <param name="signed">
    ///   if true then return a signed result for the area if the polygon is traversed in the "wrong" direction
    ///   instead of returning the area for the rest of the earth.
    /// </param>
    /// <param name="area">
    ///   area of the polygon in meters
    /// </param>
    /// <param name="perimeter">
    ///   perimeter of the polygon or length of the polyline in meters.
    /// </param>
    /// <returns>
    ///   the number of points.
    /// </returns>
    /// <remarks>
    ///   However, the data for the test point isnot saved. This lets you report a running result for the perimeter and
    ///   area as the user moves the mousecursor. Ordinary floating point arithmetic is used to accumulate the data for
    ///   the test point; thus the areaand perimeter returned are less accurate than if geod_polygon_addpoint() and
    ///   geod_polygon_compute() are used.
    /// </remarks>
    /// <seealso cref="addpoint()" />
    /// <seealso cref="compute()" />
    function TestEdge(const g: TGeodesic; const azi, distToNxt: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
  end;

{$ENDREGION}

{$REGION 'proj api adaptation'}

function CreatePJContext(const AUserData: TObject): TPJCtx;
procedure FreePJContext(var ACxt: TPJCtx);
function GetPJContextUserData(const ACxt: TPJCtx): TObject;
/// create new projection object
function CreatePJ(const def: string; ACtx: TPJCtx = nil; AErrorText: PString = nil): TPJ;
/// destroy projection object
procedure FreePJ(var pj: TPJ);
/// list of supported projections id=description
function GetProjectionsList(Dest: TStrings): Integer;
/// set additional files search paths
function SetPROJDataSearchPaths(const ANewPaths: TArray<string>): TArray<string>;
/// information about library
function GetPROJInfo(var info: TPJInfo): Boolean;
/// information about projection
function GetPJInfo(const pPJ: TPJ; var info: TPJProjInfo): Boolean;
/// get latlong projection
function GetLatLong(const APJ: TPJ; var LatLong: TPJ): Boolean;
/// GEt
function GetSpheroidInfo(const APJ: TPJ; var a: Double; var b: Double; var e2: Double): Boolean{$ifndef __proj_no_inline_code}; inline{$endif};
/// transform series of points
function TransformPoints2D(src, dst: TPJ; const x, y: PDouble; count: Integer; angular: Boolean): Integer;
/// transform 1 point
function TransformPoint2D(src, dst: TPJ; var x, y: Double; angular: Boolean): Integer;
/// init geodesic
procedure GeodesicInit(var g: TGeodesic; a, f: Double);
/// compute geodesic polygon
procedure GeodesicPolygonInit(var P: TGeodesicPolygon; AsPolyLine: Boolean);
function GeodesicPolygonCreate(const g: TGeodesic; yLats,xLons: TArray<Double>; AsPolyLine: Boolean = False): TGeodesicPolygon{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicPolygonClear(var P: TGeodesicPolygon);
procedure GeodesicPolygonAddPoint(const g: TGeodesic; var p: TGeodesicPolygon; const yLat,xLon: Double);
procedure GeodesicPolygonAddEdge(const g: TGeodesic; var p: TGeodesicPolygon; azi, distToNxt: Double);
function GeodesicPolygonCompute(const g: TGeodesic; var p: TGeodesicPolygon; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
function GeodesicPolygonTestPoint(const g: TGeodesic; var p: TGeodesicPolygon; const xLat, xLon: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
function GeodesicPolygonTestEdge(const g: TGeodesic; var p: TGeodesicPolygon; const azi, distToNxt: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
procedure GeodesicPolygonArea(const g: TGeodesic; const yLats: TArray<Double>; const xLons: TArray<Double>; var area,perimeter: Double); overload;
procedure GeodesicPolygonArea(const a, f: Double; const yLats: TArray<Double>; const xLons: TArray<Double>; var area,perimeter: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicPolygonArea(const g: TGeodesic; const Points: TArray<TLocationCoord2D>; var area: Double; var perimeter: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicPolygonArea(const a, f: Double; const Points: TArray<TLocationCoord2D>; var area: Double; var perimeter: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
/// solve geodesic direct
procedure GeodesicDirect(const g: TGeodesic; const yLat1, xLon1: Double; const Azimuth, Distance: Double; var yLat2, xLon2: Double; var ForwardAzimuth: Double); overload;
procedure GeodesicDirect(const a, f: Double; const yLat1, xLon1: Double; const Azimuth, Distance: Double; var yLat2, xLon2: Double; var ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicDirect(const g: TGeodesic; const StartPoint: TLocationCoord2D; const Azimuth, Distance: Double; var DestPoint: TLocationCoord2D; var ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicDirect(const a, f: Double; const StartPoint: TLocationCoord2D; const Azimuth, Distance: Double; var DestPoint: TLocationCoord2D; var ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
/// solve geodesic inverve
procedure GeodesicInverse(const g: TGeodesic; const yLat1, xLon1, yLat2, xLon2: Double; var Distance, Azimuth, ForwardAzimuth: Double); overload;
procedure GeodesicInverse(const a, f: Double; const yLat1, xLon1, yLat2, xLon2: Double; var Distance, Azimuth, ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicInverse(const g: TGeodesic; const StartPoint, EndPoint: TLocationCoord2D; var Distance, Azimuth, ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};
procedure GeodesicInverse(const a, f: Double; const StartPoint, EndPoint: TLocationCoord2D; var Distance, Azimuth, ForwardAzimuth: Double); overload{$ifndef __proj_no_inline_code}; inline{$endif};

{$ENDREGION}

implementation

var FPJMarshaller: TMarshaller;

const proj4libname = 'proj4.dll';

type
  MarshaledAStrings = array [0 .. MaxInt div SizeOf(MarshaledAString) - 1] of MarshaledAString;

  PMarshaledAStrings = ^MarshaledAStrings;


{$REGION '  crtl'}

{$IFDEF LIBPROJ_LINKSTATIC}

type
  plconv = ^lconv_t;

  lconv_t = record
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

  ptm_t = ^tm_t;

  tm_t = record
    tm_sec: Integer;
    tm_min: Integer;
    tm_hour: Integer;
    tm_mday: Integer;
    tm_mon: Integer;
    tm_year: Integer;
    tm_wday: Integer;
    tm_yday: Integer;
    tm_isdst: Integer;
  end;

  ptime_t = ^time_t;
  time_t = NativeInt;

  pfile_t = ^file_t;

  file_t = record
    handle: Winapi.Windows.THandle;
    static_: System.Boolean;
    Next: pfile_t;
{$IFNDEF WIN64}
    Filler1: Integer;
    Filler2: Word;
{$ENDIF}
    Flags: Word;
{$IFNDEF WIN64}
    Filler3: WideChar;
{$ENDIF}
    FD: ShortInt;
  end;

  // wchar.h
  // typedef struct
  // {
  // unsigned char  *curp;       /* Current active pointer     */
  // unsigned char  *buffer;     /* Data transfer buffer       */
  // int             level;      /* fill/empty level of buffer */
  // int             bsize;      /* Buffer size                */
  // unsigned short  istemp;     /* Temporary file indicator   */
  // unsigned short  flags;      /* File status flags          */
  // wchar_t         hold;       /* Ungetc char if no buffer   */
  // char            fd;         /* File descriptor            */
  // unsigned char   token;      /* Used for validity checking */
  // }       FILE;                       /* This is the FILE object    */
  // #endif

var errno: Integer = 0;
var open_files: pfile_t = nil;
var _huge_dble: Double = System.Math.Infinity;
var {$IFDEF WIN64}_streams{$ELSE}__streams{$ENDIF}: array [0 .. 2] of file_t = (
  ( handle: Winapi.Windows.INVALID_HANDLE_VALUE; static_: True; Flags: 0; FD: 0),
  (handle: Winapi.Windows.INVALID_HANDLE_VALUE; static_: True; Flags: 0; FD: 0),
  (handle: Winapi.Windows.INVALID_HANDLE_VALUE; static_: True; Flags: 0; FD: 0)
);

const _name_prefix = {$IFDEF UNDERSCOREIMPORTNAME}'_'{$ELSE}''{$ENDIF};
const _fltused: Integer = $9875;
const msvcrtdll = 'msvcrt.dll';

  // function atol(const __s: PAnsiChar): Integer; cdecl; external msvcrt name 'atol';
  // function memchr(s: Pointer; c: Integer; n: size_t): Pointer; cdecl; external msvcrt name 'memchr';
function crtl_getenv(value: PAnsiChar): PAnsiChar; cdecl; external msvcrtdll name 'getenv';
function {$IFDEF WIN64}strtod{$ELSE}_strtod{$ENDIF}(value: PAnsiChar; endPtr: Pointer): Double; cdecl; external msvcrtdll name 'strtod';
function {$IFDEF WIN64}strncat{$ELSE}_strncat{$ENDIF}(s1: PAnsiChar; const s2: PAnsiChar; n: size_t): PAnsiChar; cdecl; external msvcrtdll name 'strncat';
function {$IFDEF WIN64}localtime{$ELSE}_localtime{$ENDIF}(t: ptime_t): ptm_t; cdecl; external msvcrtdll name 'localtime';
function {$IFDEF WIN64}localeconv{$ELSE}_localeconv{$ENDIF}: plconv; cdecl; external msvcrtdll name 'localeconv';

function {$IFDEF WIN64}sprintf{$ELSE}_sprintf{$ENDIF}(buf: Pointer; format: PAnsiChar { args } ): Integer; cdecl; varargs; external msvcrtdll name 'sprintf';
function {$IFDEF WIN64}fprintf{$ELSE}_fprintf{$ENDIF}(fHandle: Pointer; format: PAnsiChar { args } ): Integer; cdecl; varargs; external msvcrtdll name 'fprintf';
function {$IFDEF WIN64}vsprintf{$ELSE}_vsprintf{$ENDIF}(s: PAnsiChar; const format: PAnsiChar; ap: Pointer): Integer; cdecl; external msvcrtdll name 'vsprintf';
function {$IFDEF WIN64}putchar{$ELSE}_putchar{$ENDIF}(c: Integer): Integer; cdecl; external msvcrtdll name 'putchar';

// {$ifdef WIN64}{$LINK fprintf_win64}{$else}{$LINK fprintf_win32}{$endif}
// {$ifdef WIN64}{$LINK sprintf_win64}{$else}{$LINK sprintf_win32}{$endif}
// {$ifdef WIN64}{$LINK vfprintf_win64}{$else}{$LINK vfprintf_win32}{$endif}
// {$ifdef WIN64}{$LINK vsprintf_win64}{$else}{$LINK vsprintf_win32}{$endif}
// {$ifdef WIN64}{$LINK _vsnprintf_win64}{$else}{$LINK _vsnprintf_win32}{$endif}
// {$ifdef WIN64}{$LINK printf_printer_win64}{$else}{$LINK printf_printer_win32}{$endif}
// {$ifdef WIN64}{$LINK printf_printer_utils_win64}{$else}{$LINK printf_printer_utils_win32}{$endif}
// {$ifdef WIN64}{$LINK sscanf_win64}{$else}{$LINK sscanf_win32}{$endif}
// {$ifdef WIN64}{$LINK vsscanf_win64}{$else}{$LINK vsscanf_win32}{$endif}

// function {$ifdef WIN64}sprintf{$else}sprintf{$endif}(buf: Pointer; format: PAnsiChar { args } ): Integer; cdecl; external name 'sprintf'; varargs;
// function {$ifdef WIN64}fprintf{$else}fprintf{$endif}(fHandle: Pointer; format: PAnsiChar { args } ): Integer; cdecl;external name 'fprintf'; varargs;
// function {$ifdef WIN64}vsprintf{$else}vsprintf{$endif}(s: PAnsiChar; const format: PAnsiChar; ap: Pointer): Integer; cdecl; external name 'vsprintf';

type
  TCharTypeArray = array [Byte] of Word;

const
  _IS_UPP = $0001;
  _IS_LOW = $0002;
  _IS_DIG = $0004;
  _IS_SP = $0008;
  _IS_PUN = $0010;
  _IS_CTL = $0020;
  _IS_BLK = $0040;
  _IS_HEX = $0080;
  _IS_ALPHA = $0100;

  _IS_ALNUM = _IS_DIG or _IS_ALPHA;
  _IS_GRAPH = _IS_ALNUM or _IS_HEX or _IS_PUN;

  ENOENT = {$IFDEF WIN64}2{$ELSE}2{$ENDIF};
  EINVAL = {$IFDEF WIN64}22{$ELSE}19{$ENDIF};
  EEXIST = {$IFDEF WIN64}35{$ELSE}17{$ENDIF};
  EBADF = {$IFDEF WIN64}9{$ELSE}6{$ENDIF};
  ENOMEM = {$IFDEF WIN64}12{$ELSE}8{$ENDIF};
  EACCES = {$IFDEF WIN64}13{$ELSE}5{$ENDIF};
  ESPIPE = {$IFDEF WIN64}29{$ELSE}29{$ENDIF};
  ERANGE = {$IFDEF WIN64}34{$ELSE}34{$ENDIF};

  _SEEK_CUR = 1;
  _SEEK_END = 2;
  _SEEK_SET = 0;
  _F_EOF = $0020;
  _EOF = -1;

const
  _chartype: TCharTypeArray = (_IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_SP or _IS_CTL or _IS_BLK, _IS_SP or _IS_CTL, _IS_SP or _IS_CTL, _IS_SP or _IS_CTL, _IS_SP or _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL,
    _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_SP or _IS_BLK, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_DIG or _IS_HEX,
    _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_DIG or _IS_HEX, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_UPP or _IS_HEX or _IS_ALPHA,
    _IS_UPP or _IS_HEX or _IS_ALPHA, _IS_UPP or _IS_HEX or _IS_ALPHA, _IS_UPP or _IS_HEX or _IS_ALPHA, _IS_UPP or _IS_HEX or _IS_ALPHA, _IS_UPP or _IS_HEX or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA,
    _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA,
    _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_LOW or _IS_HEX or _IS_ALPHA, _IS_LOW or _IS_HEX or _IS_ALPHA, _IS_LOW or _IS_HEX or _IS_ALPHA, _IS_LOW or _IS_HEX or _IS_ALPHA,
    _IS_LOW or _IS_HEX or _IS_ALPHA, _IS_LOW or _IS_HEX or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA,
    _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_PUN, _IS_PUN, _IS_PUN,
    _IS_PUN, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_SP or _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL,
    _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_CTL, _IS_SP or _IS_BLK, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_LOW or _IS_PUN or _IS_ALPHA, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_DIG or _IS_PUN,
    _IS_DIG or _IS_PUN, _IS_PUN, _IS_LOW or _IS_PUN or _IS_ALPHA, _IS_PUN, _IS_PUN, _IS_PUN, _IS_DIG or _IS_PUN, _IS_LOW or _IS_PUN or _IS_ALPHA, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_PUN, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA,
    _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA,
    _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_PUN, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA,
    _IS_UPP or _IS_ALPHA, _IS_UPP or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA,
    _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA,
    _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_PUN, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA, _IS_LOW or _IS_ALPHA);

function {$IFDEF WIN64}isgraph{$ELSE}_isgraph{$ENDIF}(c: Integer): Integer; cdecl;
begin
  if Cardinal(c) <= High(_chartype) then
    Result := _chartype[c] and _IS_GRAPH
  else
    Result := 0;
end;

function {$IFDEF WIN64}isspace{$ELSE}_isspace{$ENDIF}(c: Integer): Integer; cdecl;
begin
  if Cardinal(c) <= High(_chartype) then
    Result := _chartype[c] and _IS_SP
  else
    Result := 0;
end;

function {$IFDEF WIN64}isdigit{$ELSE}_isdigit{$ENDIF}(c: Integer): Integer; cdecl;
begin
  if Cardinal(c) <= High(_chartype) then
    Result := _chartype[c] and _IS_DIG
  else
    Result := 0;
end;

function strenda(const s: PAnsiChar): PAnsiChar;
label
  0, 1, 2, 3;
begin
  Result := s;

  if not Assigned(Result) then
    goto 0;

  repeat
    if Result[0] = #0 then
      goto 0;
    if Result[1] = #0 then
      goto 1;
    if Result[2] = #0 then
      goto 2;
    if Result[3] = #0 then
      goto 3;
    Inc(Result, 4);
  until False;

3:
  Inc(Result);
2:
  Inc(Result);
1:
  Inc(Result);
0:
end;

function strlena(const s: PAnsiChar): NativeUint;
begin
  if s = nil then
    Result := 0
  else
    Result := strenda(s) - s;
end;

function NewOpenFile: pfile_t;
var
  n: Integer;
  P, pPrev: pfile_t;
begin

  n := 1;
  pPrev := nil;
  P := open_files;
  while Assigned(P) and (P^.FD = n) do
  begin
    Inc(n);
    pPrev := P;
    P := P^.Next;
  end;

  Result := AllocMem(SizeOf(Result^));
  Result^.FD := n;

  if Assigned(pPrev) then
  begin
    Result^.Next := pPrev^.Next;
    pPrev^.Next := Result;
  end
  else
  begin
    Result^.Next := open_files;
    open_files := Result;
  end;
end;

procedure DeleteOpenFile(const fp: pfile_t);
var
  pPrev: pfile_t;
begin
  if Assigned(open_files) then
  begin
    if open_files = fp then
      open_files := fp^.Next
    else
    begin
      pPrev := open_files;
      repeat
        if pPrev^.Next = fp then
          Break;
        pPrev := pPrev^.Next;
      until not Assigned(pPrev);

      if Assigned(pPrev) then
        pPrev^.Next := fp^.Next;
    end;
  end;

  System.FreeMem(fp);
end;

function {$IFDEF WIN64}FOpen{$ELSE}_fopen{$ENDIF}(const FileName: PAnsiChar; Mode: PAnsiChar): pfile_t; cdecl;
var
  Access: System.Cardinal;
  Append: System.Boolean;
  disposition: System.Cardinal;
  Err: DWORD;
  handle: THandle;
begin
  Result := nil;

  if not Assigned(FileName) or (FileName^ = #0) or not Assigned(Mode) or (Mode^ = #0) then
  begin
    errno := EINVAL;
    Exit;
  end;

  case Mode^ of
    'r':
      begin
        Access := Winapi.Windows.GENERIC_READ;
        disposition := Winapi.Windows.OPEN_EXISTING;
        Append := False;
      end;
    'w':
      begin
        Access := Winapi.Windows.GENERIC_WRITE;
        disposition := Winapi.Windows.CREATE_ALWAYS;
        Append := False;
      end;
    'a':
      begin
        Access := Winapi.Windows.GENERIC_WRITE;
        disposition := Winapi.Windows.OPEN_ALWAYS;
        Append := True;
      end
  else
    errno := EINVAL;
    Exit;
  end;

  Inc(Mode);

  case Mode^ of
    'b', 't':
      Inc(Mode);
  end;

  if Mode^ = '+' then
  begin
    Access := Winapi.Windows.GENERIC_READ or Winapi.Windows.GENERIC_WRITE;
  end;

  handle := Winapi.Windows.CreateFileA(FileName, Access, Winapi.Windows.FILE_SHARE_READ or Winapi.Windows.FILE_SHARE_WRITE, nil, disposition, 0, 0);
  if handle = Winapi.Windows.INVALID_HANDLE_VALUE then
  begin
    Err := GetLastError and $FFFF;
    case Err of
      Winapi.Windows.ERROR_FILE_NOT_FOUND, Winapi.Windows.ERROR_PATH_NOT_FOUND, Winapi.Windows.ERROR_OPEN_FAILED:
        if disposition = Winapi.Windows.CREATE_ALWAYS then
          Err := EEXIST
        else
          Err := ENOENT;
    else
      Err := EINVAL;
    end;
    errno := Err;
    Exit;
  end;

  if Append then
    Winapi.Windows.SetFilePointer(handle, 0, nil, Winapi.Windows.FILE_END);

  Result := NewOpenFile;
  Result^.handle := handle;
end;

function {$IFDEF WIN64}FClose{$ELSE}_fclose{$ENDIF}(Stream: pfile_t): Integer;
begin
  if Assigned(Stream) then
  begin

    if (Stream^.handle <> Winapi.Windows.INVALID_HANDLE_VALUE) and Winapi.Windows.CloseHandle(Stream^.handle) then
    begin
      if Stream^.static_ then
        Stream^.handle := Winapi.Windows.INVALID_HANDLE_VALUE
      else
        DeleteOpenFile(Stream);
      Result := 0;
      Exit;
    end;

    Stream^.Flags := Stream^.Flags and not _F_EOF;
  end;

  Result := _EOF;
end;

function {$IFDEF WIN64}fwrite{$ELSE}_fwrite{$ENDIF}(Ptr: Pointer; size: NativeUint; n: NativeUint; Stream: pfile_t): NativeUint;
var
  bytesWriten: DWORD;
begin
  if size = 0 then
  begin
    Result := n;
  end
  else
  begin
    if Assigned(Ptr) and Assigned(Stream) then
    begin

      if (Stream^.handle <> Winapi.Windows.INVALID_HANDLE_VALUE) and Winapi.Windows.WriteFile(Stream^.handle, Ptr^, size * n, bytesWriten, nil) then
      begin
        Result := bytesWriten div size;
        Exit;
      end;

    end;

    Result := 0;
  end;
end;

function fputc(c: Integer; Stream: pfile_t): Integer; cdecl;
var
  bytesWritten: Cardinal;
begin
  if Assigned(Stream) then
  begin

    if (Stream^.handle <> Winapi.Windows.INVALID_HANDLE_VALUE) and (WriteFile(Stream^.handle, c, 1, bytesWritten, nil)) then
    begin
      Result := c;
      Exit;
    end;

  end;

  Result := _EOF;
end;

function {$IFDEF WIN64}_fputc{$ELSE}__fputc{$ENDIF}(c: Integer; f: pfile_t): Integer; cdecl;
begin
  if f = @{$IFDEF WIN64}_streams{$ELSE}__streams{$ENDIF}[1] then
  begin
    System.Write(Output, AnsiChar(c));
    Result := 0;
  end
  else if f = @{$IFDEF WIN64}_streams{$ELSE}__streams{$ENDIF}[2] then
  begin
    System.Write(ErrOutput, AnsiChar(c));
    Result := 0;
  end
  else
    Result := fputc(c, f);
end;

function {$IFDEF WIN64}fputs{$ELSE}_fputs{$ENDIF}(const s: PAnsiChar; Stream: pfile_t): Integer; cdecl;
begin
  Result := {$IFDEF WIN64}fwrite{$ELSE}_fwrite{$ENDIF}(s, strlena(s), 1, Stream);
end;

function {$IFDEF WIN64}FRead{$ELSE}_fread{$ENDIF}(Ptr: Pointer; size: NativeUint; n: NativeUint; Stream: pfile_t): NativeUint;
label
  Error;
var
  bytesRead: DWORD;
  total: NativeUint;
begin
  if size = 0 then
  begin
    Result := n
  end
  else
  begin
    if Assigned(Ptr) and Assigned(Stream) then
    begin
      total := size * n;

      if not Winapi.Windows.ReadFile(Stream^.handle, Ptr^, total, bytesRead, nil) then
        goto Error;
      Result := bytesRead;

      if Result < total then
        Stream^.Flags := Stream^.Flags or _F_EOF;
      Result := Result div size;
      Exit;
    end;

  Error:
    Result := 0;
  end;
end;

function {$IFDEF WIN64}FSeek{$ELSE}_fseek{$ENDIF}(Stream: pfile_t; offset: Integer; whence: Integer): Integer; cdecl;

const
  INVALID_SET_FILE_POINTER = DWORD(-1);
var
  dwMoveMethod: DWORD;

label
  lblReturnError;
begin
  if not Assigned(Stream) then
  begin
    errno := EBADF;
    goto lblReturnError;
  end;
  Stream^.Flags := Stream^.Flags and not _F_EOF;

  if Stream^.handle = Winapi.Windows.INVALID_HANDLE_VALUE then
  begin
    errno := EBADF;
    goto lblReturnError;
  end;

  case whence of
    _SEEK_SET:
      dwMoveMethod := Winapi.Windows.FILE_BEGIN;
    _SEEK_CUR:
      dwMoveMethod := Winapi.Windows.FILE_CURRENT;
    _SEEK_END:
      dwMoveMethod := Winapi.Windows.FILE_END;
  else
    errno := EINVAL;
    goto lblReturnError;
  end;

  if Winapi.Windows.SetFilePointer(Stream^.handle, offset, nil, dwMoveMethod) = INVALID_SET_FILE_POINTER then
  begin
    errno := ESPIPE;
    goto lblReturnError;
  end;

  Result := 0;
  Exit;

lblReturnError:
  Result := 1;
end;

function {$IFDEF WIN64}FTell{$ELSE}_ftell{$ENDIF}(Stream: pfile_t): Integer; cdecl;
begin
  if Assigned(Stream) then
  begin

    Result := Winapi.Windows.SetFilePointer(Stream^.handle, 0, nil, Winapi.Windows.FILE_CURRENT);
    Exit;

  end;

  Result := -1;
end;

procedure {$IFDEF WIN64}free{$ELSE}_free{$ENDIF}(pBlock: Pointer); cdecl;
begin
  if pBlock = nil then
    Exit;

  FreeMem(pBlock);
end;

function InternalAllocMem(size: NativeUint): Pointer; cdecl;
var
  mm: TMemoryManagerEx;
begin
  Result := nil;
  if size > 0 then
  begin
    GetMemoryManager(mm);
    Result := mm.AllocMem(size);
    // Result := mm.GetMem(size);
  end;
end;

function {$IFDEF WIN64}malloc{$ELSE}_malloc{$ENDIF}(size: NativeUint): Pointer; cdecl;
begin
  Result := InternalAllocMem(size);
end;

function {$IFDEF WIN64}realloc{$ELSE}_realloc{$ENDIF}(P: Pointer; NewSize: NativeUint): Pointer; cdecl;
begin
  ReallocMem(P, NewSize);
  Result := P;
end;

function {$IFDEF WIN64}memchr{$ELSE}_memchr{$ENDIF}(const s: Pointer; i: Integer; n: NativeUint): Pointer; cdecl;
label
  1, 2, 3;
begin
  Result := s;

  while n >= 4 do
  begin
    if Ord(PAnsiChar(Result)[0]) = i then
      Exit;
    if Ord(PAnsiChar(Result)[1]) = i then
      goto 1;
    if Ord(PAnsiChar(Result)[2]) = i then
      goto 2;
    if Ord(PAnsiChar(Result)[3]) = i then
      goto 3;
    Inc(PAnsiChar(Result), 4);
    Dec(n, 4);
  end;

  while n > 0 do
  begin
    if Ord(PAnsiChar(Result)[0]) = i then
      Exit;
    Inc(PAnsiChar(Result));
    Dec(n);
  end;

  Result := nil;
  Exit;

3:
  Inc(PAnsiChar(Result));
2:
  Inc(PAnsiChar(Result));
1:
  Inc(PAnsiChar(Result));
end;

function {$IFDEF WIN64}atol{$ELSE}_atol{$ENDIF}(const __s: PAnsiChar): Integer; cdecl;
var
  c: Byte;
  P: PByte;
  is_neg: Boolean;
begin
{$UNDEF Q_temp}{$IFOPT Q+}{$DEFINE Q_temp}{$ENDIF}{$Q-}   // disable OverFlowChecks
  Result := 0;
  P := PByte(__s);
  if Assigned(P) then
  begin
    c := P^;
    while _chartype[c] and _IS_SP <> 0 do
    begin
      Inc(P);
      c := P^;
    end;

    if (c = Ord('+')) or (c = Ord('-')) then
    begin
      is_neg := c = Ord('-');
      Inc(P);
      c := P^;
    end
    else
      is_neg := False;

    while (c >= Ord('0')) and (c <= Ord('9')) do
    begin
      Result := Result * 10;
      Inc(Result, c - Ord('0'));
      Inc(P);
      c := P^;
    end;

    if is_neg then
      Result := -Result;
  end;

{$IFDEF Q_temp}{$Q-}{$ENDIF}
end;

function {$IFDEF WIN64}strcpy{$ELSE}_strcpy{$ENDIF}(Dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
begin
  Result := Dest;
  if Assigned(Result) and Assigned(src) then
  begin
    repeat
      Result[0] := src[0];
      if src[0] = #0 then
        Break;
      Result[1] := src[1];
      if src[1] = #0 then
        Break;
      Result[2] := src[2];
      if src[2] = #0 then
        Break;
      Result[3] := src[3];
      if src[3] = #0 then
        Break;
      Inc(Result, 4);
      Inc(src, 4);
    until False;
    Result := Dest;
  end;
end;

function {$IFDEF WIN64}strncpy{$ELSE}_strncpy{$ENDIF}(Dest: PAnsiChar; src: PAnsiChar; MaxLen: NativeUint): PAnsiChar; cdecl;
begin
  Result := Dest;

  if Assigned(Dest) and Assigned(src) then
  begin
    while MaxLen >= 4 do
    begin
      Dest[0] := src[0];
      if src[0] = #0 then
        Break;
      Dest[1] := src[1];
      if src[1] = #0 then
        Break;
      Dest[2] := src[2];
      if src[2] = #0 then
        Break;
      Dest[3] := src[3];
      if src[3] = #0 then
        Break;
      Inc(Dest, 4);
      Inc(src, 4);
      Dec(MaxLen, 4);
    end;

    while MaxLen > 0 do
    begin
      Dest[0] := src[0];
      if src[0] = #0 then
        Break;
      Inc(Dest);
      Inc(src);
      Dec(MaxLen);
    end;
  end;
end;

function {$IFDEF WIN64}strcat{$ELSE}_strcat{$ENDIF}(const Dest: PAnsiChar; const src: PAnsiChar): PAnsiChar; cdecl;
begin
  Result := Dest;
{$IFDEF WIN64}strcpy{$ELSE}_strcpy{$ENDIF}(strenda(Result), src);
end;

function strcmp(s1: PAnsiChar; s2: PAnsiChar): Integer; cdecl;
var
  c1, c2: Integer;
begin
  if s1 = s2 then
  begin
    Result := 0;
  end
  else if Assigned(s1) then
    if Assigned(s2) then
    begin
      repeat
        c1 := Ord(s1[0]);
        c2 := Ord(s2[0]);
        if (c1 <> c2) or (c1 = 0) then
          Break;

        c1 := Ord(s1[1]);
        c2 := Ord(s2[1]);
        if (c1 <> c2) or (c1 = 0) then
          Break;

        c1 := Ord(s1[2]);
        c2 := Ord(s2[2]);
        if (c1 <> c2) or (c1 = 0) then
          Break;

        c1 := Ord(s1[3]);
        c2 := Ord(s2[3]);
        if (c1 <> c2) or (c1 = 0) then
          Break;

        Inc(s1, 4);
        Inc(s2, 4);
      until False;
      Result := c1 - c2;
    end
    else
    begin
      Result := 1
    end
  else
  begin
    Result := -1;
  end;
end;

function {$IFDEF WIN64}strncmp{$ELSE}_strncmp{$ENDIF}(s1: PAnsiChar; s2: PAnsiChar; MaxLen: NativeUint): Integer; cdecl;
begin
  if s1 <> s2 then
    if Assigned(s1) then
      if Assigned(s2) then
        while MaxLen > 0 do
        begin
          Result := Ord(s1^) - Ord(s2^);
          if (Result <> 0) or (s1^ = #0) then
            Exit;
          Inc(s1);
          Inc(s2);
          Dec(MaxLen);
        end
      else
      begin
        Result := 1;
        Exit;
      end
    else
    begin
      Result := -1;
      Exit;
    end;
  Result := 0;
end;

function {$IFDEF WIN64}strchr{$ELSE}_strchr{$ENDIF}(const s: PAnsiChar; const c: Integer): PAnsiChar; cdecl;
begin
  Result := s;
  while Result^ <> #0 do
  begin
    if Ord(Result^) = c then
      Exit;
    Inc(Result);
  end;
  if c <> 0 then
    Result := nil;
end;

function {$IFDEF WIN64}strrchr{$ELSE}_strrchr{$ENDIF}(s: PAnsiChar; const c: Integer): PAnsiChar; cdecl;
begin
  Result := nil;
  if Assigned(s) then
    repeat
      if Ord(s^) = c then
        Result := s;

      Inc(s);
    until s^ = #0;
end;

function {$IFDEF WIN64}strstr{$ELSE}_strstr{$ENDIF}(const s1: PAnsiChar; const s2: PAnsiChar): PAnsiChar; cdecl;
var
  i: NativeInt;
begin
  if Assigned(s1) and (s1^ <> #0) then
  begin
    Result := s1;

    if not Assigned(s2) or (s2^ = #0) then
      Exit;

    while Result^ <> #0 do
    begin
      if Result^ <> s2^ then
        Inc(Result)
      else
      begin
        i := 1;
        while (Result[i] <> #0) and (Result[i] = s2[i]) do
          Inc(i);

        if s2[i] = #0 then
          Exit;

        Inc(Result);
      end;
    end;
  end;

  Result := nil;
end;

function {$IFDEF WIN64}calloc{$ELSE}_calloc{$ENDIF}(nelem, size: NativeUint): Pointer; cdecl;
begin
  Result := InternalAllocMem(nelem * size);
end;

function {$IFDEF WIN64}memcpy{$ELSE}_memcpy{$ENDIF}(Dest: Pointer; const src: Pointer; n: NativeUint): Pointer; cdecl;
begin
  Result := Dest;

  Move(src^, Result^, n);
end;

function {$IFDEF WIN64}memset{$ELSE}_memset{$ENDIF}(const s: Pointer; const c: Integer; const n: NativeUint): Pointer; cdecl;
begin
  Result := s;
  FillChar(Result^, n, c);
end;

function {$IFDEF WIN64}memmove{$ELSE}_memmove{$ENDIF}(Dest: Pointer; const src: Pointer; n: NativeUint): Pointer; cdecl;
begin
  Result := Dest;
  Move(src^, Result^, n);
end;

function {$IFDEF WIN64}getenv{$ELSE}_getenv{$ENDIF}(const EnvVar: PAnsiChar): PAnsiChar; cdecl;
{$ifndef getenv_use_crtl}
var
  l: Cardinal;
{$endif}
begin
  Result := nil;
{$ifndef getenv_use_crtl}
  l := Winapi.Windows.GetEnvironmentVariableA(EnvVar, nil, 0);
  if l > 0 then
  begin
//    Result := {$ifdef WIN64}malloc{$else}_malloc{$endif}(l);
    Result := FPJMarshaller.AllocMem(l * SizeOf(AnsiChar)).ToPointer;
    if Assigned(Result) then
      Winapi.Windows.GetEnvironmentVariableA(EnvVar, Result, l);
  end;
{$else}
  if EnvVar <> nil then
    Result := crtl_getenv(EnvVar);
{$endif} // getenv_use_crtl
end;

function {$IFDEF WIN64}strlen{$ELSE}_strlen{$ENDIF}(const s: PAnsiChar): NativeUint; cdecl;
begin
  Result := strlena(s);
end;

function{$IFDEF WIN64}time64{$ELSE}time{$ENDIF}(const pTime: PNativeInt): NativeInt; cdecl;
const
  EPOCH_BIAS = 116444736000000000;
var
  ft: TFileTime;
  t: Int64;
begin
  GetSystemTimeAsFileTime(ft);
  t := (Int64(ft) - EPOCH_BIAS) div 10000000;

  if t > {$IFDEF WIN64}$793406FFF{$ELSE}$7FFFD27F{$ENDIF} then
    t := -1;

  Result := t;
  if Assigned(pTime) then
    pTime^ := Result;
end;

function {$IFDEF WIN64}acos{$ELSE}_acos{$ENDIF}(x: Double): Double; cdecl;
begin
  Result := System.Math.ArcCos(x);
end;

function {$IFDEF WIN64}asin{$ELSE}_asin{$ENDIF}(x: Double): Double; cdecl;
begin
  Result := System.Math.ArcSin(x);
end;

function {$IFDEF WIN64}exp{$ELSE}_exp{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsPositiveInfinity then
    Result := x
  else if x.IsNegativeInfinity then
    Result := 0
  else
    Result := System.exp(x);
end;

function {$IFDEF WIN64}exp2{$ELSE}_exp2{$ENDIF}(x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsPositiveInfinity then
    Result := x
  else
    Result := exp(x * Ln(2));
end;

function {$IFDEF WIN64}log{$ELSE}_log{$ENDIF}(num: Double): Double; cdecl;
begin
  Result := Ln(num);
end;

function {$IFDEF WIN64}tan{$ELSE}_tan{$ENDIF}(const x: Double): Double; cdecl;
begin
  Result := System.Tangent(x);
end;

function {$IFDEF WIN64}atan{$ELSE}_atan{$ENDIF}(x: Double): Double; cdecl;
begin
  Result := System.ArcTan(x);
end;

function {$IFDEF WIN64}atan2{$ELSE}_atan2{$ENDIF}(y, x: Double): Double; cdecl;
begin
  Result := System.Math.ArcTan2(y, x);
end;

function {$IFDEF WIN64}sinh{$ELSE}_sinh{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsInfinity then
    Result := x
  else
    Result := System.Math.sinh(x);
end;

function {$IFDEF WIN64}pow{$ELSE}_pow{$ENDIF}(b, e: Double): Double; cdecl;
begin
  Result := System.Math.Power(b, e);
end;

function  {$IFDEF WIN64}sqrt{$ELSE}_sqrt{$ENDIF}(const x: Double): Double; cdecl;
begin
  if (x < 0) or x.IsNan then
    Result := Double.NaN
  else if x.IsInfinity then
    Result := Double.PositiveInfinity
  else
    Result := System.Sqrt(x);
end;

function {$IFDEF WIN64}ceil{$ELSE}_ceil{$ENDIF}(const x: Double): Integer; cdecl;
begin
  Result := System.Math.Ceil(x);
end;

function {$IFDEF WIN64}cos{$ELSE}_cos{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsInfinity then
    Result := Double.NaN
  else
    Result := System.Cos(x);
end;

function {$IFDEF WIN64}cosh{$ELSE}_cosh{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsInfinity then
    Result := Double.PositiveInfinity
  else
  if x.IsNan then
    Result := Double.NaN
  else
    Result := System.Math.CosH(x);
end;

function {$IFDEF WIN64}sin{$ELSE}_sin{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsInfinity then
    Result := Double.NaN
  else
    Result := System.Sin(x);
end;

function {$IFDEF WIN64}fabs{$ELSE}_fabs{$ENDIF}(const x: Double): Double; cdecl;
begin
  Result := System.Abs(x);
end;

function {$IFDEF WIN64}fmod{$ELSE}_fmod{$ENDIF}(const n, d: Double): Double; cdecl;
begin
  Result := System.Math.FMod(n, d);
end;

function {$IFDEF WIN64}floor{$ELSE}_floor{$ENDIF}(const x: Double): Integer; cdecl;
begin
  Result := System.Math.Floor(x);
end;

function {$IFDEF WIN64}_errno{$ELSE}__errno{$ENDIF}: PInteger; cdecl;
begin
  Result := @errno;
end;

function {$IFDEF WIN64}__errno{$ELSE}___errno{$ENDIF}: PInteger; cdecl;
begin
  Result := @errno;
end;

function {$IFDEF WIN64}remainder{$ELSE}_remainder{$ENDIF}(const x,y: Double): Double; cdecl;
begin
  Result := y;

  if CompareValue(Result,0) = 0 then
    Result := Double.NaN
  else
    Result := x - (y * System.Round(x /Result));
end;

function {$IFDEF WIN64}__get_std_stream{$ELSE}___get_std_stream{$ENDIF}(num: Cardinal): Pointer; cdecl;
begin
  Result := Pointer(GetStdHandle(num));
end;

function {$IFDEF WIN64}_sinh{$ELSE}__sinh{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsNan or x.IsInfinity then
    Result := x
  else
    Result := System.Math.sinh(x);
end;

function {$IFDEF WIN64}_cosh{$ELSE}__cosh{$ENDIF}(const x: Double): Double; cdecl;
begin
  if x.IsInfinity then
    Result := Double.PositiveInfinity
  else
  if x.IsNan then
    Result := Double.NaN
  else
    Result := System.Math.CosH(x);
end;


{$IFDEF WIN64}

function _FNan: Double; cdecl;
begin
  Result := Double.NaN;
end;

function _Inf(): Double; cdecl;
begin
  Result := System.Math.Infinity;
end;

function _log(num: Double): Double; cdecl; // bcc64 from > rad 10.2.3
begin
  Result := Ln(num);
end;

function _Sinx(x: Double; y_: Cardinal; z_: Integer): Double; cdecl;
begin
  Result := Sin(x);
end;

{$ELSE}
// win32
var ___ieee_32_p_nanq: UInt32 = $7FC00000; // $FFC00000;
var __turboFloat: Integer = 0;
var __huge_dble: Double = System.Math.Infinity;

// function ___ieee_32_p_nanq: UInt32; cdecl;
// const
// cNaN: Single = 0.0 / 0.0;
// begin
// Result := UInt32(Pointer(cNaN));
// end;

// procedure __ftol; cdecl; external; {$L ftol.obj}
// function __ftol(f : double) : Integer; cdecl;
// begin
// Result := Trunc(f);
// end;

procedure __ftol;
asm
  DB $55
  DB $8B, $EC
  DB $8D, $65, $F4
  DB $9B
  DB $D9, $7D, $FC
  DB $9B
  DB $66, $8B, $45, $FC
  DB $81, $4D, $FC, $01, $0C, $00, $00
  DB $D9, $6D, $FC
  DB $DF, $7D, $F4
  DB $66, $89, $45, $FC
  DB $D9, $6D, $FC
  DB $8B, $45, $F4
  DB $8B, $55, $F8
  DB $8B, $E5
  DB $5D
end;

procedure _llumod;
asm
  push    ebp
  push    ebx
  push    esi
  push    edi

  mov     ebx,20[esp]
  mov     ecx,24[esp]
  or      ecx,ecx
  jnz     @__llumod@slow_ldiv

  or      edx,edx
  jz      @__llumod@quick_ldiv

  or      ebx,ebx
  jz      @__llumod@quick_ldiv

@__llumod@slow_ldiv:
  mov     ebp,ecx
  mov     ecx,64
  xor     edi,edi
  xor     esi,esi

@__llumod@xloop:
  shl     eax,1
  rcl     edx,1
  rcl     esi,1
  rcl     edi,1
  cmp     edi,ebp
  jb      @__llumod@nosub
  ja      @__llumod@subtract
  cmp     esi,ebx
  jb      @__llumod@nosub

@__llumod@subtract:
  sub     esi,ebx
  sbb     edi,ebp
  inc     eax

@__llumod@nosub:
  loop    @__llumod@xloop

  mov     eax,esi
  mov     edx,edi

@__llumod@finish:
  pop     edi
  pop     esi
  pop     ebx
  pop     ebp
  ret     8

@__llumod@quick_ldiv:
  div     ebx
  xchg  eax,edx
  xor     edx,edx
  jmp     @__llumod@finish
end;

procedure _lludiv;
asm
  push    ebp
  push    ebx
  push    esi
  push    edi

  mov     ebx,20[esp]
  mov     ecx,24[esp]

  or      ecx,ecx
  jnz     @__lludiv@slow_ldiv

  or      edx,edx
  jz      @__lludiv@quick_ldiv

  or      ebx,ebx
  jz      @__lludiv@quick_ldiv

@__lludiv@slow_ldiv:
  mov     ebp,ecx
  mov     ecx,64
  xor     edi,edi
  xor     esi,esi

@__lludiv@xloop:
  shl     eax,1
  rcl     edx,1
  rcl     esi,1
  rcl     edi,1
  cmp     edi,ebp
  jb      @__lludiv@nosub
  ja      @__lludiv@subtract
  cmp     esi,ebx
  jb      @__lludiv@nosub

@__lludiv@subtract:
  sub     esi,ebx
  sbb     edi,ebp
  inc     eax

@__lludiv@nosub:
  loop    @__lludiv@xloop

@__lludiv@finish:
  pop     edi
  pop     esi
  pop     ebx
  pop     ebp
  ret     8

@__lludiv@quick_ldiv:
  div     ebx
  xor     edx,edx
  jmp     @__lludiv@finish
end;

procedure _ftoul;
asm
  DB $83, $C4, $F4
  DB $9B
  DB $D9, $3C, $24
  DB $9B
  DB $8A, $44, $24, $01
  DB $80, $4C, $24, $01, $0C
  DB $D9, $2C, $24
  DB $DF, $7C, $24, $04
  DB $88, $44, $24, $01
  DB $D9, $2C, $24
  DB $8B, $44, $24, $04
  DB $8B, $54, $24, $08
  DB $83, $C4, $0C
end;

procedure _llmul;
asm
  push  edx
  push  eax

  mov   eax, [esp+16]
  mul   dword ptr [esp]
  mov   ecx, eax

  mov   eax, [esp+4]
  mul   dword ptr [esp+12]
  add   ecx, eax

  mov   eax, [esp]
  mul   dword ptr [esp+12]
  add   edx, ecx

  pop   ecx
  pop   ecx

  ret     8
end;

function _time(const pTime: PNativeInt): NativeInt; cdecl;
begin
  Result := time(pTime);
end;
{$ENDIF} // win32/win64
{$ENDIF} // LIBPROJ_LINKSTATIC
{$ENDREGION} // crtl

{$REGION '  forward symbol declarations'}
{$IFDEF LIBPROJ_LINKSTATIC}
procedure {$IFDEF WIN64}bch2bps{$ELSE}_bch2bps{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}bchgen{$ELSE}_bchgen{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}aasin{$ELSE}_aasin{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}adjlon{$ELSE}_adjlon{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_deriv{$ELSE}_pj_deriv{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}dmstor_ctx{$ELSE}_dmstor_ctx{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_calc_ellipsoid_params{$ELSE}_pj_calc_ellipsoid_params{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_ellipsoid{$ELSE}_pj_ellipsoid{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_Convert_Geocentric_To_Geodetic{$ELSE}_pj_Convert_Geocentric_To_Geodetic{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_Convert_Geodetic_To_Geocentric{$ELSE}_pj_Convert_Geodetic_To_Geocentric{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_Set_Geocentric_Parameters{$ELSE}_pj_Set_Geocentric_Parameters{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_fwd{$ELSE}_pj_fwd{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_fwd3d{$ELSE}_pj_fwd3d{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inv{$ELSE}_pj_inv{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inv3d{$ELSE}_pj_inv3d{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_strdup{$ELSE}_pj_strdup{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_gc_readcatalog{$ELSE}_pj_gc_readcatalog{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}nad_ctable2_init{$ELSE}_nad_ctable2_init{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}nad_ctable2_load{$ELSE}_nad_ctable2_load{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}nad_ctable_init{$ELSE}_nad_ctable_init{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}nad_ctable_load{$ELSE}_nad_ctable_load{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}nad_free{$ELSE}_nad_free{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_set_ctx{$ELSE}_pj_set_ctx{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_vlog{$ELSE}_pj_vlog{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}asqrt{$ELSE}_asqrt{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_enfn{$ELSE}_pj_enfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inv_mlfn{$ELSE}_pj_inv_mlfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_mlfn{$ELSE}_pj_mlfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_phi2{$ELSE}_pj_phi2{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_tsfn{$ELSE}_pj_tsfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_authlat{$ELSE}_pj_authlat{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_authset{$ELSE}_pj_authset{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_qsfn{$ELSE}_pj_qsfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_zpoly1{$ELSE}_pj_zpoly1{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_zpolyd1{$ELSE}_pj_zpolyd1{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_gauss{$ELSE}_pj_gauss{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_gauss_ini{$ELSE}_pj_gauss_ini{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inv_gauss{$ELSE}_pj_inv_gauss{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_inv_mdist{$ELSE}_proj_inv_mdist{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_mdist{$ELSE}_proj_mdist{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_mdist_ini{$ELSE}_proj_mdist_ini{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_msfn{$ELSE}_pj_msfn{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_log1p{$ELSE}_pj_log1p{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_asinh{$ELSE}_pj_asinh{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}aacos{$ELSE}_aacos{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}aatan2{$ELSE}_aatan2{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_init_ctx{$ELSE}_pj_init_ctx{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}dmstor{$ELSE}_dmstor{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_expand_init{$ELSE}_pj_expand_init{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_factors{$ELSE}_pj_factors{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_find_file{$ELSE}_pj_find_file{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_fwd4d{$ELSE}_pj_fwd4d{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_has_inverse{$ELSE}_pj_has_inverse{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inherit_ellipsoid_def{$ELSE}_pj_inherit_ellipsoid_def{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_inv4d{$ELSE}_pj_inv4d{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_left{$ELSE}_pj_left{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_make_args{$ELSE}_pj_make_args{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_right{$ELSE}_pj_right{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_trim_argc{$ELSE}_pj_trim_argc{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_trim_argv{$ELSE}_pj_trim_argv{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}rtodms{$ELSE}_rtodms{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_approx_2D_trans{$ELSE}_pj_approx_2D_trans{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}pj_approx_3D_trans{$ELSE}_pj_approx_3D_trans{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_create_argv{$ELSE}_proj_create_argv{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_destroy{$ELSE}_proj_destroy{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_log_error{$ELSE}_proj_log_error{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_vgrid_init{$ELSE}_proj_vgrid_init{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_vgrid_value{$ELSE}_proj_vgrid_value{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_hgrid_apply{$ELSE}_proj_hgrid_apply{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_hgrid_init{$ELSE}_proj_hgrid_init{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_create{$ELSE}_proj_create{$ENDIF}; cdecl; external;
procedure {$IFDEF WIN64}proj_hgrid_value{$ELSE}_proj_hgrid_value{$ENDIF}; cdecl; external;
{$ENDIF} // LIBPROJ_LINKSTATIC
{$ENDREGION}

{$REGION '  libproj public api'}
// from geodesic.h
{$REGION 'mask values for the argument to geod_lineinit().'}

const
  /// Calculate nothing
  GEOD_NONE = 0;
  /// Calculate latitude
  GEOD_LATITUDE = 1 shl 7 or 0;
  /// Calculate longitude
  GEOD_LONGITUDE = 1 shl 8 or 1 shl 3;
  /// Calculate azimuth
  GEOD_AZIMUTH = 1 shl 9 or 0;
  /// Calculate distance
  GEOD_DISTANCE = 1 shl 10 or 1 shl 0;
  /// Allow distance as input
  GEOD_DISTANCE_IN = 1 shl 11 or 1 shl 0 or 1 shl 1;
  /// Calculate reduced length
  GEOD_REDUCEDLENGTH = 1 shl 12 or 1 shl 0 or 1 shl 2;
  /// Calculate geodesic scale
  GEOD_GEODESICSCALE = 1 shl 13 or 1 shl 0 or 1 shl 2;
  /// Calculate reduced length
  GEOD_AREA = 1 shl 14 or 1 shl 4;
  /// Calculate everything
  GEOD_ALL = $7F80 or $1F;

type
  TGeodMask = GEOD_NONE .. GEOD_ALL;
{$ENDREGION}

{$REGION 'flag values for the flags argument to geod_gendirect() and geod_genposition().'}
const
  /// No flags
  GEOD_NOFLAGS = 0;
  /// Position given in terms of arc distance
  GEOD_ARCMODE = 1 shl 0;
  /// Unroll the longitude
  GEOD_LONG_UNROLL = 1 shl 15;
type
  TGeodFlagsMask = GEOD_NOFLAGS .. GEOD_LONG_UNROLL;
{$ENDREGION}

{$region 'geodesic api'}
procedure geod_init(const g: PGeodesic; const a, f: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_init';
/// Solve the direct geodesic problem.
procedure geod_direct(const g: PGeodesic; const lat1, lon1, azi1, s12: Double; var plat2, plon2, pazi2: Double); cdecl; external{$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_direct';
procedure geod_inverse(const g: PGeodesic; lat1, lon1, lat2, lon2: Double; var ps12, pazi1, pazi2: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_inverse';
/// <summary>
/// The general direct geodesic problem.
/// </summary>
/// <param name="g">
/// a pointer to the geod_geodesic object specifying the ellipsoid.
/// </param>
/// <param name="lat1">
/// latitude of point 1 (degrees).
/// </param>
/// <param name="lon1">
/// longitude of point 1 (degrees).
/// </param>
/// <param name="azi1">
/// azimuth at point 1 (degrees).
/// </param>
/// <param name="s12_a12">
/// if flags &amp; GEOD_ARCMODE is 0, this is the distance from point 1 to point 2 (meters); otherwise it is the arc
/// length from point 1 to point 2 (degrees); it can be negative.
/// </param>
/// <param name="plat2">
/// pointer to the latitude of point 2 (degrees).
/// </param>
/// <param name="plon2">
/// pointer to the longitude of point 2 (degrees).
/// </param>
/// <param name="pazi2">
/// pointer to the (forward) azimuth at point 2 (degrees).
/// </param>
/// <param name="pM12">
/// pointer to the geodesic scale of point 2 relative to point 1 (dimensionless).
/// </param>
/// <param name="pM21">
/// pointer to the geodesic scale of point 1 relative to point 2 (dimensionless).
/// </param>
/// <param name="flags">
/// bitor'ed combination of geod_flags() flags <i>GEOD_ARCMODE</i> determines the meaning of <b>s12_a12</b> and flags
/// and <i>GEOD_LONG_UNROLL</i> "unrolls" <b>lon2</b>.
/// </param>
/// <param name="ps12">
/// pointer to the distance from point 1 to point 2 (meters).
/// </param>
/// <param name="pm12">
/// pointer to the reduced length of geodesic (meters).
/// </param>
/// <param name="pS12">
/// pointer to the area under the geodesic (meters <sup>2</sup>).
/// </param>
/// <returns>
/// a12 arc length from point 1 to point 2 (degrees).
/// </returns>
function geod_gendirect(const g: PGeodesic; lat1, lon1, azi1: Double; Flags: Cardinal; s12_a12: Double; var plat2, plon2, pazi2, pdst12, prlen12, pM12, pM21, palen12): Double; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_gendirect';
function geod_geninverse(const g: PGeodesic; lat1, lon1, lat2, lon2: Double; var ps12, pazi1, pazi2, prlen12, pM12, pM21, palen21: Double): Double; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_geninverse';

procedure geod_lineinit(var l: TGeodesicLine; var g: TGeodesic; lat1, lon1, azi1: Double; caps: Cardinal); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_lineinit';
procedure geod_directline(const l: PGeodesicLine; lat1, lon1, azi1, s12: Double; caps: Cardinal); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix + {$ELSE}proj4libname name {$ENDIF}'geod_directline';
procedure geod_gendirectline(const l: PGeodesicLine; g: PGeodesic; lat1, lon1, azi1: Double; Flags: Cardinal; s12_a12: Double; caps: Cardinal); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_gendirectline';
procedure geod_inverseline(const l: PGeodesicLine; g: PGeodesic; lat1, lon1, lat2, lon2: Double; caps: Cardinal); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_inverseline';
procedure geod_position(const l: PGeodesicLine; s12: Double; var plat2, plon2, pazi2: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_position';
function geod_genposition(const l: PGeodesicLine; Flags: Cardinal; s12_a12: Double; var plat2, plon2, pazi2, pdist12, prlen12, pM12, pM21, ps12): Double; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_genposition';
procedure geod_setdistance(const l: PGeodesicLine; s13: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_setdistance';
procedure geod_gensetdistance(const l: PGeodesicLine; Flags: Cardinal; s13_a13: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_gensetdistance';

/// <summary>
///   Initialize a geod_polygon object.
/// </summary>
/// <param name="P">
///   a pointer to the object to be initialized.
/// </param>
/// <param name="polylinep">
///   polylinep non-zero if a polyline instead of a polygon.
/// </param>
/// <remarks>
///   If <i>polylinep</i> is zero, then the sequence of vertices and edges added by <br />geod_polygon_addpoint() and
///   geod_polygon_addedge() define a polygon and <br />the perimeter and area are returned by geod_polygon_compute().
///   If <br /><i>polylinep</i> is non-zero, then the vertices and edges define a polyline and <br />only the perimeter
///   is returned by geod_polygon_compute(). <br />The area and perimeter are accumulated at two times the standard
///   floating <br />point precision to guard against the loss of accuracy with many-sided <br />polygons. At any point
///   you can ask for the perimeter and area so far.
/// </remarks>
/// <seealso cref="geod_polygon_addpoint()" />
/// <seealso cref="geod_polygon_compute()" />
/// <seealso cref="geod_polygon_addedge()" />
procedure geod_polygon_init(var P: TGeodesicPolygon; polylinep: Integer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_init';
procedure geod_polygon_clear(var P: TGeodesicPolygon); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_clear';
procedure geod_polygon_addpoint(const g: PGeodesic; const P: PGeodesicPolygon; lat, lon: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_addpoint';
procedure geod_polygon_addedge(const g: PGeodesic; const P: PGeodesicPolygon; azi, distToNxt: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_addedge';
function geod_polygon_compute(const g: PGeodesic; const P: PGeodesicPolygon; reverse, sign: Integer; var pA, pP: Double): Cardinal; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_compute';
function geod_polygon_testpoint(const g: PGeodesic; const P: PGeodesicPolygon; lat, lon: Double; reverse, sign: Integer; var pA, pP: Double): Cardinal; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_testpoint';
function geod_polygon_testedge(const g: PGeodesic; const P: PGeodesicPolygon; azi, distToNxt: Double; reverse, sign: Integer; var pA, pP: Double): Cardinal; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygon_testedge';
procedure geod_polygonarea(const g: PGeodesic; const lats, lons; n: Integer; var area, perimeter: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'geod_polygonarea';
{$endregion 'geodesic related'}

{$region 'projlib'}
function pj_is_latlong(P: TPJ): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_is_latlong';
function pj_is_geocent(P: TPJ): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_is_geocent';
function pj_latlong_from_proj(P: TPJ): TPJ; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_latlong_from_proj';
procedure pj_free(P: TPJ); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_free';
function pj_transform(src, dst: TPJ; point_count: LongInt; point_offset: Integer; var x, y, z: Double): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_transform';
function pj_get_def(P: TPJ; opts: Integer): MarshaledAString; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_def';
function pj_strerrno(errno: Integer): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_strerrno';
function pj_get_release(): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_release';
function pj_ctx_fgets(ctx: TPJCtx; line: PByte; size: Integer; _file: PInteger): PByte; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_fgets';
procedure pj_log(ctx: TPJCtx; level: Integer; fmt: MarshaledAString); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_log'; varargs;
procedure pj_get_spheroid_defn(P: TPJ; var major_axis, eccentricity_squared: Double); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_spheroid_defn';
function pj_get_errno_ref(): PInteger; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_errno_ref';
function pj_context_errno(ctx: TPJCtx): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_context_errno';
function pj_ctx_alloc(): TPJCtx; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_alloc';
function pj_get_default_ctx(): TPJCtx; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_default_ctx';
function pj_get_ctx(P: TPJ): TPJCtx; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_ctx';
function pj_init_plus_ctx(ctx: TPJCtx; defn: MarshaledAString): TPJ; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_init_plus_ctx';
function pj_init_plus(const def: MarshaledAString): TPJ; overload; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_init_plus';
procedure pj_ctx_free(ctx: TPJCtx); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_free';
procedure pj_ctx_set_debug_level(ctx: TPJCtx; lvl: Integer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_set_debug';
function pj_set_log_level(ctx: TPJCtx; lvl: Integer): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_log_level';
procedure pj_ctx_set_logger(ctx: TPJCtx; Logger: Pointer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_set_logger';
procedure pj_ctx_set_fileapi(ctx: TPJCtx; pApi: PProjFileAPI); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_set_fileapi';
procedure pj_set_finder(v: Pointer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_set_finder';
function proj_get_searchpath(): PMarshaledAString; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_get_searchpath';
function proj_get_path_count(): Integer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_get_path_count';
/// Path control for callers that can't practically provide pj_set_finder() style callbacks.
/// Call with (0,nil) as args to clear the searchpath set.
procedure pj_set_searchpath(pathCount: Integer; const pzPath: PMarshaledAString); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_set_searchpath';
function pj_malloc(bBytes: NativeUint): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_malloc';
procedure pj_dalloc(P: Pointer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_dalloc';
function pj_dealloc(P: Pointer): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_dealloc';
procedure pj_ctx_set_app_data(ctx: TPJCtx; pData: Pointer); cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_set_app_data';
function pj_ctx_get_app_data(ctx: TPJCtx): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_ctx_get_app_data';
function pj_get_list_ref(): Pointer; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_get_list_ref';
/// Strip pre- and postfix whitespace. Inline comments (indicated by '#') are considered whitespace.
function pj_chomp(c: MarshaledAString): MarshaledAString; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'pj_chomp';
function pj_torad(angle_in_degrees: Double): Double; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_torad';
function pj_todeg(angle_in_radians: Double): Double; cdecl; external{$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_todeg';
/// Basic info about the current instance of the PROJ.4 library.
function proj_info: TPJInfo; cdecl; external{$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_info';
function proj_pj_info(pPJ: TPJ): TPJProjInfo; cdecl; external {$IFDEF LIBPROJ_LINKSTATIC}name _name_prefix +{$ELSE}proj4libname name {$ENDIF}'proj_pj_info';
{$endregion}
{$ENDREGION}

{$REGION '  libproj precompiled c code'}
{$IFDEF LIBPROJ_LINKSTATIC}
// ::support
{$IFDEF WIN64}{$LINK 'aasincos.o'}{$ELSE}{$LINK 'aasincos.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'adjlon.o'}{$ELSE}{$LINK 'adjlon.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'bch2bps.o'}{$ELSE}{$LINK 'bch2bps.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'bchgen.o'}{$ELSE}{$LINK 'bchgen.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_gauss.o'}{$ELSE}{$LINK 'pj_gauss.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'biveval.o'}{$ELSE}{$LINK 'biveval.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'dmstor.o'}{$ELSE}{$LINK 'dmstor.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'mk_cheby.o'}{$ELSE}{$LINK 'mk_cheby.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_auth.o'}{$ELSE}{$LINK 'pj_auth.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_deriv.o'}{$ELSE}{$LINK 'pj_deriv.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_ell_set.o'}{$ELSE}{$LINK 'pj_ell_set.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_ellps.o'}{$ELSE}{$LINK 'pj_ellps.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_errno.o'}{$ELSE}{$LINK 'pj_errno.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_factors.o'}{$ELSE}{$LINK 'pj_factors.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_fwd.o'}{$ELSE}{$LINK 'pj_fwd.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_init.o'}{$ELSE}{$LINK 'pj_init.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_inv.o'}{$ELSE}{$LINK 'pj_inv.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_list.o'}{$ELSE}{$LINK 'pj_list.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_malloc.o'}{$ELSE}{$LINK 'pj_malloc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_mlfn.o'}{$ELSE}{$LINK 'pj_mlfn.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_msfn.o'}{$ELSE}{$LINK 'pj_msfn.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_open_lib.o'}{$ELSE}{$LINK 'pj_open_lib.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_param.o'}{$ELSE}{$LINK 'pj_param.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_phi2.o'}{$ELSE}{$LINK 'pj_phi2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_pr_list.o'}{$ELSE}{$LINK 'pj_pr_list.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_qsfn.o'}{$ELSE}{$LINK 'pj_qsfn.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_strerrno.o'}{$ELSE}{$LINK 'pj_strerrno.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_tsfn.o'}{$ELSE}{$LINK 'pj_tsfn.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_units.o'}{$ELSE}{$LINK 'pj_units.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_zpoly1.o'}{$ELSE}{$LINK 'pj_zpoly1.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'rtodms.o'}{$ELSE}{$LINK 'rtodms.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'vector1.o'}{$ELSE}{$LINK 'vector1.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_release.o'}{$ELSE}{$LINK 'pj_release.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'geocent.o'}{$ELSE}{$LINK 'geocent.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_transform.o'}{$ELSE}{$LINK 'pj_transform.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_datum_set.o'}{$ELSE}{$LINK 'pj_datum_set.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_datums.o'}{$ELSE}{$LINK 'pj_datums.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_apply_gridshift.o'}{$ELSE}{$LINK 'pj_apply_gridshift.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_gc_reader.o'}{$ELSE}{$LINK 'pj_gc_reader.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_gridcatalog.o'}{$ELSE}{$LINK 'pj_gridcatalog.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'nad_cvt.o'}{$ELSE}{$LINK 'nad_cvt.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'nad_init.o'}{$ELSE}{$LINK 'nad_init.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'nad_intr.o'}{$ELSE}{$LINK 'nad_intr.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_utils.o'}{$ELSE}{$LINK 'pj_utils.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_gridlist.o'}{$ELSE}{$LINK 'pj_gridlist.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_gridinfo.o'}{$ELSE}{$LINK 'pj_gridinfo.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'proj_mdist.o'}{$ELSE}{$LINK 'proj_mdist.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_mutex.o'}{$ELSE}{$LINK 'pj_mutex.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_initcache.o'}{$ELSE}{$LINK 'pj_initcache.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_ctx.o'}{$ELSE}{$LINK 'pj_ctx.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_fileapi.o'}{$ELSE}{$LINK 'pj_fileapi.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_log.o'}{$ELSE}{$LINK 'pj_log.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_apply_vgridshift.o'}{$ELSE}{$LINK 'pj_apply_vgridshift.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_strtod.o'}{$ELSE}{$LINK 'pj_strtod.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_internal.o'}{$ELSE}{$LINK 'pj_internal.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_math.o'}{$ELSE}{$LINK 'pj_math.obj'}{$ENDIF}
// ::pseudo
{$IFDEF WIN64}{$LINK 'PJ_boggs.o'}{$ELSE}{$LINK 'PJ_boggs.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_collg.o'}{$ELSE}{$LINK 'PJ_collg.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_crast.o'}{$ELSE}{$LINK 'PJ_crast.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_denoy.o'}{$ELSE}{$LINK 'PJ_denoy.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eck1.o'}{$ELSE}{$LINK 'PJ_eck1.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eck2.o'}{$ELSE}{$LINK 'PJ_eck2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eck3.o'}{$ELSE}{$LINK 'PJ_eck3.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eck4.o'}{$ELSE}{$LINK 'PJ_eck4.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eck5.o'}{$ELSE}{$LINK 'PJ_eck5.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_fahey.o'}{$ELSE}{$LINK 'PJ_fahey.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_fouc_s.o'}{$ELSE}{$LINK 'PJ_fouc_s.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_gins8.o'}{$ELSE}{$LINK 'PJ_gins8.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_gn_sinu.o'}{$ELSE}{$LINK 'PJ_gn_sinu.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_goode.o'}{$ELSE}{$LINK 'PJ_goode.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_igh.o'}{$ELSE}{$LINK 'PJ_igh.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_hatano.o'}{$ELSE}{$LINK 'PJ_hatano.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_loxim.o'}{$ELSE}{$LINK 'PJ_loxim.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_mbt_fps.o'}{$ELSE}{$LINK 'PJ_mbt_fps.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_mbtfpp.o'}{$ELSE}{$LINK 'PJ_mbtfpp.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_mbtfpq.o'}{$ELSE}{$LINK 'PJ_mbtfpq.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_moll.o'}{$ELSE}{$LINK 'PJ_moll.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_nell.o'}{$ELSE}{$LINK 'PJ_nell.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_nell_h.o'}{$ELSE}{$LINK 'PJ_nell_h.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_putp2.o'}{$ELSE}{$LINK 'PJ_putp2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_putp3.o'}{$ELSE}{$LINK 'PJ_putp3.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_putp4p.o'}{$ELSE}{$LINK 'PJ_putp4p.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_putp5.o'}{$ELSE}{$LINK 'PJ_putp5.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_putp6.o'}{$ELSE}{$LINK 'PJ_putp6.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_robin.o'}{$ELSE}{$LINK 'PJ_robin.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_sts.o'}{$ELSE}{$LINK 'PJ_sts.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_urm5.o'}{$ELSE}{$LINK 'PJ_urm5.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_urmfps.o'}{$ELSE}{$LINK 'PJ_urmfps.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_wag2.o'}{$ELSE}{$LINK 'PJ_wag2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_wag3.o'}{$ELSE}{$LINK 'PJ_wag3.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_wink1.o'}{$ELSE}{$LINK 'PJ_wink1.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_wink2.o'}{$ELSE}{$LINK 'PJ_wink2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_isea.o'}{$ELSE}{$LINK 'PJ_isea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_calcofi.o'}{$ELSE}{$LINK 'PJ_calcofi.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_natearth.o'}{$ELSE}{$LINK 'PJ_natearth.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_natearth2.o'}{$ELSE}{$LINK 'PJ_natearth2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_times.o'}{$ELSE}{$LINK 'PJ_times.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eqearth.o'}{$ELSE}{$LINK 'PJ_eqearth.obj'}{$ENDIF}
// ::azimuthal
{$IFDEF WIN64}{$LINK 'PJ_aeqd.o'}{$ELSE}{$LINK 'PJ_aeqd.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_gnom.o'}{$ELSE}{$LINK 'PJ_gnom.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_laea.o'}{$ELSE}{$LINK 'PJ_laea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_mod_ster.o'}{$ELSE}{$LINK 'PJ_mod_ster.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_nsper.o'}{$ELSE}{$LINK 'PJ_nsper.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_nzmg.o'}{$ELSE}{$LINK 'PJ_nzmg.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_ortho.o'}{$ELSE}{$LINK 'PJ_ortho.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_stere.o'}{$ELSE}{$LINK 'PJ_stere.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_sterea.o'}{$ELSE}{$LINK 'PJ_sterea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'proj_rouss.o'}{$ELSE}{$LINK 'proj_rouss.obj'}{$ENDIF}
// :: conic
{$IFDEF WIN64}{$LINK 'PJ_aea.o'}{$ELSE}{$LINK 'PJ_aea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_bipc.o'}{$ELSE}{$LINK 'PJ_bipc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_bonne.o'}{$ELSE}{$LINK 'PJ_bonne.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eqdc.o'}{$ELSE}{$LINK 'PJ_eqdc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_imw_p.o'}{$ELSE}{$LINK 'PJ_imw_p.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_lcc.o'}{$ELSE}{$LINK 'PJ_lcc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_poly.o'}{$ELSE}{$LINK 'PJ_poly.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_rpoly.o'}{$ELSE}{$LINK 'PJ_rpoly.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_sconics.o'}{$ELSE}{$LINK 'PJ_sconics.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_lcca.o'}{$ELSE}{$LINK 'PJ_lcca.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_ccon.o'}{$ELSE}{$LINK 'PJ_ccon.obj'}{$ENDIF}
// ::cylinder
{$IFDEF WIN64}{$LINK 'PJ_cass.o'}{$ELSE}{$LINK 'PJ_cass.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_cc.o'}{$ELSE}{$LINK 'PJ_cc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_cea.o'}{$ELSE}{$LINK 'PJ_cea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_eqc.o'}{$ELSE}{$LINK 'PJ_eqc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_gall.o'}{$ELSE}{$LINK 'PJ_gall.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_labrd.o'}{$ELSE}{$LINK 'PJ_labrd.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_lsat.o'}{$ELSE}{$LINK 'PJ_lsat.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_misrsom.o'}{$ELSE}{$LINK 'PJ_misrsom.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_merc.o'}{$ELSE}{$LINK 'PJ_merc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_mill.o'}{$ELSE}{$LINK 'PJ_mill.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_ocea.o'}{$ELSE}{$LINK 'PJ_ocea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_omerc.o'}{$ELSE}{$LINK 'PJ_omerc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_patterson.o'}{$ELSE}{$LINK 'PJ_patterson.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_somerc.o'}{$ELSE}{$LINK 'PJ_somerc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_tcc.o'}{$ELSE}{$LINK 'PJ_tcc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_tcea.o'}{$ELSE}{$LINK 'PJ_tcea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_tmerc.o'}{$ELSE}{$LINK 'PJ_tmerc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_geos.o'}{$ELSE}{$LINK 'PJ_geos.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_gstmerc.o'}{$ELSE}{$LINK 'PJ_gstmerc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'proj_etmerc.o'}{$ELSE}{$LINK 'proj_etmerc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_comill.o'}{$ELSE}{$LINK 'PJ_comill.obj'}{$ENDIF}
// ::misc
{$IFDEF WIN64}{$LINK 'PJ_airy.o'}{$ELSE}{$LINK 'PJ_airy.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_aitoff.o'}{$ELSE}{$LINK 'PJ_aitoff.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_august.o'}{$ELSE}{$LINK 'PJ_august.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_bacon.o'}{$ELSE}{$LINK 'PJ_bacon.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_chamb.o'}{$ELSE}{$LINK 'PJ_chamb.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_hammer.o'}{$ELSE}{$LINK 'PJ_hammer.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_lagrng.o'}{$ELSE}{$LINK 'PJ_lagrng.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_larr.o'}{$ELSE}{$LINK 'PJ_larr.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_lask.o'}{$ELSE}{$LINK 'PJ_lask.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_nocol.o'}{$ELSE}{$LINK 'PJ_nocol.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_ob_tran.o'}{$ELSE}{$LINK 'PJ_ob_tran.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_oea.o'}{$ELSE}{$LINK 'PJ_oea.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_sch.o'}{$ELSE}{$LINK 'PJ_sch.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_tpeqd.o'}{$ELSE}{$LINK 'PJ_tpeqd.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_vandg.o'}{$ELSE}{$LINK 'PJ_vandg.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_vandg2.o'}{$ELSE}{$LINK 'PJ_vandg2.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_vandg4.o'}{$ELSE}{$LINK 'PJ_vandg4.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_wag7.o'}{$ELSE}{$LINK 'PJ_wag7.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_latlong.o'}{$ELSE}{$LINK 'PJ_latlong.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_krovak.o'}{$ELSE}{$LINK 'PJ_krovak.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_geoc.o'}{$ELSE}{$LINK 'PJ_geoc.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'pj_geocent.o'}{$ELSE}{$LINK 'pj_geocent.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_healpix.o'}{$ELSE}{$LINK 'PJ_healpix.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_qsc.o'}{$ELSE}{$LINK 'PJ_qsc.obj'}{$ENDIF}
// ::geodesic
{$IFDEF WIN64}{$LINK 'geodesic.o'}{$ELSE}{$LINK 'geodesic.obj'}{$ENDIF}
// ::pipeline
{$IFDEF WIN64}{$LINK 'proj_4D_api.o'}{$ELSE}{$LINK 'proj_4D_api.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_cart.o'}{$ELSE}{$LINK 'PJ_cart.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_pipeline.o'}{$ELSE}{$LINK 'PJ_pipeline.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_horner.o'}{$ELSE}{$LINK 'PJ_horner.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_helmert.o'}{$ELSE}{$LINK 'PJ_helmert.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_vgridshift.o'}{$ELSE}{$LINK 'PJ_vgridshift.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_hgridshift.o'}{$ELSE}{$LINK 'PJ_hgridshift.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_unitconvert.o'}{$ELSE}{$LINK 'PJ_unitconvert.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_molodensky.o'}{$ELSE}{$LINK 'PJ_molodensky.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_deformation.o'}{$ELSE}{$LINK 'PJ_deformation.obj'}{$ENDIF}
{$IFDEF WIN64}{$LINK 'PJ_axisswap.o'}{$ELSE}{$LINK 'PJ_axisswap.obj'}{$ENDIF}
{$ENDIF} // LIBPROJ_LINKSTATIC

{$ENDREGION}

{$REGION '  globals variables'}

var FGlobalContext: TPJCtx = nil;
var FPJInternalCtx: TPJCtx = nil;

procedure _DefaultLogHandler(AppData: Pointer; level: Integer; msg: Pointer); cdecl;

  function LogLevelCodeToString(value: Integer): string;
  begin
    Result := '';
    case value of
      1:
        Result := 'libPROJ error: ';
      2:
        Result := 'libPROJ debug: ';
      3:
        Result := 'libPROJ trace: ';
      4:
        Result := 'libPROJ tell: ';
    end;
  end;

var
  MsgText: string;
begin
{$IFNDEF libproj_debug}
  if level > 1 then
    Exit;
{$ENDIF}
  MsgText := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(msg));
  if MsgText <> '' then
  begin
    MsgText := LogLevelCodeToString(level) + MsgText;
{$IFDEF MSWINDOWS}
    if IsConsole then
      Writeln(MsgText)
    else
      OutputDebugString(PChar(MsgText));
{$ENDIF}
  end;
end;

function _DefaultFileApiFileOpen(ctx: TPJCtx; const zFileName, zAccess: MarshaledAString): Pointer { pfile_t }; cdecl;
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  Result := Pointer({$IFDEF WIN64}FOpen{$ELSE}_fopen{$ENDIF}(zFileName, zAccess));
{$ELSE}
    raise ENotImplemented.Create('sorry file open api not yet implemented');
{$ENDIF}
end;

function _DefaultFileApiFileRead(buffer: Pointer; size, nmemb: NativeUint; pFile: Pointer { pfile_t } ): NativeUint;
cdecl
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  Result := {$IFDEF WIN64}FRead{$ELSE}_fread{$ENDIF}(buffer, size, nmemb, pfile_t(pFile));
{$ELSE}
    raise ENotImplemented.Create('sorry file read api not yet implemented');
{$ENDIF}
end;

function _DefaultFileApiFileSeek(pFile: Pointer { pfile_t }; offset: LongInt; whence: Integer): Integer;
cdecl
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  Result := {$IFDEF WIN64}FSeek{$ELSE}_fseek{$ENDIF}(pfile_t(pFile), offset, whence);
{$ELSE}
    raise ENotImplemented.Create('sorry file seek api not yet implemented');
{$ENDIF}
end;

function _DefaultFileApiFileTell(pFile: Pointer { pfile_t } ): LongInt; cdecl;
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  Result := {$IFDEF WIN64}FTell{$ELSE}_ftell{$ENDIF}(pfile_t(pFile));
{$ELSE}
    raise ENotImplemented.Create('sorry file tell api not yet implemented');
{$ENDIF}
end;

procedure _DefaultFileApiFileClose(pFile: Pointer { pfile_t } ); cdecl;
begin
{$IFDEF LIBPROJ_LINKSTATIC}
{$IFDEF WIN64}FClose{$ELSE}_fclose{$ENDIF}(pfile_t(pFile));
{$ELSE}
    raise ENotImplemented.Create('sorry file close api not yet implemented');
{$ENDIF}
end;

function _DefaultFinderFunc(const z: MarshaledAString): MarshaledAString; cdecl;
begin
  Result := z;
end;

const
  _ProjDefaultFileAPI: TProjFileAPI = (
    FOpen: _DefaultFileApiFileOpen;
    FRead: _DefaultFileApiFileRead;
    FSeek: _DefaultFileApiFileSeek;
    FTell: _DefaultFileApiFileTell;
    FClose: _DefaultFileApiFileClose;
  );

procedure _InitPJContext(ACtx: TPJCtx; AUserData: TObject);
begin
  if not Assigned(ACtx) then Exit;

  pj_ctx_set_logger(ACtx, @_DefaultLogHandler);
  pj_ctx_set_fileapi(ACtx, @_ProjDefaultFileAPI);
  pj_set_finder(@_DefaultFinderFunc);
{$IFDEF libproj_debug}
    pj_ctx_set_debug_level(Result, 4);
    pj_set_log_level_(Result, 4);
{$ENDIF}
  if (AUserData <> nil) then
    pj_ctx_set_app_data(ACtx, AUserData);

end;

{$ENDREGION}

{$REGION '  helper functions'}
  { misc }

function CStringPointerToString(value: MarshaledAString): string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(value));
end;

function StringToCStringPointer(const value: string): Pointer;
begin
  Result := FPJMarshaller.AsAnsi(value).ToPointer;
end;

{$REGION 'strip non printable chars from description'}
function ReplaceTabCrLF(const s: string): string;

  function StripChars(const s: string; AChars: TSysCharSet): string;
  var
    c1, c2: Char;
    i, l1, l2: Integer;
  begin
    c1 := #0;
    l1 := Length(s);
    SetLength(Result, l1);
    l2 := 0;
    for i := 1 to l1 do
    begin
      c2 := s[i];
      if not((c1 = c2) and CharInSet(c2, AChars)) then
      begin
        Inc(l2);
        Result[l2] := c2;
        c1 := c2;
      end;
    end;

    SetLength(Result, l2);
  end;

begin
  Result := s;

  Result := Result.Replace(#10, ' ');
  Result := Result.Replace(#13, ' ');
  Result := Result.Replace(#9, ' ');

  Result := StripChars(Result, [' ']);
end;
{$ENDREGION}

{ TPJInfoHelper }

function TPJInfoHelper.Paths: TArray<string>;
var
  i: Integer;
  P: PMarshaledAString;
begin
  SetLength(Result, Self.iPathCount);
  P := Self.zPaths;
  for i := 0 to Self.iPathCount - 1 do
  begin
    Result[i] := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(pj_chomp(P^)));

    Inc(P);
  end;
end;

function TPJInfoHelper.Release: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(Self.zRelease));
end;

function TPJInfoHelper.SearchPath: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(Self.zSearchPath));
end;

function TPJInfoHelper.Version: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(Self.zVersion));
end;

{ TPJProjInfoHelper }

function TPJProjInfoHelper.Definition: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(zDefinition));
end;

function TPJProjInfoHelper.Description: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(zDescription));
end;

function TPJProjInfoHelper.Id: string;
begin
  Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(zId));
end;

{ TPJhelper }

function TPJhelper.Definition: string;
var
  defn: MarshaledAString;
begin
  Result := '';
  if not Self.IsValid then
    Exit;

  defn := pj_get_def(@Self,0);
  if defn <> nil then
  begin
    Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(defn));
    pj_dealloc(defn);
  end;
end;

function TPJhelper.Description: string;
var
  info: TPJProjInfo;
begin
  if not GetPJInfo(@Self, info) then
    Result := ''
  else
    Result := ReplaceTabCrLF(info.Description);
end;

function TPJhelper.GetSpheroidInfo(var a, e2: Double): Boolean;
begin
  Result := Self.IsValid;
  if not Result then
    Exit;
  pj_get_spheroid_defn(@Self, a, e2);
  Result := not(a.IsNan or a.IsInfinity or e2.IsNan or e2.IsInfinity);
end;

function TPJhelper.GetToMetersFactor: Double;
{$ifndef _proj_api_deepdive}
type
  TpjLU = record
    pjKey: string;
    wktName: string;
    toMeters: Double;
    Description: string;
  end;
const
  cPjUnitsToken = 'units';
  pjLUList: array [0 .. 20] of TpjLU = ((pjKey: 'm'; wktName: 'Meter'; toMeters: 1; Description: 'Meters'), (pjKey: 'km'; wktName: 'Kilometer'; toMeters: 1000; Description: 'Kilometers'), (pjKey: 'dm'; wktName: 'Decimeter'; toMeters: 0.1; Description: 'Decimeters'), (pjKey: 'cm';
    wktName: 'Centimeter'; toMeters: 0.01; Description: 'Centimeters'), (pjKey: 'mm'; wktName: 'Millimeter'; toMeters: 0.001; Description: 'Millimeters'), (pjKey: 'ft'; wktName: 'Foot_International'; toMeters: 0.3048; Description: 'Foots (International)'), (pjKey: 'us-ft';
    wktName: 'Foot_US'; toMeters: 0.3048006096012192; Description: 'Foots (US survey)'), (pjKey: 'ind-ft'; wktName: 'Foot_Indian'; toMeters: 0.30479841; Description: 'Foots (Indian)'), (pjKey: 'kmi'; wktName: 'Nautical_Mile_International'; toMeters: 1852.0;
    Description: 'Nautical Miles (International)'), (pjKey: 'mi'; wktName: 'Statute_Mile_International'; toMeters: 1609.344; Description: 'Statute Miles (International)'), (pjKey: 'us-mi'; wktName: 'Statute_Mile_US_Surveyor'; toMeters: 1609.347218694437;
    Description: 'Statute Miles (US survey)'), (pjKey: 'link'; wktName: 'Link'; toMeters: 0.20116684023368047; Description: 'Links (Based on US Foot)'), (pjKey: 'yd'; wktName: 'Yard_International'; toMeters: 0.9144; Description: 'Yards (International)'), (pjKey: 'us-yd';
    wktName: 'Yard_US_Surveyor'; toMeters: 0.914401828803658; Description: 'Yards (US survey)'), (pjKey: 'ind-yd'; wktName: 'Yard_Indian'; toMeters: 0.91439523; Description: 'Yards (Indian)'), (pjKey: 'in'; wktName: 'Inch_International'; toMeters: 0.0254;
    Description: 'Inchs (International)'), (pjKey: 'us-in'; wktName: 'Inch_US_Surveyor'; toMeters: 0.025400050800101603; Description: 'Inchs (US survey)'), (pjKey: 'fath'; wktName: 'Fathom_International'; toMeters: 1.8288; Description: 'Fathoms (International)'), (pjKey: 'ch';
    wktName: 'Chain_International'; toMeters: 20.1168; Description: 'Chains (International)'), (pjKey: 'us-ch'; wktName: 'Chain_US_Surveyor'; toMeters: 20.11684023368047; Description: 'Chains (US survey)'), (pjKey: 'ind-ch'; wktName: 'Chain_Indian'; toMeters: 20.11669506;
    Description: 'Chains (Indian)'));

  function internalGetToMetersFactor: Double;
  var
    i1, i2: Integer;
    defn: string;
  begin
    defn := Self.Definition;    // todo add +to_meter support

    i1 := defn.IndexOf(cPjUnitsToken) + 1;
    Inc(i1, Length(cPjUnitsToken));
    while not CharInSet(defn[i1], ['=']) and (i1 < Length(defn)) do
      Inc(i1);

    i2 := i1;
    while not CharInSet(defn[i2], [' ', '+']) and (i2 < Length(defn)) do
      Inc(i2);

    defn := defn.Substring(i1, i2 - i1).Trim;

    for i1 := 0 to High(pjLUList) do
    begin
      if SameText(defn, pjLUList[i1].pjKey) then
      begin
        Result := pjLUList[i1].toMeters;
        Exit;
      end;

    end;

    Result := 1;
  end;
{$endif}
begin
  Result := 1;
  if not Self.IsValid or Self.IsGeographic then
    Exit;
{$ifndef _proj_api_deepdive}
    Result := internalGetToMetersFactor;
{$else}
    Result := Self.to_meter;
{$endif}
end;

function TPJhelper.Id: string;
var
  info: TPJProjInfo;
begin
  if GetPJInfo(@Self, info) then
    Result := info.Id
  else
    Result := '';
end;

function TPJhelper.IsGeocentric: LongBool;
begin
  if not Self.IsValid then
    Result := LongBool(-1)
  else
{$ifndef _proj_api_deepdive}
    Result := LongBool(pj_is_geocent(@Self));
{$else}
    Result := LongBool(Self.is_geocent);
{$endif}
end;

function TPJhelper.IsGeographic: LongBool;
begin
  if not Self.IsValid then
    Result := LongBool(-1)
  else
{$ifndef _proj_api_deepdive}
    Result := LongBool(pj_is_latlong(@Self));
{$else}
    Result := LongBool(Self.is_latlong);
{$endif}
end;

function TPJhelper.IsValid: Boolean;
begin
  Result := @Self <> nil;
end;

{ TGeodesicPolygonHelper }

procedure TGeodesicPolygonHelper.AddPoint(const g: TGeodesic; const yLat,xLon: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygon_addpoint(@g,@Self, yLat, xLon);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure TGeodesicPolygonHelper.AddEdge(const g: TGeodesic; const Azi, DistToNxt: Double);
begin
  GeodesicPolygonAddEdge(g,Self,Azi, DistToNxt);
end;

procedure TGeodesicPolygonHelper.Init(AsPolyline: Boolean);
begin
  GeodesicPolygonInit(Self, AsPolyline);
end;

procedure TGeodesicPolygonHelper.Clear;
begin
  GeodesicPolygonClear(Self);
end;

function TGeodesicPolygonHelper.Compute(const g: TGeodesic; reversed, signed: boolean; var area, perimeter: double): Cardinal;
begin
  Result := GeodesicPolygonCompute(g, Self, reversed, signed, area, perimeter);
end;

class function TGeodesicPolygonHelper.Create(g: TGeodesic; const yLats, xLons: TArray<Double>; AsPolyline: Boolean): TGeodesicPolygon;
begin
  Result := GeodesicPolygonCreate(g,yLats, xLons,AsPolyline);
end;

function TGeodesicPolygonHelper.TestPoint(const g: TGeodesic; const yLat, xLon: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
begin
  Result := GeodesicPolygonTestPoint(g,Self,yLat, xLon, reversed, signed, area, perimeter);
end;

function TGeodesicPolygonHelper.TestEdge(const g: TGeodesic; const azi, distToNxt: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
begin
  Result := GeodesicPolygonTestEdge(g, Self, azi, distToNxt, reversed, signed, area, perimeter);
end;
{$ENDREGION} // helper functions

{$REGION '  proj api adaptation'}
procedure GeodesicInit(var g: TGeodesic; a, f: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_init(@g, a, f);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicPolygonInit(var P: TGeodesicPolygon; AsPolyLine: Boolean);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygon_init(p, IfThen(AsPolyLine,1,0));
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

function GeodesicPolygonCreate(const g: TGeodesic; yLats,xLons: TArray<Double>; AsPolyLine: Boolean = False): TGeodesicPolygon;
var i: integer;
begin
  GeodesicPolygonInit(Result, AsPolyline);
  if Length(yLats) = Length(xLons) then
  begin
    for i := Low(yLats) to High(yLats) do
      GeodesicPolygonAddPoint(g,Result,yLats[i],xLons[i]);
  end;
end;

procedure GeodesicPolygonClear(var P: TGeodesicPolygon);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygon_clear(p);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicPolygonAddPoint(const g: TGeodesic; var p: TGeodesicPolygon; const yLat,xLon: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygon_addpoint(@g,@p,yLat,xLon);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicPolygonAddEdge(const g: TGeodesic; var p: TGeodesicPolygon; azi, distToNxt: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygon_addedge(@g,@p,azi, distToNxt);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

function GeodesicPolygonCompute(const g: TGeodesic; var p: TGeodesicPolygon; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    Result := geod_polygon_compute(@g,@p,IfThen(reversed,1,0), IfThen(signed,1,0),area, perimeter);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

function GeodesicPolygonTestPoint(const g: TGeodesic; var p: TGeodesicPolygon; const xLat, xLon: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    Result := geod_polygon_testpoint(@g,@p, xLat, xLon,IfThen(reversed,1,0), IfThen(signed,1,0), area, perimeter);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

function GeodesicPolygonTestEdge(const g: TGeodesic; var p: TGeodesicPolygon; const azi, distToNxt: Double; reversed, signed: Boolean; var area, perimeter: Double): Cardinal;
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    Result := geod_polygon_testedge(@g,@p, azi, distToNxt,IfThen(reversed,1,0), IfThen(signed,1,0), area, perimeter);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicPolygonArea(const g: TGeodesic; const yLats: TArray<Double>; const xLons: TArray<Double>; var area,perimeter: Double);
var
  nPoints: Integer;
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  nPoints := Length(yLats);
  if (nPoints < 3) or (nPoints <> Length(xLons)) then
  begin
    area := Infinity;
    perimeter := Infinity;
    Exit;
  end;

  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_polygonarea(@g, yLats[0], xLons[0], nPoints, area, perimeter);
{$ENDIF}
    area := Abs(area);
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicPolygonArea(const a, f: Double; const yLats: TArray<Double>; const xLons: TArray<Double>; var area, perimeter: Double);
var
  g: TGeodesic;
begin
  GeodesicInit(g, a, f);
  GeodesicPolygonArea(g, yLats, xLons, area, perimeter);
end;

procedure GeodesicPolygonArea(const g: TGeodesic; const Points: TArray<TLocationCoord2D>; var area,perimeter: Double);
var
  lats, lons: TArray<Double>;
  i, nPoints: Integer;
begin
  nPoints := Length(Points);

  SetLength(lats, nPoints);
  SetLength(lons, nPoints);

  for i := 0 to nPoints - 1 do
  begin
    lats[i] := Points[i].Latitude;
    lons[i] := Points[i].Longitude;
  end;

  GeodesicPolygonArea(g, lats, lons, area, perimeter);
end;

procedure GeodesicPolygonArea(const a, f: Double; const Points: TArray<TLocationCoord2D>; var area,perimeter: Double);
var
  g: TGeodesic;
begin
  GeodesicInit(g, a, f);
  GeodesicPolygonArea(g, Points, area, perimeter);
end;

procedure GeodesicDirect(const g: TGeodesic; const yLat1, xLon1: Double; const Azimuth, Distance: Double; var yLat2, xLon2: Double; var ForwardAzimuth: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_direct(@g, yLat1, xLon1, Azimuth, Distance, yLat2, xLon2, ForwardAzimuth);
{$ENDIF}
  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicDirect(const a, f: Double; const yLat1, xLon1: Double; const Azimuth, Distance: Double; var yLat2, xLon2: Double; var ForwardAzimuth: Double);
var
  g: TGeodesic;
begin
  GeodesicInit(g, a, f);
  GeodesicDirect(g, yLat1, xLon1, Azimuth, Distance, yLat2, xLon2, ForwardAzimuth);
end;

procedure GeodesicDirect(const g: TGeodesic; const StartPoint: TLocationCoord2D; const Azimuth, Distance: Double; var DestPoint: TLocationCoord2D; var ForwardAzimuth: Double);
begin
  GeodesicDirect(g, StartPoint.Latitude, StartPoint.Longitude, Azimuth, Distance, DestPoint.Latitude, DestPoint.Longitude, ForwardAzimuth);
end;

procedure GeodesicDirect(const a, f: Double; const StartPoint: TLocationCoord2D; const Azimuth, Distance: Double; var DestPoint: TLocationCoord2D; var ForwardAzimuth: Double);
var
  g: TGeodesic;
begin
  GeodesicInit(g, a,f);
  GeodesicDirect(g, StartPoint.Latitude, StartPoint.Longitude, Azimuth, Distance, DestPoint.Latitude, DestPoint.Longitude, ForwardAzimuth);
end;

procedure GeodesicInverse(const g: TGeodesic; const yLat1,xLon1,yLat2,xLon2: Double; var Distance, Azimuth, ForwardAzimuth: Double);
var
  savedMathErrorsMask: TArithmeticExceptionMask;
begin
  savedMathErrorsMask := System.Math.GetExceptionMask;
  try
    System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$IFDEF LIBPROJ_LINKSTATIC}
    geod_inverse(@g, yLat1,xLon1,yLat2,xLon2, Distance, Azimuth, ForwardAzimuth);
{$ENDIF}
    if Distance.IsNan or Distance.IsInfinity then Distance := 0;

    if Azimuth.IsNan or Azimuth.IsInfinity then Azimuth := 0;

    if ForwardAzimuth.IsNan or ForwardAzimuth.IsInfinity then ForwardAzimuth := 0

  finally
    System.Math.SetExceptionMask(savedMathErrorsMask);
  end;
end;

procedure GeodesicInverse(const g: TGeodesic; const StartPoint,EndPoint: TLocationCoord2D; var Distance, Azimuth, ForwardAzimuth: Double);
begin
  GeodesicInverse(g,StartPoint.Latitude,StartPoint.Longitude,EndPoint.Latitude,EndPoint.Longitude,Distance,Azimuth , ForwardAzimuth	);
end;

procedure GeodesicInverse(const a, f: Double; const StartPoint,EndPoint: TLocationCoord2D; var Distance, Azimuth, ForwardAzimuth: Double);
var
  g: TGeodesic;
begin
  GeodesicInit(g, a, f);
  GeodesicInverse(g, StartPoint, EndPoint,Distance,Azimuth, ForwardAzimuth);
end;

procedure GeodesicInverse(const a, f: Double; const yLat1,xLon1,yLat2,xLon2: Double; var Distance, Azimuth, ForwardAzimuth: Double);
begin
  GeodesicInverse(a, f, yLat1,xLon1,yLat2,xLon2, Distance, Azimuth, ForwardAzimuth);
end;

function PJ_is_valid(P: TPJ): Boolean;
begin
  Result := Assigned(P);
end;

function CreatePJContext(const AUserData: TObject): TPJCtx;
begin
  Result := pj_ctx_alloc;

  if Result = nil then
  begin
    Result := FGlobalContext;
    Exit;
  end;

  _InitPJContext(Result, AUserData);
end;

function GetPJContextUserData(const ACxt: TPJCtx): TObject;
begin
  Result := nil;
  if ACxt <> nil then
    Result := pj_ctx_get_app_data(ACxt);
end;

procedure FreePJContext(var ACxt: TPJCtx);
begin
  if (ACxt <> nil) and (ACxt <> FPJInternalCtx) then
    pj_ctx_free(ACxt);
  ACxt := nil;
end;

function CreatePJ(const def: string; ACtx: TPJCtx; AErrorText: PString): TPJ;
var
  ctx: TPJCtx;
  m: TMarshaller;
  savedMathErrorsMask: TArithmeticExceptionMask;
begin

  Result := nil;

  ctx := ACtx;

  if ctx = nil then
  begin
    if not Assigned(FGlobalContext) then
    begin
{$IFDEF libproj_debug}
      pj_log(pj_get_default_ctx, 4, m.AsAnsi('"%s"').ToPointer, m.AsAnsi('using proj internal context').ToPointer);
{$ENDIF}
    end
    else
      ctx := FGlobalContext;
  end;

  if ctx <> nil then
  begin
{$IFDEF libproj_debug}
    pj_log_(ctx, 3, m.AsAnsi('"%s"').ToPointer, m.AsAnsi('create projection for ' + def).ToPointer);
{$ENDIF}
    savedMathErrorsMask := System.Math.GetExceptionMask;
    try
      System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

      Result := pj_init_plus_ctx(ctx, m.AsAnsi(def).ToPointer);
      if Assigned (AErrorText) then
      begin
        if Result <> nil then
          AErrorText^ := ''
        else
          AErrorText^ := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(pj_strerrno(pj_context_errno(ctx))))
      end;
    finally
      System.Math.SetExceptionMask(savedMathErrorsMask);
    end;

{$IFDEF libproj_debug}
    if Result = nil then
    begin
      pj_log_(ctx, 3, m.AsAnsi('"%s"').ToPointer, m.AsAnsi('projection for ' + def + ' failed witn ' + _pj_context_errno(ctx).ToString + ' code').ToPointer);
    end
    else
      pj_log_(ctx, 3, m.AsAnsi('"%s"').ToPointer, m.AsAnsi('projection for ' + def + ' created').ToPointer);
{$ENDIF}
  end;
  // pj_pr_list(Result);
end;

procedure FreePJ(var pj: TPJ);
begin
  if pj <> nil then
  begin
    pj_free(pj);
    pj := nil;
  end;
end;

function PJ_get_definition(P: TPJ): string;
var
  d: Pointer;
begin
  d := pj_get_def(P, 0);
  if d <> nil then
  begin
    Result := Trim(CStringPointerToString(d));
    pj_dealloc(d);
  end
end;

function PJ_is_geographic(P: TPJ): Boolean;
begin
  Result := Boolean(pj_is_latlong(P));
end;

function PJ_is_geocentric(P: TPJ): Boolean;
begin
  Result := Boolean(pj_is_geocent(P));
end;

function PJ_is_same_definition(p1, p2: TPJ): Integer;
var
  d1, d2: string;
begin
  Result := -2;
  if PJ_is_valid(p1) and PJ_is_valid(p2) then
  begin
    Result := -1;

    d1 := PJ_get_definition(p1);
    d2 := PJ_get_definition(p2);

    if (d1 <> '') and (d2 <> '') then
      Result := Integer(SameText(d1, d2))
  end;
end;

function TransformPoints2D(src, dst: TPJ; const x, y: PDouble; count: Integer; angular: Boolean): Integer;
{$define localcopy}
{.$define forcecheck}
var
  degSrc, degDst: Boolean;
  i: Integer;
  z: PDouble;
  savedMathErrorsMask: TArithmeticExceptionMask;
  _x, _y: {$ifdef localcopy}TArray<Double>{$else}PDouble{$endif};
begin
{$ifdef forcecheck}
  Result := -1;
  if PJ_is_valid(src) and PJ_is_valid(dst) then
  begin
    Result := PJ_is_same_definition(src, dst);
    // projections differs
    if Result = 0 then //
    begin
{$endif}
      savedMathErrorsMask := System.Math.GetExceptionMask;
      try
        System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
        if not angular then
        begin
          degSrc := PJ_is_geographic(src);
          degDst := PJ_is_geographic(dst);
        end
        else
        begin
          degSrc := False;
          degDst := False;
        end;

//        _x := GetMemory(count * SizeOf(Double));
//        _y := GetMemory(count * SizeOf(Double));
//
//        Move(x^,_x^,count * SizeOf(Double));
//        Move(y^,_y^,count * SizeOf(Double));

        {$ifdef localcopy}
           SetLength(_x,count);
           SetLength(_y,count);

           Move(x^,_x[0],count * SizeOf(Double));
           Move(y^,_y[0],count * SizeOf(Double));

          if degSrc then
          begin
            for i := 0 to count - 1 do
            begin
              _x[i] := pj_torad(_x[i]); // PJ_DEG_TO_RAD * _x^;
              _y[i] := pj_torad(_y[i]); // PJ_DEG_TO_RAD * _y^;
            end;
          end;
        {$else}
          if degSrc then
          begin
            _x := x;
            _y := y;
            for i := 0 to count - 1 do
            begin
              _x^ := pj_torad(_x^); // PJ_DEG_TO_RAD * _x^;
              _y^ := pj_torad(_x^); // PJ_DEG_TO_RAD * _y^;

              Inc(_x);
              Inc(_y);
            end;
          end;
        {$endif}

        z := nil;

        {$ifdef localcopy}
        Result := pj_transform(src, dst, count, 1, _x[0], _y[0], z^);
        {$else}
        Result := pj_transform(src, dst, count, 1, x^, y^, z^);
        {$endif}

        if Result = 0 then
        begin
          if degDst then
          begin
            {$ifdef localcopy}
            for i := 0 to count - 1 do
            begin
              _x[i] := pj_todeg(_x[i]); // PJ_RAD_TO_DEG * _x^;
              _y[i] := pj_todeg(_y[i]); // PJ_RAD_TO_DEG * _y^;
            end;
            {$else}
            _x := x;
            _y := y;
            for i := 0 to count - 1 do
            begin
              _x^ := pj_todeg(_x^); // PJ_RAD_TO_DEG * _x^;
              _y^ := pj_todeg(_y^); // PJ_RAD_TO_DEG * _y^;

              Inc(_x);
              Inc(_y);
            end;
           {$endif}
          end;
          {$ifdef localcopy}
          Move(_x[0],x^,count * SizeOf(Double));
          Move(_y[0],y^,count * SizeOf(Double));
          {$endif}
        end;
      finally
        System.Math.SetExceptionMask(savedMathErrorsMask);
      end;
{$ifdef forcecheck}
    end;
  end;
{$endif}
end;

function TransformPoint2D(src, dst: TPJ; var x, y: Double; angular: Boolean): Integer;
begin
  Result := TransformPoints2D(src, dst, @x, @y, 1, angular);
end;

function SetPROJDataSearchPaths(const ANewPaths: TArray<string>): TArray<string>;
var
  Paths: PMarshaledAString;
  newPaths: TArray<MarshaledAString>;
  i, c: Integer;
  m: TMarshaller;
begin
  Result := nil;

  c := proj_get_path_count;
  if c > 0 then
  begin
    Paths := proj_get_searchpath;
    for i := 0 to c - 1 do
      Result[i] := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(PMarshaledAStrings(Paths)[i]));
  end;

  c := Length(ANewPaths);
  if c > 0 then
  begin
    SetLength(newPaths, c);
    for i := 0 to c - 1 do
      newPaths[i] := m.AsAnsi(ANewPaths[i]).ToPointer;

    pj_set_searchpath(c, @newPaths[0]);
  end;
end;

function GetProjectionsList(Dest: TStrings): Integer;
type
  TPJListItem = record
    Id: MarshaledAString;
    pj: Pointer;
    Desc: PMarshaledAString;
  end;

  PPJListItem = ^TPJListItem;
var
  P: PPJListItem;

begin
  Dest.Clear;

  P := pj_get_list_ref();
  while Assigned(P.Id) do
  begin
    Dest.AddPair(TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(P.Id)), ReplaceTabCrLF(TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(pj_chomp(P.Desc^)))));
    Inc(P);
  end;
  Result := Dest.count;
end;

function GetPROJInfo(var info: TPJInfo): Boolean;
begin
  info := proj_info;
  Result := Assigned(info.zRelease);
end;

function GetPJInfo(const pPJ: TPJ; var info: TPJProjInfo): Boolean;
begin
  Result := Assigned(pPJ);
  if Result then
    info := proj_pj_info(pPJ);
end;

function GetLatLong(const APJ: TPJ; var LatLong: TPJ): Boolean;
begin
  Result := Assigned(APJ);
  if not Result then
    Exit;

  LatLong := pj_latlong_from_proj(APJ);
  Result := Assigned(LatLong);
end;

function GetSpheroidInfo(const APJ: TPJ; var a, b, e2: Double): Boolean;
begin
  Result := Assigned(APJ);
  if Result then
  begin
  {$ifdef _proj_api_deepdive}
    a := APJ^.a;
    e2 := APJ^.e2;
    b := APJ^.b;
  {$else}
      Result := APJ^.GetSpheroidInfo(a, e2);
      if Result then
        b := a - e2 * a;
  {$endif}
  end
  else
    b := a.PositiveInfinity;
end;

{$ENDREGION} // public api

{$region '  global variables'}
procedure projlib_init_globals;
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  FPJInternalCtx := pj_get_default_ctx;
  _InitPJContext(FPJInternalCtx, nil);

  FGlobalContext := CreatePJContext(nil);
  if FGlobalContext = nil then
    FGlobalContext := FPJInternalCtx
  else
    _InitPJContext(FGlobalContext, nil);
{$ifdef debug}
   pj_log(FGlobalContext, 0, FPJMarshaller.AsAnsi('">>> libPROJ4 static %s"').ToPointer, pj_get_release);
{$endif}
{$ENDIF}
end;

procedure projlib_final_globals;
begin
{$IFDEF LIBPROJ_LINKSTATIC}
  if Assigned(FGlobalContext) then
    FreePJContext(FGlobalContext);
{$ENDIF}
  FPJInternalCtx := nil;
  FPJMarshaller.Flush;
end;
{$endregion}

{$IFDEF LIBPROJ_LINKSTATIC}{$IFDEF WIN64}{$HINTS OFF}{$ENDIF}{$ENDIF}   // for remove BCC64 hints
initialization
projlib_init_globals;
finalization
projlib_final_globals;
end.


