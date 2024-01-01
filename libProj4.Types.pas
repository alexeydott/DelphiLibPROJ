unit libProj4.Types;

interface
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
uses

{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  System.Classes, System.SysUtils, System.Math, System.Sensors;

{$REGION 'proj4 related types'}

const
  proj_api_deepdive: Boolean = {$ifndef __proj_api_deepdive}False{$else}True{$endif};

  PJ_DIRECTION_FWD = 1;
  PJ_DIRECTION_IDENT = 0;
  PJ_DIRECTION_INV = -1;

  PJ_PARAM_GROUP_OTHER = 0;
  PJ_PARAM_GROUP_GENERAL = 1;
  PJ_PARAM_GROUP_ELLIPSOID = 2;
  PJ_PARAM_GROUP_DATUM = 3;

  PJ_PARAM_GROUP_MAX = PJ_PARAM_GROUP_DATUM + 1;

  // from projects.h

// datum_type values
  cPJD_UNKNOWN   = 0;
  cPJD_3PARAM    = 1;
  cPJD_7PARAM    = 2;
  cPJD_GRIDSHIFT = 3;
  cPJD_WGS84     = 4; // WGS84 (or anything considered equivalent)

// library errors
  cPJ_ERR_NO_ERROR                  =  0;
  cPJD_ERR_NO_ARGS                  = -1;
  cPJD_ERR_NO_OPTION_IN_INIT_FILE   = -2;
  cPJD_ERR_NO_COLON_IN_INIT_STRING  = -3;
  cPJD_ERR_PROJ_NOT_NAMED           = -4;
  cPJD_ERR_UNKNOWN_PROJECTION_ID    = -5;
  cPJD_ERR_ECCENTRICITY_IS_ONE      = -6;
  cPJD_ERR_UNKNOWN_UNIT_ID          = -7;
  cPJD_ERR_INVALID_BOOLEAN_PARAM    = -8;
  cPJD_ERR_UNKNOWN_ELLP_PARAM       = -9;
  cPJD_ERR_REV_FLATTENING_IS_ZERO   = -10;
  cPJD_ERR_REF_RAD_LARGER_THAN_90   = -11;
  cPJD_ERR_ES_LESS_THAN_ZERO        = -12;
  cPJD_ERR_MAJOR_AXIS_NOT_GIVEN     = -13;
  cPJD_ERR_LAT_OR_LON_EXCEED_LIMIT  = -14;
  cPJD_ERR_INVALID_X_OR_Y           = -15;
  cPJD_ERR_WRONG_FORMAT_DMS_VALUE   = -16;
  cPJD_ERR_NON_CONV_INV_MERI_DIST   = -17;
  cPJD_ERR_NON_CON_INV_PHI2         = -18;
  cPJD_ERR_ACOS_ASIN_ARG_TOO_LARGE  = -19;
  cPJD_ERR_TOLERANCE_CONDITION      = -20;
  cPJD_ERR_CONIC_LAT_EQUAL          = -21;
  cPJD_ERR_LAT_LARGER_THAN_90       = -22;
  cPJD_ERR_LAT1_IS_ZERO             = -23;
  cPJD_ERR_LAT_TS_LARGER_THAN_90    = -24;
  cPJD_ERR_CONTROL_POINT_NO_DIST    = -25;
  cPJD_ERR_NO_ROTATION_PROJ         = -26;
  cPJD_ERR_W_OR_M_ZERO_OR_LESS      = -27;
  cPJD_ERR_LSAT_NOT_IN_RANGE        = -28;
  cPJD_ERR_PATH_NOT_IN_RANGE        = -29;
  cPJD_ERR_H_LESS_THAN_ZERO         = -30;
  cPJD_ERR_K_LESS_THAN_ZERO         = -31;
  cPJD_ERR_LAT_1_OR_2_ZERO_OR_90    = -32;
  cPJD_ERR_LAT_0_OR_ALPHA_EQ_90     = -33;
  cPJD_ERR_ELLIPSOID_USE_REQUIRED   = -34;
  cPJD_ERR_INVALID_UTM_ZONE         = -35;
  cPJD_ERR_TCHEBY_VAL_OUT_OF_RANGE  = -36;
  cPJD_ERR_FAILED_TO_FIND_PROJ      = -37;
  cPJD_ERR_FAILED_TO_LOAD_GRID      = -38;
  cPJD_ERR_INVALID_M_OR_N           = -39;
  cPJD_ERR_N_OUT_OF_RANGE           = -40;
  cPJD_ERR_LAT_1_2_UNSPECIFIED      = -41;
  cPJD_ERR_ABS_LAT1_EQ_ABS_LAT2     = -42;
  cPJD_ERR_LAT_0_HALF_PI_FROM_MEAN  = -43;
  cPJD_ERR_UNPARSEABLE_CS_DEF       = -44;
  cPJD_ERR_GEOCENTRIC               = -45;
  cPJD_ERR_UNKNOWN_PRIME_MERIDIAN   = -46;
  cPJD_ERR_AXIS                     = -47;
  cPJD_ERR_GRID_AREA                = -48;
  cPJD_ERR_INVALID_SWEEP_AXIS       = -49;
  cPJD_ERR_MALFORMED_PIPELINE       = -50;
  cPJD_ERR_UNIT_FACTOR_LESS_THAN_0  = -51;
  cPJD_ERR_INVALID_SCALE            = -52;
  cPJD_ERR_NON_CONVERGENT           = -53;
  cPJD_ERR_MISSING_ARGS             = -54;
  cPJD_ERR_LAT_0_IS_ZERO            = -55;
  cPJD_ERR_ELLIPSOIDAL_UNSUPPORTED  = -56;
  cPJD_ERR_TOO_MANY_INITS           = -57;
  cPJD_ERR_INVALID_ARG              = -58;
  cPJD_ERR_INCONSISTENT_UNIT        = -59;

  cPJ_ERR_MIN = cPJ_ERR_NO_ERROR;
  cPJ_ERR_MAX = -cPJD_ERR_INCONSISTENT_UNIT;


  cPJD_ERR_DESC: array[cPJ_ERR_MIN..cPJ_ERR_MAX -1] of string = (
{$ifndef __proj_ru_explain}
    'no arguments in initialization list',
    'no options found in init file',
    'no colon in init= string',
    'projection not named',
    'unknown projection id',
    'effective eccentricity = 1.',
    'unknown unit conversion id',
    'invalid boolean param argument',
    'unknown elliptical parameter name',
    'reciprocal flattening (1/f) = 0',
    '|radius reference latitude| > 90',
    'squared eccentricity < 0',
    'major axis or radius = 0 or not given',
    'latitude or longitude exceeded limits',
    'invalid x or y',
    'improperly formed DMS value',
    'non-convergent inverse meridional distance',
    'non-convergent inverse phi2',
    'acos/asin: |arg| >1.+1e-14',
    'tolerance condition error',
    'conic lat_1 = -lat_2',
    'lat_1 >= 90',
    'lat_1 = 0',
    'lat_ts >= 90',
    'no distance between control points',
    'projection not selected to be rotated',
    'W <= 0 or M <= 0',
    'lsat not in 1-5 range',
    'path not in range',
    'h <= 0',
    'k <= 0',
    'lat_0 = 0 or 90 or alpha = 90',
    'lat_1=lat_2 or lat_1=0 or lat_2=90',
    'elliptical usage required',
    'invalid UTM zone number',
    'artument out of range for Chebyshev polynomials evaluation',
    'failed to find projection to be rotated',
    'failed to load datum shift file',
    'both n & m must be spec''d and > 0',
    'n <= 0, n > 1 or not specified',
    'lat_1 or lat_2 not specified',
    '|lat_1| == |lat_2|',
    'lat_0 is pi/2 from mean latitude',
    'unparseable coordinate system definition',
    'geocentric transformation missing z or ellps',
    'unknown prime meridian conversion id',
    'illegal axis orientation combination',
    'point not within available datum shift grids',
    'invalid sweep axis, choose x or y',
    'malformed pipeline',
    'unit conversion factor must be > 0',
    'invalid scale',
    'non-convergent computation',
    'missing required arguments',
    'lat_0 = 0',
    'ellipsoidal usage unsupported',
    'only one +init allowed for non-pipeline operations',
    'argument not numerical or out of range',
    'inconsistent unit type between input and output'
{$else}
    'нет аргументов в списке инициализации',
    'в файле init не найдено никаких опций',
    'отсутствие двоеточия в строке init=',
    'в списке аргументов не указана проекция',
    'неизвестный идентификатор проекции',
    'фактический эксцентриситет = 1.0',
    'неизвестных идентификатор перевода единиц измерения',
    'недопустимый аргумент параметра boolean',
    'неизвестное имя эллиптического параметра',
    'обратное сжатие (1/f) = 0',
    'значение широты > 90',
    'квадрат эксцентриситета < 0',
    'большая полуось или радиус равны 0 или не указаны',
    'широта или долгота превышают установленные пределы',
    'недопустимые x или y',
    'неправильно сформированное значение DMS',
    'ошибка расчета сходимости меридианов',
    'ошибка расчета сходимости параллелей',
    'acos/asin: |arg| >1.+1e-14',
    'ошибка условия допуска',
    'конусность lat_1 = -lat_2',
    'lat_1 >= 90',
    'lat_1 = 0',
    'lat_ts >= 90',
    'отсутствие расстояния между контрольными точками',
    'не выбрана проекция для поворота',
    'W <= 0 или M <= 0',
    'lsat не в диапазоне 1-5',
    'путь не в диапазоне',
    'h <= 0',
    'k / k_0 <= 0',
    'lat_0 = 0 или 90 или alpha = 90',
    'lat_1=lat_2 или lat_1=0 или lat_2=90',
    'требуется использование эллиптического тренажера',
    'неверный номер зоны UTM. Значение Должно быть в диапазоне 1..60',
    'аргумент за пределами допустимого диапазона для вычислений полиномов Чебышева',
    'не удалось найти проекцию для поворота',
    'не удалось загрузить файл сдвига точек отсчета',
    'оба n и m должны быть специфицированы и > 0',
    'n <= 0, n > 1 или не указано',
    'lat_1 или lat_2 не указаны',
    '|lat_1| == |lat_2|',
    'lat_0 - это пи/2 от средней латы',
    'определение непарной системы координат',
    'геоцентрическое преобразование, в котором отсутствует z или эллипс',
    'неизвестный идентификатор нулевого меридиана',
    'ошибочная комбинация ориентации осей',
    'точка, не входит в доступные сетки сдвига',
    'недействительная координатная ось, выберите x или y',
    'неправильно заданный  pipeline',
    'коэффициент пересчета единиц измерения должен быть > 0',
    'недействительная шкала',
    'неконвергентное вычисление',
    'отсутствие необходимых аргументов',
    'lat_0 = 0',
    'использование в форме эллипсоида не поддерживается',
    'для операций не связанных с pipeline разрешен для только один аргумент +init',
    'аргумент не является числовым или находится вне диапазона',
    'несоответствие единиц измерения между входными и выходными данными'
{$endif}
  );

type

  EProj4Error = class(Exception)
    pjErrorCode: Integer;
  end;

  // from proj.h
  TPJParamRec = record
    Name: string;
    DisplayName: string;
    Desctiption: string;
    Value: string;
    Group: Integer;
  end;

  TPJxyzt_t = record
    x,y,z,t: Double;
  end;

  TPJuvwt_t = record
    u,v,w,t: Double;
  end;

  TPJlpzt_t = record
    lam,phi,z,t: Double;
  end;
  /// Rotations: omega, phi, kappa
  TPJopk_t = record
    o,p,k: Double;
  end;

  /// East, North, Up
  TPJenu_t = record
    e,n,u: Double;
  end;
  /// Geodesic length, fwd azi, rev azi
  TPJgeod_t = record
    s,a1,a2: Double;
  end;

  TPJuv_t = record
    u,v: Double;
  end;

  TPJxy_t = record
    x,y: Double;
  end;

  TPJlp_t = record
    lam,phi: Double
  end;

  TPJxyz_t = record
    x,y,z: Double
  end;

  TPJuvw_t = record
    u,v,w: Double;
  end;

  TPJlpz_t = record
    lam,phi,z: Double;
  end;

  TPJcoord_t = record
  case Integer of
    0: (values: array [0..4] of Double);
    1: (xyzt: TPJxyzt_t);
    2: (uvwt: TPJuvwt_t);
    3: (lpzt: TPJlpzt_t);
    4: (geod: TPJgeod_t);
    5: (opk: TPJopk_t);
    6: (enu: TPJenu_t);
    7: (xyz: TPJxyz_t);
    8: (uvw: TPJuvw_t);
    9: (lpz: TPJlpz_t);
   10: (xy: TPJxy_t);
   11: (uv: TPJuv_t);
   12: (lp: TPJlp_t);
  end;

  PPJInfo = ^TPJInfo;
  TPJInfo = record
    iMajor: Integer;
    iMinor: Integer;
    iPatch: Integer;
    zRelease: MarshaledAString;
    zVersion: MarshaledAString;
    zSearchPath: MarshaledAString;
    zPaths: PMarshaledAString;
    iPathCount: NativeUint;
  end;

  PPJProjInfo = ^TPJProjInfo;
  TPJProjInfo = record
    zId: MarshaledAString;
    zDescription: MarshaledAString;
    zDefinition: MarshaledAString;
    bHasInverse: LongBool;
    dAccuracy: Double;
  end;

  PPJInitInfo = ^TPJInitInfo;
  TPJInitInfo = record
    Name: array [0 .. 32] of AnsiChar;
    FileName: array [0 .. 260] of AnsiChar;
    Version: array [0 .. 32] of AnsiChar;
    Origin: array [0 .. 32] of AnsiChar;
    LastUpdate: array [0 .. 16] of AnsiChar;
  end;

  TPJLogger_t = procedure(ctx: Pointer; level: Integer; zText: MarshaledAString); cdecl;
  TPJdestructor_t = function(pPJ: Pointer; par1: Integer): Pointer cdecl;
  /// A function taking a PJ_COORD and a pointer-to-PJ as args, applying the PJ to the PJ_COORD, and returning the resulting PJ_COORD.
  TPJOperator_t = function(coord: TPJcoord_t; pPJ: Pointer): TPJcoord_t; cdecl;

  PPJCtx_t = ^TPJCtx_t;
  TPJCtx_t = record
    LastErrno: Integer;
    DebugLevel: Integer;
    Logger: TPJLogger_t;
    AppData: Pointer;
    Fileapi: Pointer;
  end;
  TPJCtx = PPJCtx_t;

  TP5factors_t = record
    ///meridional_scale
    h: Double;
    /// parallel_scale
    k: Double;
    /// areal_scale
    s: Double;
    /// angular_distortion
    omega: Double;
    /// meridian_parallel_angle
    thetaPrime: Double;
    /// meridian_convergence
    alpha: Double;
    /// tissot_semimajor
    a: Double;
    /// tissot_semiminor
    b: Double;
    dx_dlam, dx_dphi: Double;
    dy_dlam, dy_dphi: Double
  end;

  TPJregion_t = record
    /// lower left corner coordinates (radians)
    ll_long,ll_lat: Double;
    /// upper right corner coordinates (radians)
    ur_long,ur_lat: Double;
  end;

  PPJconsts_t = ^TPJconsts_t;
  TPJconsts_t = record
    {$region 'general parameter struct'}
    pprojCtx : TPJCtx; // *ctx;
    /// From pj_list.h or individual PJ_*.c file */
    descr: MarshaledAString;
    /// Parameter list
    params: Pointer;
    /// Full textual definition (usually 0 - set by proj_pj_info)
    def_full: MarshaledAString;
    /// Shape and size parameters extracted from params
    def_size: MarshaledAString;
    def_shape: MarshaledAString;
    def_spherification: MarshaledAString;
    def_ellps: MarshaledAString;
    /// For geodesic computations
    geod: Pointer; // struct geod_geodesic *
    /// Projection specific parameters, Defined in PJ_*.c
    opaque: Pointer; // struct pj_opaque *
    inverted: Integer;
    /// Tell high level API functions to swap inv/fwd
    {$endregion}
    {$region 'function pointers'}
      /// For projection xxx, these are pointers to functions in the corresponding PJ_xxx.c file.
      /// pj_init() delegates the setup of these to pj_projection_specific_setup_xxx(), a name which is currently hidden behind the magic curtain of the PROJECTION macro.
     _fwd: function(lp: TPJlp_t; pPJ: Pointer): TPJxy_t; cdecl;
     _inv: function(xy: TPJxy_t; pPJ: Pointer): TPJlp_t; cdecl;
     _fwd3d: function(lpz: TPJlpz_t; pPJ: Pointer): TPJxyz_t; cdecl;
     _inv3d: function(xyz: TPJxy_t; pPJ: Pointer): TPJlpz_t; cdecl;
     _fwd4d: function(coord: TPJcoord_t; pPJ: Pointer): TPJcoord_t; cdecl; // TPJOperator_t
     _inv4d:  function(coord: TPJcoord_t; pPJ: Pointer): TPJcoord_t; cdecl; // TPJOperator_t
     _destructor: function(pPJ: Pointer {var pj: TPJconsts_t}; v2: Integer): Pointer cdecl; // TPJdestructor_t;
    {$endregion}
    {$region 'linear parameters'}
    /// semimajor axis (radius if eccentricity==0)
    a: Double;
    /// semiminor axis
    b: Double;
    /// 1/a
    ra: Double;
    /// 1/b
    rb: Double;
    {$endregion}

    {$region 'eccentricities'}
    /// angular eccentricity
    alpha: Double;
    /// first eccentricity
    e: Double;
    /// first eccentricity squared
    es: Double;
    /// second eccentricity
    e2: Double;
    /// second eccentricity squared
    e2s: Double;
    /// third  eccentricity
    e3: Double;
    /// third eccentricity squared
    e3s: Double;
    /// 1 - e^2
    one_es: Double;
    /// 1/one_es
    rone_es: Double;
    {$endregion}

    {$region 'flattenings'}
    /// first flattening */
    f: Double;
    /// second flattening */
    f2: Double;
    /// third flattening */
    n: Double;
    /// 1/f
    rf: Double;
    /// 1/f2
    rf2: Double;
    /// 1/n
    rn: Double;
    /// Dynamic form factor (this one's for GRS80)
    J: Double;
    /// es and a before any +proj related adjustment */
    es_orig, a_orig: Double;
    {$endregion}

    {$region 'coordinate handling'}
       /// Over-range flag
      over: Integer;
       /// Geocentric latitude flag
      geoc: Integer;
      /// proj=latlong ... not really a projection at all
      is_latlong: Integer;
      /// proj=geocent ... not really a projection at all
      is_geocent: Integer;
        /// 1 if PJ represents a pipeline
      is_pipeline: Integer;
      /// 0 for operations that are purely cartesian
      need_ellps: Integer;
      skip_fwd_prepare: Integer;
      skip_fwd_finalize: Integer;
      skip_inv_prepare: Integer;
      skip_inv_finalize: Integer;
      /// Flags for input/output coordinate types
      left: Integer; // enum pj_io_units ;
      right: Integer;// enum pj_io_units;

      // These PJs are used for implementing cs2cs style coordinate handling in the 4D API
      axisswap: Pointer; // pPJ_t;
      cart: Pointer; // pPJ_t;
      cart_wgs84: Pointer; // pPJ_t;
      helmert: Pointer; // pPJ_t;
      hgridshift: Pointer; // pPJ_t;
      vgridshift: Pointer; // pPJ_t;
    {$endregion}
    {$region 'cartographic offsets'}
      lam0, phi0: Double;
      x0, y0, z0, t0: Double;
    {$endregion}
    {$region 'scaling'}
      /// General scaling factor - e.g. the 0.9996 of UTM
      k0: Double;
      /// Plane coordinate scaling. Internal unit [m]
      to_meter,fr_meter: Double;
      /// Vertical scaling. Internal unit [m]
      vto_meter, vfr_meter: Double;
    {$endregion}

    {$region 'datums and height systems'}
      /// PJD_UNKNOWN/3PARAM/7PARAM/GRIDSHIFT/WGS84
      datum_type: Integer;
      /// Parameters for 3PARAM and 7PARAM
      datum_params: array [0..7] of Double;
      ppgridlist: PPointer;
      gridlist_count: Integer;
      has_geoid_vgrids: Integer;
      vgridlist_geoid: PPointer;
      vgridlist_geoid_count: Integer;
      /// prime meridian offset (in radians)
      from_greenwich: Double;
      /// 0.0 for -180 to 180, actually in radians
      long_wrap_center: Double;
      axis: array[0..4] of MarshaledAString;
      catalog_name: MarshaledAString;
      pCatalog: Pointer;
      datum_date: Double;
      last_before_grid: Pointer;
      last_before_region: TPJregion_t;
      last_before_date: Double;
      last_after_grid: Pointer;
      last_after_region: TPJregion_t;
      last_after_date: double;
    {$endregion}
  end;

  TPJ_t = {$ifndef __proj_api_deepdive}record end{$else}TPJconsts_t{$endif};
  PPJ_t = {$ifndef __proj_api_deepdive}^TPJ_t{$else}PPJconsts_t{$endif};
  TPJ = PPJ_t;

  PProjFileAPI = ^TProjFileAPI;
  /// file reading api, like stdio
  TProjFileAPI = record
    FOpen: function(ctx: TPJCtx; const zFileName, zAccess: MarshaledAString): Pointer { pfile_t }; cdecl;
    FRead: function(buffer: Pointer; size, nmemb: NativeUint; pFile: Pointer { pfile_t } ): NativeUint; cdecl;
    FSeek: function(pFile: Pointer { pfile_t }; offset: LongInt; whence: Integer): Integer; cdecl;
    FTell: function(pFile: Pointer { pfile_t } ): Longint; cdecl;
    FClose: procedure(pFile: Pointer); cdecl;
  end;

  PGeodesic = ^TGeodesic;
  /// The struct containing information about the ellipsoid.
  /// This must be initialized by geod_init() before use.
  TGeodesic = record
    a, f: Double;
    f1, e2, ep2, n, b, c2, etol2: Double;
    A3x: array [0 .. 6] of Double;
    C3x: array [0 .. 15] of Double;
    C4x: array [0 .. 21] of Double;
  end;

  /// The struct containing information about a single geodesic.
  /// This must be initialized by geod_lineinit(), geod_directline(), geod_gendirectline(), or geod_inverseline() before use.
  PGeodesicLine = ^TGeodesicLine;

  TGeodesicLine = record
    lat1, lon1, azi1, a, f, salp1, calp1, a13, s13: Double;
    b, c2, f1, salp0, calp0, k2, ssig1, csig1, dn1, stau1, ctau1, somg1, comg1, A1m1, A2m1, A3c, B11, B21, B31, A4, B41: Double;
    C1a: array [0 .. 7] of Double;
    C1pa: array [0 .. 7] of Double;
    C2a: array [0 .. 7] of Double;
    C3a: array [0 .. 6] of Double;
    C4a: array [0 .. 6] of Double;
    caps: Cardinal;
  end;

  /// The struct for accumulating information about a geodesic polygon.
  /// This is used for computing the perimeter and area of a polygon.
  /// This must be initialized by geod_polygon_init() before use.
  PGeodesicPolygon = ^TGeodesicPolygon;

  TGeodesicPolygon = record
    lat, lon: Double;
    lat0, lon0: Double;
    a: array [0 .. 2] of Double;
    P: array [0 .. 2] of Double;
    polyline, crossings: Integer;
    num: Cardinal;
  end;
{$ENDREGION}

implementation



initialization

finalization
end.
