/// <summary>
///   plain api of libProj c library
/// </summary>
/// <remarks>
///   currently using Proj4 api functions
/// </remarks>
unit LibPROJApi;


interface

uses Types, SysUtils;

type
  TPJ_LOG_FUNCTION =
	procedure (app_data: pointer; level: integer; msg: Pointer) of object; cdecl;

  TProjPJ = type Pointer;
  TProjCtx = type Pointer;


/// <summary>
///   This function converts a string representation of a coordinate system
///   definition into a projPJ object suitable for use with other API
///   functions. On failure the function will return NULL and set <see cref="LibPROJApi|PJ_strerrno(Integer)">
///   pj_errno</see>.
/// </summary>
/// <param name="def">
///   The definition in the general form. <see href="https://github.com/OSGeo/proj.4/wiki/GenParms/">
///   See PROJ.4 documentation</see>
/// </param>
/// <returns>
///   <b>nil</b> if error (u must see last error code) and else pointer to
///   Coordinate system object
/// </returns>
/// <remarks>
///   Coordinate system objects allocated with pj_init_plus() should be
///   deallocated with <see cref="LibPROJApi|PJ_free(Pointer,Boolean)">
///   pj_free()</see>.
/// </remarks>
function PJ_init_plus(const def: string): TProjPJ; cdecl;
/// <summary>
///   Returns TRUE if input coordinate system object is geographic.
/// </summary>
/// <param name="p">
///   input coordinate system handle.
/// </param>
function PJ_is_geographic(p: TProjPJ): Boolean;
/// <summary>
///   Returns TRUE if input coordinate system object is geocentric.
/// </summary>
/// <param name="p">
///   input coordinate system handle. <br />
/// </param>
function PJ_is_geocentric(p: TProjPJ): Boolean;
/// <summary>
///   Return a projection handle defining the lat/long coordinate system on
///   which a projection is based. If the coordinate system passed in is
///   latlong, a clone of the same will be returned.
/// </summary>
/// <param name="p">
///   input projection handle.
/// </param>
function PJ_latlong_from_proj(p: TProjPJ): TProjPJ;
/// <param name="src">
///   source (input) coordinate system handle.
/// </param>
/// <param name="dst">
///   destination (output) coordinate system handle.
/// </param>
/// <param name="x">
///   x coordinate value.
/// </param>
/// <param name="y">
///   y coordinate value. <br />
/// </param>
/// <param name="angular">
///   false if x/y values in degrees and source or dest coordinate system is geographic.
/// </param>
/// <returns>
///   The return is zero on success, or a PROJ.4 error code.
/// </returns>
/// <remarks>
///   If there is an overall failure, an error code will be returned from the
///   function. Input value that are HUGE_VAL will not be transformed.
/// </remarks>
function PJ_transform_point2D(src, dst: TProjPJ; var x, y: Double; angular:
    Boolean): Integer; overload;
/// <param name="src">
///   source (input) coordinate system handle.
/// </param>
/// <param name="dst">
///   destination (output) coordinate system handle.
/// </param>
/// <param name="x">
///   x coordinate value.
/// </param>
/// <param name="y">
///   y coordinate value. <br />
/// </param>
/// <param name="angular">
///   false if x/y values in degrees.
/// </param>
/// <returns>
///   The return is zero on success, or a PROJ.4 error code.
/// </returns>
/// <remarks>
///   If there is an overall failure, an error code will be returned from the
///   function. Input value that are HUGE_VAL will not be transformed.
/// </remarks>
function PJ_transform_point2D(src, dst: TProjPJ; x, y: PDouble; angular:
    Boolean): Integer; overload;
/// <param name="src">
///   source (input) coordinate system handle.
/// </param>
/// <param name="dst">
///   destination (output) coordinate system handle.
/// </param>
/// <param name="x">
///   pointer to first x coordinate value.
/// </param>
/// <param name="y">
///   pointer to first y coordinate value. <br />
/// </param>
/// <param name="count">
///   point counts.
/// </param>
/// <param name="angular">
///   false if x/y values in degrees.
/// </param>
/// <returns>
///   The return is zero on success, or a PROJ.4 error code.
/// </returns>
/// <remarks>
///   If there is an overall failure, an error code will be returned from the
///   function. Input value that are HUGE_VAL will not be transformed.
/// </remarks>
function PJ_transform_points2D(src, dst: TProjPJ; x, y: PDouble; count:
    Integer; angular: Boolean): Integer;
/// <summary>
///   Returns the PROJ.4 command string that would produce this definition
///   expanded as much as possible. For instance +init= calls and +datum=
///   definitions would be expanded.
/// </summary>
/// <param name="p">
///   coordinate system handle.
/// </param>
function PJ_get_definition(p: TProjPJ): string;
/// <summary>
///   returns 0 if full definitions presented coordinate system handles is
///   same. <br />
/// </summary>
/// <param name="p1">
///   first coordinate system handle.
/// </param>
/// <param name="p2">
///   second coordinate system handle. <br />
/// </param>
/// <returns>
///   <para>
///     -1 if error
///   </para>
///   <para>
///     0 if definitions same
///   </para>
///   <para>
///     1 if definitions not same <br />
///   </para>
/// </returns>
function PJ_is_same_definition(p1, p2: TProjPJ): Integer;
/// <summary>
///   cleanup memory associated with libproj coordinate system object. <br />
/// </summary>
/// <param name="p">
///   coordinate system handle.
/// </param>
procedure PJ_free(var p: TProjPJ);
/// <summary>
///   get libproj version string
/// </summary>
function PJ_get_version_string(): string;
/// <summary>
///   validates ProjPJ handle
/// </summary>
function PJ_is_valid(p: TProjPJ): Boolean;
/// <summary>
///   get string description of libproj error code
/// </summary>
/// <param name="errno">
///   libproj error code
/// </param>
function PJ_strerrno(errno: integer): string;
/// <summary>
///   Fetch the internal definition of the spheroid.
/// </summary>
/// <param name="p">
///   coordinate system handle.
/// </param>
/// <param name="major_axis">
///   value of major axis in meters
/// </param>
/// <param name="eccentricity_squared">
///   square of eccentricity
/// </param>
/// <remarks>
///   <para>
///     You can compute "minor axis" from eccentricity_squared as:
///   </para>
///   <para>
///     <i>b = a * sqrt(1 - e2)</i>
///   </para>
/// </remarks>
function PJ_get_spheroid_defn(p: TProjPJ; out major_axis, eccentricity_squared: Double): Boolean;
function PJ_get_errno(): Integer;

implementation

uses
	Math,{$IFDEF MSWINDOWS}PROJ.Crtl, Windows{$ENDIF};

var
  FDefaultContext: TProjCtx = nil;

const
	PJ_RAD_TO_DEG = 57.295779513082320876798154814105000; //(1*180 / PI)
	PJ_DEG_TO_RAD = 0.017453292519943295769236907684886;

// obj\$(Platform)\$(Config)
NAME_PREFIX = {$IFDEF WIN32}'_'{$ELSE}''{$ENDIF};

	{$REGION 'external declarations to avoid linker errors'}
{$IFNDEF WIN32}
  procedure nad_cvt(); external;
  function pj_sinu(p1: Pointer): Pointer; external;
  function bchgen(): integer; external;
  function bch2bps: integer; external;
  function pj_deriv(): Integer; external;
  function dmstor_ctx: Double; external;
  function pj_gc_readcatalog(): Pointer; external;
  procedure nad_free(var ct: Pointer); external;
  procedure nad_ctable_load(); external;
  procedure nad_ctable2_load(); external;
  procedure nad_ctable2_init(); external;
  procedure nad_ctable_init(); external;
  function pj_ctx_ftell(): Cardinal; external;
  procedure pj_gridinfo_free( ct: Pointer; var Pointer); external;
  procedure pj_gridinfo_init; external;
  procedure pj_datum_set; external;
  procedure pj_prime_meridians; external;
  procedure pj_ctx_get_errno; external;
  procedure pj_aea; external;
  procedure pj_s_aea; external;
  procedure pj_aeqd; external;
  procedure pj_s_aeqd; external;
  procedure pj_airy; external;
  procedure pj_s_airy; external;
  procedure pj_aitoff; external;
  procedure pj_s_aitoff; external;
  procedure pj_alsk; external;
  procedure pj_s_alsk; external;
  procedure pj_apian; external;
  procedure pj_s_apian; external;
  procedure pj_august; external;
  procedure pj_s_august; external;
  procedure pj_axisswap; external;
  procedure pj_s_axisswap; external;
  procedure pj_bacon; external;
  procedure pj_s_bacon; external;
  procedure pj_bipc; external;
  procedure pj_s_bipc; external;
  procedure pj_boggs; external;
  procedure pj_s_boggs; external;
  procedure pj_bonne; external;
  procedure pj_s_bonne; external;
  procedure pj_calcofi; external;
  procedure pj_s_calcofi; external;
  procedure pj_cart; external;
  procedure pj_s_cart; external;
  procedure pj_cass; external;
  procedure pj_s_cass; external;
  procedure pj_cc; external;
  procedure pj_s_cc; external;
  procedure pj_ccon; external;
  procedure pj_s_ccon; external;
  procedure pj_cea; external;
  procedure pj_s_cea; external;
  procedure pj_chamb; external;
  procedure pj_s_chamb; external;
  procedure pj_collg; external;
  procedure pj_s_collg; external;
  procedure pj_comill; external;
  procedure pj_s_comill; external;
  procedure pj_crast; external;
  procedure pj_s_crast; external;
  procedure pj_deformation; external;
  procedure pj_s_deformation; external;
  procedure pj_denoy; external;
  procedure pj_s_denoy; external;
  procedure pj_eck1; external;
  procedure pj_s_eck1; external;
  procedure pj_eck2; external;
  procedure pj_s_eck2; external;
  procedure pj_eck3; external;
  procedure pj_s_eck3; external;
  procedure pj_eck4; external;
  procedure pj_s_eck4; external;
  procedure pj_eck5; external;
  procedure pj_s_eck5; external;
  procedure pj_eck6; external;
  procedure pj_s_eck6; external;
  procedure pj_eqc; external;
  procedure pj_s_eqc; external;
  procedure pj_eqdc; external;
  procedure pj_s_eqdc; external;
  procedure pj_euler; external;
  procedure pj_s_euler; external;
  procedure pj_etmerc; external;
  procedure pj_s_etmerc; external;
  procedure pj_fahey; external;
  procedure pj_s_fahey; external;
  procedure pj_fouc; external;
  procedure pj_s_fouc; external;
  procedure pj_fouc_s; external;
  procedure pj_s_fouc_s; external;
  procedure pj_gall; external;
  procedure pj_s_gall; external;
  procedure pj_geoc; external;
  procedure pj_s_geoc; external;
  procedure pj_geocent; external;
  procedure pj_s_geocent; external;
  procedure pj_geos; external;
  procedure pj_s_geos; external;
  procedure pj_gins8; external;
  procedure pj_s_gins8; external;
  procedure pj_gn_sinu; external;
  procedure pj_s_gn_sinu; external;
  procedure pj_gnom; external;
  procedure pj_s_gnom; external;
  procedure pj_goode; external;
  procedure pj_s_goode; external;
  procedure pj_gs48; external;
  procedure pj_s_gs48; external;
  procedure pj_gs50; external;
  procedure pj_s_gs50; external;
  procedure pj_hammer; external;
  procedure pj_s_hammer; external;
  procedure pj_hatano; external;
  procedure pj_s_hatano; external;
  procedure pj_healpix; external;
  procedure pj_s_healpix; external;
  procedure pj_rhealpix; external;
  procedure pj_s_rhealpix; external;
  procedure pj_helmert; external;
  procedure pj_s_helmert; external;
  procedure pj_hgridshift; external;
  procedure pj_s_hgridshift; external;
  procedure pj_horner; external;
  procedure pj_s_horner; external;
  procedure pj_igh; external;
  procedure pj_s_igh; external;
  procedure pj_imw_p; external;
  procedure pj_s_imw_p; external;
  procedure pj_isea; external;
  procedure pj_s_isea; external;
  procedure pj_kav5; external;
  procedure pj_s_kav5; external;
  procedure pj_kav7; external;
  procedure pj_s_kav7; external;
  procedure pj_krovak; external;
  procedure pj_s_krovak; external;
  procedure pj_labrd; external;
  procedure pj_s_labrd; external;
  procedure pj_laea; external;
  procedure pj_s_laea; external;
  procedure pj_lagrng; external;
  procedure pj_s_lagrng; external;
  procedure pj_larr; external;
  procedure pj_s_larr; external;
  procedure pj_lask; external;
  procedure pj_s_lask; external;
  procedure pj_lonlat; external;
  procedure pj_s_lonlat; external;
  procedure pj_latlon; external;
  procedure pj_s_latlon; external;
  procedure pj_latlong; external;
  procedure pj_s_latlong; external;
  procedure pj_longlat; external;
  procedure pj_s_longlat; external;
  procedure pj_lcc; external;
  procedure pj_s_lcc; external;
  procedure pj_lcca; external;
  procedure pj_s_lcca; external;
  procedure pj_leac; external;
  procedure pj_s_leac; external;
  procedure pj_lee_os; external;
  procedure pj_s_lee_os; external;
  procedure pj_loxim; external;
  procedure pj_s_loxim; external;
  procedure pj_lsat; external;
  procedure pj_s_lsat; external;
  procedure pj_mbt_s; external;
  procedure pj_s_mbt_s; external;
  procedure pj_mbt_fps; external;
  procedure pj_s_mbt_fps; external;
  procedure pj_mbtfpp; external;
  procedure pj_s_mbtfpp; external;
  procedure pj_mbtfpq; external;
  procedure pj_s_mbtfpq; external;
  procedure pj_mbtfps; external;
  procedure pj_s_mbtfps; external;
  procedure pj_merc; external;
  procedure pj_s_merc; external;
  procedure pj_mil_os; external;
  procedure pj_s_mil_os; external;
  procedure pj_mill; external;
  procedure pj_s_mill; external;
  procedure pj_misrsom; external;
  procedure pj_s_misrsom; external;
  procedure pj_s_moll; external;
  procedure pj_molodensky; external;
  procedure pj_s_molodensky; external;
  procedure pj_murd1; external;
  procedure pj_s_murd1; external;
  procedure pj_murd2; external;
  procedure pj_s_murd2; external;
  procedure pj_murd3; external;
  procedure pj_s_murd3; external;
  procedure pj_natearth; external;
  procedure pj_s_natearth; external;
  procedure pj_natearth2; external;
  procedure pj_s_natearth2; external;
  procedure pj_nell; external;
  procedure pj_s_nell; external;
  procedure pj_nell_h; external;
  procedure pj_s_nell_h; external;
  procedure pj_nicol; external;
  procedure pj_s_nicol; external;
  procedure pj_nsper; external;
  procedure pj_s_nsper; external;
  procedure pj_nzmg; external;
  procedure pj_s_nzmg; external;
  procedure pj_ob_tran; external;
  procedure pj_s_ob_tran; external;
  procedure pj_ocea; external;
  procedure pj_s_ocea; external;
  procedure pj_oea; external;
  procedure pj_s_oea; external;
  procedure pj_omerc; external;
  procedure pj_s_omerc; external;
  procedure pj_ortel; external;
  procedure pj_s_ortel; external;
  procedure pj_ortho; external;
  procedure pj_s_ortho; external;
  procedure pj_pconic; external;
  procedure pj_s_pconic; external;
  procedure pj_patterson; external;
  procedure pj_s_patterson; external;
  procedure pj_pipeline; external;
  procedure pj_s_pipeline; external;
  procedure pj_poly; external;
  procedure pj_s_poly; external;
  procedure pj_putp1; external;
  procedure pj_s_putp1; external;
  procedure pj_putp2; external;
  procedure pj_s_putp2; external;
  procedure pj_putp3; external;
  procedure pj_s_putp3; external;
  procedure pj_putp3p; external;
  procedure pj_s_putp3p; external;
  procedure pj_putp4p; external;
  procedure pj_s_putp4p; external;
  procedure pj_putp5; external;
  procedure pj_s_putp5; external;
  procedure pj_putp5p; external;
  procedure pj_s_putp5p; external;
  procedure pj_putp6; external;
  procedure pj_s_putp6; external;
  procedure pj_putp6p; external;
  procedure pj_s_putp6p; external;
  procedure pj_qua_aut; external;
  procedure pj_s_qua_aut; external;
  procedure pj_qsc; external;
  procedure pj_s_qsc; external;
  procedure pj_robin; external;
  procedure pj_s_robin; external;
  procedure pj_rpoly; external;
  procedure pj_s_rpoly; external;
  procedure pj_sch; external;
  procedure pj_s_sch; external;
  procedure pj_s_sinu; external;
  procedure pj_somerc; external;
  procedure pj_s_somerc; external;
  procedure pj_stere; external;
  procedure pj_s_stere; external;
  procedure pj_sterea; external;
  procedure pj_s_sterea; external;
  procedure pj_gstmerc; external;
  procedure pj_s_gstmerc; external;
  procedure pj_tcc; external;
  procedure pj_s_tcc; external;
  procedure pj_tcea; external;
  procedure pj_s_tcea; external;
  procedure pj_times; external;
  procedure pj_s_times; external;
  procedure pj_tissot; external;
  procedure pj_s_tissot; external;
  procedure pj_tmerc; external;
  procedure pj_s_tmerc; external;
  procedure pj_tpeqd; external;
  procedure pj_s_tpeqd; external;
  procedure pj_tpers; external;
  procedure pj_s_tpers; external;
  procedure pj_unitconvert; external;
  procedure pj_s_unitconvert; external;
  procedure pj_ups; external;
  procedure pj_s_ups; external;
  procedure pj_urm5; external;
  procedure pj_s_urm5; external;
  procedure pj_urmfps; external;
  procedure pj_s_urmfps; external;
  procedure pj_utm; external;
  procedure pj_s_utm; external;
  procedure pj_vandg; external;
  procedure pj_s_vandg; external;
  procedure pj_vandg2; external;
  procedure pj_s_vandg2; external;
  procedure pj_vandg3; external;
  procedure pj_s_vandg3; external;
  procedure pj_vandg4; external;
  procedure pj_s_vandg4; external;
  procedure pj_vitk1; external;
  procedure pj_s_vitk1; external;
  procedure pj_vgridshift; external;
  procedure pj_s_vgridshift; external;
  procedure pj_wag1; external;
  procedure pj_s_wag1; external;
  procedure pj_wag2; external;
  procedure pj_s_wag2; external;
  procedure pj_wag3; external;
  procedure pj_s_wag3; external;
  procedure pj_wag4; external;
  procedure pj_s_wag4; external;
  procedure pj_wag5; external;
  procedure pj_s_wag5; external;
  procedure pj_wag6; external;
  procedure pj_s_wag6; external;
  procedure pj_wag7; external;
  procedure pj_s_wag7; external;
  procedure pj_webmerc; external;
  procedure pj_s_webmerc; external;
  procedure pj_weren; external;
  procedure pj_s_weren; external;
  procedure pj_wink1; external;
  procedure pj_s_wink1; external;
  procedure pj_wink2; external;
  procedure pj_s_wink2; external;
  procedure pj_wintri; external;
  procedure pj_s_wintri; external;
  procedure pj_fwd4d; external;
  procedure pj_inv4d; external;
  procedure dmstor; external;
  procedure pj_ellipsoid; external;
  procedure pj_rouss; external;
  procedure pj_s_rouss; external;
  procedure pj_inv3d; external;
  procedure pj_strdup; external;
  procedure pj_find_file; external;
  procedure pj_has_inverse; external;
  procedure pj_inherit_ellipsoid_def; external;
  procedure pj_left; external;
  procedure pj_make_args; external;
  procedure pj_right; external;
  procedure pj_trim_argc; external;
  procedure pj_trim_argv; external;
  procedure proj_get_path_count; external;
  procedure proj_get_searchpath; external;
  procedure rtodms; external;
  procedure pj_approx_2D_trans; external;
  procedure pj_approx_3D_trans; external;
  procedure proj_create_argv; external;
  procedure proj_destroy; external;
  procedure proj_log_error; external;
  procedure proj_vgrid_init; external;
  procedure proj_vgrid_value; external;
  procedure proj_hgrid_apply; external;
  procedure proj_hgrid_init; external;
  procedure proj_create; external;
  procedure proj_hgrid_value; external;
  procedure pj_expand_init; external;
  procedure pj_factors; external;
  procedure pj_fwd; external;
  procedure pj_inv; external;
  procedure pj_fwd3d; external;
  procedure pj_set_ctx; external;
  procedure pj_vlog; external;
  procedure pj_ctx_fopen; external;
  procedure pj_apply_vgridshift; external;
  procedure pj_apply_gridshift_2; external;
  procedure proj_mdist_ini; external;
  procedure proj_mdist; external;
  procedure proj_inv_mdist; external;
  procedure pj_eqearth; external;
  procedure pj_s_eqearth; external;
{$ELSE}
  procedure _nad_cvt(); external;
  function _pj_sinu(p1: Pointer): Pointer; external;
  function _bchgen(): integer; external;
  function _bch2bps: integer; external;
  function _pj_deriv(): Integer; external;
  function _dmstor_ctx: Double; external;
  function _pj_gc_readcatalog(): Pointer; external;
  procedure _nad_free(var ct: Pointer); external;
  procedure _nad_ctable_load(); external;
  procedure _nad_ctable2_load(); external;
  procedure _nad_ctable2_init(); external;
  procedure _nad_ctable_init(); external;
  function _pj_ctx_ftell(): Cardinal; external;
  procedure _pj_gridinfo_free( ct: Pointer; var Pointer); external;
  procedure _pj_gridinfo_init; external;
  procedure _pj_datum_set; external;
  procedure _pj_prime_meridians; external;
  procedure _pj_ctx_get_errno; external;
  procedure _pj_aea; external;
  procedure _pj_s_aea; external;
  procedure _pj_aeqd; external;
  procedure _pj_s_aeqd; external;
  procedure _pj_airy; external;
  procedure _pj_s_airy; external;
  procedure _pj_aitoff; external;
  procedure _pj_s_aitoff; external;
  procedure _pj_alsk; external;
  procedure _pj_s_alsk; external;
  procedure _pj_apian; external;
  procedure _pj_s_apian; external;
  procedure _pj_august; external;
  procedure _pj_s_august; external;
  procedure _pj_axisswap; external;
  procedure _pj_s_axisswap; external;
  procedure _pj_bacon; external;
  procedure _pj_s_bacon; external;
  procedure _pj_bipc; external;
  procedure _pj_s_bipc; external;
  procedure _pj_boggs; external;
  procedure _pj_s_boggs; external;
  procedure _pj_bonne; external;
  procedure _pj_s_bonne; external;
  procedure _pj_calcofi; external;
  procedure _pj_s_calcofi; external;
  procedure _pj_cart; external;
  procedure _pj_s_cart; external;
  procedure _pj_cass; external;
  procedure _pj_s_cass; external;
  procedure _pj_cc; external;
  procedure _pj_s_cc; external;
  procedure _pj_ccon; external;
  procedure _pj_s_ccon; external;
  procedure _pj_cea; external;
  procedure _pj_s_cea; external;
  procedure _pj_chamb; external;
  procedure _pj_s_chamb; external;
  procedure _pj_collg; external;
  procedure _pj_s_collg; external;
  procedure _pj_comill; external;
  procedure _pj_s_comill; external;
  procedure _pj_crast; external;
  procedure _pj_s_crast; external;
  procedure _pj_deformation; external;
  procedure _pj_s_deformation; external;
  procedure _pj_denoy; external;
  procedure _pj_s_denoy; external;
  procedure _pj_eck1; external;
  procedure _pj_s_eck1; external;
  procedure _pj_eck2; external;
  procedure _pj_s_eck2; external;
  procedure _pj_eck3; external;
  procedure _pj_s_eck3; external;
  procedure _pj_eck4; external;
  procedure _pj_s_eck4; external;
  procedure _pj_eck5; external;
  procedure _pj_s_eck5; external;
  procedure _pj_eck6; external;
  procedure _pj_s_eck6; external;
  procedure _pj_eqc; external;
  procedure _pj_s_eqc; external;
  procedure _pj_eqdc; external;
  procedure _pj_s_eqdc; external;
  procedure _pj_euler; external;
  procedure _pj_s_euler; external;
  procedure _pj_etmerc; external;
  procedure _pj_s_etmerc; external;
  procedure _pj_fahey; external;
  procedure _pj_s_fahey; external;
  procedure _pj_fouc; external;
  procedure _pj_s_fouc; external;
  procedure _pj_fouc_s; external;
  procedure _pj_s_fouc_s; external;
  procedure _pj_gall; external;
  procedure _pj_s_gall; external;
  procedure _pj_geoc; external;
  procedure _pj_s_geoc; external;
  procedure _pj_geocent; external;
  procedure _pj_s_geocent; external;
  procedure _pj_geos; external;
  procedure _pj_s_geos; external;
  procedure _pj_gins8; external;
  procedure _pj_s_gins8; external;
  procedure _pj_gn_sinu; external;
  procedure _pj_s_gn_sinu; external;
  procedure _pj_gnom; external;
  procedure _pj_s_gnom; external;
  procedure _pj_goode; external;
  procedure _pj_s_goode; external;
  procedure _pj_gs48; external;
  procedure _pj_s_gs48; external;
  procedure _pj_gs50; external;
  procedure _pj_s_gs50; external;
  procedure _pj_hammer; external;
  procedure _pj_s_hammer; external;
  procedure _pj_hatano; external;
  procedure _pj_s_hatano; external;
  procedure _pj_healpix; external;
  procedure _pj_s_healpix; external;
  procedure _pj_rhealpix; external;
  procedure _pj_s_rhealpix; external;
  procedure _pj_helmert; external;
  procedure _pj_s_helmert; external;
  procedure _pj_hgridshift; external;
  procedure _pj_s_hgridshift; external;
  procedure _pj_horner; external;
  procedure _pj_s_horner; external;
  procedure _pj_igh; external;
  procedure _pj_s_igh; external;
  procedure _pj_imw_p; external;
  procedure _pj_s_imw_p; external;
  procedure _pj_isea; external;
  procedure _pj_s_isea; external;
  procedure _pj_kav5; external;
  procedure _pj_s_kav5; external;
  procedure _pj_kav7; external;
  procedure _pj_s_kav7; external;
  procedure _pj_krovak; external;
  procedure _pj_s_krovak; external;
  procedure _pj_labrd; external;
  procedure _pj_s_labrd; external;
  procedure _pj_laea; external;
  procedure _pj_s_laea; external;
  procedure _pj_lagrng; external;
  procedure _pj_s_lagrng; external;
  procedure _pj_larr; external;
  procedure _pj_s_larr; external;
  procedure _pj_lask; external;
  procedure _pj_s_lask; external;
  procedure _pj_lonlat; external;
  procedure _pj_s_lonlat; external;
  procedure _pj_latlon; external;
  procedure _pj_s_latlon; external;
  procedure _pj_latlong; external;
  procedure _pj_s_latlong; external;
  procedure _pj_longlat; external;
  procedure _pj_s_longlat; external;
  procedure _pj_lcc; external;
  procedure _pj_s_lcc; external;
  procedure _pj_lcca; external;
  procedure _pj_s_lcca; external;
  procedure _pj_leac; external;
  procedure _pj_s_leac; external;
  procedure _pj_lee_os; external;
  procedure _pj_s_lee_os; external;
  procedure _pj_loxim; external;
  procedure _pj_s_loxim; external;
  procedure _pj_lsat; external;
  procedure _pj_s_lsat; external;
  procedure _pj_mbt_s; external;
  procedure _pj_s_mbt_s; external;
  procedure _pj_mbt_fps; external;
  procedure _pj_s_mbt_fps; external;
  procedure _pj_mbtfpp; external;
  procedure _pj_s_mbtfpp; external;
  procedure _pj_mbtfpq; external;
  procedure _pj_s_mbtfpq; external;
  procedure _pj_mbtfps; external;
  procedure _pj_s_mbtfps; external;
  procedure _pj_merc; external;
  procedure _pj_s_merc; external;
  procedure _pj_mil_os; external;
  procedure _pj_s_mil_os; external;
  procedure _pj_mill; external;
  procedure _pj_s_mill; external;
  procedure _pj_misrsom; external;
  procedure _pj_s_misrsom; external;
  procedure _pj_s_moll; external;
  procedure _pj_molodensky; external;
  procedure _pj_s_molodensky; external;
  procedure _pj_murd1; external;
  procedure _pj_s_murd1; external;
  procedure _pj_murd2; external;
  procedure _pj_s_murd2; external;
  procedure _pj_murd3; external;
  procedure _pj_s_murd3; external;
  procedure _pj_natearth; external;
  procedure _pj_s_natearth; external;
  procedure _pj_natearth2; external;
  procedure _pj_s_natearth2; external;
  procedure _pj_nell; external;
  procedure _pj_s_nell; external;
  procedure _pj_nell_h; external;
  procedure _pj_s_nell_h; external;
  procedure _pj_nicol; external;
  procedure _pj_s_nicol; external;
  procedure _pj_nsper; external;
  procedure _pj_s_nsper; external;
  procedure _pj_nzmg; external;
  procedure _pj_s_nzmg; external;
  procedure _pj_ob_tran; external;
  procedure _pj_s_ob_tran; external;
  procedure _pj_ocea; external;
  procedure _pj_s_ocea; external;
  procedure _pj_oea; external;
  procedure _pj_s_oea; external;
  procedure _pj_omerc; external;
  procedure _pj_s_omerc; external;
  procedure _pj_ortel; external;
  procedure _pj_s_ortel; external;
  procedure _pj_ortho; external;
  procedure _pj_s_ortho; external;
  procedure _pj_pconic; external;
  procedure _pj_s_pconic; external;
  procedure _pj_patterson; external;
  procedure _pj_s_patterson; external;
  procedure _pj_pipeline; external;
  procedure _pj_s_pipeline; external;
  procedure _pj_poly; external;
  procedure _pj_s_poly; external;
  procedure _pj_putp1; external;
  procedure _pj_s_putp1; external;
  procedure _pj_putp2; external;
  procedure _pj_s_putp2; external;
  procedure _pj_putp3; external;
  procedure _pj_s_putp3; external;
  procedure _pj_putp3p; external;
  procedure _pj_s_putp3p; external;
  procedure _pj_putp4p; external;
  procedure _pj_s_putp4p; external;
  procedure _pj_putp5; external;
  procedure _pj_s_putp5; external;
  procedure _pj_putp5p; external;
  procedure _pj_s_putp5p; external;
  procedure _pj_putp6; external;
  procedure _pj_s_putp6; external;
  procedure _pj_putp6p; external;
  procedure _pj_s_putp6p; external;
  procedure _pj_qua_aut; external;
  procedure _pj_s_qua_aut; external;
  procedure _pj_qsc; external;
  procedure _pj_s_qsc; external;
  procedure _pj_robin; external;
  procedure _pj_s_robin; external;
  procedure _pj_rpoly; external;
  procedure _pj_s_rpoly; external;
  procedure _pj_sch; external;
  procedure _pj_s_sch; external;
  procedure _pj_s_sinu; external;
  procedure _pj_somerc; external;
  procedure _pj_s_somerc; external;
  procedure _pj_stere; external;
  procedure _pj_s_stere; external;
  procedure _pj_sterea; external;
  procedure _pj_s_sterea; external;
  procedure _pj_gstmerc; external;
  procedure _pj_s_gstmerc; external;
  procedure _pj_tcc; external;
  procedure _pj_s_tcc; external;
  procedure _pj_tcea; external;
  procedure _pj_s_tcea; external;
  procedure _pj_times; external;
  procedure _pj_s_times; external;
  procedure _pj_tissot; external;
  procedure _pj_s_tissot; external;
  procedure _pj_tmerc; external;
  procedure _pj_s_tmerc; external;
  procedure _pj_tpeqd; external;
  procedure _pj_s_tpeqd; external;
  procedure _pj_tpers; external;
  procedure _pj_s_tpers; external;
  procedure _pj_unitconvert; external;
  procedure _pj_s_unitconvert; external;
  procedure _pj_ups; external;
  procedure _pj_s_ups; external;
  procedure _pj_urm5; external;
  procedure _pj_s_urm5; external;
  procedure _pj_urmfps; external;
  procedure _pj_s_urmfps; external;
  procedure _pj_utm; external;
  procedure _pj_s_utm; external;
  procedure _pj_vandg; external;
  procedure _pj_s_vandg; external;
  procedure _pj_vandg2; external;
  procedure _pj_s_vandg2; external;
  procedure _pj_vandg3; external;
  procedure _pj_s_vandg3; external;
  procedure _pj_vandg4; external;
  procedure _pj_s_vandg4; external;
  procedure _pj_vitk1; external;
  procedure _pj_s_vitk1; external;
  procedure _pj_vgridshift; external;
  procedure _pj_s_vgridshift; external;
  procedure _pj_wag1; external;
  procedure _pj_s_wag1; external;
  procedure _pj_wag2; external;
  procedure _pj_s_wag2; external;
  procedure _pj_wag3; external;
  procedure _pj_s_wag3; external;
  procedure _pj_wag4; external;
  procedure _pj_s_wag4; external;
  procedure _pj_wag5; external;
  procedure _pj_s_wag5; external;
  procedure _pj_wag6; external;
  procedure _pj_s_wag6; external;
  procedure _pj_wag7; external;
  procedure _pj_s_wag7; external;
  procedure _pj_webmerc; external;
  procedure _pj_s_webmerc; external;
  procedure _pj_weren; external;
  procedure _pj_s_weren; external;
  procedure _pj_wink1; external;
  procedure _pj_s_wink1; external;
  procedure _pj_wink2; external;
  procedure _pj_s_wink2; external;
  procedure _pj_wintri; external;
  procedure _pj_s_wintri; external;
  procedure _pj_fwd4d; external;
  procedure _pj_inv4d; external;
  procedure _dmstor; external;
  procedure _pj_expand_init; external;
  procedure _pj_factors; external;
  procedure _pj_fwd; external;
  procedure _pj_inv; external;
  procedure _pj_fwd3d; external;
  procedure _pj_set_ctx; external;
  procedure _pj_vlog; external;
  procedure _pj_ctx_fopen; external;
  procedure _pj_apply_vgridshift; external;
  procedure _pj_apply_gridshift_2; external;
  procedure _proj_mdist_ini; external;
  procedure _proj_mdist; external;
  procedure _proj_inv_mdist; external;
  procedure _pj_ellipsoid; external;
  procedure _pj_rouss; external;
  procedure _pj_s_rouss; external;
  procedure _pj_inv3d; external;
  procedure _pj_strdup; external;
  procedure _pj_find_file; external;
  procedure _pj_has_inverse; external;
  procedure _pj_inherit_ellipsoid_def; external;
  procedure _pj_left; external;
  procedure _pj_make_args; external;
  procedure _pj_right; external;
  procedure _pj_trim_argc; external;
  procedure _pj_trim_argv; external;
  procedure _proj_get_path_count; external;
  procedure _proj_get_searchpath; external;
  procedure _rtodms; external;
  procedure _pj_approx_2D_trans; external;
  procedure _pj_approx_3D_trans; external;
  procedure _proj_create_argv; external;
  procedure _proj_destroy; external;
  procedure _proj_log_error; external;
  procedure _proj_vgrid_init; external;
  procedure _proj_vgrid_value; external;
  procedure _proj_hgrid_apply; external;
  procedure _proj_hgrid_init; external;
  procedure _proj_create; external;
  procedure _proj_hgrid_value; external;
  procedure _pj_eqearth; external;
  procedure _pj_s_eqearth; external;
{$ENDIF}
{$ENDREGION}

  {$REGION 'precompiled lib proj c code'}

  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_aeqd.o'} {$ELSE} {$L 'PJ_aeqd.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_gnom.o'} {$ELSE} {$L 'PJ_gnom.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_laea.o'} {$ELSE} {$L 'PJ_laea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_mod_ster.o'} {$ELSE} {$L 'PJ_mod_ster.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_nsper.o'} {$ELSE} {$L 'PJ_nsper.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_nzmg.o'} {$ELSE} {$L 'PJ_nzmg.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_ortho.o'} {$ELSE} {$L 'PJ_ortho.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_stere.o'} {$ELSE} {$L 'PJ_stere.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_sterea.o'} {$ELSE} {$L 'PJ_sterea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'proj_rouss.o'} {$ELSE} {$L 'proj_rouss.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_aea.o'} {$ELSE} {$L 'PJ_aea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_bipc.o'} {$ELSE} {$L 'PJ_bipc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_bonne.o'} {$ELSE} {$L 'PJ_bonne.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eqdc.o'} {$ELSE} {$L 'PJ_eqdc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_imw_p.o'} {$ELSE} {$L 'PJ_imw_p.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_lcc.o'} {$ELSE} {$L 'PJ_lcc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_poly.o'} {$ELSE} {$L 'PJ_poly.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_rpoly.o'} {$ELSE} {$L 'PJ_rpoly.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_sconics.o'} {$ELSE} {$L 'PJ_sconics.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_lcca.o'} {$ELSE} {$L 'PJ_lcca.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_ccon.o'} {$ELSE} {$L 'PJ_ccon.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_cass.o'} {$ELSE} {$L 'PJ_cass.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_cc.o'} {$ELSE} {$L 'PJ_cc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_cea.o'} {$ELSE} {$L 'PJ_cea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eqc.o'} {$ELSE} {$L 'PJ_eqc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_gall.o'} {$ELSE} {$L 'PJ_gall.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_labrd.o'} {$ELSE} {$L 'PJ_labrd.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_lsat.o'} {$ELSE} {$L 'PJ_lsat.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_misrsom.o'} {$ELSE} {$L 'PJ_misrsom.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_merc.o'} {$ELSE} {$L 'PJ_merc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_mill.o'} {$ELSE} {$L 'PJ_mill.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_ocea.o'} {$ELSE} {$L 'PJ_ocea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_omerc.o'} {$ELSE} {$L 'PJ_omerc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_patterson.o'} {$ELSE} {$L 'PJ_patterson.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_somerc.o'} {$ELSE} {$L 'PJ_somerc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_tcc.o'} {$ELSE} {$L 'PJ_tcc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_tcea.o'} {$ELSE} {$L 'PJ_tcea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_tmerc.o'} {$ELSE} {$L 'PJ_tmerc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_geos.o'} {$ELSE} {$L 'PJ_geos.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_gstmerc.o'} {$ELSE} {$L 'PJ_gstmerc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'proj_etmerc.o'} {$ELSE} {$L 'proj_etmerc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_comill.o'} {$ELSE} {$L 'PJ_comill.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_airy.o'} {$ELSE} {$L 'PJ_airy.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_aitoff.o'} {$ELSE} {$L 'PJ_aitoff.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_august.o'} {$ELSE} {$L 'PJ_august.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_bacon.o'} {$ELSE} {$L 'PJ_bacon.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_chamb.o'} {$ELSE} {$L 'PJ_chamb.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_hammer.o'} {$ELSE} {$L 'PJ_hammer.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_lagrng.o'} {$ELSE} {$L 'PJ_lagrng.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_larr.o'} {$ELSE} {$L 'PJ_larr.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_lask.o'} {$ELSE} {$L 'PJ_lask.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_nocol.o'} {$ELSE} {$L 'PJ_nocol.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_ob_tran.o'} {$ELSE} {$L 'PJ_ob_tran.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_oea.o'} {$ELSE} {$L 'PJ_oea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_sch.o'} {$ELSE} {$L 'PJ_sch.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_tpeqd.o'} {$ELSE} {$L 'PJ_tpeqd.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_vandg.o'} {$ELSE} {$L 'PJ_vandg.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_vandg2.o'} {$ELSE} {$L 'PJ_vandg2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_vandg4.o'} {$ELSE} {$L 'PJ_vandg4.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_wag7.o'} {$ELSE} {$L 'PJ_wag7.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_latlong.o'} {$ELSE} {$L 'PJ_latlong.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_krovak.o'} {$ELSE} {$L 'PJ_krovak.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_geoc.o'} {$ELSE} {$L 'PJ_geoc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_geocent.o'} {$ELSE} {$L 'pj_geocent.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_healpix.o'} {$ELSE} {$L 'PJ_healpix.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_qsc.o'} {$ELSE} {$L 'PJ_qsc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_boggs.o'} {$ELSE} {$L 'PJ_boggs.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_collg.o'} {$ELSE} {$L 'PJ_collg.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_crast.o'} {$ELSE} {$L 'PJ_crast.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_denoy.o'} {$ELSE} {$L 'PJ_denoy.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eck1.o'} {$ELSE} {$L 'PJ_eck1.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eck2.o'} {$ELSE} {$L 'PJ_eck2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eck3.o'} {$ELSE} {$L 'PJ_eck3.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eck4.o'} {$ELSE} {$L 'PJ_eck4.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eck5.o'} {$ELSE} {$L 'PJ_eck5.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_fahey.o'} {$ELSE} {$L 'PJ_fahey.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_fouc_s.o'} {$ELSE} {$L 'PJ_fouc_s.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_gins8.o'} {$ELSE} {$L 'PJ_gins8.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_gn_sinu.o'} {$ELSE} {$L 'PJ_gn_sinu.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_goode.o'} {$ELSE} {$L 'PJ_goode.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_igh.o'} {$ELSE} {$L 'PJ_igh.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_hatano.o'} {$ELSE} {$L 'PJ_hatano.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_loxim.o'} {$ELSE} {$L 'PJ_loxim.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_mbt_fps.o'} {$ELSE} {$L 'PJ_mbt_fps.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_mbtfpp.o'} {$ELSE} {$L 'PJ_mbtfpp.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_mbtfpq.o'} {$ELSE} {$L 'PJ_mbtfpq.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_moll.o'} {$ELSE} {$L 'PJ_moll.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_nell.o'} {$ELSE} {$L 'PJ_nell.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_nell_h.o'} {$ELSE} {$L 'PJ_nell_h.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_putp2.o'} {$ELSE} {$L 'PJ_putp2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_putp3.o'} {$ELSE} {$L 'PJ_putp3.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_putp4p.o'} {$ELSE} {$L 'PJ_putp4p.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_putp5.o'} {$ELSE} {$L 'PJ_putp5.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_putp6.o'} {$ELSE} {$L 'PJ_putp6.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_robin.o'} {$ELSE} {$L 'PJ_robin.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_sts.o'} {$ELSE} {$L 'PJ_sts.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_urm5.o'} {$ELSE} {$L 'PJ_urm5.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_urmfps.o'} {$ELSE} {$L 'PJ_urmfps.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_wag2.o'} {$ELSE} {$L 'PJ_wag2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_wag3.o'} {$ELSE} {$L 'PJ_wag3.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_wink1.o'} {$ELSE} {$L 'PJ_wink1.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_wink2.o'} {$ELSE} {$L 'PJ_wink2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_isea.o'} {$ELSE} {$L 'PJ_isea.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_calcofi.o'} {$ELSE} {$L 'PJ_calcofi.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_natearth.o'} {$ELSE} {$L 'PJ_natearth.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_natearth2.o'} {$ELSE} {$L 'PJ_natearth2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_times.o'} {$ELSE} {$L 'PJ_times.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_eqearth.o'} {$ELSE} {$L 'PJ_eqearth.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'aasincos.o'} {$ELSE} {$L 'aasincos.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'adjlon.o'} {$ELSE} {$L 'adjlon.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'bch2bps.o'} {$ELSE} {$L 'bch2bps.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'bchgen.o'} {$ELSE} {$L 'bchgen.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_gauss.o'} {$ELSE} {$L 'pj_gauss.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'biveval.o'} {$ELSE} {$L 'biveval.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'dmstor.o'} {$ELSE} {$L 'dmstor.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'mk_cheby.o'} {$ELSE} {$L 'mk_cheby.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_auth.o'} {$ELSE} {$L 'pj_auth.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_deriv.o'} {$ELSE} {$L 'pj_deriv.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_ell_set.o'} {$ELSE} {$L 'pj_ell_set.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_ellps.o'} {$ELSE} {$L 'pj_ellps.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_errno.o'} {$ELSE} {$L 'pj_errno.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_factors.o'} {$ELSE} {$L 'pj_factors.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_fwd.o'} {$ELSE} {$L 'pj_fwd.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_init.o'} {$ELSE} {$L 'pj_init.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_inv.o'} {$ELSE} {$L 'pj_inv.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_list.o'} {$ELSE} {$L 'pj_list.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_malloc.o'} {$ELSE} {$L 'pj_malloc.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_mlfn.o'} {$ELSE} {$L 'pj_mlfn.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_msfn.o'} {$ELSE} {$L 'pj_msfn.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_open_lib.o'} {$ELSE} {$L 'pj_open_lib.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_param.o'} {$ELSE} {$L 'pj_param.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_phi2.o'} {$ELSE} {$L 'pj_phi2.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_pr_list.o'} {$ELSE} {$L 'pj_pr_list.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_qsfn.o'} {$ELSE} {$L 'pj_qsfn.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_strerrno.o'} {$ELSE} {$L 'pj_strerrno.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_tsfn.o'} {$ELSE} {$L 'pj_tsfn.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_units.o'} {$ELSE} {$L 'pj_units.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_zpoly1.o'} {$ELSE} {$L 'pj_zpoly1.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'rtodms.o'} {$ELSE} {$L 'rtodms.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'vector1.o'} {$ELSE} {$L 'vector1.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_release.o'} {$ELSE} {$L 'pj_release.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'geocent.o'} {$ELSE} {$L 'geocent.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_transform.o'} {$ELSE} {$L 'pj_transform.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_datum_set.o'} {$ELSE} {$L 'pj_datum_set.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_datums.o'} {$ELSE} {$L 'pj_datums.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_apply_gridshift.o'} {$ELSE} {$L 'pj_apply_gridshift.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_gc_reader.o'} {$ELSE} {$L 'pj_gc_reader.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_gridcatalog.o'} {$ELSE} {$L 'pj_gridcatalog.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'nad_cvt.o'} {$ELSE} {$L 'nad_cvt.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'nad_init.o'} {$ELSE} {$L 'nad_init.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'nad_intr.o'} {$ELSE} {$L 'nad_intr.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_utils.o'} {$ELSE} {$L 'pj_utils.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_gridlist.o'} {$ELSE} {$L 'pj_gridlist.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_gridinfo.o'} {$ELSE} {$L 'pj_gridinfo.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'proj_mdist.o'} {$ELSE} {$L 'proj_mdist.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_mutex.o'} {$ELSE} {$L 'pj_mutex.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_initcache.o'} {$ELSE} {$L 'pj_initcache.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_ctx.o'} {$ELSE} {$L 'pj_ctx.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_fileapi.o'} {$ELSE} {$L 'pj_fileapi.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_log.o'} {$ELSE} {$L 'pj_log.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_apply_vgridshift.o'} {$ELSE} {$L 'pj_apply_vgridshift.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_strtod.o'} {$ELSE} {$L 'pj_strtod.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_internal.o'} {$ELSE} {$L 'pj_internal.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'pj_math.o'} {$ELSE} {$L 'pj_math.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'proj_4D_api.o'} {$ELSE} {$L 'proj_4D_api.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_cart.o'} {$ELSE} {$L 'PJ_cart.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_pipeline.o'} {$ELSE} {$L 'PJ_pipeline.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_horner.o'} {$ELSE} {$L 'PJ_horner.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_helmert.o'} {$ELSE} {$L 'PJ_helmert.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_vgridshift.o'} {$ELSE} {$L 'PJ_vgridshift.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_hgridshift.o'} {$ELSE} {$L 'PJ_hgridshift.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_unitconvert.o'} {$ELSE} {$L 'PJ_unitconvert.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_molodensky.o'} {$ELSE} {$L 'PJ_molodensky.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_deformation.o'} {$ELSE} {$L 'PJ_deformation.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'PJ_axisswap.o'} {$ELSE} {$L 'PJ_axisswap.obj'} {$ENDIF} {$ENDIF}
  {$IFDEF MSWINDOWS} {$IFDEF WIN64} {$L 'geodesic.o'} {$ELSE} {$L 'geodesic.obj'} {$ENDIF} {$ENDIF}
  {$ENDREGION}

	{$REGION 'api calls'}
	function _pj_is_latlong(p: TProjPJ): integer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_is_latlong';
	function _pj_is_geocent(p: TProjPJ): integer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_is_geocent';
	function _pj_latlong_from_proj(p: TProjPJ): TProjPJ; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_latlong_from_proj';
	procedure _pj_free(p: TProjPJ); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_free';
	function _pj_transform(src,dst: TProjPJ; point_count,point_offset: integer; x,y,z: PDouble): Integer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_transform';
	function _pj_get_def(p: TProjPJ): PAnsiString; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_def';
	function _pj_strerrno(errno: Integer): Pointer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_strerrno';
	function _pj_get_release(): Pointer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_release';
	function _pj_ctx_fgets(ctx: TProjCtx; line: PByte; Size: Integer; _file: PInteger): PByte; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_ctx_fgets';
	procedure _pj_log(ctx: TProjCtx; level: Integer; fmt: MarshaledAString); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_log'; varargs;
	procedure _pj_get_spheroid_defn(p: TProjPJ; major_axis: PDouble; eccentricity_squared: PDouble); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_spheroid_defn';
	function _pj_get_errno_ref(): PInteger; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_errno_ref';
	function _pj_context_errno(ctx: TProjCtx): Integer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'proj_context_errno';
  function _pj_ctx_alloc: TProjCtx; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_ctx_alloc';
  function _pj_get_default_ctx(): TProjCtx; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_default_ctx';
  function _pj_get_ctx(p: TProjPJ): TProjCtx; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_get_ctx';
  function _pj_init_plus_ctx(ctx: TProjCtx; defn: MarshaledAString): TProjPJ; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_init_plus_ctx';
	function _pj_init_plus(const def: MarshaledAString): TProjPJ; overload; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_init_plus';
  procedure _pj_ctx_free( ctx: TProjCtx ); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_ctx_free';
  procedure _pj_ctx_set_debug( ctx: TProjCtx; lvl: Integer ); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_ctx_set_debug';
  function _pj_set_log_level(ctx: TProjCtx; lvl: Integer): Integer; cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'proj_log_level';
	procedure _pj_ctx_set_logger(ctx: TProjCtx; logger: Pointer); cdecl; external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'pj_ctx_set_logger';

  function _pj_torad (angle_in_degrees: double): double;cdecl;  external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'proj_torad';
  function _pj_todeg (angle_in_radians: double): double;cdecl;  external {$IFDEF MSWINDOWS}name NAME_PREFIX+{$ENDIF}'proj_todeg';
{$ENDREGION 'Api calls'}

{$IFDEF MSWINDOWS}

    function _sinx_impl(x: double; n: cardinal; m: integer): double; cdecl; inline;
    begin
      if x.IsNan or x.IsInfinity then
        Result := Math.NaN
      else
      begin
        if n and $01 <> 0 then
          result := system.cos(x)
        else
          result := system.sin(x);

        if n and $02 <> 0 then
          Result := -Result;
      end;

    end;


{$IFDEF WIN64}

    function _Sinx(x: double; n: cardinal; m: integer): double; cdecl;  inline;
    begin
      result := _sinx_impl(x,n,m);
    end;

    function _Log(x: double): double;cdecl;  inline;
    begin
      Result := System.Ln(x);
    end;

    function _Cosh(v: double): double;cdecl;  inline;
    begin
      Result := Math.Cosh(v);
    end;
    function _Sinh(v: double): double;cdecl;  inline;
    begin
      Result := Math.Sinh(v);
    end;
    function _FNan(): single; cdecl; inline;
    begin
      Result := Math.NaN;
    end;
    function _Inf(): Double;cdecl; inline;
    begin
      Result := Math.Infinity;
    end;

    procedure exit(status: integer); external msvcrt name 'exit';
{$ELSE}
    function _fabs(v: double): double; cdecl; inline;
    begin
      result := System.Abs(v);
    end;
    function __Sinx(const x: double; n: cardinal; m: integer): double; cdecl; inline;
    begin
      result := _sinx_impl(x,n,m);
    end;
    function __Inf(): Double; cdecl; inline;
    begin
      Result := Math.Infinity;
    end;
    function __Log(x: double): double; cdecl; inline;
    begin
      Result := System.Ln(x);
    end;
    function __Sinh(v: double): double; cdecl; inline;
    begin
      Result := Math.Sinh(v);
    end;
    function __Cosh(v: double): double; cdecl; inline;
    begin
      Result := Math.Cosh(v);
    end;
    function __FNan(): single; cdecl; inline;
    begin
      Result := Math.NaN;
    end;
    function _ldexp(const x: Double; const p: integer): double; cdecl; inline;
    begin
      Result := Math.Ldexp(x,p);
    end;
{$ENDIF}
{$ENDIF}

//------------------------------------------------
// helper functions
function CStringPointerToString(Value: MarshaledAString): string;
begin
	Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(Value));
end;

function StringToCStringPointer(const Value: string): Pointer;
var
  m: TMarshaller;
begin
  Result := m.AsAnsi(Value).ToPointer;
end;

//------------------------------------------------

procedure _default_stderr_log_handler(appdata: Pointer; level: Integer; msg: Pointer); cdecl;
var
	MsgText: string;

	function LogLevelCodeToString(value: Integer): string;
	begin
		Result := '';
		case value of
			1: Result := 'libPROJ error: ';
			2: Result := 'libPROJ debug: ';
			3: Result := 'libPROJ trace: ';
			4: Result := 'libPROJ tell: ';
		end;
	end;
begin
	MsgText := CStringPointerToString(msg);
	if MsgText <> '' then
	begin
		MsgText := LogLevelCodeToString(level) + MsgText;
  {$IFDEF MSWINDOWS}
    if IsConsole then
      Writeln( MsgText )
    else
		  OutputDebugString( PChar( MsgText ));
  {$ENDIF}
	end;
end;

function PJ_init_plus(const def: string): TProjPJ;
var
	ctx: TProjCtx;
  m: TMarshaller;
begin

  Result := nil;

  if not Assigned(FDefaultContext) then
    begin
      ctx := _pj_get_default_ctx;

{$IfOpt D+}
      _pj_ctx_set_logger(ctx,@_default_stderr_log_handler);
      _pj_ctx_set_debug(ctx,4);
      _pj_set_log_level(ctx,4);

      _pj_log(ctx,4,m.AsAnsi( '"%s"' ).ToPointer,m.AsAnsi('using proj internal context').ToPointer);
{$endif}
    end
  else
    ctx := FDefaultContext;

  if ctx <> nil then
  begin
 {$IfOpt D+}
    _pj_log(ctx,3,m.AsAnsi( '"%s"' ).ToPointer,m.AsAnsi(
      'create projection for '+def).ToPointer);
 {$endif}

	  Result := _pj_init_plus_ctx(ctx, m.AsAnsi( def ).ToPointer );

 {$IfOpt D+}
    if Result = nil then
    begin

      _pj_log(ctx,3,m.AsAnsi( '"%s"' ).ToPointer,m.AsAnsi(
        'projection for '+def+
        ' failed witn '+ _pj_context_errno(ctx).ToString + ' code').ToPointer);
    end
    else
      _pj_log(ctx,3,m.AsAnsi( '"%s"' ).ToPointer,m.AsAnsi(
        'projection for '+def+' created').ToPointer);
 {$endif}

  end;

end;

function PJ_is_geographic(p: TProjPJ): Boolean;
begin
	Result := Boolean(_pj_is_latlong(p));
end;

function PJ_is_geocentric(p: TProjPJ): Boolean;
begin
	Result := Boolean(_pj_is_geocent(p));
end;

function PJ_latlong_from_proj(p: TProjPJ): TProjPJ;
begin
	Result := _pj_latlong_from_proj(p);
end;

function PJ_get_version_string(): string;
begin
	Result := 'proj ' + CStringPointerToString(_pj_get_release());
end;

function PJ_strerrno(errno: integer): string;
begin
	Result := CStringPointerToString(_pj_strerrno(errno));
end;

function PJ_get_spheroid_defn(p: TProjPJ; out major_axis,
    eccentricity_squared: Double): Boolean;
begin
	Result := PJ_is_valid(p);// Assigned(p);
	if Result then
		_pj_get_spheroid_defn(p,@major_axis,@eccentricity_squared);
end;

function PJ_get_errno(): Integer;
begin
	Result := _pj_get_errno_ref()^;
end;

function PJ_transform_points2D(src, dst: TProjPJ; x, y: PDouble; count:
    Integer; angular: Boolean): Integer;
var
	degSrc, degDst: Boolean;
	i: Integer;
	_x,_y, z: PDouble;
begin

  Result := -1;

  if PJ_is_valid(src) and PJ_is_valid(dst) then
  begin
    Result := PJ_is_same_definition(src,dst);
    // projections differs
    if Result = 0 then //
    begin
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

      if degSrc then
      begin
        _x := x;
        _y := y;
        for i := 0 to count -1 do
        begin
          _x^ := _pj_torad(x^); // PJ_DEG_TO_RAD * _x^;
          _y^ := _pj_torad(_y^); // PJ_DEG_TO_RAD * _y^;

          Inc(_x);
          Inc(_y);
        end;
      end;

      z := nil;

      Result := _pj_transform(src,dst,count,1,x,y,z);

      if Result = 0 then
      begin
        if degDst then
        begin
          _x := x;
          _y := y;
          for i := 0 to count -1 do
          begin
            _x^ := _pj_todeg(_x^); // PJ_RAD_TO_DEG * _x^;
            _y^ := _pj_todeg(_y^); // PJ_RAD_TO_DEG * _y^;

            Inc(_x);
            Inc(_y);
          end;
        end;
      end;
    end
  end;
end;

function PJ_transform_point2D(src, dst: TProjPJ; x, y: PDouble; angular:
    Boolean): Integer;
begin
	Result := PJ_transform_points2D(src,dst,x,y,1,angular);
end;

function PJ_transform_point2D(src, dst: TProjPJ; var x, y: Double; angular:
    Boolean): Integer;
begin
	Result := PJ_transform_point2D(src,dst,@x,@y,angular);
end;

function PJ_get_definition(p: TProjPJ): string;
var
  d: Pointer;
begin
  d := _pj_get_def(p);
  if d <> nil then
  begin
    Result := CStringPointerToString( d );
    FreeMem( d );
  end

end;

function PJ_is_same_definition(p1, p2: TProjPJ): Integer;
var d1,d2: string;
begin
	Result := -2;
  if PJ_is_valid(p1) and PJ_is_valid(p2) then
	begin
    Result := -1;

		d1 := PJ_get_definition(p1);
		d2 := PJ_get_definition(p2);

		if (d1 <> '') and (d2 <> '') then
      Result := Integer(SameText(d1,d2))
	end;
end;

function PJ_is_valid(p: TProjPJ): Boolean;
begin
  Result := Assigned(p);
end;

procedure PJ_free(var p: TProjPJ);
begin
  if PJ_is_valid(p) then
  begin
  	_pj_free(p);
    p := nil;
  end;
end;

initialization

  FDefaultContext := _pj_ctx_alloc();

  if Assigned(FDefaultContext) then
  begin
    _pj_ctx_set_logger(FDefaultContext,@_default_stderr_log_handler);
{$IfOpt D+}
    _pj_ctx_set_debug(FDefaultContext,4);
    _pj_set_log_level(FDefaultContext,4);
{$endif}
  end;

finalization
  if Assigned(FDefaultContext) then
  begin
    _pj_ctx_free(FDefaultContext);
  end;
  end.

