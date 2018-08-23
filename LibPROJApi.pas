/// <summary>
///   plain api of libProj c library
/// </summary>
/// <remarks>
///   currently using Proj4 api functions
/// </remarks>
unit LibPROJApi;


interface

uses Classes, SysUtils;

type TPJ_LOG_FUNCTION =
	procedure (app_data: pointer; level: integer; msg: Pointer) of object; cdecl;

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
function PJ_init_plus(const def: string): Pointer;
/// <summary>
///   Returns TRUE if input coordinate system object is geographic.
/// </summary>
/// <param name="p">
///   input coordinate system handle.
/// </param>
function PJ_is_geographic(p: Pointer): Boolean;
/// <summary>
///   Returns TRUE if input coordinate system object is geocentric.
/// </summary>
/// <param name="p">
///   input coordinate system handle. <br />
/// </param>
function PJ_is_geocentric(p: Pointer): Boolean;
/// <summary>
///   Return a projection handle defining the lat/long coordinate system on
///   which a projection is based. If the coordinate system passed in is
///   latlong, a clone of the same will be returned.
/// </summary>
/// <param name="p">
///   input projection handle.
/// </param>
function PJ_latlong_from_proj(p: Pointer): Pointer;
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
function PJ_transform_point2D(src,dst: Pointer; var x,y: Double; angular: Boolean): Integer; overload;
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
function PJ_transform_point2D(src,dst: Pointer; x,y: PDouble; angular: Boolean): Integer; overload;
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
function PJ_transform_points2D(src,dst: Pointer; x,y: PDouble; count: Integer; angular: Boolean): Integer;
/// <summary>
///   Returns the PROJ.4 command string that would produce this definition
///   expanded as much as possible. For instance +init= calls and +datum=
///   definitions would be expanded.
/// </summary>
/// <param name="p">
///   coordinate system handle.
/// </param>
function PJ_get_definition(p: Pointer): string;
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
function PJ_is_same_definition(p1,p2: Pointer): Integer;
/// <summary>
///   cleanup memory associated with libproj coordinate system object. <br />
/// </summary>
/// <param name="p">
///   coordinate system handle.
/// </param>
procedure PJ_free(p: Pointer);
/// <summary>
///   get libproj version string
/// </summary>
function PJ_get_version_string(): string;
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
function PJ_get_spheroid_defn(p: pointer; out major_axis, eccentricity_squared: Double): Boolean;
function PJ_get_errno(): Integer;

implementation

uses
	Windows, Math, MSVCrtl;

const
	PJ_RAD_TO_DEG = 57.295779513082320876798154814105000; //(1*180 / PI)
	PJ_DEG_TO_RAD = 0.017453292519943295769236907684886;

// obj\$(Platform)\$(Config)

// precompiled c code
	{$REGION 'external declarations to avoid linker errors'}
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
  procedure _pj_ctx_alloc; external;
  procedure _pj_ctx_free; external;
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
  procedure _pj_init_plus_ctx; external;
  procedure _proj_mdist_ini; external;
  procedure _proj_mdist; external;
  procedure _proj_inv_mdist; external;
{$ENDREGION}

	{$REGION 'projections related declarations'}

  {$LINK 'PJ_aea.obj'}
  {$LINK 'PJ_aeqd.obj'}
  {$LINK 'PJ_airy.obj'}
  {$LINK 'PJ_aitoff.obj'}
  {$LINK 'PJ_august.obj'}
  {$LINK 'PJ_axisswap.obj'}
  {$LINK 'PJ_bacon.obj'}
  {$LINK 'PJ_bipc.obj'}
  {$LINK 'PJ_boggs.obj'}
  {$LINK 'PJ_bonne.obj'}
  {$LINK 'PJ_calcofi.obj'}
  {$LINK 'PJ_cart.obj'}
  {$LINK 'PJ_cass.obj'}
  {$LINK 'PJ_cc.obj'}
  {$LINK 'PJ_ccon.obj'}
  {$LINK 'PJ_cea.obj'}
  {$LINK 'PJ_chamb.obj'}
  {$LINK 'PJ_collg.obj'}
  {$LINK 'PJ_comill.obj'}
  {$LINK 'PJ_crast.obj'}
  {$LINK 'PJ_deformation.obj'}
  {$LINK 'PJ_denoy.obj'}
  {$LINK 'PJ_eck1.obj'}
  {$LINK 'PJ_eck2.obj'}
  {$LINK 'PJ_eck3.obj'}
  {$LINK 'PJ_eck4.obj'}
  {$LINK 'PJ_eck5.obj'}
  {$LINK 'PJ_eqc.obj'}
  {$LINK 'PJ_eqdc.obj'}
  {$LINK 'PJ_fahey.obj'}
  {$LINK 'PJ_fouc_s.obj'}
  {$LINK 'PJ_gall.obj'}
  {$LINK 'PJ_geoc.obj'}
  {$LINK 'PJ_geos.obj'}
  {$LINK 'PJ_gins8.obj'}
  {$LINK 'PJ_gnom.obj'}
  {$LINK 'PJ_gn_sinu.obj'}
  {$LINK 'PJ_goode.obj'}
  {$LINK 'PJ_igh.obj'}
  {$LINK 'PJ_gstmerc.obj'}
  {$LINK 'PJ_hammer.obj'}
  {$LINK 'PJ_hatano.obj'}
  {$LINK 'PJ_helmert.obj'}
  {$LINK 'PJ_hgridshift.obj'}
  {$LINK 'PJ_horner.obj'}
  {$LINK 'PJ_isea.obj'}
  {$LINK 'PJ_imw_p.obj'}
  {$LINK 'PJ_krovak.obj'}
  {$LINK 'PJ_labrd.obj'}
  {$LINK 'PJ_laea.obj'}
  {$LINK 'PJ_lagrng.obj'}
  {$LINK 'PJ_larr.obj'}
  {$LINK 'PJ_lask.obj'}
  {$LINK 'PJ_latlong.obj'}
  {$LINK 'PJ_lcca.obj'}
  {$LINK 'PJ_lcc.obj'}
  {$LINK 'PJ_loxim.obj'}
  {$LINK 'PJ_lsat.obj'}
  {$LINK 'PJ_misrsom.obj'}
  {$LINK 'PJ_mbt_fps.obj'}
  {$LINK 'PJ_mbtfpp.obj'}
  {$LINK 'PJ_mbtfpq.obj'}
  {$LINK 'PJ_merc.obj'}
  {$LINK 'PJ_mill.obj'}
  {$LINK 'PJ_mod_ster.obj'}
  {$LINK 'PJ_moll.obj'}
  {$LINK 'PJ_molodensky.obj'}
  {$LINK 'PJ_natearth.obj'}
  {$LINK 'PJ_natearth2.obj'}
  {$LINK 'PJ_nell.obj'}
  {$LINK 'PJ_nell_h.obj'}
  {$LINK 'PJ_nocol.obj'}
  {$LINK 'PJ_nsper.obj'}
  {$LINK 'PJ_nzmg.obj'}
  {$LINK 'PJ_ob_tran.obj'}
  {$LINK 'PJ_ocea.obj'}
  {$LINK 'PJ_oea.obj'}
  {$LINK 'PJ_omerc.obj'}
  {$LINK 'PJ_ortho.obj'}
  {$LINK 'PJ_patterson.obj'}
  {$LINK 'PJ_pipeline.obj'}
  {$LINK 'PJ_poly.obj'}
  {$LINK 'PJ_putp2.obj'}
  {$LINK 'PJ_putp3.obj'}
  {$LINK 'PJ_putp4p.obj'}
  {$LINK 'PJ_putp5.obj'}
  {$LINK 'PJ_putp6.obj'}
  {$LINK 'PJ_qsc.obj'}
  {$LINK 'PJ_robin.obj'}
  {$LINK 'PJ_rpoly.obj'}
  {$LINK 'PJ_sch.obj'}
  {$LINK 'PJ_sconics.obj'}
  {$LINK 'PJ_somerc.obj'}
  {$LINK 'PJ_sterea.obj'}
  {$LINK 'PJ_stere.obj'}
  {$LINK 'PJ_sts.obj'}
  {$LINK 'PJ_tcc.obj'}
  {$LINK 'PJ_tcea.obj'}
  {$LINK 'PJ_times.obj'}
  {$LINK 'PJ_tmerc.obj'}
  {$LINK 'PJ_tpeqd.obj'}
  {$LINK 'PJ_unitconvert.obj'}
  {$LINK 'PJ_urm5.obj'}
  {$LINK 'PJ_urmfps.obj'}
  {$LINK 'PJ_vandg.obj'}
  {$LINK 'PJ_vandg2.obj'}
  {$LINK 'PJ_vandg4.obj'}
  {$LINK 'PJ_vgridshift.obj'}
  {$LINK 'PJ_wag2.obj'}
  {$LINK 'PJ_wag3.obj'}
  {$LINK 'PJ_wag7.obj'}
  {$LINK 'PJ_wink1.obj'}
  {$LINK 'PJ_wink2.obj'}
  {$LINK 'proj_etmerc.obj'}
{$ENDREGION}

	{$REGION 'proj core functions'}

	{$LINK 'aasincos.obj'}
  {$LINK 'adjlon.obj'}
  {$LINK 'bchgen.obj'}
  {$LINK 'bch2bps.obj'}
  {$LINK 'biveval.obj'}
  {$LINK 'dmstor.obj'}
  {$LINK 'emess.obj'}
  {$LINK 'geocent.obj'}
  {$LINK 'geodesic.obj'}
  {$LINK 'mk_cheby.obj'}
  {$LINK 'nad_cvt.obj'}
  {$LINK 'nad_init.obj'}
  {$LINK 'nad_intr.obj'}
  {$LINK 'pj_apply_gridshift.obj'}
  {$LINK 'pj_apply_vgridshift.obj'}
  {$LINK 'pj_auth.obj'}
  {$LINK 'pj_ctx.obj'}
  {$LINK 'pj_fileapi.obj'}
  {$LINK 'pj_datum_set.obj'}
  {$LINK 'pj_datums.obj'}
  {$LINK 'pj_deriv.obj'}
  {$LINK 'pj_ell_set.obj'}
  {$LINK 'pj_ellps.obj'}
  {$LINK 'pj_errno.obj'}
  {$LINK 'pj_factors.obj'}
  {$LINK 'pj_fwd.obj'}
  {$LINK 'pj_gauss.obj'}
  {$LINK 'pj_gc_reader.obj'}
  {$LINK 'pj_geocent.obj'}
  {$LINK 'pj_gridcatalog.obj'}
  {$LINK 'pj_gridinfo.obj'}
  {$LINK 'pj_gridlist.obj'}
  {$LINK 'PJ_healpix.obj'}
  {$LINK 'pj_init.obj'}
  {$LINK 'pj_initcache.obj'}
  {$LINK 'pj_inv.obj'}
  {$LINK 'pj_list.obj'}
  {$LINK 'pj_log.obj'}
  {$LINK 'pj_malloc.obj'}
  {$LINK 'pj_math.obj'}
  {$LINK 'pj_mlfn.obj'}
  {$LINK 'pj_msfn.obj'}
  {$LINK 'pj_mutex.obj'}
  {$LINK 'proj_4D_api.obj'}
  {$LINK 'pj_internal.obj'}
  {$LINK 'pj_open_lib.obj'}
  {$LINK 'pj_param.obj'}
  {$LINK 'pj_phi2.obj'}
  {$LINK 'pj_pr_list.obj'}
  {$LINK 'pj_qsfn.obj'}
  {$LINK 'pj_release.obj'}
  {$LINK 'pj_strerrno.obj'}
  {$LINK 'pj_transform.obj'}
  {$LINK 'pj_tsfn.obj'}
  {$LINK 'pj_units.obj'}
  {$LINK 'pj_utils.obj'}
  {$LINK 'pj_zpoly1.obj'}
  {$LINK 'proj_mdist.obj'}
  {$LINK 'proj_rouss.obj'}
  {$LINK 'rtodms.obj'}
  {$LINK 'vector1.obj'}
  {$LINK 'pj_strtod.obj'}

{$ENDREGION}

	{$REGION 'Api calls'}

  function _pj_init_plus(const def: PAnsiChar): Pointer; cdecl; external name '_pj_init_plus';
  function _pj_is_latlong(p: Pointer): integer; cdecl; external name '_pj_is_latlong';
  function _pj_is_geocent(p: Pointer): integer; cdecl; external name '_pj_is_geocent';
  function _pj_latlong_from_proj(p: Pointer): Pointer; cdecl; external name '_pj_latlong_from_proj';
  procedure _pj_free(p: Pointer); cdecl; external name '_pj_free';
  function _pj_transform(src,dst: Pointer; point_count,point_offset: integer; x,y,z: PDouble): Integer; cdecl; external name '_pj_transform';
  function _pj_get_def(p: Pointer): Pointer; cdecl; external name '_pj_get_def';
  function _pj_strerrno(errno: Integer): Pointer; cdecl; external name '_pj_strerrno';
  function _pj_get_ctx(p: Pointer): Pointer; cdecl; external name '_pj_get_ctx';
  procedure _pj_ctx_set_logger(ctx: pointer; logger: Pointer); cdecl; external name '_pj_ctx_set_logger';
  function _pj_get_release(): Pointer; cdecl; external name '_pj_get_release';
  function _pj_ctx_fgets(ctx: pointer; line: PByte; Size: Integer; _file: PInteger): PByte; cdecl; external;
  procedure _pj_log(ctx: pointer; level: Integer; fmt: PAnsiChar); cdecl; external name '_pj_log'; varargs;
	procedure _pj_get_spheroid_defn(p: pointer; major_axis: PDouble; eccentricity_squared: PDouble); cdecl; external name '_pj_get_spheroid_defn';
	function _pj_get_errno_ref(): PInteger; cdecl; external name '_pj_get_errno_ref';

	function _proj_list_operations(): Pointer; cdecl; external name '_proj_list_operations';
{$ENDREGION 'Api calls'}

//------------------------------------------------
// helper functions
function CStringPointerToString(Value: Pointer): string;
begin
	Result := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(Value));
end;

function StringToCStringPointer(Value: string): Pointer;
begin
	Result := TMarshal.AllocStringAsAnsi(Value).ToPointer;
end;

//--------------------------------------


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

		//OutputDebugString( PChar( MsgText ));
	end;
end;

function PJ_init_plus(const def: string): Pointer;
var
	ctx: Pointer;
begin
	Result := _pj_init_plus( StringToCStringPointer( def ) );
 {$IfOpt D+}
	if Result <> nil then
	begin
		ctx := _pj_get_ctx(Result);
		if ctx <> nil then
			_pj_ctx_set_logger(ctx,@_default_stderr_log_handler);
	end;
 {$ifend}
end;

function PJ_is_geographic(p: Pointer): Boolean;
begin
	Result := Boolean(_pj_is_latlong(p));
end;

function PJ_is_geocentric(p: Pointer): Boolean;
begin
	Result := Boolean(_pj_is_geocent(p));
end;

function PJ_latlong_from_proj(p: Pointer): Pointer;
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

function PJ_get_spheroid_defn(p: pointer; out major_axis, eccentricity_squared: Double): Boolean;
begin
	Result := Assigned(p);
	if Result then
		_pj_get_spheroid_defn(p,@major_axis,@eccentricity_squared);
end;

function PJ_get_errno(): Integer;
begin
	Result := _pj_get_errno_ref()^;
end;

function PJ_transform_points2D(src,dst: Pointer; x,y: PDouble; count: Integer; angular: Boolean): Integer;
var
	degSrc, degDst: Boolean;
	i: Integer;
	_x,_y: PDouble;
begin

	Result := PJ_is_same_definition(src,dst);

	if Result = 0 then
		Exit;

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
			_x^ := PJ_DEG_TO_RAD * _x^;
			_y^ := PJ_DEG_TO_RAD * _y^;

			Inc(_x);
			Inc(_y);
		end;
	end;

	Result := _pj_transform(src,dst,count,1,x,y,PtrToNil);

	if Result = 0 then
	begin
		if degDst then
		begin
			_x := x;
			_y := y;
			for i := 0 to count -1 do
			begin
				_x^ := PJ_RAD_TO_DEG * _x^;
				_y^ := PJ_RAD_TO_DEG * _y^;

				Inc(_x);
				Inc(_y);
			end;
		end;
	end;
end;

function PJ_transform_point2D(src,dst: Pointer;
	x,y: PDouble; angular: Boolean): Integer;
begin
	Result := PJ_transform_points2D(src,dst,x,y,1,angular);
end;

function PJ_transform_point2D(src,dst: Pointer; var x,y: Double; angular: Boolean): Integer;
begin
	Result := PJ_transform_point2D(src,dst,@x,@y,angular);
end;

function PJ_get_definition(p: Pointer): string;
begin
	Result := CStringPointerToString(_pj_get_def(p));
end;

function PJ_is_same_definition(p1,p2: Pointer): Integer;
var d1,d2: string;
begin
	Result := -1;
	if (p1 <> nil) and (p2 <> nil) then
	begin
		d1 := PJ_get_definition(p1);
		d2 := PJ_get_definition(p2);
		if (d1 <> '') and (d2 <> '') and SameText(d1,d2) then
			Result := 0
		else
			Result := 1;
	end;
end;

procedure PJ_free(p: Pointer);
begin
	_pj_free(p);
end;

end.




