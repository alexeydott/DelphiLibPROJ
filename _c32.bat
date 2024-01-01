@ECHO OFF

SET SRCROOT=%ROOTDIR%c_src
SET OBJPATH=%ROOTDIR%c_src

SET PLATFORM=_win32

DEL /Q %OBJPATH%\*.obj

SET SYSINCLUDE=%BCCROOT%\include;%BCCROOT%\include\dinkumware;%BCCROOT%\include\windows\crtl;%BCCROOT%\include\windows\sdk;%BCCROOT%\include\windows\rtl;
SET INCLUDE=%SYSINCLUDE%%SRCROOT%

SET DEFINES=-DNDEBUG -DUSEPACKAGES -DMUTEX_win32 
SET BCC=%BCCROOT%\bin\bcc32.exe -q -y -c -tM -tW -C8 -w- -O2 -v- -vi %DEFINES% -I%INCLUDE% -n%OBJPATH%

if /i "DEBUG"=="%build_type%"  (
  SET DEFINES=-D_DEBUG -DUSEPACKAGES -DMUTEX_win32 -DHAVE_STR_ERROR=1
  SET BCC=%BCCROOT%\bin\bcc32.exe -n%OBJPATH% %DEFINES% -I%INCLUDE% -q -y -k -r- -c -tM -tW -C8 -w- -Od -v -vi- -wpro  
)

::support
%BCC% %SRCROOT%\aasincos.c
%BCC% %SRCROOT%\adjlon.c
%BCC% %SRCROOT%\bch2bps.c
%BCC% %SRCROOT%\bchgen.c
%BCC% %SRCROOT%\pj_gauss.c
%BCC% %SRCROOT%\biveval.c
%BCC% %SRCROOT%\dmstor.c
%BCC% %SRCROOT%\mk_cheby.c
%BCC% %SRCROOT%\pj_auth.c
%BCC% %SRCROOT%\pj_deriv.c
%BCC% %SRCROOT%\pj_ell_set.c
%BCC% %SRCROOT%\pj_ellps.c
%BCC% %SRCROOT%\pj_errno.c
%BCC% %SRCROOT%\pj_factors.c
%BCC% %SRCROOT%\pj_fwd.c
%BCC% %SRCROOT%\pj_init.c
%BCC% %SRCROOT%\pj_inv.c
%BCC% %SRCROOT%\pj_list.c
%BCC% %SRCROOT%\pj_malloc.c
%BCC% %SRCROOT%\pj_mlfn.c
%BCC% %SRCROOT%\pj_msfn.c
%BCC% %SRCROOT%\pj_open_lib.c
%BCC% %SRCROOT%\pj_param.c
%BCC% %SRCROOT%\pj_phi2.c
%BCC% %SRCROOT%\pj_pr_list.c
%BCC% %SRCROOT%\pj_qsfn.c
%BCC% %SRCROOT%\pj_strerrno.c
%BCC% %SRCROOT%\pj_tsfn.c
%BCC% %SRCROOT%\pj_units.c
%BCC% %SRCROOT%\pj_zpoly1.c
%BCC% %SRCROOT%\rtodms.c
%BCC% %SRCROOT%\vector1.c
%BCC% %SRCROOT%\pj_release.c
%BCC% %SRCROOT%\geocent.c
%BCC% %SRCROOT%\pj_transform.c
%BCC% %SRCROOT%\pj_datum_set.c
%BCC% %SRCROOT%\pj_datums.c
%BCC% %SRCROOT%\pj_apply_gridshift.c
%BCC% %SRCROOT%\pj_gc_reader.c
%BCC% %SRCROOT%\pj_gridcatalog.c
%BCC% %SRCROOT%\nad_cvt.c
%BCC% %SRCROOT%\nad_init.c
%BCC% %SRCROOT%\nad_intr.c
%BCC% %SRCROOT%\pj_utils.c
%BCC% %SRCROOT%\pj_gridlist.c
%BCC% %SRCROOT%\pj_gridinfo.c
%BCC% %SRCROOT%\proj_mdist.c
%BCC% %SRCROOT%\pj_mutex.c
%BCC% %SRCROOT%\pj_initcache.c
%BCC% %SRCROOT%\pj_ctx.c
%BCC% %SRCROOT%\pj_fileapi.c
%BCC% %SRCROOT%\pj_log.c
%BCC% %SRCROOT%\pj_apply_vgridshift.c
%BCC% %SRCROOT%\pj_strtod.c
%BCC% %SRCROOT%\pj_internal.c
%BCC% %SRCROOT%\pj_math.c
::pseudo
%BCC% %SRCROOT%\PJ_boggs.c
%BCC% %SRCROOT%\PJ_collg.c
%BCC% %SRCROOT%\PJ_crast.c
%BCC% %SRCROOT%\PJ_denoy.c
%BCC% %SRCROOT%\PJ_eck1.c
%BCC% %SRCROOT%\PJ_eck2.c
%BCC% %SRCROOT%\PJ_eck3.c
%BCC% %SRCROOT%\PJ_eck4.c
%BCC% %SRCROOT%\PJ_eck5.c
%BCC% %SRCROOT%\PJ_fahey.c
%BCC% %SRCROOT%\PJ_fouc_s.c
%BCC% %SRCROOT%\PJ_gins8.c
%BCC% %SRCROOT%\PJ_gn_sinu.c
%BCC% %SRCROOT%\PJ_goode.c
%BCC% %SRCROOT%\PJ_igh.c
%BCC% %SRCROOT%\PJ_hatano.c
%BCC% %SRCROOT%\PJ_loxim.c
%BCC% %SRCROOT%\PJ_mbt_fps.c
%BCC% %SRCROOT%\PJ_mbtfpp.c
%BCC% %SRCROOT%\PJ_mbtfpq.c
%BCC% %SRCROOT%\PJ_moll.c
%BCC% %SRCROOT%\PJ_nell.c
%BCC% %SRCROOT%\PJ_nell_h.c
%BCC% %SRCROOT%\PJ_putp2.c
%BCC% %SRCROOT%\PJ_putp3.c
%BCC% %SRCROOT%\PJ_putp4p.c
%BCC% %SRCROOT%\PJ_putp5.c
%BCC% %SRCROOT%\PJ_putp6.c
%BCC% %SRCROOT%\PJ_robin.c
%BCC% %SRCROOT%\PJ_sts.c
%BCC% %SRCROOT%\PJ_urm5.c
%BCC% %SRCROOT%\PJ_urmfps.c
%BCC% %SRCROOT%\PJ_wag2.c
%BCC% %SRCROOT%\PJ_wag3.c
%BCC% %SRCROOT%\PJ_wink1.c
%BCC% %SRCROOT%\PJ_wink2.c
%BCC% %SRCROOT%\PJ_isea.c
%BCC% %SRCROOT%\PJ_calcofi.c
%BCC% %SRCROOT%\PJ_natearth.c
%BCC% %SRCROOT%\PJ_natearth2.c
%BCC% %SRCROOT%\PJ_times.c
%BCC% %SRCROOT%\PJ_eqearth.c
::azimuthal
%BCC% %SRCROOT%\PJ_aeqd.c
%BCC% %SRCROOT%\PJ_gnom.c
%BCC% %SRCROOT%\PJ_laea.c
%BCC% %SRCROOT%\PJ_mod_ster.c
%BCC% %SRCROOT%\PJ_nsper.c
%BCC% %SRCROOT%\PJ_nzmg.c
%BCC% %SRCROOT%\PJ_ortho.c
%BCC% %SRCROOT%\PJ_stere.c
%BCC% %SRCROOT%\PJ_sterea.c
%BCC% %SRCROOT%\proj_rouss.c
:: conic
%BCC% %SRCROOT%\PJ_aea.c
%BCC% %SRCROOT%\PJ_bipc.c
%BCC% %SRCROOT%\PJ_bonne.c
%BCC% %SRCROOT%\PJ_eqdc.c
%BCC% %SRCROOT%\PJ_imw_p.c
%BCC% %SRCROOT%\PJ_lcc.c
%BCC% %SRCROOT%\PJ_poly.c
%BCC% %SRCROOT%\PJ_rpoly.c
%BCC% %SRCROOT%\PJ_sconics.c
%BCC% %SRCROOT%\PJ_lcca.c
%BCC% %SRCROOT%\PJ_ccon.c
:: cylinder
%BCC% %SRCROOT%\PJ_cass.c
%BCC% %SRCROOT%\PJ_cc.c
%BCC% %SRCROOT%\PJ_cea.c
%BCC% %SRCROOT%\PJ_eqc.c
%BCC% %SRCROOT%\PJ_gall.c
%BCC% %SRCROOT%\PJ_labrd.c
%BCC% %SRCROOT%\PJ_lsat.c
%BCC% %SRCROOT%\PJ_misrsom.c
%BCC% %SRCROOT%\PJ_merc.c
%BCC% %SRCROOT%\PJ_mill.c
%BCC% %SRCROOT%\PJ_ocea.c
%BCC% %SRCROOT%\PJ_omerc.c
%BCC% %SRCROOT%\PJ_patterson.c
%BCC% %SRCROOT%\PJ_somerc.c
%BCC% %SRCROOT%\PJ_tcc.c
%BCC% %SRCROOT%\PJ_tcea.c
%BCC% %SRCROOT%\PJ_tmerc.c
%BCC% %SRCROOT%\PJ_geos.c
%BCC% %SRCROOT%\PJ_gstmerc.c
%BCC% %SRCROOT%\proj_etmerc.c
%BCC% %SRCROOT%\PJ_comill.c
:: misc
%BCC% %SRCROOT%\PJ_airy.c
%BCC% %SRCROOT%\PJ_aitoff.c
%BCC% %SRCROOT%\PJ_august.c
%BCC% %SRCROOT%\PJ_bacon.c
%BCC% %SRCROOT%\PJ_chamb.c
%BCC% %SRCROOT%\PJ_hammer.c
%BCC% %SRCROOT%\PJ_lagrng.c
%BCC% %SRCROOT%\PJ_larr.c
%BCC% %SRCROOT%\PJ_lask.c
%BCC% %SRCROOT%\PJ_nocol.c
%BCC% %SRCROOT%\PJ_ob_tran.c
%BCC% %SRCROOT%\PJ_oea.c
%BCC% %SRCROOT%\PJ_sch.c
%BCC% %SRCROOT%\PJ_tpeqd.c
%BCC% %SRCROOT%\PJ_vandg.c
%BCC% %SRCROOT%\PJ_vandg2.c
%BCC% %SRCROOT%\PJ_vandg4.c
%BCC% %SRCROOT%\PJ_wag7.c
%BCC% %SRCROOT%\PJ_latlong.c
%BCC% %SRCROOT%\PJ_krovak.c
%BCC% %SRCROOT%\PJ_geoc.c
%BCC% %SRCROOT%\pj_geocent.c
%BCC% %SRCROOT%\PJ_healpix.c
%BCC% %SRCROOT%\PJ_qsc.c
::geodesic
%BCC% %SRCROOT%\geodesic.c
::pipeline
%BCC% %SRCROOT%\proj_4D_api.c
%BCC% %SRCROOT%\PJ_cart.c
%BCC% %SRCROOT%\PJ_pipeline.c
%BCC% %SRCROOT%\PJ_horner.c
%BCC% %SRCROOT%\PJ_helmert.c
%BCC% %SRCROOT%\PJ_vgridshift.c
%BCC% %SRCROOT%\PJ_hgridshift.c
%BCC% %SRCROOT%\PJ_unitconvert.c
%BCC% %SRCROOT%\PJ_molodensky.c
%BCC% %SRCROOT%\PJ_deformation.c
%BCC% %SRCROOT%\PJ_axisswap.c

copy %SRCROOT%\*.obj %ROOTDIR%