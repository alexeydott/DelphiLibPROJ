{$IFDEF MSWINDOWS}
  {$define LIBPROJ_LINKSTATIC} // link precompiled object files, currently windows only///
  {$define __proj_api_deepdive} // use deep dive in PJ* types
  {$define __proj_include_full_epsgdb} // include full epsg database
  {.$define __proj_ru_explain} // russian translate for parameters explain
  {.$define __proj_db_convert}
{$ENDIF}
