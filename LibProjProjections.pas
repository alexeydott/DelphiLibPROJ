/// <summary>
///   Projections definition
/// </summary>
unit LibProjProjections;

interface

uses
	Classes, SysUtils, Contnrs,

	LibProjApi
	;

function LibProjSupportedProjections(): TStrings;
function LibProjDefnFromEpsgCode(const Code: Integer): string;

implementation

function LibProjDefnFromEpsgCode(const Code: Integer): string;
const
	gk_tpl = '+proj=tmerc +lat_0=0 +lon_0=%d +k=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs';
	utm_tpl = '+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
var
	GKZoneOffset: Integer;
begin
	case Code of
    // Sphere Mercator ESRI:53004
		53004: Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';
    // Popular Visualisation CRS / Mercator
		3785: Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
		900913: Result := '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs';
		// WGS 84 / World Mercator
		3395: Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
		// NAD83
		4269: Result := '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs';
		// WGS 84
		4326: Result := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
		// Pulkovo 1995
		2463..2491:
		begin
			GKZoneOffset := 21 + (Code - 2463) * 6;
			if GKZoneOffset > 180 then
				GKZoneOffset := GKZoneOffset - 360; // normalized always

			Result := Format(gk_tpl,[GKZoneOffset, 500000, 0]);
		end;
		// Pulkovo 1942
		2492..2522:
		begin
			GKZoneOffset := 9 + (Code - 2492) * 6;
			if GKZoneOffset > 180 then
				GKZoneOffset := GKZoneOffset - 360; // normalized always

			Result := Format(gk_tpl,[GKZoneOffset, 500000, 0]);
		end;
    // UTM
		32601..32660: Result := Format(utm_tpl, [Code - 32600]);
	else
    Result := '';
  end;
end;


var
	FSupportedProjections: TStrings;
//http://cfconventions.org/wkt-proj-4.html
function LibProjSupportedProjections(): TStrings;
begin
	if FSupportedProjections = nil then
	begin
		FSupportedProjections := TStringList.Create;

		FSupportedProjections.Text :=
			'aea=Albers Equal Area'+sLineBreak+
			'aeqd=Azimuthal Equidistant'+sLineBreak+
			'airy=Airy'+sLineBreak+
			'aitoff=Aitoff'+sLineBreak+
			'alsk=Mod. Stererographics of Alaska'+sLineBreak+
			'apian=Apian Globular I'+sLineBreak+
			'august=August Epicycloidal'+sLineBreak+
			'axisswap=Axis ordering'+sLineBreak+
			'bacon=Bacon Globular'+sLineBreak+
			'bipc=Bipolar conic of western hemisphere'+sLineBreak+
			'boggs=Boggs Eumorphic'+sLineBreak+
			'bonne=Bonne (Werner lat_1=90)'+sLineBreak+
			'calcofi=Cal Coop Ocean Fish Invest Lines/Stations'+sLineBreak+
			'cart,    "Geodetic/cartesian conversions'+sLineBreak+
			'cass=Cassini'+sLineBreak+
			'cc=Central Cylindrical'+sLineBreak+
			'ccon=Central Conic'+sLineBreak+
			'cea=Equal Area Cylindrical'+sLineBreak+
			'chamb=Chamberlin Trimetric'+sLineBreak+
			'collg=Collignon'+sLineBreak+
			'comill=Compact Miller'+sLineBreak+
			'crast=Craster Parabolic (Putnins P4)'+sLineBreak+
			'deformation=Kinematic grid shift'+sLineBreak+
			'denoy=Denoyer Semi-Elliptical'+sLineBreak+
			'eck1=Eckert I'+sLineBreak+
			'eck2=Eckert II'+sLineBreak+
			'eck3=Eckert III'+sLineBreak+
			'eck4=Eckert IV'+sLineBreak+
			'eck5=Eckert V'+sLineBreak+
			'eck6=Eckert VI'+sLineBreak+
			'eqc=Equidistant Cylindrical (Plate Caree)'+sLineBreak+
			'eqdc=Equidistant Conic'+sLineBreak+
			'euler=Euler'+sLineBreak+
			'etmerc=Extended Transverse Mercator'+sLineBreak+
			'fahey=Fahey'+sLineBreak+
			'fouc=Foucaut'+sLineBreak+
			'fouc_s=Foucaut Sinusoidal'+sLineBreak+
			'gall=Gall (Gall Stereographic)'+sLineBreak+
			'geoc=Geocentric Latitude'+sLineBreak+
			'geocent=Geocentric'+sLineBreak+
			'geos=Geostationary Satellite View'+sLineBreak+
			'gins8=Ginsburg VIII (TsNIIGAiK)'+sLineBreak+
			'gn_sinu=General Sinusoidal Series'+sLineBreak+
			'gnom=Gnomonic'+sLineBreak+
			'goode=Goode Homolosine'+sLineBreak+
			'gs48=Mod. Stererographics of 48 U.S.'+sLineBreak+
			'gs50=Mod. Stererographics of 50 U.S.'+sLineBreak+
			'hammer=Hammer & Eckert-Greifendorff'+sLineBreak+
			'hatano=Hatano Asymmetrical Equal Area'+sLineBreak+
			'healpix=HEALPix'+sLineBreak+
			'rhealpix=rHEALPix'+sLineBreak+
			'helmert,   "3- and 7-parameter Helmert shift'+sLineBreak+
			'hgridshift=Horizontal grid shift'+sLineBreak+
			'horner,    "Horner polynomial evaluation'+sLineBreak+
			'igh,  "Interrupted Goode Homolosine'+sLineBreak+
			'imw_p=International Map of the World Polyconic'+sLineBreak+
			'isea=Icosahedral Snyder Equal Area'+sLineBreak+
			'kav5=Kavraisky V'+sLineBreak+
			'kav7=Kavraisky VII'+sLineBreak+
			'krovak=Krovak'+sLineBreak+
			'labrd=Laborde'+sLineBreak+
			'laea=Lambert Azimuthal Equal Area'+sLineBreak+
			'lagrng=Lagrange'+sLineBreak+
			'larr=Larrivee'+sLineBreak+
			'lask=Laskowski'+sLineBreak+
			'lonlat=Lat/long (Geodetic)'+sLineBreak+
			'latlon=Lat/long (Geodetic alias)'+sLineBreak+
			'latlong=Lat/long (Geodetic alias)'+sLineBreak+
			'longlat=Lat/long (Geodetic alias)'+sLineBreak+
			'lcc=Lambert Conformal Conic'+sLineBreak+
			'lcca=Lambert Conformal Conic Alternative'+sLineBreak+
			'leac=Lambert Equal Area Conic'+sLineBreak+
			'lee_os=Lee Oblated Stereographic'+sLineBreak+
			'loxim=Loximuthal'+sLineBreak+
			'lsat=Space oblique for LANDSAT'+sLineBreak+
			'mbt_s=McBryde-Thomas Flat-Polar Sine'+sLineBreak+
			'mbt_fps=McBryde-Thomas Flat-Pole Sine (No. 2)'+sLineBreak+
			'mbtfpp=McBride-Thomas Flat-Polar Parabolic'+sLineBreak+
			'mbtfpq=McBryde-Thomas Flat-Polar Quartic'+sLineBreak+
			'mbtfps=McBryde-Thomas Flat-Polar Sinusoidal'+sLineBreak+
			'merc=Mercator'+sLineBreak+
			'mil_os=Miller Oblated Stereographic'+sLineBreak+
			'mill=Miller Cylindrical'+sLineBreak+
			'misrsom=Space oblique for MISR'+sLineBreak+
			'moll=Mollweide'+sLineBreak+
			'molodensky=Molodensky transform'+sLineBreak+
			'murd1=Murdoch I'+sLineBreak+
			'murd2=Murdoch II'+sLineBreak+
			'murd3=Murdoch III'+sLineBreak+
			'natearth=Natural Earth'+sLineBreak+
			'natearth2=Natural Earth II'+sLineBreak+
			'nell=Nell'+sLineBreak+
			'nell_h=Nell-Hammer'+sLineBreak+
			'nicol=Nicolosi Globular'+sLineBreak+
			'nsper=Near-sided perspective'+sLineBreak+
			'nzmg=New Zealand Map Grid'+sLineBreak+
			'ob_tran=General Oblique Transformation'+sLineBreak+
			'ocea=Oblique Cylindrical Equal Area'+sLineBreak+
			'oea=Oblated Equal Area'+sLineBreak+
			'omerc=Oblique Mercator'+sLineBreak+
			'ortel=Ortelius Oval'+sLineBreak+
			'ortho=Orthographic'+sLineBreak+
			'pconic=Perspective Conic'+sLineBreak+
			'patterson=Patterson Cylindrical'+sLineBreak+
			'pipeline=Transformation pipeline manager'+sLineBreak+
			'poly=Polyconic (American)'+sLineBreak+
			'putp1=Putnins P1'+sLineBreak+
			'putp2=Putnins P2'+sLineBreak+
			'putp3=Putnins P3'+sLineBreak+
			'putp3p=Putnins P3'''+sLineBreak+
			'putp4p=Putnins P4'+sLineBreak+
			'putp5=Putnins P5'+sLineBreak+
			'putp5p=Putnins P5'''+sLineBreak+
			'putp6=Putnins P6'+sLineBreak+
			'putp6p=Putnins P6'+sLineBreak+
			'qua_aut=Quartic Authalic'+sLineBreak+
			'qsc=Quadrilateralized Spherical Cube'+sLineBreak+
			'robin=Robinson'+sLineBreak+
			'rouss=Roussilhe Stereographic'+sLineBreak+
			'rpoly=Rectangular Polyconic'+sLineBreak+
			'sch=Spherical Cross-track Height'+sLineBreak+
			'sinu=Sinusoidal (Sanson-Flamsteed)'+sLineBreak+
			'somerc=Swiss. Obl. Mercator'+sLineBreak+
			'stere=Stereographic'+sLineBreak+
			'sterea=Oblique Stereographic Alternative'+sLineBreak+
			'gstmerc=Gauss-Schreiber Transverse Mercator (aka Gauss-Laborde Reunion)'+sLineBreak+
			'tcc=Transverse Central Cylindrical'+sLineBreak+
			'tcea=Transverse Cylindrical Equal Area'+sLineBreak+
			'times=Times Projection'+sLineBreak+
			'tissot=Tissot Conic'+sLineBreak+
			'tmerc=Transverse Mercator'+sLineBreak+
			'tpeqd=Two Point Equidistant'+sLineBreak+
			'tpers=Tilted perspective'+sLineBreak+
			'unitconvert=Unit conversion'+sLineBreak+
			'ups=Universal Polar Stereographic'+sLineBreak+
			'urm5=Urmaev V'+sLineBreak+
			'urmfps=Urmaev Flat-Polar Sinusoidal'+sLineBreak+
			'utm=Universal Transverse Mercator (UTM)'+sLineBreak+
			'vandg=van der Grinten (I)'+sLineBreak+
			'vandg2=van der Grinten II'+sLineBreak+
			'vandg3=van der Grinten III'+sLineBreak+
			'vandg4=van der Grinten IV'+sLineBreak+
			'vitk1=Vitkovsky I'+sLineBreak+
			'vgridshift=Vertical grid shift'+sLineBreak+
			'wag1=Wagner I (Kavraisky VI)'+sLineBreak+
			'wag2=Wagner II'+sLineBreak+
			'wag3=Wagner III'+sLineBreak+
			'wag4=Wagner IV'+sLineBreak+
			'wag5=Wagner V'+sLineBreak+
			'wag6=Wagner VI'+sLineBreak+
			'wag7=Wagner VII'+sLineBreak+
			'webmerc=Web Mercator / Pseudo Mercator'+sLineBreak+
			'weren=Werenskiold I'+sLineBreak+
			'wink1=Winkel I'+sLineBreak+
			'wink2=Winkel II'+sLineBreak+
			'wintri=Winkel Tripel';
	end;

	Result := FSupportedProjections;
end;

end.
