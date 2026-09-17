/// <summary>
/// Projections definition
//https://cfconventions.org/wkt-proj-4.html
/// </summary>
unit libProj4.Projections;

interface
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
{$IFDEF USER_DEFINES_INC}{$I user_defines.inc}{$ENDIF}
{$ifdef __proj4mappl}{$I ..\mappl_config.inc}{$endif}
uses
	System.Types, System.Classes, System.SysUtils, System.Math,
  libProj4.Types;

type
	ENotSupportedProjection = class(ENotSupportedException)end;

type
  TProjectionInfo = record
    Key: string;
    Name: string;
    Desc: string;
    Latmax,Latmin,Lonmax,Lonmin: Single;
    Defn: string;
    Hidden: Boolean;
  end;


  /// <summary>Parsed MapInfo CoordSys definition.</summary>
  TMapInfoCoordSys = record
    IsEarth: Boolean;
    ProjectionID: Integer;
    DatumID: Integer;
    EllipsoidID: Integer;
    UnitsName: string;
    /// <summary>
    /// Custom datum values. ID 999: dx,dy,dz. ID 9999:
    /// dx,dy,dz,rx,ry,rz,scale,prime-meridian.
    /// </summary>
    DatumParameters: TArray<Double>;
    /// <summary>Projection parameters in native MapInfo order.</summary>
    ProjectionParameters: TArray<Double>;
    HasBounds: Boolean;
    BoundsMinX: Double;
    BoundsMinY: Double;
    BoundsMaxX: Double;
    BoundsMaxY: Double;
  end;

const
  cSupportedProjectionsList: array[0..136] of TProjectionInfo = (
    (Key: 'aea'; Name: 'Albers Equal Area'; Desc: 'Коническая равновеликая Альберса'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5'; Hidden: False),
    (Key: 'aeqd'; Name: 'Azimuthal Equidistant'; Desc: 'Азимутальная равнопромежуточная'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=aeqd +ellps=WGS84'; Hidden: False),
    (Key: 'airy'; Name: 'Airy'; Desc: 'Эйри'; Latmax: 90; Latmin: -90; Lonmax: 90; Lonmin: -90; Defn: '+proj=airy +ellps=WGS84'; Hidden: True),
    (Key: 'aitoff'; Name: 'Aitoff'; Desc: 'Азимутальная Аитова'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=aitoff +ellps=WGS84'; Hidden: False),
    (Key: 'alsk'; Name: 'Mod. Stererographics of Alaska'; Desc: 'Стереографическая модифицированная (Аляска)'; Latmax: 75; Latmin: 50; Lonmax: -130; Lonmin: -175; Defn: '+proj=alsk +ellps=WGS84'; Hidden: True),
    (Key: 'apian'; Name: 'Apian Globular I'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=apian +ellps=WGS84'; Hidden: False),
    (Key: 'august'; Name: 'August Epicycloidal'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=august +ellps=WGS84'; Hidden: False),
    (Key: 'bacon'; Name: 'Bacon Globular'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=bacon +ellps=WGS84'; Hidden: False),
    (Key: 'bipc'; Name: 'Bipolar conic of western hemisphere'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=bipc +ns +ellps=WGS84'; Hidden: True),
    (Key: 'boggs'; Name: 'Boggs Eumorphic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=boggs +ellps=WGS84'; Hidden: False),
    (Key: 'bonne'; Name: 'Bonne (Werner lat_1=90)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=bonne +lat_1=10 +ellps=WGS84'; Hidden: False),
    (Key: 'cass'; Name: 'Cassini'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=cass +ellps=WGS84'; Hidden: False),
    (Key: 'cc'; Name: 'Central Cylindrical'; Desc: ''; Latmax: 75; Latmin: -75; Lonmax: 180; Lonmin: -180; Defn: '+proj=cc +ellps=WGS84'; Hidden: True),
    (Key: 'ccon'; Name: 'Central Conic'; Desc: ''; Latmax: 70; Latmin: 34; Lonmax: -30; Lonmin: 68; Defn: '+proj=ccon +lat_1=52 +lon_0=19 +ellps=WGS84'; Hidden: True),
    (Key: 'cea'; Name: 'Equal Area Cylindrical'; Desc: 'Цилиндрическая равновеликая Бермана'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=cea +ellps=WGS84'; Hidden: False),
    (Key: 'chamb'; Name: 'Chamberlin Trimetric'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=chamb +lat_1=10 +lon_1=30 +lon_2=40 +ellps=WGS84'; Hidden: False),
    (Key: 'collg'; Name: 'Collignon'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=collg +ellps=WGS84'; Hidden: False),
    (Key: 'comill'; Name: 'Compact Miller'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=comill +ellps=WGS84'; Hidden: False),
    (Key: 'crast'; Name: 'Craster Parabolic (Putnins P4)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=crast +ellps=WGS84'; Hidden: False),
    (Key: 'denoy'; Name: 'Denoyer Semi-Elliptical'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=denoy +ellps=WGS84'; Hidden: False),
    (Key: 'eck1'; Name: 'Eckert I'; Desc: 'Псевдоциллиндрическая Эккерта I'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck1 +ellps=WGS84'; Hidden: False),
    (Key: 'eck2'; Name: 'Eckert II'; Desc: 'Псевдоциллиндрическая Эккерта II'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck2 +ellps=WGS84'; Hidden: False),
    (Key: 'eck3'; Name: 'Eckert III'; Desc: 'Псевдоциллиндрическая Эккерта III'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck3 +ellps=WGS84'; Hidden: False),
    (Key: 'eck4'; Name: 'Eckert IV'; Desc: 'Псевдоциллиндрическая Эккерта IV'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck4 +ellps=WGS84'; Hidden: False),
    (Key: 'eck5'; Name: 'Eckert V'; Desc: 'Псевдоциллиндрическая Эккерта V'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck5 +ellps=WGS84'; Hidden: False),
    (Key: 'eck6'; Name: 'Eckert VI'; Desc: 'Псевдоциллиндрическая Эккерта VI'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eck6 +ellps=WGS84'; Hidden: False),
    (Key: 'eqc'; Name: 'Equidistant Cylindrical (Plate Caree)'; Desc: 'Циллиндрическая равнопромежуточная'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eqc +ellps=WGS84+over'; Hidden: False),
    (Key: 'eqdc'; Name: 'Equidistant Conic'; Desc: 'Коническая равнопромежуточная'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eqdc +lat_1=55 +lat_2=60 +ellps=WGS84'; Hidden: False),
    (Key: 'eqearth'; Name: 'Equal Earth'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=eqearth +ellps=WGS84'; Hidden: False),
    (Key: 'euler'; Name: 'Euler'; Desc: 'Сферическая Эйлера'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=euler +lat_1=67 +lat_2=75 +ellps=WGS84'; Hidden: False),
    (Key: 'etmerc'; Name: 'Extended Transverse Mercator'; Desc: 'Поперечная Меркатора (расширенная)'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=etmerc +lon_0=-40 +ellps=WGS84'; Hidden: True),
    (Key: 'fahey'; Name: 'Fahey'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=fahey +ellps=WGS84'; Hidden: False),
    (Key: 'fouc'; Name: 'Foucaut'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=fouc +ellps=WGS84'; Hidden: False),
    (Key: 'fouc_s'; Name: 'Foucaut Sinusoidal'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=fouc_s +ellps=WGS84'; Hidden: False),
    (Key: 'gall'; Name: 'Gall (Gall Stereographic)'; Desc: 'Циллиндрическая стереографическая Голла'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=gall +ellps=WGS84'; Hidden: False),
    (Key: 'geos'; Name: 'Geostationary Satellite View'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 15; Lonmin: -135; Defn: '+proj=geos +h=35785831.0 +lon_0=-60 +sweep=y +ellps=WGS84'; Hidden: True),
    (Key: 'gins8'; Name: 'Ginsburg VIII (TsNIIGAiK)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=gins8 +ellps=WGS84'; Hidden: False),
    (Key: 'gn_sinu'; Name: 'General Sinusoidal Series'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=gn_sinu +m=2 +n=3 +ellps=WGS84'; Hidden: False),
    (Key: 'gnom'; Name: 'Gnomonic'; Desc: ''; Latmax: 90; Latmin: 50; Lonmax: 180; Lonmin: -180; Defn: '+proj=gnom +lat_0=90 +lon_0=-50 +ellps=WGS84'; Hidden: True),
    (Key: 'goode'; Name: 'Goode Homolosine'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=goode +ellps=WGS84'; Hidden: False),
    (Key: 'gs48'; Name: 'Mod. Stererographics of 48 U.S.'; Desc: ''; Latmax: 45; Latmin: 25; Lonmax: -70; Lonmin: -125; Defn: '+proj=gs48 +ellps=WGS84'; Hidden: False),
    (Key: 'gs50'; Name: 'Mod. Stererographics of 50 U.S.'; Desc: ''; Latmax: 75; Latmin: 15; Lonmax: -55; Lonmin: -170; Defn: '+proj=gs50 +ellps=WGS84'; Hidden: True),
    (Key: 'hammer'; Name: 'Hammer & Eckert-Greifendorff'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=hammer +ellps=WGS84'; Hidden: True),
    (Key: 'hatano'; Name: 'Hatano Asymmetrical Equal Area'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=hatano +ellps=WGS84'; Hidden: False),
    (Key: 'igh'; Name: 'Interrupted Goode Homolosine'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=igh +ellps=WGS84'; Hidden: False),
    (Key: 'imw_p'; Name: 'International Map of the World Polyconic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=imw_p +lat_1=30 +lat_2=-40 +ellps=WGS84'; Hidden: True),
    (Key: 'isea'; Name: 'Icosahedral Snyder Equal Area'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=isea +ellps=WGS84'; Hidden: True),
    (Key: 'kav5'; Name: 'Kavraisky V'; Desc: 'Каврайского V'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=kav5 +ellps=WGS84'; Hidden: False),
    (Key: 'kav7'; Name: 'Kavraisky VII'; Desc: 'Каврайского VII'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=kav7 +ellps=WGS84'; Hidden: False),
    (Key: 'krovak'; Name: 'Krovak'; Desc: ''; Latmax: 58; Latmin: 42; Lonmax: 25; Lonmin: 5; Defn: '+proj=krovak +ellps=WGS84'; Hidden: True),
    (Key: 'labrd'; Name: 'Laborde'; Desc: ''; Latmax: 0; Latmin: -35; Lonmax: 55; Lonmin: 30; Defn: '+proj=labrd +lon_0=40 +lat_0=-10 +ellps=WGS84'; Hidden: True),
    (Key: 'laea'; Name: 'Lambert Azimuthal Equal Area'; Desc: 'Азимутальная равноплощадная Ламберта'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=laea +ellps=WGS84'; Hidden: False),
    (Key: 'lagrng'; Name: 'Lagrange'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=lagrng +ellps=WGS84 +W=2'; Hidden: False),
    (Key: 'larr'; Name: 'Larrivee'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=larr +ellps=WGS84'; Hidden: False),
    (Key: 'lask'; Name: 'Laskowski'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=lask +ellps=WGS84'; Hidden: False),
    (Key: 'lcc'; Name: 'Lambert Conformal Conic'; Desc: 'Равноугольная коническая Ламберта'; Latmax: 90; Latmin: 10; Lonmax: -10; Lonmin: -160; Defn: '+proj=lcc +lon_0=-90 +ellps=WGS84 +lat_1=33 +lat_2=45'; Hidden: True),
    (Key: 'lcca'; Name: 'Lambert Conformal Conic Alternative'; Desc: 'Равноугольная коническая Ламберта 2'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=lcca +lat_0=35 +ellps=WGS84'; Hidden: False),
    (Key: 'leac'; Name: 'Lambert Equal Area Conic'; Desc: 'Азимутальная равновеликая Ламберта'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=leac +ellps=WGS84'; Hidden: False),
    (Key: 'lee_os'; Name: 'Lee Oblated Stereographic'; Desc: ''; Latmax: 85; Latmin: -85; Lonmax: -80; Lonmin: -180; Defn: '+proj=lee_os +ellps=WGS84'; Hidden: True),
		(Key: 'longlat'; Name: 'Geodetic (Long/Lat)'; Desc: 'Градусы (Широта/Долгота)'; Latmax: 90; Latmin: -90; Lonmax: -180; Lonmin: -180; Defn: '+proj=longlat +ellps=WGS84'; Hidden: False),
		(Key: 'latlong'; Name: 'Geodetic (Lat/Long)'; Desc: 'Градусы (Долгота/Широта)'; Latmax: 90; Latmin: -90; Lonmax: -180; Lonmin: -180; Defn: '+proj=latlong +ellps=WGS84'; Hidden: False),
    (Key: 'loxim'; Name: 'Loximuthal'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=loxim +ellps=WGS84'; Hidden: False),
    (Key: 'lsat'; Name: 'Space oblique for LANDSAT'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=lsat +ellps=GRS80 +lsat=2 +path=2'; Hidden: True),
    (Key: 'mbt_s'; Name: 'McBryde-Thomas Flat-Polar Sine'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mbt_s +ellps=WGS84'; Hidden: False),
    (Key: 'mbt_fps'; Name: 'McBryde-Thomas Flat-Pole Sine (No. 2)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mbt_fps +ellps=WGS84'; Hidden: False),
    (Key: 'mbtfpp'; Name: 'McBride-Thomas Flat-Polar Parabolic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mbtfpp +ellps=WGS84'; Hidden: False),
    (Key: 'mbtfpq'; Name: 'McBryde-Thomas Flat-Polar Quartic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mbtfpq +ellps=WGS84'; Hidden: False),
    (Key: 'mbtfps'; Name: 'McBryde-Thomas Flat-Polar Sinusoidal'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mbtfps +ellps=WGS84'; Hidden: False),
    (Key: 'merc'; Name: 'Mercator'; Desc: ''; Latmax: 85; Latmin: -85; Lonmax: 180; Lonmin: -180; Defn: '+proj=merc +ellps=WGS84'; Hidden: False),
    (Key: 'mil_os'; Name: 'Miller Oblated Stereographic'; Desc: ''; Latmax: 80; Latmin: -40; Lonmax: 80; Lonmin: -40; Defn: '+proj=mil_os +ellps=WGS84'; Hidden: False),
    (Key: 'mill'; Name: 'Miller Cylindrical'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=mill +ellps=WGS84'; Hidden: False),
    (Key: 'misrsom'; Name: 'Space oblique for MISR'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=misrsom +path=1 +ellps=WGS84'; Hidden: False),
    (Key: 'moll'; Name: 'Mollweide'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=moll +ellps=WGS84'; Hidden: False),
    (Key: 'murd1'; Name: 'Murdoch I'; Desc: ''; Latmax: 85; Latmin: -40; Lonmax: 180; Lonmin: -180; Defn: '+proj=murd1 +lat_1=30 +lat_2=50 +ellps=WGS84'; Hidden: False),
    (Key: 'murd2'; Name: 'Murdoch II'; Desc: ''; Latmax: 85; Latmin: -40; Lonmax: 180; Lonmin: -180; Defn: '+proj=murd2 +lat_1=30 +lat_2=50 +ellps=WGS84'; Hidden: False),
    (Key: 'murd3'; Name: 'Murdoch III'; Desc: ''; Latmax: 85; Latmin: -40; Lonmax: 180; Lonmin: -180; Defn: '+proj=murd3 +lat_1=30 +lat_2=50 +ellps=WGS84'; Hidden: False),
    (Key: 'natearth'; Name: 'Natural Earth'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=natearth +ellps=WGS84'; Hidden: False),
    (Key: 'natearth2'; Name: 'Natural Earth II'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=natearth2 +ellps=WGS84'; Hidden: False),
    (Key: 'nell'; Name: 'Nell'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=nell +ellps=WGS84'; Hidden: False),
    (Key: 'nell_h'; Name: 'Nell-Hammer'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=nell_h +ellps=WGS84'; Hidden: False),
    (Key: 'nicol'; Name: 'Nicolosi Globular'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=nicol +ellps=WGS84'; Hidden: False),
    (Key: 'nsper'; Name: 'Near-sided perspective'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=nsper +h=3000000 +lat_0=-20 +lon_0=145 +ellps=WGS84'; Hidden: False),
    (Key: 'nzmg'; Name: 'New Zealand Map Grid'; Desc: ''; Latmax: -32; Latmin: -48; Lonmax: 180; Lonmin: 165; Defn: '+proj=nzmg +ellps=WGS84'; Hidden: True),
    (Key: 'ob_tran'; Name: 'General Oblique Transformation'; Desc: 'Общее косое преобразование'; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=ob_tran +o_proj=mill +o_lon_p=40 +o_lat_p=50 +lon_0=60 +ellps=WGS84'; Hidden: False),
    (Key: 'ocea'; Name: 'Oblique Cylindrical Equal Area'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=ocea +ellps=WGS84'; Hidden: False),
    (Key: 'oea'; Name: 'Oblated Equal Area'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=oea +m=1 +n=2 +ellps=WGS84'; Hidden: False),
    (Key: 'omerc'; Name: 'Oblique Mercator'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=omerc +lat_1=45 +lat_2=55 +ellps=WGS84'; Hidden: False),
    (Key: 'ortel'; Name: 'Ortelius Oval'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=ortel +ellps=WGS84'; Hidden: False),
    (Key: 'ortho'; Name: 'Orthographic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 90; Lonmin: -90; Defn: '+proj=ortho +ellps=WGS84'; Hidden: False),
    (Key: 'pconic'; Name: 'Perspective Conic'; Desc: ''; Latmax: 90; Latmin: 0; Lonmax: 180; Lonmin: -180; Defn: '+proj=pconic +lat_1=25 +lat_2=75 +ellps=WGS84'; Hidden: False),
    (Key: 'patterson'; Name: 'Patterson Cylindrical'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=patterson +ellps=WGS84'; Hidden: False),
    (Key: 'poly'; Name: 'Polyconic (American)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=poly +ellps=WGS84'; Hidden: False),
    (Key: 'putp1'; Name: 'Putnins P1'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp1 +ellps=WGS84'; Hidden: False),
    (Key: 'putp2'; Name: 'Putnins P2'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp2 +ellps=WGS84'; Hidden: False),
    (Key: 'putp3'; Name: 'Putnins P3'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp3 +ellps=WGS84'; Hidden: False),
    (Key: 'putp3p'; Name: 'Putnins P3'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp3p +ellps=WGS84'; Hidden: False),
    (Key: 'putp4p'; Name: 'Putnins P4'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp4p +ellps=WGS84'; Hidden: False),
    (Key: 'putp5'; Name: 'Putnins P5'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp5 +ellps=WGS84'; Hidden: False),
    (Key: 'putp5p'; Name: 'Putnins P5'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp5p +ellps=WGS84'; Hidden: False),
    (Key: 'putp6'; Name: 'Putnins P6'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp6 +ellps=WGS84'; Hidden: False),
    (Key: 'putp6p'; Name: 'Putnins P6'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=putp6p +ellps=WGS84'; Hidden: False),
    (Key: 'qua_aut'; Name: 'Quartic Authalic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=qua_aut +ellps=WGS84'; Hidden: False),
    (Key: 'qsc'; Name: 'Quadrilateralized Spherical Cube'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=qsc +ellps=WGS84'; Hidden: False),
    (Key: 'robin'; Name: 'Robinson'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=robin +ellps=WGS84'; Hidden: False),
    (Key: 'rouss'; Name: 'Roussilhe Stereographic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=rouss +ellps=WGS84'; Hidden: False),
    (Key: 'rpoly'; Name: 'Rectangular Polyconic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=rpoly +ellps=WGS84'; Hidden: False),
    (Key: 'sinu'; Name: 'Sinusoidal (Sanson-Flamsteed)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=sinu +ellps=WGS84'; Hidden: False),
    (Key: 'somerc'; Name: 'Swiss. Obl. Mercator'; Desc: ''; Latmax: 60; Latmin: 30; Lonmax: 30; Lonmin: -15; Defn: '+proj=somerc +ellps=WGS84'; Hidden: False),
    (Key: 'stere'; Name: 'Stereographic'; Desc: ''; Latmax: 90.0; Latmin: 50.0; Lonmax: 180; Lonmin: -180; Defn: '+proj=stere +lat_0=90 +lat_ts=75 +ellps=WGS84'; Hidden: False),
    (Key: 'sterea'; Name: 'Oblique Stereographic Alternative'; Desc: ''; Latmax: 90; Latmin: 50; Lonmax: 180; Lonmin: -180; Defn: '+proj=sterea +lat_0=90 +ellps=WGS84'; Hidden: False),
    (Key: 'gstmerc'; Name: 'Gauss-Schreiber Transverse Mercator (aka Gauss-Laborde Reunion)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=gstmerc +ellps=WGS84'; Hidden: False),
    (Key: 'tcc'; Name: 'Transverse Central Cylindrical'; Desc: ''; Latmax: 85; Latmin: 30; Lonmax: 45; Lonmin: -45; Defn: '+proj=tcc +ellps=WGS84'; Hidden: False),
    (Key: 'tcea'; Name: 'Transverse Cylindrical Equal Area'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=tcea +ellps=WGS84'; Hidden: False),
    (Key: 'tissot'; Name: 'Tissot Conic'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=tissot +lat_1=60 +lat_2=65 +ellps=WGS84'; Hidden: False),
    (Key: 'tmerc'; Name: 'Transverse Mercator'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 90; Lonmin: -90; Defn: '+proj=tmerc +ellps=WGS84'; Hidden: False),
    (Key: 'tpeqd'; Name: 'Two Point Equidistant'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=tpeqd +lat_1=60 +lat_2=65 +ellps=WGS84'; Hidden: False),
    (Key: 'tpers'; Name: 'Tilted perspective'; Desc: ''; Latmax: 90; Latmin: 0; Lonmax: 75; Lonmin: -75; Defn: '+proj=tpers +h=5500000 +lat_0=40 +ellps=WGS84'; Hidden: False),
    (Key: 'ups'; Name: 'Universal Polar Stereographic'; Desc: ''; Latmax: 90; Latmin: -20; Lonmax: 180; Lonmin: -180; Defn: '+proj=ups +ellps=WGS84'; Hidden: False),
    (Key: 'urm5'; Name: 'Urmaev V'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=urm5 +n=0.9 +alpha=2 +q=4 +ellps=WGS84'; Hidden: False),
    (Key: 'urmfps'; Name: 'Urmaev Flat-Polar Sinusoidal'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=urmfps +n=0.5 +ellps=WGS84'; Hidden: False),
    (Key: 'utm'; Name: 'Universal Transverse Mercator (UTM)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=utm +ellps=WGS84'; Hidden: False),
    (Key: 'vandg'; Name: 'van der Grinten (I)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=vandg +ellps=WGS84'; Hidden: False),
    (Key: 'vandg2'; Name: 'van der Grinten II'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=vandg2 +ellps=WGS84'; Hidden: False),
    (Key: 'vandg3'; Name: 'van der Grinten III'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=vandg3 +ellps=WGS84'; Hidden: False),
    (Key: 'vandg4'; Name: 'van der Grinten IV'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=vandg4 +ellps=WGS84'; Hidden: False),
    (Key: 'vitk1'; Name: 'Vitkovsky I'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=vitk1 +lat_1=45 +lat_2=55 +ellps=WGS84'; Hidden: False),
    (Key: 'wag1'; Name: 'Wagner I (Kavraisky VI)'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag1 +ellps=WGS84'; Hidden: False),
    (Key: 'wag2'; Name: 'Wagner II'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag2 +ellps=WGS84'; Hidden: False),
    (Key: 'wag3'; Name: 'Wagner III'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag3 +ellps=WGS84'; Hidden: False),
    (Key: 'wag4'; Name: 'Wagner IV'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag4 +ellps=WGS84'; Hidden: False),
    (Key: 'wag5'; Name: 'Wagner V'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag5 +ellps=WGS84'; Hidden: False),
    (Key: 'wag6'; Name: 'Wagner VI'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag6 +ellps=WGS84'; Hidden: False),
    (Key: 'wag7'; Name: 'Wagner VII'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wag7 +ellps=WGS84'; Hidden: False),
    (Key: 'weren'; Name: 'Werenskiold I'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=weren +ellps=WGS84'; Hidden: False),
    (Key: 'wink1'; Name: 'Winkel I'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wink1 +ellps=WGS84'; Hidden: False),
    (Key: 'wink2'; Name: 'Winkel II'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wink2 +ellps=WGS84'; Hidden: False),
    (Key: 'wintri'; Name: 'Winkel Tripel'; Desc: ''; Latmax: 90; Latmin: -90; Lonmax: 180; Lonmin: -180; Defn: '+proj=wintri +ellps=WGS84'; Hidden: False)
  );


function LibProjDefnFromEpsgCode(const Code: Integer): string;
function MIInvariantFloat(const V: Double): string; forward;

function LibProjDefnToWKTProjection(const ADefn: string; PrettyWKT: Boolean): string;
function WKTProjectionToLibProjDefn(const ADefn: string): string;

function TryParseMapInfoCoordSys(const AValue: string; out ACoordSys: TMapInfoCoordSys): Boolean;
function MapInfoCoordSysToString(const ACoordSys: TMapInfoCoordSys;
  const IncludeCoordSysKeyword: Boolean = True): string;
function MapInfoCoordSysToLibProjDefn(const ACoordSys: TMapInfoCoordSys): string; overload;
function MapInfoCoordSysToLibProjDefn(const ACoordSys: string): string; overload;
function LibProjDefnToMapinfoCoordSys(const ADefn: string;
  out ACoordSys: TMapInfoCoordSys): Boolean; overload;
function LibProjDefnToMapinfoCoordSys(const ADefn: string): string; overload;
function LibProjParseParamsString(const ADefn: string; var AParams: TArray<TPJParamRec>): Integer;
function LibProjKnownParams(var AParams: TArray<TPJParamRec>): Integer;
function LibProjProjectionAllowedParams(const AProjID: string; var AParams: TArray<TPJParamRec>): integer;
function LibProjParamDisplayName(const AParam: string): string;
function LibProjParamExplain(const AKey: string): string;


implementation
uses
	libProj4.Intf, {$ifdef __proj_include_full_epsgdb}libProj4.Projections.EPSG,{$endif} libProj4.WellKnownText;
type
	TpjEllipseType = (ellNone, ellMerit, ellSGS85, ellGRS80, ellIAU76, ellAiry, ellAPL49, ellNWL9D, ellModAriy,
		ellAndrae, ellAustSA, ellGRS67, ellBessel, ellBesselNamibia, ellClark66, ellClark80, ellCPM, ellDelambre,
		ellEngelis, ellEverest30, ellEverest48, ellEverest56, ellEverest69, ellEverestSS, ellFischer60, ellFischer60m,
		ellFischer68, ellHelmert, ellHough, ellInternational, ellKrass, ellKaula, ellLerch, ellMaupertius,
		ellNewInternational, ellPlessis, ellSEAsia, ellWalbeck, ellWGS60, ellWGS66, ellWGS72, ellWGS84, ellSphere);

	PpjEllipse = ^TpjEllipse;
	TpjEllipse = packed record
		pjKey: string; // proj code
		eWktKey: string;
		eName: string;
		epsgCode: Integer; // epsgcode
		a: double; // major axis or radius if es=0
		b: double; // minor axis or radius if es=0
		e: double; // eccentricity
		es: double; // e ^ 2
		one_es: double; // 1 - e^2
		rone_es: double; // 1/(1 - e^2)
		ra: double; // 1/A
		Rf: double; // Reciproc Flattening
	end;

	PpjDatum = ^TpjDatum;
	TpjDatum = packed record
	 pjKey: string;     // code
	 pjEllKey: string;  // ellipse proj code
	 dWktKey: string;  // ellipse proj code
	 dName: string;     // datum readable name
	 toWgs: string;     // to wgs definition
	 epsgCode: Integer; // epsg code
	end;

	PpjPrime = ^TpjPrime;
	TpjPrime = packed record
	 pjKey: string;     // proj code
	 pmName: string;    // prime name
	 pmLon: Double;     // prime lon
	 epsgCode: Integer; // epsg code
	end;

	PpjLinearUnit = ^TpjLinearUnit;
	TpjLinearUnit = packed record
	 pjKey: string;   // code
	 wktName: string;  // wkt name
	 toMeters: Double; // co meters conversion factor
	 Description: string;
	 epsgCode: Integer;// epsg code
	end;

	TpjProjectionInfo = packed record
		pjKey: string;
		wktName: string;
		epsgCode: Integer;
		Description: string;
    DisplayCaption: string
	end;

	TpjParam = record
		pjKey: string;
		wktName: string;
		epsgCode: Integer;
		Description: string;
    paramGroup: Integer;
	end;

const

	pjDatumsList: array[0..8] of TpjDatum = (
	(pjKey: 'WGS84'; pjEllKey: 'WGS84'; dWktKey: 'WGS_84'; dName: 'WGS 84'; toWgs: '0,0,0,0,0,0,0'),
	(pjKey: 'GGRS87'; pjEllKey: 'GRS80'; dWktKey: 'Greek_Geodetic_Reference_System_1987'; dName: 'Greek_Geodetic_Reference_System_1987'; toWgs: '-199.87,74.79,246.62,0,0,0,0'),
	(pjKey: 'NAD83'; pjEllKey: 'GRS80'; dWktKey: 'North_American_Datum_1983'; dName: 'North_American_Datum_1983'; toWgs: '0,0,0,0,0,0,0'),
	(pjKey: 'potsdam'; pjEllKey: 'bessel'; dWktKey: 'Potsdam_Rauenberg_1950_DHDN'; dName: 'Potsdam Rauenberg 1950 DHDN'; toWgs: '606.0,23.0,413.0,0,0,0,0'),
	(pjKey: 'carthage'; pjEllKey: 'clark80'; dWktKey: 'Carthage_1934_Tunisia'; dName: 'Carthage 1934 Tunisia'; toWgs: '-263.0,6.0,431.0,0,0,0,0'),
	(pjKey: 'hermannskogel'; pjEllKey: 'bessel'; dWktKey: 'Hermannskogel'; dName: 'Hermannskogel'; toWgs: '653.0,-212.0,449.0,0,0,0,0'),
	(pjKey: 'ire65'; pjEllKey: 'mod_airy'; dWktKey: 'Ireland_1965'; dName: 'Ireland 1965'; toWgs: '482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15'),
	(pjKey: 'nzgd49'; pjEllKey: 'intl'; dWktKey: 'New_Zealand_Geodetic_Datum 1949'; dName: 'New Zealand Geodetic Datum 1949'; toWgs: '59.47,-5.04,187.44,0.47,-0.1,1.024,-4.5993'),
	(pjKey: 'OSGB36'; pjEllKey: 'airy'; dWktKey: 'Airy_1830'; dName: 'Airy 1830'; toWgs: '446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894')
	);

//		pjKey: string; // proj code
//		eWktKey: string;
//		eName: string;
//		epsgCode: Integer; // epsgcode
//		a: double; // major axis or radius if es=0
//		b: double; // minor axis or radius if es=0
//		e: double; // eccentricity
//		es: double; // e ^ 2
//		one_es: double; // 1 - e^2
//		rone_es: double; // 1/(1 - e^2)
//		ra: double; // 1/A
//		Rf: double; // Reciproc Flattening

	pjEllipsoidsList:
	array [TpjEllipseType] of TpjEllipse = (
		(pjKey: ''; eWktKey:''; eName: 'empty'; a: NaN; b: NaN; e: NaN; es: NaN; one_es: NaN; rone_es: NaN; ra: NaN; Rf: NaN),
		(pjKey: 'MERIT'; eWktKey:'MERIT_1983'; eName: 'MERIT 1983'; a: 6378137.0; b: 6356752.29821597; e: 0.08181922; es: 0.00669438499958719; one_es: 0.993305615000413; rone_es: 1.00673950181947; ra: 0.15678559428874E-6; Rf: 0.00335281317789691),
		(pjKey: 'SGS85'; eWktKey:'SGS85'; eName: 'Soviet Geodetic System 85'; a: 6378136.0; b: 6356751.30156878; e: 0.08181922; es: 0.0066943849995883; one_es: 0.993305615000412; rone_es: 1.00673950181947; ra: 0.156785618870466E-6; Rf: 0.00335281317789691),
		(pjKey: 'GRS80'; eWktKey:'GRS_1980'; eName: 'GRS 1980(IUGG 1980)'; a: 6378137.0; b: 6356752.31414036; e: 0.08181919; es: 0.00669438002289957; one_es: 0.9933056199771; rone_es: 1.00673949677548; ra: 0.15678559428874E-6; Rf: 0.00335281068118232),
		(pjKey: 'IAU76'; eWktKey:'IAU_1976'; eName: 'IAU 1976'; a: 6378140.0; b: 6356755.28815753; e: 0.08181922; es: 0.00669438499958741; one_es: 0.993305615000413; rone_es: 1.00673950181947; ra: 0.156785520543607E-6; Rf: 0.00335281317789691),
		(pjKey: 'airy'; eWktKey:'Airy_1830'; eName: 'Airy 1830'; a: 6377563.396; b: 6356256.91; e: 0.08167337; es: 0.00667053976159737; one_es: 0.993329460238403; rone_es: 1.00671533466852; ra: 0.156799695731319E-6; Rf: 0.00334085052190358),
		(pjKey: 'APL4.9'; eWktKey:'APL4_9'; eName: 'Appl. Physics. 1965'; a: 6378137.0; b: 6356751.79631182; e: 0.08182018; es: 0.0066945418545874; one_es: 0.993305458145413; rone_es: 1.00673966079587; ra: 0.15678559428874E-6; Rf: 0.00335289186923722),
		(pjKey: 'NWL9D'; eWktKey:'NWL9D'; eName: 'Naval Weapons Lab 1965'; a: 6378145.0; b: 6356759.76948868; e: 0.08182018; es: 0.00669454185458873; one_es: 0.993305458145411; rone_es: 1.00673966079587; ra: 0.156785397635206E-6; Rf: 0.00335289186923722),
		(pjKey: 'mod_airy'; eWktKey:'Modified_Airy'; eName: 'Modified Airy'; a: 6377340.189; b: 6356034.446; e: 0.08167338; es: 0.00667054060589878; one_es: 0.993329459394101; rone_es: 1.0067153355242; ra: 0.156805183722966E-6; Rf: 0.00334085094546921),
		(pjKey: 'andrae'; eWktKey:'Andrae_1876'; eName: 'Andrae 1876 (Den.Iclnd.)'; a: 6377104.43; b: 6355847.41523333; e: 0.08158159; es: 0.00665555555555652; one_es: 0.993344444444443; rone_es: 1.00670014876791; ra: 0.156810980747888E-6; Rf: 0.00333333333333333),
		(pjKey: 'aust_SA'; eWktKey:'Australian_SA'; eName: 'Australian Natl & S. Amer. 1969'; a: 6378160.0; b: 6356774.71919531; e: 0.08182018; es: 0.0066945418545864; one_es: 0.993305458145414; rone_es: 1.00673966079587; ra: 0.156785028911159E-6; Rf: 0.00335289186923722),
		(pjKey: 'GRS67'; eWktKey:'GRS_67'; eName: 'GRS 67(IUGG 1967)'; a: 6378160.0; b: 6356774.51609071; e: 0.08182057; es: 0.00669460532856936; one_es: 0.993305394671431; rone_es: 1.00673972512833; ra: 0.156785028911159E-6; Rf: 0.00335292371299641),
		(pjKey: 'bessel'; eWktKey:'Bessel_1841'; eName: 'Bessel 1841'; a: 6377397.155; b: 6356078.96281819; e: 0.081696831215255833813584066738272; es: 0.006674372230614; one_es: 0.993325627769386; rone_es: 1.00671921879917; ra: 0.156803783063123E-6; Rf: 0.00334277318217481),
		(pjKey: 'bess_nam'; eWktKey:'Bessel_1841_Namibia'; eName: 'Bessel 1841 (Namibia)'; a: 6377483.865; b: 6356165.38296633; e: 0.08169683; es: 0.00667437223180078; one_es: 0.993325627768199; rone_es: 1.00671921879917; ra: 0.156801651116369E-6; Rf: 0.00334277318217481),
		(pjKey: 'clrk66'; eWktKey:'Clarke_1866'; eName: 'Clarke 1866'; a: 6378206.4; b: 6356583.8; e: 0.08227185; es: 0.0067686579972912; one_es: 0.993231342002709; rone_es: 1.00681478494592; ra: 0.156783888335755E-6; Rf: 0.00339007530392876),
		(pjKey: 'clrk80'; eWktKey:'Clarke_modified'; eName: 'Clarke 1880 mod.'; a: 6378249.145; b: 6356514.96582849; e: 0.08248322; es: 0.00680348119602181; one_es: 0.993196518803978; rone_es: 1.00685008562476; ra: 0.156782837619931E-6; Rf: 0.00340754628384929),
		(pjKey: 'CPM'; eWktKey:'CPM_1799'; eName: 'Comm. des Poids et Mesures 1799'; a: 6375738.7; b: 6356666.22191211; e: 0.07729088; es: 0.00597388071841887; one_es: 0.994026119281581; rone_es: 1.00600978244187; ra: 0.156844570810281E-6; Rf: 0.00299141463998325),
		(pjKey: 'delmbr'; eWktKey:'Delambre_1810'; eName: 'Delambre 1810 (Belgium)'; a: 6376428; b: 6355957.92616372; e: 0.08006397; es: 0.00641023989446932; one_es: 0.993589760105531; rone_es: 1.00645159617364; ra: 0.15682761571212E-6; Rf: 0.00321027287319422),
		(pjKey: 'engelis'; eWktKey:'Engelis_1985'; eName: 'Engelis 1985'; a: 6378136.05; b: 6356751.32272154; e: 0.08181928; es: 0.00669439396253357; one_es: 0.993305606037466; rone_es: 1.00673951090364; ra: 0.15678561764138E-6; Rf: 0.00335281767444543),
		(pjKey: 'evrst30'; eWktKey:'Everest_1830'; eName: 'Everest 1830'; a: 6377276.345; b: 6356075.41314024; e: 0.08147298; es: 0.00663784663019951; one_es: 0.9933621533698; rone_es: 1.00668220206264; ra: 0.156806753526376E-6; Rf: 0.00332444929666288),
		(pjKey: 'evrst48'; eWktKey:'Everest_1948'; eName: 'Everest 1948'; a: 6377304.063; b: 6356103.03899315; e: 0.08147298; es: 0.00663784663020128; one_es: 0.993362153369799; rone_es: 1.00668220206264; ra: 0.156806071989232E-6; Rf: 0.00332444929666288),
		(pjKey: 'evrst56'; eWktKey:'Everest_1956'; eName: 'Everest 1956'; a: 6377301.243; b: 6356100.2283681; e: 0.08147298; es: 0.00663784663020017; one_es: 0.9933621533698; rone_es: 1.00668220206264; ra: 0.156806141327829E-6; Rf: 0.00332444929666288),
		(pjKey: 'evrst69'; eWktKey:'Everest_1969'; eName: 'Everest 1969'; a: 6377295.664; b: 6356094.6679152; e: 0.08147298; es: 0.00663784663020106; one_es: 0.993362153369799; rone_es: 1.00668220206264; ra: 0.156806278505327E-6; Rf: 0.00332444929666288),
		(pjKey: 'evrstSS'; eWktKey:'Everest_SS'; eName: 'Everest (Sabah & Sarawak)'; a: 6377298.556; b: 6356097.5503009; e: 0.08147298; es: 0.00663784663019851; one_es: 0.993362153369801; rone_es: 1.00668220206264; ra: 0.156806207396259E-6; Rf: 0.00332444929666288),
		(pjKey: 'fschr60'; eWktKey:'Fischer_1960'; eName: 'Fischer (Mercury Datum) 1960'; a: 6378166; b: 6356784.28360711; e: 0.08181333; es: 0.00669342162296482; one_es: 0.993306578377035; rone_es: 1.00673852541468; ra: 0.156784881422026E-6; Rf: 0.00335232986925913),
		(pjKey: 'fschr60m'; eWktKey:'Modified_Fischer_1960'; eName: 'Modified Fischer 1960'; a: 6378155; b: 6356773.32048274; e: 0.08181333; es: 0.00669342162296449; one_es: 0.993306578377036; rone_es: 1.00673852541468; ra: 0.156785151818982E-6; Rf: 0.00335232986925913),
		(pjKey: 'fschr68'; eWktKey:'Fischer_1968'; eName: 'Fischer 1968'; a: 6378150; b: 6356768.33724438; e: 0.08181333; es: 0.00669342162296749; one_es: 0.993306578377033; rone_es: 1.00673852541468; ra: 0.156785274726998E-6; Rf: 0.00335232986925913),
		(pjKey: 'helmert'; eWktKey:'Helmert_1906'; eName: 'Helmert 1906'; a: 6378200; b: 6356818.16962789; e: 0.08181333; es: 0.00669342162296627; one_es: 0.993306578377034; rone_es: 1.00673852541468; ra: 0.156784045655514E-6; Rf: 0.00335232986925913),
		(pjKey: 'hough'; eWktKey:'Hough'; eName: 'Hough'; a: 6378270.0; b: 6356794.34343434; e: 0.08199189; es: 0.00672267002233429; one_es: 0.993277329977666; rone_es: 1.00676817019722; ra: 0.15678232498781E-6; Rf: 0.00336700336700337),
		(pjKey: 'intl'; eWktKey:'International_1909'; eName: 'International 1909 (Hayford)'; a: 6378388.0; b: 6356911.94612795; e: 0.08199189; es: 0.00672267002233207; one_es: 0.993277329977668; rone_es: 1.00676817019722; ra: 0.156779424519173E-6; Rf: 0.00336700336700337),
		(pjKey: 'krass'; eWktKey:'Krassowsky_1940'; eName: 'Krassowsky 1940'; a: 6378245; b: 6356863.01877305; e: 0.081813334; es: 0.00669342162296504; one_es: 0.993306578377035; rone_es: 1.00673852541468; ra: 0.156782939507655E-6; Rf: 0.00335232986925913),
		(pjKey: 'kaula'; eWktKey:'Kaula_1961'; eName: 'Kaula 1961'; a: 6378163; b: 6356776.99208691; e: 0.08182155; es: 0.0066947659459099; one_es: 0.99330523405409; rone_es: 1.00673988791802; ra: 0.156784955166558E-6; Rf: 0.00335300429184549),
		(pjKey: 'lerch'; eWktKey:'Lerch_1979'; eName: 'Lerch 1979'; a: 6378139; b: 6356754.29151034; e: 0.08181922; es: 0.00669438499958852; one_es: 0.993305615000411; rone_es: 1.00673950181947; ra: 0.15678554512531E-6; Rf: 0.00335281317789691),
		(pjKey: 'mprts'; eWktKey:'Maupertius_1738'; eName: 'Maupertius 1738'; a: 6397300; b: 6363806.28272251; e: 0.10219488; es: 0.0104437926591934; one_es: 0.989556207340807; rone_es: 1.0105540166205; ra: 0.15631594578963E-6; Rf: 0.00523560209424084),
		(pjKey: 'new_intl'; eWktKey:'New International_1967'; eName: 'New International 1967'; a: 6378157.5; b: 6356772.2; e: 0.08182023; es: 0.0066945504730862; one_es: 0.993305449526914; rone_es: 1.00673966953093; ra: 0.156785090365047E-6; Rf: 0.00335289619298362),
		(pjKey: 'plessis'; eWktKey:'Plessis_1817'; eName: 'Plessis 1817 (France)'; a: 6376523; b: 6355863; e: 0.08043334; es: 0.00646952287129587; one_es: 0.993530477128704; rone_es: 1.00651165014081; ra: 0.15682527923133E-6; Rf: 0.00324001026891929),
		(pjKey: 'SEasia'; eWktKey:'Southeast_Asia'; eName: 'Southeast Asia'; a: 6378155.0; b: 6356773.3205; e: 0.08181333; es: 0.00669342161757036; one_es: 0.99330657838243; rone_es: 1.00673852540921; ra: 0.156785151818982E-6; Rf: 0.00335232986655221),
		(pjKey: 'Walbeck'; eWktKey:'Walbeck'; eName: 'Walbeck'; a: 6376896.0; b: 6355834.8467; e: 0.08120682; es: 0.00659454809019966; one_es: 0.9934054519098; rone_es: 1.00663832484261; ra: 0.156816106143177E-6; Rf: 0.00330272805139054),
		(pjKey: 'WGS60'; eWktKey:'WGS_60'; eName: 'WGS 60'; a: 6378165.0; b: 6356783.28695944; e: 0.08181333; es: 0.00669342162296482; one_es: 0.993306578377035; rone_es: 1.00673852541468; ra: 0.156784906003529E-6; Rf: 0.00335232986925913),
		(pjKey: 'WGS66'; eWktKey:'WGS_66'; eName: 'WGS 66'; a: 6378145.0; b: 6356759.76948868; e: 0.08182018; es: 0.00669454185458873; one_es: 0.993305458145411; rone_es: 1.00673966079587; ra: 0.156785397635206E-6; Rf: 0.00335289186923722),
		(pjKey: 'WGS72'; eWktKey:'WGS_72'; eName: 'WGS 72'; a: 6378135.0; b: 6356750.52001609; e: 0.08181881; es: 0.00669431777826779; one_es: 0.993305682221732; rone_es: 1.00673943368903; ra: 0.1567856434522E-6; Rf: 0.0033527794541675),
		(pjKey: 'WGS84'; eWktKey:'WGS_84'; eName: 'WGS 84'; a: 6378137.0; b: 6356752.31424518; e: 0.08181919; es: 0.00669437999014111; one_es: 0.99330562000985889; rone_es: 1.0067394967422762251591434067861; ra: 0.15678559428874E-6; Rf: 0.00335281066474748),
		(pjKey: 'sphere'; eWktKey:''; eName: 'Normal Sphere (r=6370997)'; a: 6370997.0; b: 6370997; e: 0; es: 0; one_es: 1; rone_es: 1; ra: 0.156961304486566E-6; Rf: 0)
	);

 pjPrimeList: array[0..12] of TpjPrime =
 (
	 (pjKey: 'greenwich'; pmName: 'greenwich'; pmLon: 0),
	 (pjKey: 'lisbon'; pmName: 'lisbon'; pmLon: -9.131906111111112),
	 (pjKey: 'paris'; pmName: 'paris'; pmLon: 2.337229166666667),
	 (pjKey: 'bogota'; pmName: 'bogota'; pmLon: -74.08091666666667),
	 (pjKey: 'madrid'; pmName: 'madrid'; pmLon: -3.687938888888889),
	 (pjKey: 'rome'; pmName: 'rome'; pmLon: 12.45233333333333),
	 (pjKey: 'bern'; pmName: 'bern'; pmLon: 7.439583333333333),
	 (pjKey: 'jakarta'; pmName: 'jakarta'; pmLon: 106.8077194444444),
	 (pjKey: 'ferro'; pmName: 'ferro'; pmLon: -17.66666666666667),
	 (pjKey: 'brussels'; pmName: 'brussels'; pmLon: 4.367975),
	 (pjKey: 'stockholm'; pmName: 'stockholm'; pmLon: 18.05827777777778),
	 (pjKey: 'athens'; pmName: 'athens'; pmLon: 23.7163375),
	 (pjKey: 'oslo'; pmName: 'oslo'; pmLon: 10.72291666666667)
 );

 pjLinearUnitsList: array[0..20] of TpjLinearUnit =
 (
	 (pjKey: 'm'; wktName: 'Meter'; toMeters: 1; Description: 'Meters'),
	 (pjKey: 'km'; wktName: 'Kilometer'; toMeters: 1000; Description: 'Kilometers'),
	 (pjKey: 'dm'; wktName: 'Decimeter'; toMeters: 0.1; Description: 'Decimeters'),
	 (pjKey: 'cm'; wktName: 'Centimeter'; toMeters: 0.01; Description: 'Centimeters'),
	 (pjKey: 'mm';wktName: 'Millimeter'; toMeters: 0.001; Description: 'Millimeters'),
	 (pjKey: 'ft'; wktName: 'Foot_International'; toMeters: 0.3048; Description: 'Foots (International)'),
	 (pjKey: 'us-ft'; wktName: 'Foot_US'; toMeters: 0.3048006096012192; Description: 'Foots (US survey)'),
	 (pjKey: 'ind-ft'; wktName: 'Foot_Indian'; toMeters: 0.30479841; Description: 'Foots (Indian)'),
	 (pjKey: 'kmi'; wktName: 'Nautical_Mile_International'; toMeters: 1852.0; Description: 'Nautical Miles (International)'),
	 (pjKey: 'mi'; wktName: 'Statute_Mile_International'; toMeters: 1609.344; Description: 'Statute Miles (International)'),
	 (pjKey: 'us-mi'; wktName: 'Statute_Mile_US_Surveyor'; toMeters: 1609.347218694437; Description: 'Statute Miles (US survey)'),
	 (pjKey: 'link'; wktName: 'Link'; toMeters: 0.20116684023368047; Description: 'Links (Based on US Foot)'),
	 (pjKey: 'yd'; wktName: 'Yard_International'; toMeters: 0.9144; Description: 'Yards (International)'),
	 (pjKey: 'us-yd'; wktName: 'Yard_US_Surveyor'; toMeters: 0.914401828803658; Description: 'Yards (US survey)'),
	 (pjKey: 'ind-yd'; wktName: 'Yard_Indian'; toMeters: 0.91439523; Description: 'Yards (Indian)'),
	 (pjKey: 'in'; wktName: 'Inch_International'; toMeters: 0.0254; Description: 'Inchs (International)'),
	 (pjKey: 'us-in'; wktName: 'Inch_US_Surveyor'; toMeters: 0.025400050800101603; Description: 'Inchs (US survey)'),
	 (pjKey: 'fath'; wktName: 'Fathom_International'; toMeters: 1.8288; Description: 'Fathoms (International)'),
	 (pjKey: 'ch'; wktName: 'Chain_International'; toMeters: 20.1168; Description: 'Chains (International)'),
	 (pjKey: 'us-ch'; wktName: 'Chain_US_Surveyor'; toMeters: 20.11684023368047; Description: 'Chains (US survey)'),
	 (pjKey: 'ind-ch'; wktName: 'Chain_Indian'; toMeters: 20.11669506; Description: 'Chains (Indian)')
 );

	pjProjectionsList: array[0..145] of TpjProjectionInfo =
	(
		(pjKey:'aea'; wktName: 'Albers_Conic_Equal_Area'; Description: 'Albers Equal Area'; DisplayCaption: 'Альберта равноплощадная'),
		(pjKey:'aea'; wktName: 'Albers'; Description: '[ESRI] Albers Equal Area'; DisplayCaption: ''),
		(pjKey:'aeqd'; wktName: 'Azimuthal_Equidistant'; Description: 'Azimuthal Equidistant'; DisplayCaption: 'Азимутальная равнопромежуточная'),
		(pjKey:'airy'; wktName: 'Airy'; Description: 'Airy'; DisplayCaption: 'Эйри'),
		(pjKey:'aitoff'; wktName: 'Aitoff'; Description: '[ESRI] Aitoff'; DisplayCaption: 'Аитова'),
		(pjKey:'alsk'; wktName: 'Mod_Stererographics_of_Alaska';Description: 'Mod. Stererographics of Alaska'; DisplayCaption: 'Стереографическая модифицированная (Аляска)'),
		(pjKey:'apian'; wktName: 'Apian_Globular_I'; Description: 'Apian Globular I'; DisplayCaption: ''),
		(pjKey:'august'; wktName: 'August_Epicycloidal'; Description: 'August Epicycloidal'; DisplayCaption: ''),
		(pjKey:'bacon'; wktName: 'Bacon_Globular'; Description: 'Bacon Globular'; DisplayCaption: ''),
		(pjKey:'bipc'; wktName: 'Bipolar_conic_of_western_hemisphere'; Description: 'Bipolar conic of western hemisphere'; DisplayCaption: ''),
		(pjKey:'boggs'; wktName: 'Boggs_Eumorphic'; Description: ' Boggs Eumorphic'; DisplayCaption: ''),
		(pjKey:'bonne'; wktName: 'Bonne'; Description: 'Bonne (Werner lat_1=90)'; DisplayCaption: ''),
		(pjKey:'cass'; wktName: 'Cassini_Soldner'; Description: 'Cassini'; DisplayCaption: ''),
		(pjKey:'cass'; wktName: 'Cassini'; Description: '[ESRI] Cassini'; DisplayCaption: ''),
		(pjKey:'cc'; wktName: 'Central_Cylindrical'; Description: 'Central Cylindrical'; DisplayCaption: ''),
		(pjKey:'cea'; wktName: 'Cylindrical_Equal_Area'; Description: 'Equal Area Cylindrical'; DisplayCaption: ''),
		(pjKey:'cea'; wktName: 'Behrmann'; Description: '[ESRI] Behrmann (standard parallel = 30)'; DisplayCaption: ''),
		(pjKey:'chamb'; wktName: 'Chamberlin_Trimetric'; Description: 'Chamberlin Trimetric'; DisplayCaption: ''),
		(pjKey:'collg'; wktName: 'Collignon'; Description: 'Collignon'; DisplayCaption: ''),
		(pjKey:'crast'; wktName: 'Craster_Parabolic'; Description: '[ESRI] Craster Parabolic (Putnins P4)'; DisplayCaption: ''),
		(pjKey:'denoy'; wktName: 'Denoyer_Semi_Elliptical'; Description: 'Denoyer Semi-Elliptical'; DisplayCaption: ''),
		(pjKey:'eck1'; wktName: 'Eckert_I'; Description: 'Eckert I'; DisplayCaption: 'Псевдоциллиндрическая Эккерта I'),
		(pjKey:'eck2'; wktName: 'Eckert_II'; Description: 'Eckert II'; DisplayCaption: 'Псевдоциллиндрическая Эккерта II'),
		(pjKey:'eck3'; wktName: 'Eckert_III'; Description: 'Eckert III'; DisplayCaption: 'Псевдоциллиндрическая Эккерта III'),
		(pjKey:'eck4'; wktName: 'Eckert_IV'; Description: 'Eckert IV'; DisplayCaption: 'Псевдоциллиндрическая Эккерта IV'),
		(pjKey:'eck5'; wktName: 'Eckert_V'; Description: 'Eckert V'; DisplayCaption: 'Псевдоциллиндрическая Эккерта V'),
		(pjKey:'eck6'; wktName: 'Eckert_VI'; Description: 'Eckert VI'; DisplayCaption: 'Псевдоциллиндрическая Эккерта VI'),
		(pjKey:'eqc'; wktName: 'Equirectangular'; Description: 'Equidistant Cylindrical (Plate Caree)'; DisplayCaption: 'Циллиндрическая равнопромежуточная'),
		(pjKey:'eqc'; wktName: 'Equidistant_Cylindrical'; Description: '[ESRI] Equidistant Cylindrical (Plate Caree)'; DisplayCaption: 'Цилиндрическая равнопромежуточная'),
		(pjKey:'eqc'; wktName: 'Plate_Carree'; Description: '[ESRI] Equidistant Cylindrical (Plate Caree)'; DisplayCaption: 'Цилиндрическая равнопромежуточная'),
		(pjKey:'eqdc'; wktName: 'Equidistant_Conic'; Description: 'Equidistant Conic'; DisplayCaption: 'Коническая равнопромежуточная'),
		(pjKey:'euler'; wktName: 'Euler'; Description: 'Euler'; DisplayCaption: 'Сферическая Эйлера'),
		(pjKey:'etmerc'; wktName: 'Extended_Transverse_Mercator'; Description: 'Extended Transverse Mercator'; DisplayCaption: 'Поперечная Меркатора (расширенная)'),
		(pjKey:'fahey'; wktName: 'Fahey'; Description: 'Fahey'; DisplayCaption: 'Псевдоцилиндрическая Фейхи'),
		(pjKey:'fouc'; wktName: 'Foucault'; Description: ' Foucaut'; DisplayCaption: 'Псевдоциллиндрическая равноплощадная Фуко'),
		(pjKey:'fouc_s'; wktName: 'Foucault_Sinusoidal'; Description: 'Foucaut Sinusoidal'; DisplayCaption: 'Псевдоциллиндрическая синусоидальная Фуко'),
		(pjKey:'gall'; wktName: 'Gall_Stereographic'; Description: 'Gall (Gall Stereographic)'; DisplayCaption: 'Стереографическая Галла'),
		(pjKey:'geocent'; wktName: 'Geocentric'; Description: 'Geocentric'; DisplayCaption: 'Геоцентрическая'),
		(pjKey:'geos'; wktName: 'GEOS'; Description: 'Geostationary Satellite View'; DisplayCaption: 'Азимутальная геостационарная (Вид со спутника)'),
		(pjKey:'gins8'; wktName: 'Ginsburg_VIII'; Description: 'Ginsburg VIII (TsNIIGAiK)'; DisplayCaption: 'Псевдоциллиндрическая Гинзбурга (ЦНИИГАик)'),
		(pjKey:'gn_sinu'; wktName: 'General_Sinusoidal_Series'; Description: 'General Sinusoidal Series'; DisplayCaption: 'Псевдоциллиндрическая синусоидальная '),
		(pjKey:'gnom'; wktName: 'Gnomonic'; Description: 'Gnomonic'; DisplayCaption: 'Гномоническая'),
		(pjKey:'goode'; wktName: 'Goode_Homolosine'; Description: 'Goode Homolosine'; DisplayCaption: ''),
		(pjKey:'gs48'; wktName: 'Mod_Stererographics_48'; Description: 'Mod. Stererographics of 48 U.S.'; DisplayCaption: ''),
		(pjKey:'gs50'; wktName: 'Mod_Stererographics_50'; Description: 'Mod. Stererographics of 50 U.S.'; DisplayCaption: ''),
		(pjKey:'hammer'; wktName: 'Hammer_Eckert_Greifendorff'; Description: 'Hammer & Eckert-Greifendorff'; DisplayCaption: ''),
		(pjKey:'hatano'; wktName: 'Hatano_Asymmetrical_Equal_Area'; Description: 'Hatano Asymmetrical Equal Area'; DisplayCaption: ''),
		(pjKey:'igh'; wktName: 'World_Goode_Homolosine_Land'; Description: 'Interrupted Goode Homolosine'; DisplayCaption: ''),
		(pjKey:'imw_p'; wktName: 'International_Map_of_the_World_Polyconic'; Description: 'International Map of the World Polyconic'; DisplayCaption: ''),
		(pjKey:'kav5'; wktName: 'Kavraisky_V'; Description: 'Kavraisky V'; DisplayCaption: 'Каврайского V'),
		(pjKey:'kav7'; wktName: 'Kavraisky_VII'; Description: 'Kavraisky VII'; DisplayCaption: 'Каврайского VII'),
		(pjKey:'krovak'; wktName: 'Krovak'; Description: 'Krovak'; DisplayCaption: 'Кровака'),
		(pjKey:'labrd'; wktName: 'Laborde_Oblique_Mercator'; Description: 'Laborde'; DisplayCaption: ''),
		(pjKey:'laea'; wktName: 'Lambert_Azimuthal_Equal_Area'; Description: 'Lambert Azimuthal Equal Area'; DisplayCaption: 'Азимутальная равноплощажная Ламберта'),
		(pjKey:'lagrng'; wktName: 'Lagrange'; Description: 'Lagrange'; DisplayCaption: ''),
		(pjKey:'larr'; wktName: 'Larrivee'; Description: 'Larrivee'; DisplayCaption: ''),
		(pjKey:'lask'; wktName: 'Laskowski'; Description: 'Laskowski'; DisplayCaption: ''),
		(pjKey:'latlon'; wktName: 'Geodetic'; Description: 'Lat/long'; DisplayCaption: ''),
		(pjKey:'latlong'; wktName: 'Geodetic'; Description: 'Lat/long'; DisplayCaption: 'Градусы (Широта/Долгота)'),
		(pjKey:'longlat'; wktName: 'Geodetic'; Description: 'Long/Lat'; DisplayCaption: 'Градусы (Долгота/Широта)'),
		(pjKey:'lonlat'; wktName: 'Geodetic'; Description: 'Long/Lat'; DisplayCaption: 'Градусы (Долгота/Широта)'),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic_1SP'; Description: 'Lambert Conformal Conic (1 standard parallel)'; DisplayCaption: ''),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic_2SP'; Description: 'Lambert Conformal Conic (2 standard parallels)'; DisplayCaption: ''),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic'; Description: 'Lambert Conformal Conic'; DisplayCaption: ''),
		(pjKey:'lcca'; wktName: 'Lambert_Conformal_Conic_Alternative'; Description: 'Lambert Conformal Conic Alternative'; DisplayCaption: ''),
		(pjKey:'leac'; wktName: 'Lambert_Equal_Area_Conic'; Description: 'Lambert Equal Area Conic'; DisplayCaption: ''),
		(pjKey:'lee_os'; wktName: 'Lee_Oblated_Stereographic'; Description: 'Lee Oblated Stereographic'; DisplayCaption: ''),
		(pjKey:'loxim'; wktName: 'Loximuthal'; Description: '[ESRI] Loximuthal'; DisplayCaption: ''),
		(pjKey:'lsat'; wktName: 'Space_oblique_for_LANDSAT'; Description: 'Space oblique for LANDSAT'; DisplayCaption: ''),
		(pjKey:'mbt_s'; wktName: 'McBryde_Thomas_Flat_Polar_Sine'; Description: 'McBryde-Thomas Flat-Polar Sine'; DisplayCaption: ''),
		(pjKey:'mbt_fps'; wktName: 'McBryde_Thomas_Flat_Polar_Sine_2'; Description: 'McBryde-Thomas Flat-Pole Sine (No. 2)'; DisplayCaption: ''),
		(pjKey:'mbtfpp'; wktName: 'McBryde_Thomas_Flat_Polar_Parabolic'; Description: 'McBride-Thomas Flat-Polar Parabolic'; DisplayCaption: ''),
		(pjKey:'mbtfpq'; wktName: 'Flat_Polar_Quartic'; Description: '[ESRI] McBryde-Thomas Flat-Polar Quartic'; DisplayCaption: ''),
		(pjKey:'mbtfps'; wktName: 'McBryde_Thomas_Flat_Polar_Sinusoidal'; Description: 'McBryde-Thomas Flat-Polar Sinusoidal'; DisplayCaption: ''),
		(pjKey:'merc'; wktName: 'Mercator'; Description: '[ESRI] Mercator'; DisplayCaption: 'Меркатора'),
		(pjKey:'merc'; wktName: 'Mercator_1SP'; Description: 'Mercator (1 standard parallel)'; DisplayCaption: 'Меркатора'),
		(pjKey:'merc'; wktName: 'Mercator_2SP'; Description: 'Mercator (2 standard parallels)'; DisplayCaption: 'Меркатора'),
		(pjKey:'mil_os'; wktName: 'Miller_Oblated_Stereographic'; Description: 'Miller Oblated Stereographic'; DisplayCaption: 'Стереографическая Миллера'),
		(pjKey:'mill'; wktName: 'Miller_Cylindrical'; Description: 'Miller Cylindrical'; DisplayCaption: 'Циллиндрическая Миллера'),
		(pjKey:'moll'; wktName: 'Mollweide'; Description: 'Mollweide'; DisplayCaption: ''),
		(pjKey:'murd1'; wktName: 'Murdoch_I'; Description: 'Murdoch I'; DisplayCaption: ''),
		(pjKey:'murd2'; wktName: 'Murdoch_II'; Description: 'Murdoch II'; DisplayCaption: ''),
		(pjKey:'murd3'; wktName: 'Murdoch_III'; Description: 'Murdoch III'; DisplayCaption: ''),
		(pjKey:'nell'; wktName: 'Nell'; Description: 'Nell'; DisplayCaption: ''),
		(pjKey:'nell_h'; wktName: 'Nell_Hammer'; Description: 'Nell-Hammer'; DisplayCaption: ''),
		(pjKey:'nicol'; wktName: 'Nicolosi_Globular'; Description: 'Nicolosi Globular'; DisplayCaption: ''),
		(pjKey:'nsper'; wktName: 'Near_sided_perspective'; Description: 'Near-sided perspective'; DisplayCaption: ''),
		(pjKey:'nzmg'; wktName: 'New_Zealand_Map_Grid'; Description: 'New Zealand Map Grid'; DisplayCaption: ''),
		(pjKey:'ob_tran'; wktName: 'General_Oblique_Transformation'; Description: 'General Oblique Transformation'; DisplayCaption: 'Общее косое преобразования'),
		(pjKey:'ocea'; wktName: 'Oblique_Cylindrical_Equal_Area'; Description: 'Oblique Cylindrical Equal Area'; DisplayCaption: ''),
		(pjKey:'oea'; wktName: 'Oblated_Equal_Area'; Description: 'Oblated Equal Area'; DisplayCaption: ''),
		(pjKey:'omerc'; wktName: 'Hotine_Oblique_Mercator'; Description: 'Oblique Mercator'; DisplayCaption: ''),
		(pjKey:'omerc'; wktName: 'Oblique_Mercator'; Description: 'Oblique Mercator'; DisplayCaption: ''),
		(pjKey:'ortel'; wktName: 'Ortelius_Oval'; Description: 'Ortelius Oval'; DisplayCaption: ''),
		(pjKey:'ortho'; wktName: 'Orthographic'; Description: 'Orthographic (ESRI: World from Space)'; DisplayCaption: ''),
		(pjKey:'pconic'; wktName: 'Perspective_Conic'; Description: 'Perspective Conic'; DisplayCaption: ''),
		(pjKey:'poly'; wktName: 'Polyconic'; Description: 'Polyconic (American)'; DisplayCaption: ''),
		(pjKey:'putp1'; wktName: 'Putnins_P1'; Description: 'Putnins P1'; DisplayCaption: ''),
		(pjKey:'putp2'; wktName: 'Putnins_P2'; Description: 'Putnins P2'; DisplayCaption: ''),
		(pjKey:'putp3'; wktName: 'Putnins_P3'; Description: 'Putnins P3'; DisplayCaption: ''),
		(pjKey:'putp3p'; wktName: 'Putnins_P3'''; Description: 'Putnins P3'''; DisplayCaption: ''),
		(pjKey:'putp4p'; wktName: 'Putnins_P4'''; Description: 'Putnins P4'''; DisplayCaption: ''),
		(pjKey:'putp5'; wktName: 'Putnins_P5'; Description: 'Putnins P5'; DisplayCaption: ''),
		(pjKey:'putp5p'; wktName: 'Putnins_P5'''; Description: 'Putnins P5'''; DisplayCaption: ''),
		(pjKey:'putp6'; wktName: 'Putnins_P6'; Description: 'Putnins P6'; DisplayCaption: ''),
		(pjKey:'putp6p'; wktName: 'Putnins_P6'''; Description: 'Putnins P6'''; DisplayCaption: ''),
		(pjKey:'qua_aut'; wktName: 'Quartic_Authalic'; Description: '[ESRI] Quartic Authalic'; DisplayCaption: ''),
		(pjKey:'robin'; wktName: 'Robinson'; Description: 'Robinson'; DisplayCaption: 'Робинсона'),
		(pjKey:'rouss'; wktName: 'Roussilhe_Stereographic'; Description: 'Roussilhe Stereographic'; DisplayCaption: ''),
		(pjKey:'rpoly'; wktName: 'Rectangular_Polyconic'; Description: 'Rectangular Polyconic'; DisplayCaption: ''),
		(pjKey:'sinu'; wktName: 'Sinusoidal'; Description: 'Sinusoidal (Sanson-Flamsteed)'; DisplayCaption: ''),
		(pjKey:'somerc'; wktName: 'Hotine_Oblique_Mercator'; Description: 'Swiss Oblique Mercator'; DisplayCaption: ''),
		(pjKey:'somerc'; wktName: 'Swiss_Oblique_Cylindrical'; Description: 'Swiss Oblique Cylindrical'; DisplayCaption: ''),
		(pjKey:'somerc'; wktName: 'Hotine_Oblique_Mercator_Azimuth_Center'; Description: '[ESRI] Swiss Oblique Mercator/Cylindrical'; DisplayCaption: ''),
		(pjKey:'stere'; wktName: 'Polar_Stereographic'; Description: 'Stereographic'; DisplayCaption: ''),
		(pjKey:'stere'; wktName: 'Stereographic'; Description: '[ESRI] Stereographic'; DisplayCaption: ''),
		(pjKey:'sterea'; wktName: 'Oblique_Stereographic'; Description: 'Oblique Stereographic Alternative'; DisplayCaption: ''),
		(pjKey:'gstmerc'; wktName: 'Gauss_Schreiber_Transverse_Mercator'; Description: 'Gauss-Schreiber Transverse Mercator (aka Gauss-Laborde Reunion)'; DisplayCaption: ''),
		(pjKey:'tcc'; wktName: 'Transverse_Central_Cylindrical'; Description: 'Transverse Central Cylindrical'; DisplayCaption: ''),
		(pjKey:'tcea'; wktName: 'Transverse_Cylindrical_Equal_Area'; Description: 'Transverse Cylindrical Equal Area'; DisplayCaption: ''),
		(pjKey:'tissot'; wktName: 'Tissot_Conic'; Description: 'Tissot Conic'; DisplayCaption: ''),
		(pjKey:'tmerc'; wktName: 'Transverse_Mercator'; Description: 'Transverse Mercator'; DisplayCaption: ''),
		(pjKey:'tmerc'; wktName: 'Gauss_Kruger'; Description: 'Gauss Kruger'; DisplayCaption: ''),
		(pjKey:'tpeqd'; wktName: 'Two_Point_Equidistant'; Description: 'Two Point Equidistant'; DisplayCaption: ''),
		(pjKey:'tpers'; wktName: 'Tilted_perspective'; Description: 'Tilted perspective'; DisplayCaption: ''),
		(pjKey:'ups'; wktName: 'Universal_Polar_Stereographic'; Description: 'Universal Polar Stereographic'; DisplayCaption: ''),
		(pjKey:'urm5'; wktName: 'Urmaev_V'; Description: 'Urmaev V'; DisplayCaption: ''),
		(pjKey:'urmfps'; wktName: 'Urmaev_Flat_Polar_Sinusoidal'; Description: 'Urmaev Flat-Polar Sinusoidal'; DisplayCaption: ''),
		(pjKey:'utm'; wktName: 'Transverse_Mercator'; Description: 'Universal Transverse Mercator (UTM)'; DisplayCaption: ''),
		(pjKey:'vandg'; wktName: 'Van_Der_Grinten_I'; Description: '[ESRI] van der Grinten (I)'; DisplayCaption: ''),
		(pjKey:'vandg'; wktName: 'VanDerGrinten'; Description: 'van der Grinten (I)'; DisplayCaption: ''),
		(pjKey:'vandg2'; wktName: 'VanDerGrinten_II'; Description: 'van der Grinten II'; DisplayCaption: ''),
		(pjKey:'vandg3'; wktName: 'VanDerGrinten_III'; Description: 'van der Grinten III'; DisplayCaption: ''),
		(pjKey:'vandg4'; wktName: 'VanDerGrinten_IV'; Description: 'van der Grinten IV'; DisplayCaption: ''),
		(pjKey:'vitk1'; wktName: 'Vitkovsky_I'; Description: 'Vitkovsky I'; DisplayCaption: 'Витковского'),
		(pjKey:'wag1'; wktName: 'Wagner_I'; Description: 'Wagner I (Kavraisky VI)'; DisplayCaption: ''),
		(pjKey:'wag2'; wktName: 'Wagner_II'; Description: 'Wagner II'; DisplayCaption: ''),
		(pjKey:'wag3'; wktName: 'Wagner_III'; Description: 'Wagner III'; DisplayCaption: ''),
		(pjKey:'wag4'; wktName: 'Wagner_IV'; Description: 'Wagner IV'; DisplayCaption: ''),
		(pjKey:'wag5'; wktName: 'Wagner_V'; Description: 'Wagner V'; DisplayCaption: ''),
		(pjKey:'wag6'; wktName: 'Wagner_VI'; Description: 'Wagner VI'; DisplayCaption: ''),
		(pjKey:'wag7'; wktName: 'Wagner_VII'; Description: 'Wagner VII'; DisplayCaption: ''),
		(pjKey:'weren'; wktName: 'Werenskiold_I'; Description: 'Werenskiold I'; DisplayCaption: ''),
		(pjKey:'wink1'; wktName: 'Winkel_I'; Description: 'Winkel I'; DisplayCaption: ''),
		(pjKey:'wink2'; wktName: 'Winkel_II'; Description: 'Winkel II'; DisplayCaption: ''),
		(pjKey:'wintri'; wktName: 'Winkel_Tripel'; Description: 'Winkel Tripel'; DisplayCaption: '')
	 );

 pjParamsListMax = 79;
 pjParamsList: array[0..pjParamsListMax] of TpjParam =
 (
		// General Projection Parameters'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
    (pjKey:'a'; wktName: ''; Description: 'Радиус большой полуоси (м.)'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
    (pjKey:'b'; wktName: ''; Description: 'Радиус малой полуоси (м.)'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'k_0'; wktName: 'scale_factor'; Description: 'Масштабный коэффициент'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'k'; wktName: 'scale_factor'; Description: 'Масштабный коэффициент'; paramGroup: PJ_PARAM_GROUP_GENERAL), //deprecated
		(pjKey:'lat_0'; wktName: 'latitude_of_origin'; Description: 'Долгота центра'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lat_0'; wktName: 'latitude_of_center'; Description: 'Долгота центра'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lat_0'; wktName: 'central_parallel'; Description: 'Центральная параллель'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lat_1'; wktName: 'standard_parallel_1'; Description: 'Стандартная псевдопараллель 1'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lat_2'; wktName: 'standard_parallel_2'; Description: 'Стандартная псевдопараллель 2'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lat_ts'; wktName: 'standard_parallel_1'; Description: 'Широта центра'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lon_0'; wktName: 'central_meridian'; Description: 'Центральный меридиан'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lon_0'; wktName: 'longitude_of_center'; Description: 'Долгота начальной точки'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'lonc'; wktName: 'longitude_of_center'; Description: 'Долгота начальной точки'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'x_0'; wktName: 'false_easting'; Description: 'Смещение по долготе'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'y_0'; wktName: 'false_northing'; Description: 'Смещение по широте'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		// Additional Projection Parameters'; paramGroup: 0),
		(pjKey:'alpha'; wktName: 'azimuth'; Description: 'Азимут начальной линии'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'azi'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'belgium'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'beta'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'czech'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'gamma'; wktName: 'rectified_grid_angle'; Description: ''; paramGroup: 0),
		(pjKey:'e'; wktName: ''; Description: 'Эксцентриситет'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'es'; wktName: ''; Description: 'Квадрат эксцентриситета'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'ellps'; wktName: ''; Description: 'Эллипсоид'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'geoc'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'datum'; wktName: ''; Description: 'Имя датума'; paramGroup: PJ_PARAM_GROUP_DATUM),
		(pjKey:'guam'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'f'; wktName: ''; Description: 'Сжатие (f)'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'h'; wktName: 'satellite_height'; Description: 'Высота спутника'; paramGroup: 0),
		(pjKey:'lat_b'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'lat_t'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'lon_1'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'lon_2'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'lsat'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'m'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'M'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'n'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'no_defs'; wktName: ''; Description: 'Игнорировать значения по умолчанию'; paramGroup: 0),
		(pjKey:'no_cut'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'no_off'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'no_rot'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'ns'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_alpha'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lat_1'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lat_2'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lat_c'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lat_p'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lon_1'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lon_2'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lon_c'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_lon_p'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'o_proj'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'proj'; wktName: ''; Description: 'Название проекции'; paramGroup: 0),
		(pjKey:'over'; wktName: ''; Description: 'Не ограничивать по долготе'; paramGroup: 0), // Allow longitude output outside -180 to 180 range, disables wrapping
		(pjKey:'p'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'path'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'q'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'R'; wktName: ''; Description: 'Радиус сферы'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
    // ellipsoid spherification paramerets
		(pjKey:'R_A'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // Sphere with the same surface area as the ellipsoid
		(pjKey:'R_V'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // Sphere with same volume as ellipsoid
		(pjKey:'R_a'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), //R_a = (a + b)/2)
		(pjKey:'R_g'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // R = sqrt(a*b)
		(pjKey:'R_h'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // R = 2*a*b/(a+b)
		(pjKey:'R_lat_a'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // Arithmetic mean of of the corresponding ellipsoid at given latitude.
		(pjKey:'R_lat_g'; wktName: ''; Description: ''; paramGroup: PJ_PARAM_GROUP_ELLIPSOID), // Geometric mean of of the corresponding ellipsoid at given latitude.
    //
		(pjKey:'rot'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'rf'; wktName: ''; Description: 'Обратное сжатие (1/f)'; paramGroup: PJ_PARAM_GROUP_ELLIPSOID),
		(pjKey:'s'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'sym'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'t'; wktName: ''; Description: ''; paramGroup: 0),
    (pjKey:'th'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'theta'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'to_meter'; wktName: ''; Description: 'Коэфициент перевода в метры'; paramGroup: PJ_PARAM_GROUP_GENERAL),
		(pjKey:'vto_meter'; wktName: ''; Description: ''), // Vertical plane coordinate scaling. Internal unit
		(pjKey:'towgs84'; wktName: ''; Description: 'Параметры перехода к WGS84'; paramGroup: PJ_PARAM_GROUP_DATUM),
		(pjKey:'tilt'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'units'; wktName: ''; Description: 'Единицы проекции'; paramGroup: 0),
		(pjKey:'vopt'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'W'; wktName: ''; Description: ''; paramGroup: 0),
		(pjKey:'westo'; wktName: ''; Description: ''; paramGroup: 0)
	);

type
  pjMapInfoDatumInfo = record
    EPSGCode: Integer;
    ID: Integer;
    OGCName: string;
    EllID: Integer;
    dX: Double;
    dY: Double;
    dZ: Double;
    rX: Double;
    rY: Double;
    rZ: Double;
    ScaleFactor: Double;
    PrimeMeridian: Double;
end;

const
  cMapInfoDatumInfoListCount = 198;
  pjMapInfoDatumInfoList: array[0..cMapInfoDatumInfoListCount -1] of pjMapInfoDatumInfo = (
    (EPSGCode: 0; ID: 104; OGCName: 'WGS_1984'; EllID: 28; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6269; ID: 74; OGCName: 'North_American_Datum_1983'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 0; OGCName: ''; EllID: 29; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0), // Datum ignore
    (EPSGCode: 6201; ID: 1; OGCName: 'Adindan'; EllID: 6; dX: -162; dY: -12; dZ: 206; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6205; ID: 2; OGCName: 'Afgooye'; EllID: 3; dX: -43; dY: -163; dZ: 45; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6204; ID: 3; OGCName: 'Ain_el_Abd_1970'; EllID: 4; dX: -150; dY: -251; dZ: -2; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 4; OGCName: 'Anna_1_Astro_1965'; EllID: 2; dX: -491; dY: -22; dZ: 435; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6209; ID: 5; OGCName: 'Arc_1950'; EllID: 15; dX: -143; dY: -90; dZ: -294; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6210; ID: 6; OGCName: 'Arc_1960'; EllID: 6; dX: -160; dY: -8; dZ: -300; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 7; OGCName: 'Ascension_Islands'; EllID: 4; dX: -207; dY: 107; dZ: 52; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 8; OGCName: 'Astro_Beacon_E'; EllID: 4; dX: 145; dY: 75; dZ: -272; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 9; OGCName: 'Astro_B4_Sorol_Atoll'; EllID: 4; dX: 114; dY: -116; dZ: -333; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 10; OGCName: 'Astro_Dos_71_4'; EllID: 4; dX: -320; dY: 550; dZ: -494; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 11; OGCName: 'Astronomic_Station_1952'; EllID: 4; dX: 124; dY: -234; dZ: -25; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6202; ID: 12; OGCName: 'Australian_Geodetic_Datum_66'; EllID: 2; dX: -133; dY: -48; dZ: 148; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6203; ID: 13; OGCName: 'Australian_Geodetic_Datum_84'; EllID: 2; dX: -134; dY: -48; dZ: 149; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 14; OGCName: 'Bellevue_Ign'; EllID: 4; dX: -127; dY: -769; dZ: 472; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6216; ID: 15; OGCName: 'Bermuda_1957'; EllID: 7; dX: -73; dY: 213; dZ: 296; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6218; ID: 16; OGCName: 'Bogota'; EllID: 4; dX: 307; dY: 304; dZ: -318; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6221; ID: 17; OGCName: 'Campo_Inchauspe'; EllID: 4; dX: -148; dY: 136; dZ: 90; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 18; OGCName: 'Canton_Astro_1966'; EllID: 4; dX: 298; dY: -304; dZ: -375; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6222; ID: 19; OGCName: 'Cape'; EllID: 6; dX: -136; dY: -108; dZ: -292; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6717; ID: 20; OGCName: 'Cape_Canaveral'; EllID: 7; dX: -2; dY: 150; dZ: 181; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6223; ID: 21; OGCName: 'Carthage'; EllID: 6; dX: -263; dY: 6; dZ: 431; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6672; ID: 22; OGCName: 'Chatham_1971'; EllID: 4; dX: 175; dY: -38; dZ: 113; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6224; ID: 23; OGCName: 'Chua'; EllID: 4; dX: -134; dY: 229; dZ: -29; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6225; ID: 24; OGCName: 'Corrego_Alegre'; EllID: 4; dX: -206; dY: 172; dZ: -6; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6211; ID: 25; OGCName: 'Batavia'; EllID: 10; dX: -377; dY: 681; dZ: -50; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 26; OGCName: 'Dos_1968'; EllID: 4; dX: 230; dY: -199; dZ: -752; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6719; ID: 27; OGCName: 'Easter_Island_1967'; EllID: 4; dX: 211; dY: 147; dZ: 111; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6230; ID: 28; OGCName: 'European_Datum_1950'; EllID: 4; dX: -87; dY: -98; dZ: -121; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6668; ID: 29; OGCName: 'European_Datum_1979'; EllID: 4; dX: -86; dY: -98; dZ: -119; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6233; ID: 30; OGCName: 'Gandajika_1970'; EllID: 4; dX: -133; dY: -321; dZ: 50; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6272; ID: 31; OGCName: 'New_Zealand_GD49'; EllID: 4; dX: 84; dY: -22; dZ: 209; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6272; ID: 31; OGCName: 'New_Zealand_Geodetic_Datum_1949'; EllID: 4; dX: 84; dY: -22; dZ: 209; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 32; OGCName: 'GRS_67'; EllID: 21; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 33; OGCName: 'GRS_80'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6171; ID: 33; OGCName: 'Reseau_Geodesique_Francais_1993'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6619; ID: 33; OGCName: 'SWEREF99'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6675; ID: 34; OGCName: 'Guam_1963'; EllID: 7; dX: -100; dY: -248; dZ: 259; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 35; OGCName: 'Gux_1_Astro'; EllID: 4; dX: 252; dY: -209; dZ: -751; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6254; ID: 36; OGCName: 'Hito_XVIII_1963'; EllID: 4; dX: 16; dY: 196; dZ: 93; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6658; ID: 37; OGCName: 'Hjorsey_1955'; EllID: 4; dX: -73; dY: 46; dZ: -86; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6738; ID: 38; OGCName: 'Hong_Kong_1963'; EllID: 4; dX: -156; dY: -271; dZ: -189; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6236; ID: 39; OGCName: 'Hu_Tzu_Shan'; EllID: 4; dX: -634; dY: -549; dZ: -201; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 40; OGCName: 'Indian_Thailand_Vietnam'; EllID: 11; dX: 214; dY: 836; dZ: 303; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 41; OGCName: 'Indian_Bangladesh'; EllID: 11; dX: 289; dY: 734; dZ: 257; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6299; ID: 42; OGCName: 'Ireland_1965'; EllID: 13; dX: 506; dY: -122; dZ: 611; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 43; OGCName: 'ISTS_073_Astro_1969'; EllID: 4; dX: 208; dY: -435; dZ: -229; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6725; ID: 44; OGCName: 'Johnston_Island_1961'; EllID: 4; dX: 191; dY: -77; dZ: -204; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6244; ID: 45; OGCName: 'Kandawala'; EllID: 11; dX: -97; dY: 787; dZ: 86; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 46; OGCName: 'Kerguyelen_Island'; EllID: 4; dX: 145; dY: -187; dZ: 103; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6245; ID: 47; OGCName: 'Kertau'; EllID: 17; dX: -11; dY: 851; dZ: 5; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 48; OGCName: 'L_C_5_Astro'; EllID: 7; dX: 42; dY: 124; dZ: 147; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6251; ID: 49; OGCName: 'Liberia_1964'; EllID: 6; dX: -90; dY: 40; dZ: 88; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 50; OGCName: 'Luzon_Phillippines'; EllID: 7; dX: -133; dY: -77; dZ: -51; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 51; OGCName: 'Luzon_Mindanao_Island'; EllID: 7; dX: -133; dY: -79; dZ: -72; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6256; ID: 52; OGCName: 'Mahe_1971'; EllID: 6; dX: 41; dY: -220; dZ: -134; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 53; OGCName: 'Marco_Astro'; EllID: 4; dX: -289; dY: -124; dZ: 60; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6262; ID: 54; OGCName: 'Massawa'; EllID: 10; dX: 639; dY: 405; dZ: 60; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6261; ID: 55; OGCName: 'Merchich'; EllID: 16; dX: 31; dY: 146; dZ: 47; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 56; OGCName: 'Midway_Astro_1961'; EllID: 4; dX: 912; dY: -58; dZ: 1227; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6263; ID: 57; OGCName: 'Minna'; EllID: 6; dX: -92; dY: -93; dZ: 122; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 58; OGCName: 'Nahrwan_Masirah_Island'; EllID: 6; dX: -247; dY: -148; dZ: 369; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 59; OGCName: 'Nahrwan_Un_Arab_Emirates'; EllID: 6; dX: -249; dY: -156; dZ: 381; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 60; OGCName: 'Nahrwan_Saudi_Arabia'; EllID: 6; dX: -231; dY: -196; dZ: 482; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6271; ID: 61; OGCName: 'Naparima_1972'; EllID: 4; dX: -2; dY: 374; dZ: 172; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6267; ID: 62; OGCName: 'NAD_1927'; EllID: 7; dX: -8; dY: 160; dZ: 176; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6267; ID: 62; OGCName: 'North_American_Datum_1927'; EllID: 7; dX: -8; dY: 160; dZ: 176; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 63; OGCName: 'NAD_27_Alaska'; EllID: 7; dX: -5; dY: 135; dZ: 172; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 64; OGCName: 'NAD_27_Bahamas'; EllID: 7; dX: -4; dY: 154; dZ: 178; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 65; OGCName: 'NAD_27_San_Salvador'; EllID: 7; dX: 1; dY: 140; dZ: 165; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 66; OGCName: 'NAD_27_Canada'; EllID: 7; dX: -10; dY: 158; dZ: 187; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 67; OGCName: 'NAD_27_Canal_Zone'; EllID: 7; dX: 0; dY: 125; dZ: 201; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 68; OGCName: 'NAD_27_Caribbean'; EllID: 7; dX: -7; dY: 152; dZ: 178; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 69; OGCName: 'NAD_27_Central_America'; EllID: 7; dX: 0; dY: 125; dZ: 194; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 70; OGCName: 'NAD_27_Cuba'; EllID: 7; dX: -9; dY: 152; dZ: 178; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 71; OGCName: 'NAD_27_Greenland'; EllID: 7; dX: 11; dY: 114; dZ: 195; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 72; OGCName: 'NAD_27_Mexico'; EllID: 7; dX: -12; dY: 130; dZ: 190; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 73; OGCName: 'NAD_27_Michigan'; EllID: 8; dX: -8; dY: 160; dZ: 176; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 75; OGCName: 'Observatorio_1966'; EllID: 4; dX: -425; dY: -169; dZ: 81; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 76; OGCName: 'Old_Egyptian'; EllID: 22; dX: -130; dY: 110; dZ: -13; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6135; ID: 77; OGCName: 'Old_Hawaiian'; EllID: 7; dX: 61; dY: -285; dZ: -181; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 78; OGCName: 'Oman'; EllID: 6; dX: -346; dY: -1; dZ: 224; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6277; ID: 79; OGCName: 'OSGB_1936'; EllID: 9; dX: 375; dY: -111; dZ: 431; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 80; OGCName: 'Pico_De_Las_Nieves'; EllID: 4; dX: -307; dY: -92; dZ: 127; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6729; ID: 81; OGCName: 'Pitcairn_Astro_1967'; EllID: 4; dX: 185; dY: 165; dZ: 42; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6248; ID: 82; OGCName: 'Provisional_South_American'; EllID: 4; dX: -288; dY: 175; dZ: -376; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6139; ID: 83; OGCName: 'Puerto_Rico'; EllID: 7; dX: 11; dY: 72; dZ: -101; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6614; ID: 84; OGCName: 'Qatar_National'; EllID: 4; dX: -128; dY: -283; dZ: 22; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6287; ID: 85; OGCName: 'Qornoq'; EllID: 4; dX: 164; dY: 138; dZ: -189; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6627; ID: 86; OGCName: 'Reunion'; EllID: 4; dX: 94; dY: -948; dZ: -1262; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6265; ID: 87; OGCName: 'Monte_Mario'; EllID: 4; dX: -225; dY: -65; dZ: 9; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 88; OGCName: 'Santo_Dos'; EllID: 4; dX: 170; dY: 42; dZ: 84; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 89; OGCName: 'Sao_Braz'; EllID: 4; dX: -203; dY: 141; dZ: 53; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6292; ID: 90; OGCName: 'Sapper_Hill_1943'; EllID: 4; dX: -355; dY: 16; dZ: 74; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6293; ID: 91; OGCName: 'Schwarzeck'; EllID: 14; dX: 616; dY: 97; dZ: -251; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6618; ID: 92; OGCName: 'South_American_Datum_1969'; EllID: 24; dX: -57; dY: 1; dZ: -41; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 93; OGCName: 'South_Asia'; EllID: 19; dX: 7; dY: -10; dZ: -26; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 94; OGCName: 'Southeast_Base'; EllID: 4; dX: -499; dY: -249; dZ: 314; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 95; OGCName: 'Southwest_Base'; EllID: 4; dX: -104; dY: 167; dZ: -38; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6298; ID: 96; OGCName: 'Timbalai_1948'; EllID: 11; dX: -689; dY: 691; dZ: -46; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6301; ID: 97; OGCName: 'Tokyo'; EllID: 10; dX: -128; dY: 481; dZ: 664; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 98; OGCName: 'Tristan_Astro_1968'; EllID: 4; dX: -632; dY: 438; dZ: -609; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6731; ID: 99; OGCName: 'Viti_Levu_1916'; EllID: 6; dX: 51; dY: 391; dZ: -36; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 100; OGCName: 'Wake_Entiwetok_1960'; EllID: 23; dX: 101; dY: 52; dZ: -39; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 101; OGCName: 'WGS_60'; EllID: 26; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6760; ID: 102; OGCName: 'WGS_66'; EllID: 27; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6322; ID: 103; OGCName: 'WGS_1972'; EllID: 1; dX: 0; dY: 8; dZ: 10; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6322; ID: 103; OGCName: 'World_Geodetic_System_1972'; EllID: 1; dX: 0; dY: 8; dZ: 10; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6326; ID: 104; OGCName: 'WGS_1984'; EllID: 28; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6309; ID: 105; OGCName: 'Yacare'; EllID: 4; dX: -155; dY: 171; dZ: 37; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6311; ID: 106; OGCName: 'Zanderij'; EllID: 4; dX: -265; dY: 120; dZ: -358; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 107; OGCName: 'NTF'; EllID: 30; dX: -168; dY: -60; dZ: 320; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6231; ID: 108; OGCName: 'European_Datum_1987'; EllID: 4; dX: -83; dY: -96; dZ: -113; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 109; OGCName: 'Netherlands_Bessel'; EllID: 10; dX: 593; dY: 26; dZ: 478; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 110; OGCName: 'Belgium_Hayford'; EllID: 4; dX: 81; dY: 120; dZ: 129; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 111; OGCName: 'NWGL_10'; EllID: 1; dX: -1; dY: 15; dZ: 1; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6124; ID: 112; OGCName: 'Rikets_koordinatsystem_1990'; EllID: 10; dX: 498; dY: -36; dZ: 568; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 113; OGCName: 'Lisboa_DLX'; EllID: 4; dX: -303; dY: -62; dZ: 105; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 114; OGCName: 'Melrica_1973_D73'; EllID: 4; dX: -223; dY: 110; dZ: 37; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6258; ID: 115; OGCName: 'Euref_89'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6283; ID: 116; OGCName: 'GDA94'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6283; ID: 116; OGCName: 'Geocentric_Datum_of_Australia_1994'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6167; ID: 117; OGCName: 'NZGD2000'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6167; ID: 117; OGCName: 'New_Zealand_Geodetic_Datum_2000'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6169; ID: 118; OGCName: 'America_Samoa'; EllID: 7; dX: -115; dY: 118; dZ: 426; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 119; OGCName: 'Antigua_Astro_1965'; EllID: 6; dX: -270; dY: 13; dZ: 62; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6713; ID: 120; OGCName: 'Ayabelle_Lighthouse'; EllID: 6; dX: -79; dY: -129; dZ: 145; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6219; ID: 121; OGCName: 'Bukit_Rimpah'; EllID: 10; dX: -384; dY: 664; dZ: -48; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 122; OGCName: 'Estonia_1937'; EllID: 10; dX: 374; dY: 150; dZ: 588; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6155; ID: 123; OGCName: 'Dabola'; EllID: 6; dX: -83; dY: 37; dZ: 124; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6736; ID: 124; OGCName: 'Deception_Island'; EllID: 6; dX: 260; dY: 12; dZ: -147; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 125; OGCName: 'Fort_Thomas_1955'; EllID: 6; dX: -7; dY: 215; dZ: 225; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 126; OGCName: 'Graciosa_base_1948'; EllID: 4; dX: -104; dY: 167; dZ: -38; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6255; ID: 127; OGCName: 'Herat_North'; EllID: 4; dX: -333; dY: -222; dZ: 114; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 128; OGCName: 'Hermanns_Kogel'; EllID: 10; dX: 682; dY: -203; dZ: 480; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6240; ID: 129; OGCName: 'Indian'; EllID: 50; dX: 283; dY: 682; dZ: 231; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6239; ID: 130; OGCName: 'Indian_1954'; EllID: 11; dX: 217; dY: 823; dZ: 299; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6131; ID: 131; OGCName: 'Indian_1960'; EllID: 11; dX: 198; dY: 881; dZ: 317; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6240; ID: 132; OGCName: 'Indian_1975'; EllID: 11; dX: 210; dY: 814; dZ: 289; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6238; ID: 133; OGCName: 'Indonesian_Datum_1974'; EllID: 4; dX: -24; dY: -15; dZ: 5; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 134; OGCName: 'ISTS061_Astro_1968'; EllID: 4; dX: -794; dY: 119; dZ: -298; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 135; OGCName: 'Kusaie_Astro_1951'; EllID: 4; dX: 647; dY: 1777; dZ: -1124; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6250; ID: 136; OGCName: 'Leigon'; EllID: 6; dX: -130; dY: 29; dZ: 364; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 137; OGCName: 'Montserrat_Astro_1958'; EllID: 6; dX: 174; dY: 359; dZ: 365; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6266; ID: 138; OGCName: 'Mporaloko'; EllID: 6; dX: -74; dY: -130; dZ: 42; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 139; OGCName: 'North_Sahara_1959'; EllID: 6; dX: -186; dY: -93; dZ: 310; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 140; OGCName: 'Observatorio_Met_1939'; EllID: 4; dX: -425; dY: -169; dZ: 81; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6620; ID: 141; OGCName: 'Point_58'; EllID: 6; dX: -106; dY: -129; dZ: 165; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6282; ID: 142; OGCName: 'Pointe_Noire'; EllID: 6; dX: -148; dY: 51; dZ: -291; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6615; ID: 143; OGCName: 'Porto_Santo_1936'; EllID: 4; dX: -499; dY: -249; dZ: 314; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6616; ID: 144; OGCName: 'Selvagem_Grande_1938'; EllID: 4; dX: -289; dY: -124; dZ: 60; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 145; OGCName: 'Sierra_Leone_1960'; EllID: 6; dX: -88; dY: 4; dZ: 101; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6156; ID: 146; OGCName: 'S_JTSK_Ferro'; EllID: 10; dX: 589; dY: 76; dZ: 480; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6297; ID: 147; OGCName: 'Tananarive_1925'; EllID: 4; dX: -189; dY: -242; dZ: -91; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6811; ID: 148; OGCName: 'Voirol_1874'; EllID: 6; dX: -73; dY: -247; dZ: 227; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 149; OGCName: 'Virol_1960'; EllID: 6; dX: -123; dY: -206; dZ: 219; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6148; ID: 150; OGCName: 'Hartebeesthoek94'; EllID: 28; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6122; ID: 151; OGCName: 'ATS77'; EllID: 51; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6612; ID: 152; OGCName: 'JGD2000'; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 153; OGCName: 'HGRS87'; EllID: 0; dX: -199.87; dY: 74.79; dZ: 246.62; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6214; ID: 154; OGCName: 'Beijing 1954'; EllID: 3; dX: -31.4; dY: 144.3; dZ: 81.2; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6754; ID: 155; OGCName: 'Libya (LGD 2006)'; EllID: 4; dX: 208.4058; dY: 109.8777; dZ: 2.5764; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6317; ID: 156; OGCName: 'Dealul Piscului 1970'; EllID: 3; dX: 28; dY: -121; dZ: -77; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 157; OGCName: 'WGS_1984'; EllID: 54; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 6150; ID: 158; OGCName: 'CH1903+ datum for Switzerland'; EllID: 10; dX: 674.374; dY: 15.056; dZ: 405.346; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 159; OGCName: 'Schwarzeck (updated) datum for Namibia'; EllID: 14; dX: 616.8; dY: 103.3; dZ: -256.9; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0 ),
    (EPSGCode: 0; ID: 161; OGCName: 'NOAA GCS_Sphere'; EllID: 55; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0 ),
    (EPSGCode: 0; ID: 1000; OGCName: 'DHDN_Potsdam_Rauenberg'; EllID: 10; dX: 582; dY: 105; dZ: 414; rX: -1.04; rY: -0.35; rZ: 3.08; ScaleFactor: 8.3; PrimeMeridian: 0),
    (EPSGCode: 6284; ID: 1001; OGCName: 'Pulkovo_1942'; EllID: 3; dX: 24; dY: -123; dZ: -94; rX: -0.02; rY: 0.25; rZ: 0.13; ScaleFactor: 1.1; PrimeMeridian: 0),
    (EPSGCode: 6807; ID: 1002; OGCName: 'NTF_Paris_Meridian'; EllID: 30; dX: -168; dY: -60; dZ: 320; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 2.337229166667),
    (EPSGCode: 6149; ID: 1003; OGCName: 'Switzerland_CH_1903'; EllID: 10; dX: 660.077; dY: 13.551; dZ: 369.344; rX: 0.804816; rY: 0.577692; rZ: 0.952236; ScaleFactor: 5.66; PrimeMeridian: 0),
    (EPSGCode: 6237; ID: 1004; OGCName: 'Hungarian_Datum_1972'; EllID: 21; dX: -56; dY: 75.77; dZ: 15.31; rX: -0.37; rY: -0.2; rZ: -0.21; ScaleFactor: -1.01; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1005; OGCName: 'Cape_7_Parameter'; EllID: 28; dX: -134.73; dY: -110.92; dZ: -292.66; rX: 0; rY: 0; rZ: 0; ScaleFactor: 1; PrimeMeridian: 0),
    (EPSGCode: 6203; ID: 1006; OGCName: 'AGD84_7_Param_Aust'; EllID: 2; dX: -117.763; dY: -51.51; dZ: 139.061; rX: -0.292; rY: -0.443; rZ: -0.277; ScaleFactor: -0.191; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1007; OGCName: 'AGD66_7_Param_ACT'; EllID: 2; dX: -129.193; dY: -41.212; dZ: 130.73; rX: -0.246; rY: -0.374; rZ: -0.329; ScaleFactor: -2.955; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1008; OGCName: 'AGD66_7_Param_TAS'; EllID: 2; dX: -120.271; dY: -64.543; dZ: 161.632; rX: -0.2175; rY: 0.0672; rZ: 0.1291; ScaleFactor: 2.4985; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1009; OGCName: 'AGD66_7_Param_VIC_NSW'; EllID: 2; dX: -119.353; dY: -48.301; dZ: 139.484; rX: -0.415; rY: -0.26; rZ: -0.437; ScaleFactor: -0.613; PrimeMeridian: 0),
    (EPSGCode: 6272; ID: 1010; OGCName: 'NZGD_7_Param_49'; EllID: 4; dX: 59.47; dY: -5.04; dZ: 187.44; rX: -0.47; rY: 0.1; rZ: -1.024; ScaleFactor: -4.5993; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1011; OGCName: 'Rikets_Tri_7_Param_1990'; EllID: 10; dX: 419.3836; dY: 99.3335; dZ: 591.3451; rX: -0.850389; rY: -1.817277; rZ: 7.862238; ScaleFactor: -0.99496; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1012; OGCName: 'Russia_PZ90'; EllID: 52; dX: -1.08; dY: -0.27; dZ: -0.9; rX: 0; rY: 0; rZ: -0.16; ScaleFactor: -0.12; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1013; OGCName: 'Russia_SK42'; EllID: 52; dX: 23.92; dY: -141.27; dZ: -80.9; rX: 0; rY: -0.35; rZ: -0.82; ScaleFactor: -0.12; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1014; OGCName: 'Russia_SK95'; EllID: 52; dX: 24.82; dY: -131.21; dZ: -82.66; rX: 0; rY: 0; rZ: -0.16; ScaleFactor: -0.12; PrimeMeridian: 0),
    (EPSGCode: 6301; ID: 1015; OGCName: 'Tokyo'; EllID: 10; dX: -146.414; dY: 507.337; dZ: 680.507; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1016; OGCName: 'Finnish_KKJ'; EllID: 4; dX: -96.062; dY: -82.428; dZ: -121.754; rX: -4.801; rY: -0.345; rZ: 1.376; ScaleFactor: 1.496; PrimeMeridian: 0),
    (EPSGCode: 6610; ID: 1017; OGCName: 'Xian 1980'; EllID: 53; dX: 24; dY: -123; dZ: -94; rX: -0.02; rY: -0.25; rZ: 0.13; ScaleFactor: 1.1; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1018; OGCName: 'Lithuanian Pulkovo 1942'; EllID: 4; dX: -40.59527; dY: -18.54979; dZ: -69.33956; rX: -2.508; rY: -1.8319; rZ: 2.6114; ScaleFactor: -4.2991; PrimeMeridian: 0),
    (EPSGCode: 6313; ID: 1019; OGCName: 'Belgian 1972 7 Parameter'; EllID: 4; dX: -99.059; dY: 53.322; dZ: -112.486; rX: -0.419; rY: 0.83; rZ: -1.885; ScaleFactor: 0.999999; PrimeMeridian: 0),
    (EPSGCode: 6818; ID: 1020; OGCName: 'S-JTSK with Ferro prime meridian'; EllID: 10; dX: 589; dY: 76; dZ: 480; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: -17.666666666667),
    (EPSGCode: 1031; ID: 1021; OGCName: 'Serbia datum MGI 1901'; EllID: 10; dX: 574.027; dY: 170.175; dZ: 401.545; rX: 4.88786; rY: -0.66524; rZ: -13.24673; ScaleFactor: 6.88933; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1022; OGCName: 'North Sahara 7-parameter'; EllID: 6; dX: -38.7086; dY: -128.8054; dZ: 118.8837; rX: 0.83822; rY: 7.38459; rZ: -1.57989; ScaleFactor: 3.9904; PrimeMeridian: 0),
    (EPSGCode: 0; ID: 1023; OGCName: 'Hungarian Projection System (EOV) - updated'; EllID: 21; dX: 52.684; dY: -71.194; dZ: -13.975; rX: 0.312; rY: 0.1063; rZ: 0.3729; ScaleFactor: 1.0191; PrimeMeridian: 0 ),
    (EPSGCode: 1052; ID: 1024; OGCName: 'S-JTSK (Krovak) Coordinate system - updated'; EllID: 10; dX: 570.6934; dY: 85.6936; dZ: 462.8393; rX: -4.99825; rY: -1.58663; rZ: -5.26114; ScaleFactor: 3.5430155; PrimeMeridian: 0 ),
    (EPSGCode: 0; ID: 1025; OGCName: 'JTSK03 (Slovak Republic)'; EllID: 10; dX: 485.014055; dY: 169.473618; dZ: 483.842943; rX: -7.78625453; rY: -4.39770887; rZ: -4.10248899; ScaleFactor: 0; PrimeMeridian: 0 ),
    (EPSGCode: 0; ID: 9999; OGCName: 'Bosnia-Herzegovina'; EllID: 10; dX: 472.8677; dY: 187.8769; dZ: 544.7084; rX: -5.76198422; rY: -5.3222842; rZ: 12.80666941; ScaleFactor: 1.54517287; PrimeMeridian: 0 ),
    (EPSGCode: 6181; ID: 9999; OGCName: 'Luxembourg 1930 / Gauss'; EllID: 4; dX: -192.986; dY: 13.673; dZ: -39.309; rX: 0.4099; rY: 2.9332; rZ: -2.6881; ScaleFactor: 0.43; PrimeMeridian: 0 ),
    (EPSGCode: -1; ID: -1; OGCName: ''; EllID: 0; dX: 0; dY: 0; dZ: 0; rX: 0; rY: 0; rZ: 0; ScaleFactor: 0; PrimeMeridian: 0) // Datum ignore
  );

type
  pjMapInfoSpheroidInfo = record
    ID: Integer;
    Name: string;
    A: Double;
    RF: Double;
  end;

const
  pjMapInfoSpheroidInfoList: array[0..53] of pjMapInfoSpheroidInfo = (
    (ID: 9; Name: 'Airy 1930'; A: 6377563.396; RF: 299.3249646),
    (ID: 13; Name: 'Airy 1930 (modified for Ireland 1965'; A: 6377340.189; RF: 299.3249646),
    (ID: 51; Name: 'ATS77 (Average Terrestrial System 1977)'; A: 6378135; RF: 298.257),
    (ID: 2; Name: 'Australian'; A: 6378160.0; RF: 298.25),
    (ID: 10; Name: 'Bessel 1841'; A: 6377397.155; RF: 299.1528128),
    (ID: 35; Name: 'Bessel 1841 (modified for NGO 1948)'; A: 6377492.0176; RF: 299.15281),
    (ID: 14; Name: 'Bessel 1841 (modified for Schwarzeck)'; A: 6377483.865; RF: 299.1528128),
    (ID: 36; Name: 'Clarke 1858'; A: 6378293.639; RF: 294.26068),
    (ID: 7; Name: 'Clarke 1866'; A: 6378206.4; RF: 294.9786982),
    (ID: 8; Name: 'Clarke 1866 (modified for Michigan)'; A: 6378450.047484481; RF: 294.9786982),
    (ID: 6; Name: 'Clarke 1880'; A: 6378249.145; RF: 293.465),
    (ID: 15; Name: 'Clarke 1880 (modified for Arc 1950)'; A: 6378249.145326; RF: 293.4663076),
    (ID: 30; Name: 'Clarke 1880 (modified for IGN)'; A: 6378249.2; RF: 293.4660213),
    (ID: 37; Name: 'Clarke 1880 (modified for Jamaica)'; A: 6378249.136; RF: 293.46631),
    (ID: 16; Name: 'Clarke 1880 (modified for Merchich)'; A: 6378249.2; RF: 293.46598),
    (ID: 38; Name: 'Clarke 1880 (modified for Palestine)'; A: 6378300.79; RF: 293.46623),
    (ID: 39; Name: 'Everest (Brunei and East Malaysia)'; A: 6377298.556; RF: 300.8017),
    (ID: 11; Name: 'Everest (India 1830)'; A: 6377276.345; RF: 300.8017),
    (ID: 40; Name: 'Everest (India 1956)'; A: 6377301.243; RF: 300.80174),
    (ID: 50; Name: 'Everest (Pakistan)'; A: 6377309.613; RF: 300.8017),
    (ID: 17; Name: 'Everest (W. Malaysia and Singapore 1948)'; A: 6377304.063; RF: 300.8017),
    (ID: 48; Name: 'Everest (West Malaysia 1969)'; A: 6377304.063; RF: 300.8017),
    (ID: 18; Name: 'Fischer 1960'; A: 6378166.0; RF: 298.3),
    (ID: 19; Name: 'Fischer 1960 (modified for South Asia)'; A: 6378155.0; RF: 298.3),
    (ID: 20; Name: 'Fischer 1968'; A: 6378150.0; RF: 298.3),
    (ID: 21; Name: 'GRS 67'; A: 6378160.0; RF: 298.247167427),
    (ID: 0; Name: 'GRS 80'; A: 6378137.0; RF: 298.257222101),
    (ID: 5; Name: 'Hayford'; A: 6378388.0; RF: 297.0),
    (ID: 22; Name: 'Helmert 1906'; A: 6378200.0; RF: 298.3),
    (ID: 23; Name: 'Hough'; A: 6378270.0; RF: 297.0),
    (ID: 31; Name: 'IAG 75'; A: 6378140.0; RF: 298.257222),
    (ID: 41; Name: 'Indonesian'; A: 6378160.0; RF: 298.247),
    (ID: 4; Name: 'International 1924'; A: 6378388.0; RF: 297.0),
    (ID: 49; Name: 'Irish (WOFO)'; A: 6377542.178; RF: 299.325),
    (ID: 3; Name: 'Krassovsky'; A: 6378245.0; RF: 298.3),
    (ID: 32; Name: 'MERIT 83'; A: 6378137.0; RF: 298.257),
    (ID: 33; Name: 'New International 1967'; A: 6378157.5; RF: 298.25),
    (ID: 42; Name: 'NWL 9D'; A: 6378145.0; RF: 298.25),
    (ID: 43; Name: 'NWL 10D'; A: 6378135.0; RF: 298.26),
    (ID: 44; Name: 'OSU86F'; A: 6378136.2; RF: 298.25722),
    (ID: 45; Name: 'OSU91A'; A: 6378136.3; RF: 298.25722),
    (ID: 46; Name: 'Plessis 1817'; A: 6376523.0; RF: 308.64),
    (ID: 52; Name: 'PZ90'; A: 6378136.0; RF: 298.257839303),
    (ID: 24; Name: 'South American'; A: 6378160.0; RF: 298.25),
    (ID: 12; Name: 'Sphere'; A: 6370997.0; RF: 0.0),
    (ID: 47; Name: 'Struve 1860'; A: 6378297.0; RF: 294.73),
    (ID: 34; Name: 'Walbeck'; A: 6376896.0; RF: 302.78),
    (ID: 25; Name: 'War Office'; A: 6378300.583; RF: 296.0),
    (ID: 26; Name: 'WGS 60'; A: 6378165.0; RF: 298.3),
    (ID: 27; Name: 'WGS 66'; A: 6378145.0; RF: 298.25),
    (ID: 1; Name: 'WGS 72'; A: 6378135.0; RF: 298.26),
    (ID: 28; Name: 'WGS 84'; A: 6378137.0; RF: 298.257223563),
    (ID: 29; Name: 'WGS 84 (MAPINFO Datum 0)'; A: 6378137.01; RF: 298.257223563),
    (ID: 54; Name: 'WGS 84 (MAPINFO Datum 157)'; A: 6378137.01; RF: 298.257223563)
  );

type
  TMapInfoUnitInfo = record
    ID: Integer;
    MapInfoName: string;
    ProjName: string;
    ToMeters: Double;
  end;

const
  pjMapInfoUnitsList: array[0..11] of TMapInfoUnitInfo = (
    (ID: 0;  MapInfoName: 'mi';        ProjName: 'mi';    ToMeters: 1609.344),
    (ID: 1;  MapInfoName: 'km';        ProjName: 'km';    ToMeters: 1000.0),
    (ID: 2;  MapInfoName: 'in';        ProjName: 'in';    ToMeters: 0.0254),
    (ID: 3;  MapInfoName: 'ft';        ProjName: 'ft';    ToMeters: 0.3048),
    (ID: 4;  MapInfoName: 'yd';        ProjName: 'yd';    ToMeters: 0.9144),
    (ID: 5;  MapInfoName: 'mm';        ProjName: 'mm';    ToMeters: 0.001),
    (ID: 6;  MapInfoName: 'cm';        ProjName: 'cm';    ToMeters: 0.01),
    (ID: 7;  MapInfoName: 'm';         ProjName: 'm';     ToMeters: 1.0),
    (ID: 8;  MapInfoName: 'survey ft'; ProjName: 'us-ft'; ToMeters: 0.3048006096012192),
    (ID: 9;  MapInfoName: 'nmi';       ProjName: 'kmi';   ToMeters: 1852.0),
    (ID: 30; MapInfoName: 'li';        ProjName: '';      ToMeters: 0.20116684023368047),
    (ID: 31; MapInfoName: 'ch';        ProjName: '';      ToMeters: 20.116684023368047)
  );

const
  // Keep the canonical MapInfo/GDAL 2.4 decimal spellings for units that
  // have no equivalent PROJ 5.2 named unit.  Formatting the corresponding
  // Double back to text may legitimately end in a neighbouring last digit
  // (for example 0.20116684023368048), so these strings must not be rebuilt
  // from the binary floating-point value when generating +to_meter.
  MAPINFO_LINK_TO_METERS_TEXT  = '0.20116684023368047';
  MAPINFO_CHAIN_TO_METERS_TEXT = '20.116684023368047';
  MAPINFO_ROD_TO_METERS_TEXT   = '5.02921005842012';
  MAPINFO_ROD_TO_METERS        = 5.02921005842012;

  // PROJ 5.2 Krovak hard-codes its internal S0/UQ constants and ignores
  // external pseudo-standard-parallel/alpha parameters.  GDAL 2.4 uses the
  // following values as Krovak WKT/MapInfo metadata.  In particular, keep
  // 30.28813972222222 exactly: it is the GDAL 2.4 legacy constant and is
  // intentionally not the 90deg-UQ value (30.288139752778...).
  MAPINFO_KROVAK_PSEUDO_STD_PARALLEL = 78.5;
  MAPINFO_KROVAK_AZIMUTH              = 30.28813972222222;
  MAPINFO_KROVAK_PARAM_TOLERANCE      = 1.0E-6;

//typedef struct
//{
//    int         nMapInfoId;
//    const char *pszMapinfoName;
//    double      dfA; /* semi major axis in meters */
//    double      dfInvFlattening; /* Inverse flattening */
//} pjMapInfoSpheroidInfo;

// This table was hand entered from Appendix I of the mapinfo 6 manuals.

//const pjMapInfoSpheroidInfoList[] =
//{
//{ 9,"Airy 1930",                                6377563.396,    299.3249646},
//{13,"Airy 1930 (modified for Ireland 1965",     6377340.189,    299.3249646},
//{51,"ATS77 (Average Terrestrial System 1977)",  6378135,        298.257},
//{ 2,"Australian",                               6378160.0,      298.25},
//{10,"Bessel 1841",                              6377397.155,    299.1528128},
//{35,"Bessel 1841 (modified for NGO 1948)",      6377492.0176,   299.15281},
//{14,"Bessel 1841 (modified for Schwarzeck)",    6377483.865,    299.1528128},
//{36,"Clarke 1858",                              6378293.639,    294.26068},
//{ 7,"Clarke 1866",                              6378206.4,      294.9786982},
//{ 8,"Clarke 1866 (modified for Michigan)",      6378450.047484481,294.9786982},
//{ 6,"Clarke 1880",                              6378249.145,    293.465},
//{15,"Clarke 1880 (modified for Arc 1950)",      6378249.145326, 293.4663076},
//{30,"Clarke 1880 (modified for IGN)",           6378249.2,      293.4660213},
//{37,"Clarke 1880 (modified for Jamaica)",       6378249.136,    293.46631},
//{16,"Clarke 1880 (modified for Merchich)",      6378249.2,      293.46598},
//{38,"Clarke 1880 (modified for Palestine)",     6378300.79,     293.46623},
//{39,"Everest (Brunei and East Malaysia)",       6377298.556,    300.8017},
//{11,"Everest (India 1830)",                     6377276.345,    300.8017},
//{40,"Everest (India 1956)",                     6377301.243,    300.80174},
//{50,"Everest (Pakistan)",                       6377309.613,    300.8017},
//{17,"Everest (W. Malaysia and Singapore 1948)", 6377304.063,    300.8017},
//{48,"Everest (West Malaysia 1969)",             6377304.063,    300.8017},
//{18,"Fischer 1960",                             6378166.0,      298.3},
//{19,"Fischer 1960 (modified for South Asia)",   6378155.0,      298.3},
//{20,"Fischer 1968",                             6378150.0,      298.3},
//{21,"GRS 67",                                   6378160.0,      298.247167427},
//{ 0,"GRS 80",                                   6378137.0,      298.257222101},
//{ 5,"Hayford",                                  6378388.0,      297.0},
//{22,"Helmert 1906",                             6378200.0,      298.3},
//{23,"Hough",                                    6378270.0,      297.0},
//{31,"IAG 75",                                   6378140.0,      298.257222},
//{41,"Indonesian",                               6378160.0,      298.247},
//{ 4,"International 1924",                       6378388.0,      297.0},
//{49,"Irish (WOFO)",                             6377542.178,    299.325},
//{ 3,"Krassovsky",                               6378245.0,      298.3},
//{32,"MERIT 83",                                 6378137.0,      298.257},
//{33,"New International 1967",                   6378157.5,      298.25},
//{42,"NWL 9D",                                   6378145.0,      298.25},
//{43,"NWL 10D",                                  6378135.0,      298.26},
//{44,"OSU86F",                                   6378136.2,      298.25722},
//{45,"OSU91A",                                   6378136.3,      298.25722},
//{46,"Plessis 1817",                             6376523.0,      308.64},
//{52,"PZ90",                                     6378136.0,      298.257839303},
//{24,"South American",                           6378160.0,      298.25},
//{12,"Sphere",                                   6370997.0,      0.0},
//{47,"Struve 1860",                              6378297.0,      294.73},
//{34,"Walbeck",                                  6376896.0,      302.78},
//{25,"War Office",                               6378300.583,    296.0},
//{26,"WGS 60",                                   6378165.0,      298.3},
//{27,"WGS 66",                                   6378145.0,      298.25},
//{ 1,"WGS 72",                                   6378135.0,      298.26},
//{28,"WGS 84",                                   6378137.0,      298.257223563},
//{29,"WGS 84 (MAPINFO Datum 0)",                 6378137.01,     298.257223563},
//{54,"WGS 84 (MAPINFO Datum 157)",               6378137.01,     298.257223563},
//{-1,nullptr,                                       0.0,            0.0}
//};


var FWktName2ProjName: TStrings = nil;
var FWktEll2Proj: TStrings = nil;
var FWktParamName2Proj: TStrings = nil;
var FProjDefParams: TStrings = nil;
var FProjParamExplain: TStrings = nil;

const PROJ_PAIR_SEPARATOR: Char = '+';
			PROJ_PAIR_DELIMETER: Char = '=';

function LibProjDefnFromEpsgCode(const Code: Integer): string;
const
	gk_tpl = '+proj=tmerc +lat_0=0 +lon_0=%d +k_0=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs';
	utm_tpl = '+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
var
	GKZoneOffset: Integer;
begin
	case Code of
		// Sphere Mercator ESRI:53004
		53004:
			Result := '+proj=merc +lon_0=0 +k_0=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';
		// Popular Visualisation CRS / Mercator
		3785:
			Result := '+proj=merc +lon_0=0 +k_0=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
		900913:
			Result := '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k_0=1.0 +units=m +nadgrids=@null +wktext +no_defs';
		// WGS 84 / World Mercator
		3395:
			Result := '+proj=merc +lon_0=0 +k_0=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
		// NAD83
		4269:
			Result := '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs';
		// WGS 84
		4326:
			Result := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
		// Pulkovo 1995
		2463 .. 2491:
			begin
				GKZoneOffset := 21 + (Code - 2463) * 6;
				if GKZoneOffset > 180 then
					GKZoneOffset := GKZoneOffset - 360; // normalized always

				Result := Format(gk_tpl, [GKZoneOffset, 500000, 0]);
			end;
		// Pulkovo 1942
		2492 .. 2522:
			begin
				GKZoneOffset := 9 + (Code - 2492) * 6;
				if GKZoneOffset > 180 then
					GKZoneOffset := GKZoneOffset - 360; // normalized always

				Result := Format(gk_tpl, [GKZoneOffset, 500000, 0]);
			end;
		// UTM
		32601 .. 32660:
			Result := Format(utm_tpl, [Code - 32600]);
	else
    {$ifdef __proj_include_full_epsgdb}
    if not EPSGDB.FindByEPSG(Code,Result,Result) then
    {$endif}
      Result := '';
	end;
end;

procedure InvalidProjection(const id,defn: string);
resourcestring
	 SErrorInvalidProjection = 'unknown or unsupported projection id "%s" ' + sLineBreak+ 'in definition "%s"';
begin
	raise ENotSupportedProjection.CreateResFmt(@SErrorInvalidProjection,[id,defn]);
end;


function EllipseInfo(const AKey: string; out Info: PpjEllipse): Boolean; overload;
var
	Et: TpjEllipseType;
begin
	for Et := Low(TpjEllipseType) to High(TpjEllipseType) do
	begin
		Info := @pjEllipsoidsList[Et];
		if SameText(Info^.pjKey,AKey) then
			Exit(Info^.pjKey <> '');
	end;

	Info := nil;
	Result := False;
end;

function EllipseInfo(const pjCode: string; var SpheroidName: string; var A,F: Double): Boolean; overload;
var
	Info: PpjEllipse;
begin
	Result := EllipseInfo(pjCode,Info);
	if Result then
	begin
		SpheroidName := Info^.eWktKey;

		A := Info^.a;
		F := Info^.Rf;
		if F <> 0 then
			F := 1/F;
	end;
end;

function EllipseInfo(const pjCode: string; var SpheroidName, A,F: string): Boolean; overload;
var
	AF,FF: Double;
begin
	Result := EllipseInfo(pjCode,SpheroidName,AF,FF);
	if Result then
	begin
		A := AF.ToString(TFormatSettings.Invariant);
		F := FF.ToString(TFormatSettings.Invariant);
  end;
end;

function DatumInfo(const AKey: string; out Info: PpjDatum): Boolean;
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjDatumsList) -1 do
	begin
		Info := @pjDatumsList[Idx];
		Result := SameText(Info^.pjKey,AKey);
		if Result then
			Exit;
	end;

	Result := False;
	Info := nil;
end;

function PrimeInfo(const AKey: string; out Info: PpjPrime): Boolean; overload;
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjPrimeList) -1 do
	begin
		Info := @pjPrimeList[Idx];

		Result := SameText(Info.pjKey,AKey);
		if Result then
			Exit;
	end;
	Info := @pjPrimeList[0];
	Result := False; // force set to default...
end;

function PrimeInfo(const pjCode: string; out PrimeName: string; out Lon: Double): Boolean; overload;
var
	Pm: PpjPrime;
begin
	Result := PrimeInfo(pjCode,Pm);

	PrimeName := Pm.pmName;
	Lon := Pm.pmLon;
end;

function PrimeInfo(const AKey: string; out PrimeName,PrimeLon: string): Boolean; overload;
var
	Lon: Double;
begin
	Result := PrimeInfo(AKey,PrimeName,Lon);

	PrimeLon := Lon.ToString(TFormatSettings.Invariant);
end;

procedure UnitsInfo(const AKey: string; var wktName, toMerersFactor: string);
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjLinearUnitsList) -1 do
	begin
		if SameText(pjLinearUnitsList[Idx].pjKey,AKey) then
		begin
			wktName := pjLinearUnitsList[Idx].wktName;
			toMerersFactor := pjLinearUnitsList[Idx].toMeters.ToString(TFormatSettings.Invariant);
			Exit;
    end;
	end;
end;

procedure WktUnitsInfo(const AWktName: string; var pjKey, toMerersFactor: string);
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjLinearUnitsList) -1 do
	begin
		if SameText(pjLinearUnitsList[Idx].wktName, AWktName) then
		begin
			pjKey := pjLinearUnitsList[Idx].pjKey;
			toMerersFactor := pjLinearUnitsList[Idx].toMeters.ToString(TFormatSettings.Invariant);
			Exit;
    end;
	end;
end;

function TranslateProjectionName(const AKey: string; var wktValue: string): Boolean;
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjProjectionsList) -1 do
	begin
		Result := SameText(pjProjectionsList[Idx].pjKey,AKey);
		if Result then
		begin
			wktValue := pjProjectionsList[Idx].wktName;
			Exit;
		end;
	end;
	Result := False;
end;

function TranslateProjectionParam(const AKey: string; var wktValue: string): Boolean;
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjParamsList) -1 do
	begin
		Result := SameText(pjParamsList[Idx].pjKey,AKey);
		if Result then
		begin
			wktValue := pjParamsList[Idx].wktName;
			Exit(wktValue <> '');
		end;
	end;

	Result := False;
end;



function FetchProjPairs(const ASource: string; Dest: TStrings): Integer;
var
	Pair: string;
begin
	Dest.BeginUpdate;
	try
		Dest.Delimiter := PROJ_PAIR_SEPARATOR;
		Dest.StrictDelimiter := True;
		Dest.DelimitedText := ASource;

		for Result := Dest.Count -1 downto 0 do
		begin
			Pair := Trim(Dest[Result]);
			if Pair = '' then
				Dest.Delete(Result)
			else
				Dest[Result] := Pair;
		end;
	finally
		Dest.EndUpdate;
	end;

	Result := Dest.Count;
end;

function TryFindProjPairValue(Pairs: TStrings; const PairName: string; var PairValue: string;
	const DefaultValue: string): Boolean;
var
	Index: Integer;
begin
	Index := Pairs.IndexOfName(PairName);
	if Index > -1 then
	begin
		PairValue := Pairs.ValueFromIndex[Index];
		if PairValue = '' then
			PairValue := DefaultValue;
		Exit(True);
	end;

  // Boolean PROJ parameters are commonly written without '=value'
  // (e.g. +south, +czech, +wktext, +no_defs). TStringList.IndexOfName()
  // does not reliably treat such an item as a name, so check the raw token.
  for Index := 0 to Pairs.Count - 1 do
    if SameText(Trim(Pairs[Index]),PairName) then
    begin
      PairValue := DefaultValue;
      Exit(True);
    end;

  PairValue := '';
  Result := False;
end;

function LibProjDefnToWKTProjection(const ADefn: string; PrettyWKT: Boolean): string;
const
	DEF_ELL_N = 'WGS_1984';
	DEF_DATUM_N = 'WGS_1984';
	DEF_ELL_A = '6378137.0';
	DEF_ELL_B = '6356752.31424518';
	DEF_ELL_F = '298.2572236';
	DEF_ELL_TOWGS = '0,0,0,0,0,0,0';
var
	Cnt: Integer;
	WtkDefn: TWKTCRSDefinition;

	DtmInfo: PpjDatum;
	ProjPairs: TStrings;
	// geocs
	FProjID,FGeoCSName,FEllipseName,FEllipseA,FEllipseF,FEllipsePrms,
	FDatumName,FDatumToWgs,FPrimeName,FPrimeLon,FDatumU,FFDatumUToAng,
	// projcs
	FProjCSName, FProjectionName, FProjUnits,FFProjUnitsFactor: string;
	FProjCSParameters: TStrings;


	procedure FetchPrimeMeridian();
	var
		PmValue: string;
		PmLon: Double;
	begin
		if TryFindProjPairValue(ProjPairs, 'pm', PmValue, 'greenwich') then
		begin
			if TryStrToFloat(PmValue,PmLon,TFormatSettings.Invariant) then
			begin
				FPrimeName := 'unnamed';
				FPrimeLon := FloatToStr(PmLon,TFormatSettings.Invariant);
			end
			else
				PrimeInfo(PmValue,FPrimeName,FPrimeLon);
		end
		else
			PrimeInfo('greenwich',FPrimeName,FPrimeLon)
	end;

	procedure FetchEllipse();
	var
		EA, EB, EF, ES, Ecc: Double;
		S,BaseEllps,BaseName,BaseA,BaseRF: string;
    HaveBase,ExplicitShape,ExplicitA: Boolean;

    procedure SetInvFlatteningFromF(const F: Double);
    begin
      if SameValue(F,0.0) then FEllipseF := '0'
      else FEllipseF := MIInvariantFloat(1.0/F);
    end;
	begin
		FEllipseName := 'unnamed';
		FDatumName := 'unknown';
		FDatumToWgs := '';
		FEllipsePrms := '';
		FEllipseA := '';
		FEllipseF := '';
    BaseEllps := '';
    HaveBase := False;
    ExplicitShape := False;
    ExplicitA := False;

    // A datum supplies both a base ellipsoid and, when defined by the legacy
    // table, its default datum transform. Explicit ellipsoid/shape parameters
    // below may override only the ellipsoid part.
    if TryFindProjPairValue(ProjPairs,'datum',S,'') and DatumInfo(S,DtmInfo) then
    begin
      FDatumName := DtmInfo.dName;
      FDatumToWgs := DtmInfo.toWgs;
      BaseEllps := DtmInfo.pjEllKey;
    end;

    if TryFindProjPairValue(ProjPairs,'ellps',S,'') then
      BaseEllps := S;

    if (BaseEllps <> '') and EllipseInfo(BaseEllps,BaseName,BaseA,BaseRF) then
    begin
      FEllipseName := BaseName;
      FEllipseA := BaseA;
      FEllipseF := BaseRF;
      HaveBase := True;
    end;

		if TryFindProjPairValue(ProjPairs,'R',S,'') then
		begin
      if TryStrToFloat(S,EA,TFormatSettings.Invariant) and (EA > 0.0) then
      begin
        FEllipseA := MIInvariantFloat(EA);
        FEllipseF := '0';
        FEllipseName := 'unnamed sphere';
        FGeoCSName := 'Sphere based';
      end;
		end
    else
    begin
      if TryFindProjPairValue(ProjPairs,'a',S,'') and
         TryStrToFloat(S,EA,TFormatSettings.Invariant) and (EA > 0.0) then
      begin
        FEllipseA := MIInvariantFloat(EA);
        ExplicitA := True;
        if not HaveBase then FEllipseF := '0'; // PROJ 5.2: +a alone is a sphere.
      end;

      if TryFindProjPairValue(ProjPairs,'rf',S,'') and
         TryStrToFloat(S,EF,TFormatSettings.Invariant) then
      begin
        FEllipseF := MIInvariantFloat(EF);
        ExplicitShape := True;
      end
      else if TryFindProjPairValue(ProjPairs,'f',S,'') and
              TryStrToFloat(S,EF,TFormatSettings.Invariant) then
      begin
        SetInvFlatteningFromF(EF);
        ExplicitShape := True;
      end
      else if TryFindProjPairValue(ProjPairs,'es',S,'') and
              TryStrToFloat(S,ES,TFormatSettings.Invariant) and
              (ES >= 0.0) and (ES < 1.0) then
      begin
        EF := 1.0-Sqrt(1.0-ES);
        SetInvFlatteningFromF(EF);
        ExplicitShape := True;
      end
      else if TryFindProjPairValue(ProjPairs,'e',S,'') and
              TryStrToFloat(S,Ecc,TFormatSettings.Invariant) and
              (Ecc >= 0.0) and (Ecc < 1.0) then
      begin
        ES := Ecc*Ecc;
        EF := 1.0-Sqrt(1.0-ES);
        SetInvFlatteningFromF(EF);
        ExplicitShape := True;
      end
      else if TryFindProjPairValue(ProjPairs,'b',S,'') and
              TryStrToFloat(FEllipseA,EA,TFormatSettings.Invariant) and
              TryStrToFloat(S,EB,TFormatSettings.Invariant) and
              (EA > 0.0) and (EB > 0.0) then
      begin
        if SameValue(EA,EB) then FEllipseF := '0'
        else FEllipseF := MIInvariantFloat(EA/(EA-EB));
        ExplicitShape := True;
      end;

      if (FEllipseA = '') or (FEllipseF = '') then
      begin
        // Preserve the historical WGS84 fallback for definitions relying on
        // proj_def.dat defaults, but do not pretend an explicit +a is WGS84.
        if not ExplicitA then
        begin
          FEllipseName := DEF_ELL_N;
          FEllipseA := DEF_ELL_A;
          FEllipseF := DEF_ELL_F;
        end;
      end;

      if ExplicitA or ExplicitShape then
        FEllipseName := 'unnamed';
      if FEllipseF = '0' then
      begin
        FEllipseName := 'unnamed sphere';
        FGeoCSName := 'Sphere based';
      end;
    end;

		TryFindProjPairValue(ProjPairs,'towgs84',FDatumToWgs,DEF_ELL_TOWGS);
		if (FEllipseA <> '') and (FEllipseF <> '') then
			FEllipsePrms := FEllipseA + ',' + FEllipseF;
	end;

	procedure FetchDatumUnits();
	begin
		FDatumU := 'degree';
		FFDatumUToAng := '0.0174532925199433';
  end;

	procedure ProcessGeoCS();
	// GEOGCS["<name>", <datum>, <prime meridian>, <angular unit>]
	//-----------------------------------------------------
	// GEOGCS["<name>,
	//    DATUM  ["<name>", ...],
	//    PRIMEM ["<name>", <longitude>],
	//    UNIT   ["<name>", <conversion factor>],
	//   *AXIS   ["<name>", NORTH|SOUTH|EAST|WEST|UP|DOWN|OTHER], AXIS...
	// ]
	//-----------------------------------------------------
	// +datum = GEOGCS["", DATUM["", ...] ]
	// +ellps = GEOGCS["", DATUM["", SPHEROID["", ...] ] ]
	// +towgs84 = GEOGCS["", DATUM["", TOWGS84[<7 params>] ]
	// +pm = GEOGCS["",PRIMEM["",]
	begin
		FetchPrimeMeridian();
		FetchEllipse();
		FetchDatumUnits();

    if FGeoCSName = '' then
      FGeoCSName := 'unnamed';
	end;

	procedure FetchProjectionName();
  var
    S: string;
    V: Double;
	begin
    { Match the canonical OGC WKT1 methods produced by GDAL 2.4 when it
      imports a PROJ.4 definition.  The projection table also contains
      alternative names accepted on input, therefore simply returning the
      first table entry is not sufficient for canonical output. }
    if SameText(FProjID,'merc') then
    begin
      if TryFindProjPairValue(ProjPairs,'lat_ts',S,'') then
      begin
        if not TryStrToFloat(S,V,TFormatSettings.Invariant) then
          raise EArgumentException.CreateFmt('Invalid +lat_ts value in projection: %s',[ADefn]);
        if V < 999.0 then
          FProjectionName := 'Mercator_2SP'
        else
          FProjectionName := 'Mercator_1SP';
      end
      else
        FProjectionName := 'Mercator_1SP';
    end
    else if SameText(FProjID,'etmerc') then
      { GDAL 2.4 serializes etmerc as a standard Transverse_Mercator WKT
        method and preserves the exact PROJ definition in EXTENSION. }
      FProjectionName := 'Transverse_Mercator'
    else if not TranslateProjectionName(FProjID,FProjectionName) then
			InvalidProjection(FProjID,ADefn);

		FProjCSName := 'unnamed';
	end;

	procedure FetchProjectionUnits();
  const
    cDefUnitsName = 'Meter';
  var
    S: string;
    P: Integer;
    N,D,F: Double;
  begin
    FProjUnits := cDefUnitsName;
    FFProjUnitsFactor := '1';

    // PROJ 5.2 gives +units precedence over +to_meter.
    if TryFindProjPairValue(ProjPairs,'units',S,'') then
    begin
      FProjUnits := S;
      UnitsInfo(S,FProjUnits,FFProjUnitsFactor);
      Exit;
    end;

    if TryFindProjPairValue(ProjPairs,'to_meter',S,'') then
    begin
      // +to_meter also accepts ratios (for example 1/39.37).
      P := Pos('/',S);
      if P > 0 then
      begin
        if TryStrToFloat(Copy(S,1,P-1),N,TFormatSettings.Invariant) and
           TryStrToFloat(Copy(S,P+1,MaxInt),D,TFormatSettings.Invariant) and
           not SameValue(D,0.0) then
        begin
          F := N/D;
          FFProjUnitsFactor := MIInvariantFloat(F);
        end;
      end
      else if TryStrToFloat(S,F,TFormatSettings.Invariant) and (F > 0.0) then
        FFProjUnitsFactor := MIInvariantFloat(F);
      FProjUnits := 'unknown';
    end;
  end;

	procedure FetchUtm();
	const
		pjTmParams: array[0..4] of string = ('k_0','lat_0','lon_0','x_0','y_0');
		var
			Zone,South: string;
			SouthHemesphere: Boolean;
			Code: Integer;
      UnitFactor: Double;
			pjTmParamsValues: array[0..4] of string;
	begin
    UnitFactor := 1.0;
    if not TryStrToFloat(FFProjUnitsFactor,UnitFactor,TFormatSettings.Invariant) or
       (UnitFactor <= 0.0) then UnitFactor := 1.0;
		if TryFindProjPairValue(ProjPairs,'zone',Zone,'') then
		begin
			if Code.TryParse(Zone,Code) then
			begin
				SouthHemesphere := TryFindProjPairValue(ProjPairs,'south',South,'1') and (South = '1');
				if (Code > 0) and (Code <= 60) then
				begin
					FProjCSName := 'UTM Zone '+Zone+', ';
					if  SouthHemesphere then
						begin
							pjTmParamsValues[4] := MIInvariantFloat(10000000.0/UnitFactor);
							FProjCSName := FProjCSName + 'Southern Hemisphere'
						end
					else
					begin
						pjTmParamsValues[4] := '0';
						FProjCSName := FProjCSName + 'Northern Hemisphere';
					end;

					pjTmParamsValues[0] := '0.9996';
					pjTmParamsValues[1] := '0';
					pjTmParamsValues[2] := MIInvariantFloat((Code * 6) - 183);
					pjTmParamsValues[3] := MIInvariantFloat(500000.0/UnitFactor);

					for Code := 0 to Length(pjTmParams) -1 do
					begin
						if TranslateProjectionParam(pjTmParams[Code],Zone) then
						begin
							FProjCSParameters.Add(Zone+'='+pjTmParamsValues[Code]);
						end;
					end;

					FetchEllipse();
				end;
			end;
		end;
  end;

	procedure FetchProjectionParameters();
	// https://proj4.org/usage/projections.html
	// https://github.com/OSGeo/proj.4/wiki/GenParms
	const
		pjGenParams: array[0..11] of string = (
			'k','k_0','lat_0','lat_1','lat_2','lat_ts','lon_0','lonc','x_0','y_0',
      'alpha','gamma');
	var
		ParamName,ParamValue,WktParamName: string;
		Idx: Integer;
    V,UnitFactor: Double;

    function ParameterNameForWKT(const PName: string; out WName: string): Boolean;
    begin
      Result := True;
      if SameText(PName,'k') or SameText(PName,'k_0') then WName := 'scale_factor'
      else if SameText(PName,'lat_1') then WName := 'standard_parallel_1'
      else if SameText(PName,'lat_2') then WName := 'standard_parallel_2'
      else if SameText(PName,'lat_ts') then WName := 'standard_parallel_1'
      else if SameText(PName,'lonc') then WName := 'longitude_of_center'
      else if SameText(PName,'alpha') then WName := 'azimuth'
      else if SameText(PName,'gamma') then WName := 'rectified_grid_angle'
      else if SameText(PName,'x_0') then WName := 'false_easting'
      else if SameText(PName,'y_0') then WName := 'false_northing'
      else if SameText(PName,'lat_0') then
      begin
        if SameText(FProjID,'aea') or SameText(FProjID,'eqdc') or
           SameText(FProjID,'laea') or SameText(FProjID,'aeqd') or
           SameText(FProjID,'omerc') or SameText(FProjID,'krovak') then
          WName := 'latitude_of_center'
        else
          WName := 'latitude_of_origin';
      end
      else if SameText(PName,'lon_0') then
      begin
        if SameText(FProjID,'aea') or SameText(FProjID,'eqdc') or
           SameText(FProjID,'laea') or SameText(FProjID,'aeqd') or
           SameText(FProjID,'krovak') then
          WName := 'longitude_of_center'
        else
          WName := 'central_meridian';
      end
      else
        Result := TranslateProjectionParam(PName,WName);
    end;

    procedure FetchMercatorParameters;
    var
      S: string;
      V: Double;

      procedure AddAngularOrScale(const WktName, ProjName, DefaultValue: string);
      begin
        if not TryFindProjPairValue(ProjPairs,ProjName,S,'') then
          S := DefaultValue;
        if not TryStrToFloat(S,V,TFormatSettings.Invariant) then
          raise EArgumentException.CreateFmt('Invalid +%s value in projection: %s',[ProjName,ADefn]);
        FProjCSParameters.Add(WktName+'='+MIInvariantFloat(V));
      end;

      procedure AddLinear(const WktName, ProjName: string);
      begin
        if not TryFindProjPairValue(ProjPairs,ProjName,S,'') then
          S := '0';
        if not TryStrToFloat(S,V,TFormatSettings.Invariant) then
          raise EArgumentException.CreateFmt('Invalid +%s value in projection: %s',[ProjName,ADefn]);
        FProjCSParameters.Add(WktName+'='+MIInvariantFloat(V/UnitFactor));
      end;

    begin
      { GDAL 2.4 SetMercator()/SetMercator2SP() materialize the defaults in
        WKT even when the corresponding PROJ parameters are omitted.  Do the
        same so the WKT remains a complete CRS without relying on EXTENSION. }
      if SameText(FProjectionName,'Mercator_2SP') then
      begin
        AddAngularOrScale('standard_parallel_1','lat_ts','0');
        FProjCSParameters.Add('latitude_of_origin=0');
      end
      else
        FProjCSParameters.Add('latitude_of_origin=0');

      AddAngularOrScale('central_meridian','lon_0','0');

      if SameText(FProjectionName,'Mercator_1SP') then
      begin
        if TryFindProjPairValue(ProjPairs,'k_0',S,'') then
        begin
          if not TryStrToFloat(S,V,TFormatSettings.Invariant) then
            raise EArgumentException.CreateFmt('Invalid +k_0 value in projection: %s',[ADefn]);
        end
        else if TryFindProjPairValue(ProjPairs,'k',S,'') then
        begin
          if not TryStrToFloat(S,V,TFormatSettings.Invariant) then
            raise EArgumentException.CreateFmt('Invalid +k value in projection: %s',[ADefn]);
        end
        else
          V := 1.0;
        FProjCSParameters.Add('scale_factor='+MIInvariantFloat(V));
      end;

      AddLinear('false_easting','x_0');
      AddLinear('false_northing','y_0');
    end;
	begin
		if FProjCSParameters = nil then
			FProjCSParameters := TStringList.Create
		else
			FProjCSParameters.Clear;

    UnitFactor := 1.0;
    if not TryStrToFloat(FFProjUnitsFactor,UnitFactor,TFormatSettings.Invariant) or
       (UnitFactor <= 0.0) then
      UnitFactor := 1.0;

		if SameText(FProjID,'utm') then
		begin
			FetchUtm();
		end
    else if SameText(FProjID,'merc') then
    begin
      FetchMercatorParameters();
      FProjCSName := FProjCSName + ' ('+FProjectionName+ '/' + FGeoCSName +')';
    end
		else
		begin
			for Idx := 0 to Length(pjGenParams) -1 do
			begin
				ParamName := pjGenParams[Idx];
        // k_0 supersedes the deprecated k spelling; avoid duplicate WKT
        // scale_factor parameters when both are present.
        if SameText(ParamName,'k') and
           TryFindProjPairValue(ProjPairs,'k_0',ParamValue,'') then Continue;
        // Oblique Mercator uses lonc. Its internal lon_0 is derived by the
        // projection and must not be serialized as a second center longitude.
        if SameText(FProjID,'omerc') and SameText(ParamName,'lon_0') and
           TryFindProjPairValue(ProjPairs,'lonc',ParamValue,'') then Continue;
				if TryFindProjPairValue(ProjPairs,ParamName,ParamValue,'') and
           ParameterNameForWKT(ParamName,WktParamName) then
				begin
          // PROJ x_0/y_0 are internal metres; WKT parameters use the
          // projected CRS linear unit.
          if (SameText(ParamName,'x_0') or SameText(ParamName,'y_0')) and
             TryStrToFloat(ParamValue,V,TFormatSettings.Invariant) then
            ParamValue := MIInvariantFloat(V/UnitFactor);
					FProjCSParameters.Add(WktParamName+'='+ParamValue);
				end;
			end;

      if SameText(FProjID,'krovak') then
      begin
        if FProjCSParameters.IndexOfName('pseudo_standard_parallel_1') < 0 then
          FProjCSParameters.Add('pseudo_standard_parallel_1='+
            MIInvariantFloat(MAPINFO_KROVAK_PSEUDO_STD_PARALLEL));
        if FProjCSParameters.IndexOfName('azimuth') < 0 then
          FProjCSParameters.Add('azimuth='+MIInvariantFloat(MAPINFO_KROVAK_AZIMUTH));
        if FProjCSParameters.IndexOfName('scale_factor') < 0 then
          FProjCSParameters.Add('scale_factor=0.9999');
      end;

			FProjCSName :=  FProjCSName + ' ('+FProjectionName+ '/' + FGeoCSName +')';
		end;
	end;

	procedure ProcessProjCS();
	// PROJCS["<name>", <geographic cs>, <projection>, {<parameter>,}* <linear unit> {,<twin axes>}]
	//-----------------------------------------------------
		(*PROJCS["<name>
			 GEOGCS    [ ...... ],
			 PROJECTION["<name>"],
			*PARAMETER ["<name>", <value>], ...
			 UNIT      ["<name>", <conversion factor>],
			*AXIS      ["<name>", NORTH|SOUTH|EAST|WEST|UP|DOWN|OTHER],
			*AXIS      ["<name>", NORTH|SOUTH|EAST|WEST|UP|DOWN|OTHER]
		]*)
	begin
		FProjectionName := 'unknown';

		FetchProjectionName();
		FetchProjectionUnits();
		FetchProjectionParameters();
	end;

begin
	if ADefn = '' then
		Exit('');

  FProjCSParameters := nil;
	ProjPairs := TStringList.Create();
	ProjPairs.NameValueSeparator := PROJ_PAIR_DELIMETER;
	try
		Cnt := FetchProjPairs(ADefn, ProjPairs);
		if Cnt = 0 then
			Exit;

		if not TryFindProjPairValue(ProjPairs, 'proj', FProjID, '') then
			Exit;

		if SameText('longlat', FProjID) or
			 SameText('latlong', FProjID) or
			 SameText('geocent', FProjID) then
			begin
				if SameText('geocent', FProjID) then
					FGeoCSName := 'Geocentric'
				else
					FGeoCSName := 'Geodetic';

				WtkDefn := TWKTGeoCRS.Create(nil);
			end
		else
			WtkDefn := TWKTProjectedCRS.Create(nil);

		ProcessGeoCS;
		ProcessProjCS;

		if WtkDefn is TWKTGeoCRS then
		begin
			TWKTGeoCRS(WtkDefn).Name := FGeoCSName;
			TWKTGeoCRS(WtkDefn).DatumName := FDatumName;
			TWKTGeoCRS(WtkDefn).SpheroidName := FEllipseName;
			TWKTGeoCRS(WtkDefn).SpheroidAF := FEllipsePrms;
			TWKTGeoCRS(WtkDefn).PrimeMeridianName := FPrimeName;
			TWKTGeoCRS(WtkDefn).PrimeMeridianLon := FPrimeLon;
			TWKTGeoCRS(WtkDefn).UnitsName := FDatumU;
			TWKTGeoCRS(WtkDefn).UnitsConversionFactor := FFDatumUToAng;
			TWKTGeoCRS(WtkDefn).ToWGS := FDatumToWgs;
		end
		else
    if WtkDefn is TWKTProjectedCRS then
		begin
			// name
			TWKTProjectedCRS(WtkDefn).Name := FProjCSName;
			// projection
			TWKTProjectedCRS(WtkDefn).ProjectionName := FProjectionName;
			// geocs
			TWKTProjectedCRS(WtkDefn).GeoCS.Name := FGeoCSName;
			TWKTProjectedCRS(WtkDefn).GeoCS.DatumName := FDatumName;
			TWKTProjectedCRS(WtkDefn).GeoCS.SpheroidName := FEllipseName;
			TWKTProjectedCRS(WtkDefn).GeoCS.SpheroidAF := FEllipsePrms;
			TWKTProjectedCRS(WtkDefn).GeoCS.PrimeMeridianName := FPrimeName;
			TWKTProjectedCRS(WtkDefn).GeoCS.PrimeMeridianLon := FPrimeLon;
			TWKTProjectedCRS(WtkDefn).GeoCS.ToWGS := FDatumToWgs;
			TWKTProjectedCRS(WtkDefn).GeoCS.UnitsName := FDatumU;
			TWKTProjectedCRS(WtkDefn).GeoCS.UnitsConversionFactor := FFDatumUToAng;
			// parameters
			for Cnt := 0 to FProjCSParameters.Count -1 do
				TWKTProjectedCRS(WtkDefn).Parameter[FProjCSParameters.Names[Cnt]] := FProjCSParameters.ValueFromIndex[Cnt];
			// units
			TWKTProjectedCRS(WtkDefn).UnitsName := FProjUnits;
			TWKTProjectedCRS(WtkDefn).UnitsToMeters := FFProjUnitsFactor;
		end;

    WtkDefn.AddChild('EXTENSION','"PROJ4"',ADefn.QuotedString('"'));

		Result := WtkDefn.SaveToString(PrettyWKT);

		FreeAndNil(WtkDefn);
	finally
		FreeAndNil(ProjPairs);
		FreeAndNil(FProjCSParameters);
	end;
end;

	(*
		GEOGCS["", DATUM["", ...] ] 	+datum
		GEOGCS["", DATUM["", SPHEROID["", ...] ] ] +ellps
		GEOGCS["", DATUM["", TOWGS84[<7 params>] ] +towgs84
		GEOGCS["",PRIMEM["",] +pm
	*)

	(*
		parameters map

		'false_easting=+x_0'
		'false_northing=+y_0'
		'scale_factor=+k_0'
		'standard_parallel_1=+lat_1'
		'standard_parallel_2=+lat_2'
		'longitude_of_center=+lon_0'
		'central_meridian=+lon_0'
		'latitude_of_origin=+lat_0'
		'latitude_of_center=+lat_0'
		'central_meridian=+lon_0'
	*)

	function WKTProjectionName2ProjName(const WktName: string; out ProjName: string): Boolean;
	// http://geotiff.maptools.org/proj_list
	var
		Index: Integer;
	begin
		Index := FWktName2ProjName.IndexOfName(WktName);

		Result := Index > -1;
		if Result then
			ProjName := FWktName2ProjName.ValueFromIndex[Index]
		else
			ProjName := '';
	end;

	function WKTParameterName2ProjParameterName(const WktName: string; out ProjName: string): Boolean;
	var
		Index: Integer;
	begin
		Index := FWktParamName2Proj.IndexOfName(WktName);

		Result := Index > -1;
		if Result then
			ProjName := FWktParamName2Proj.ValueFromIndex[Index]
		else
			ProjName := '';
	end;

	function WktEllps2ProjEllps(const WktName: string; out ProjName: string): Boolean;
	// proj/src/pj_ellps.c.
	var
		Index: Integer;
	begin
		Index := FWktEll2Proj.IndexOfName(WktName);
		Result := Index > -1;
		if Result then
			ProjName := FWktEll2Proj.ValueFromIndex[Index]
		else
			ProjName := '';
	end;


function WKTProjectionToLibProjDefn(const ADefn: string): string;
var
  defn: string;
  crs: TWKTCRSDefinition;
  proj4Params: TStrings;
  // +proj=tmerc +lat_0=55.6666666667 +lon_0=37.5 +k=1 +x_0=0 +y_0=0 +ellps=bessel  +towgs84=395.983199420091,165.38760309981,557.737156050949,-0.0484611377151178,0.0432372459439537,0.00813946145874134,0 +units=m +no_defs
  // +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +over +no_defs*/

  // +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0

  function ExtractProj4Defn: string;
  var
    n: TWktNode;
  begin
    Result := '';

    if not crs.FindByAttributeName('EXTENSION','PROJ4',n) then Exit;

    Result := n.AttributeValue[1];

  end;

  function ProcessGeoCS(d: TWKTGeoCRS): Boolean;
  var
    s, a,b: string;
    PMValue: Double;

    function ValidateTowgs84(const Value: string): Boolean;
    var
      Values: TStringList;
      I: Integer;
      D: Double;
    begin
      Result := False;
      Values := TStringList.Create;
      try
        Values.Delimiter := ',';
        Values.StrictDelimiter := True;
        Values.DelimitedText := Value;
        if (Values.Count <> 3) and (Values.Count <> 7) then Exit;
        for I := 0 to Values.Count - 1 do
          if not TryStrToFloat(Trim(Values[I]),D,TFormatSettings.Invariant) then Exit;
        Result := True;
      finally
        Values.Free;
      end;
    end;

    function CalcAB(const af: string): boolean;
    var
      af_: TArray<string>;
      b_,a_,f_: double;
    begin
      af_ := af.DeQuotedString('"').Split([',']);
      Result := Length(af_) = 2;
      if not Result then
        Exit;

      Result := TryStrToFloat(af_[0],a_,TFormatSettings.Invariant) and
        TryStrToFloat(af_[1],f_,TFormatSettings.Invariant);

      if not Result then
        Exit;

      if SameValue(f_,0.0) then
        b_ := a_
      else
        b_ := a_ * (1.0 - 1.0 / f_);

      Result := (a_ > 0) and (b_ > 0);

      if Result then
      begin
        a := a_.ToString(TFormatSettings.Invariant);
        b := b_.ToString(TFormatSettings.Invariant);
      end;
    end;

  begin
    Result := False;
    if WktEllps2ProjEllps(d.SpheroidName,s) then
      proj4Params.Add('+ellps='+s)
    else
    begin
      if not CalcAB(d.SpheroidAF) then Exit;
      proj4Params.Add('+a='+a);
      proj4Params.Add('+b='+b);
    end;

    s := d.ToWGS;
    // Absence of TOWGS84 is meaningful. Do not invent an identity datum
    // transformation for a WKT datum that did not specify one.
    if s <> '' then
    begin
      if not ValidateTowgs84(s) then Exit;
      proj4Params.Add('+towgs84='+s);
    end;

    if not SameText(d.PrimeMeridianName,'Greenwich') or
       ((d.PrimeMeridianLon <> '') and
        not SameText(d.PrimeMeridianLon,'0') and
        not SameText(d.PrimeMeridianLon,'0.0')) then
    begin
      if not TryStrToFloat(d.PrimeMeridianLon,PMValue,TFormatSettings.Invariant) then Exit;
      proj4Params.Add('+pm='+MIInvariantFloat(PMValue));
    end;
    Result := True;
  end;

  function ProcessProjCS(d: TWKTProjectedCRS): Boolean;
  var
    i: Integer;
    k,v, ProjParam, ProjMethod, UnitKey, UnitFactorText: string;
    UnitFactor,KnownUnitFactor,ParamValue: Double;

    function MapParameter(const WktName: string; out ProjName: string): Boolean;
    begin
      Result := True;
      if SameText(WktName,'false_easting') then ProjName := 'x_0'
      else if SameText(WktName,'false_northing') then ProjName := 'y_0'
      else if SameText(WktName,'scale_factor') then ProjName := 'k_0'
      else if SameText(WktName,'standard_parallel_2') then ProjName := 'lat_2'
      else if SameText(WktName,'standard_parallel_1') then
      begin
        if SameText(ProjMethod,'merc') or SameText(ProjMethod,'cea') or
           SameText(ProjMethod,'eqc') then ProjName := 'lat_ts'
        else ProjName := 'lat_1';
      end
      else if SameText(WktName,'longitude_of_center') then
      begin
        if SameText(ProjMethod,'omerc') then ProjName := 'lonc'
        else ProjName := 'lon_0';
      end
      else if SameText(WktName,'central_meridian') then ProjName := 'lon_0'
      else if SameText(WktName,'latitude_of_origin') or
              SameText(WktName,'latitude_of_center') then ProjName := 'lat_0'
      else if SameText(WktName,'azimuth') then ProjName := 'alpha'
      else if SameText(WktName,'rectified_grid_angle') then ProjName := 'gamma'
      else if SameText(WktName,'pseudo_standard_parallel_1') and
              SameText(ProjMethod,'krovak') then
      begin
        // PROJ 5.2 Krovak hard-codes 78.5 degrees and has no parameter for it.
        ProjName := '';
      end
      else
        Result := WKTParameterName2ProjParameterName(WktName,ProjName);
    end;

  begin
    Result := False;
    if not WKTProjectionName2ProjName(d.ProjectionName,ProjMethod) then Exit;

    if not TryStrToFloat(d.UnitsToMeters,UnitFactor,TFormatSettings.Invariant) or
       (UnitFactor <= 0.0) then Exit;

    for i := 0 to d.ProjectionParameters.Count - 1 do
    begin
      k := d.ProjectionParameters[i].AttributeName;
      v := d.ProjectionParameters[i].AttributeValue[1];
      if not TryStrToFloat(v,ParamValue,TFormatSettings.Invariant) then Exit;

      if SameText(k,'pseudo_standard_parallel_1') and SameText(ProjMethod,'krovak') then
      begin
        if not SameValue(ParamValue,MAPINFO_KROVAK_PSEUDO_STD_PARALLEL,
                         MAPINFO_KROVAK_PARAM_TOLERANCE) then Exit;
        Continue;
      end;

      if not MapParameter(k,ProjParam) then
        Exit;
      if ProjParam = '' then
        Continue;

      // WKT false easting/northing are expressed in the projected CRS unit,
      // while PROJ 5.2 +x_0/+y_0 are stored internally in metres.
      if SameText(ProjParam,'x_0') or SameText(ProjParam,'y_0') then
        ParamValue := ParamValue*UnitFactor;
      proj4Params.Add('+'+ProjParam+'='+MIInvariantFloat(ParamValue));
    end;

    UnitKey := '';
    UnitFactorText := '';
    WktUnitsInfo(d.UnitsName,UnitKey,UnitFactorText);
    KnownUnitFactor := 0.0;
    if (UnitKey <> '') and
       TryStrToFloat(UnitFactorText,KnownUnitFactor,TFormatSettings.Invariant) and
       SameValue(KnownUnitFactor,UnitFactor,1E-12) then
      proj4Params.Add('+units='+UnitKey)
    else
      proj4Params.Add('+to_meter='+MIInvariantFloat(UnitFactor));

    Result := True;
  end;

begin
  crs := TWKTCRSDefinition.NewFromString(Trim(ADefn));

  if (crs = nil) then Exit;

  try
    if crs.Empty then Exit;

    defn := ExtractProj4Defn();

    if defn <> '' then
    begin
      Result := defn.DeQuotedString('"');
      Exit;
    end;

    proj4Params := TStringList.Create(dupIgnore,False,False);
    try
      if SameText(crs.Keyword,'PROJCS') then
      begin
        if not WKTProjectionName2ProjName((crs as TWKTProjectedCRS).ProjectionName, defn) then
          Exit;

        proj4Params.Add('+proj='+defn);
        if not ProcessGeoCS((crs as TWKTProjectedCRS).GeoCS) then Exit;
        if not ProcessProjCS((crs as TWKTProjectedCRS)) then Exit;
      end
      else
      if SameText(crs.Keyword,'GEOGCS') then
      begin
        proj4Params.Add('+proj=longlat');
        if not ProcessGeoCS(crs as TWKTGeoCRS) then Exit;
      end;

      proj4Params.Add('+no_defs');
      proj4Params.LineBreak := ' ';
      Result := Trim(proj4Params.Text);
    finally
      FreeAndNil(proj4Params);
    end;

  finally
    FreeAndNil(crs);
  end;
end;

function MIInvariantFloat(const V: Double): string;
begin
  Result := FloatToStrF(V,ffGeneral,17,0,TFormatSettings.Invariant);
end;

function MITryFloat(const S: string; out V: Double): Boolean;
begin
  Result := TryStrToFloat(Trim(S),V,TFormatSettings.Invariant);
end;

function MITryProjFactor(const S: string; out V: Double): Boolean;
var
  P: Integer;
  N,D: Double;
  T: string;
begin
  T := Trim(S);
  P := Pos('/',T);
  if P <= 0 then
    Exit(MITryFloat(T,V));

  Result := MITryFloat(Copy(T,1,P-1),N) and
            MITryFloat(Copy(T,P+1,MaxInt),D) and not SameValue(D,0.0);
  if Result then V := N/D;
end;

function MIQuote(const S: string): string;
begin
  Result := '"' + StringReplace(S,'"','""',[rfReplaceAll]) + '"';
end;

function MIUnquoteToken(const S: string): string;
var
  T: string;
begin
  T := Trim(S);
  if (Length(T) >= 2) and (T[1] = '"') and (T[Length(T)] = '"') then
  begin
    T := Copy(T,2,Length(T)-2);
    T := StringReplace(T,'""','"',[rfReplaceAll]);
  end;
  Result := T;
end;

function MIUnitInfoByName(const AName: string; out AProjName: string;
  out AToMeters: Double): Boolean;
var
  I: Integer;
begin
  for I := Low(pjMapInfoUnitsList) to High(pjMapInfoUnitsList) do
    if SameText(AName,pjMapInfoUnitsList[I].MapInfoName) or
       SameText(AName,pjMapInfoUnitsList[I].ProjName) or
       ((pjMapInfoUnitsList[I].ID = 8) and SameText(AName,'survey foot')) then
    begin
      AProjName := pjMapInfoUnitsList[I].ProjName;
      AToMeters := pjMapInfoUnitsList[I].ToMeters;
      Exit(True);
    end;

  if SameText(AName,'rd') then
  begin
    AProjName := '';
    AToMeters := MAPINFO_ROD_TO_METERS;
    Exit(True);
  end;

  AProjName := '';
  AToMeters := 1.0;
  Result := False;
end;

function MIUnitFactorText(const AMapInfoName: string;
  const AToMeters: Double): string;
begin
  // MapInfo li/ch/rd are based on US-survey-foot-derived values.  They do
  // not match PROJ 5.2 named units link/ch, so emit +to_meter using the
  // exact legacy GDAL 2.4 decimal constants instead of re-formatting a
  // Double and changing the last decimal digit.
  if SameText(AMapInfoName,'li') then
    Exit(MAPINFO_LINK_TO_METERS_TEXT);
  if SameText(AMapInfoName,'ch') then
    Exit(MAPINFO_CHAIN_TO_METERS_TEXT);
  if SameText(AMapInfoName,'rd') then
    Exit(MAPINFO_ROD_TO_METERS_TEXT);
  Result := MIInvariantFloat(AToMeters);
end;

function MIUnitInfoByProj(const AProjName: string; const AToMeters: Double;
  out AMapInfoName: string; out AFactor: Double): Boolean;
var
  I: Integer;
begin
  for I := Low(pjMapInfoUnitsList) to High(pjMapInfoUnitsList) do
    if ((AProjName <> '') and SameText(AProjName,pjMapInfoUnitsList[I].ProjName)) or
       ((AProjName = '') and SameValue(AToMeters,pjMapInfoUnitsList[I].ToMeters,1E-12)) then
    begin
      AMapInfoName := pjMapInfoUnitsList[I].MapInfoName;
      AFactor := pjMapInfoUnitsList[I].ToMeters;
      Exit(True);
    end;

  if (AProjName = '') and SameValue(AToMeters,MAPINFO_ROD_TO_METERS,1E-12) then
  begin
    AMapInfoName := 'rd';
    AFactor := MAPINFO_ROD_TO_METERS;
    Exit(True);
  end;

  AMapInfoName := '';
  AFactor := 1.0;
  Result := False;
end;

function MIMapInfoSpheroidByID(const AID: Integer;
  out AInfo: pjMapInfoSpheroidInfo): Boolean;
var
  I: Integer;
begin
  for I := Low(pjMapInfoSpheroidInfoList) to High(pjMapInfoSpheroidInfoList) do
    if pjMapInfoSpheroidInfoList[I].ID = AID then
    begin
      AInfo := pjMapInfoSpheroidInfoList[I];
      Exit(True);
    end;
  AInfo := Default(pjMapInfoSpheroidInfo);
  Result := False;
end;

function MIMapInfoSpheroidByARF(const A,RF: Double;
  out AInfo: pjMapInfoSpheroidInfo): Boolean;
var
  I: Integer;
begin
  for I := Low(pjMapInfoSpheroidInfoList) to High(pjMapInfoSpheroidInfoList) do
    if SameValue(A,pjMapInfoSpheroidInfoList[I].A,1E-7) and
       SameValue(RF,pjMapInfoSpheroidInfoList[I].RF,1E-9) then
    begin
      AInfo := pjMapInfoSpheroidInfoList[I];
      Exit(True);
    end;
  AInfo := Default(pjMapInfoSpheroidInfo);
  Result := False;
end;

function MIMapInfoDatumByID(const AID: Integer;
  out AInfo: pjMapInfoDatumInfo): Boolean;
var
  I: Integer;
begin
  for I := Low(pjMapInfoDatumInfoList) to High(pjMapInfoDatumInfoList) do
    if pjMapInfoDatumInfoList[I].ID = AID then
    begin
      AInfo := pjMapInfoDatumInfoList[I];
      Exit(True);
    end;
  AInfo := Default(pjMapInfoDatumInfo);
  Result := False;
end;

function MIDoubleArrayValue(const AValues: TArray<Double>;
  const AIndex: Integer; const ADefault: Double = 0.0): Double;
begin
  if (AIndex >= 0) and (AIndex < Length(AValues)) then
    Result := AValues[AIndex]
  else
    Result := ADefault;
end;

function MIFindKeywordOutsideQuotes(const S,Keyword: string): Integer;
var
  I,L: Integer;
  InQuotes: Boolean;
begin
  Result := 0;
  InQuotes := False;
  L := Length(Keyword);
  I := 1;
  while I <= Length(S) - L + 1 do
  begin
    if S[I] = '"' then
    begin
      // In a quoted MapInfo token two consecutive quote characters encode
      // one literal quote and do not terminate the quoted field.
      if InQuotes and (I < Length(S)) and (S[I+1] = '"') then
      begin
        Inc(I,2);
        Continue;
      end;
      InQuotes := not InQuotes;
    end
    else if not InQuotes and SameText(Copy(S,I,L),Keyword) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I);
  end;
end;

function MIParseBounds(const S: string; out MinX,MinY,MaxX,MaxY: Double): Boolean;
var
  T: string;
  L: TStringList;
  V: array[0..3] of Double;
  I,N: Integer;
begin
  Result := False;
  T := StringReplace(S,'(', ' ',[rfReplaceAll]);
  T := StringReplace(T,')', ' ',[rfReplaceAll]);
  T := StringReplace(T,',', ' ',[rfReplaceAll]);
  L := TStringList.Create;
  try
    L.Delimiter := ' ';
    L.StrictDelimiter := True;
    L.DelimitedText := Trim(T);
    N := 0;
    for I := 0 to L.Count - 1 do
      if Trim(L[I]) <> '' then
      begin
        if N > High(V) then Exit;
        if not MITryFloat(L[I],V[N]) then Exit;
        Inc(N);
      end;
    if N <> 4 then Exit;
    MinX := V[0]; MinY := V[1]; MaxX := V[2]; MaxY := V[3];
    Result := True;
  finally
    L.Free;
  end;
end;

function TryParseMapInfoCoordSys(const AValue: string;
  out ACoordSys: TMapInfoCoordSys): Boolean;
var
  S,MainPart,BoundsPart: string;
  P,I,Index,DatumCount: Integer;
  Tokens: TStringList;
  V: Double;
begin
  ACoordSys := Default(TMapInfoCoordSys);
  ACoordSys.EllipsoidID := -1;
  Result := False;
  S := Trim(AValue);
  if S = '' then Exit;

  if SameText(Copy(S,1,9),'CoordSys ') then
    S := Trim(Copy(S,10,MaxInt));

  if SameText(Copy(S,1,16),'Earth Projection') then
  begin
    ACoordSys.IsEarth := True;
    S := Trim(Copy(S,17,MaxInt));
  end
  else if SameText(Copy(S,1,14),'NonEarth Units') then
  begin
    ACoordSys.IsEarth := False;
    ACoordSys.ProjectionID := 0;
    ACoordSys.DatumID := 0;
    S := Trim(Copy(S,15,MaxInt));
  end
  else Exit;

  P := MIFindKeywordOutsideQuotes(S,' Bounds ');
  if P > 0 then
  begin
    MainPart := Trim(Copy(S,1,P-1));
    BoundsPart := Trim(Copy(S,P+8,MaxInt));
    if not MIParseBounds(BoundsPart,ACoordSys.BoundsMinX,ACoordSys.BoundsMinY,
      ACoordSys.BoundsMaxX,ACoordSys.BoundsMaxY) then Exit;
    ACoordSys.HasBounds := True;
  end
  else MainPart := S;

  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ',';
    Tokens.StrictDelimiter := True;
    Tokens.QuoteChar := '"';
    Tokens.DelimitedText := MainPart;
    // TStringList only recognizes QuoteChar when it is the first character
    // of a field. MapInfo normally writes a space after each comma, e.g.
    // `..., 104, "m", ...`, so trim and dequote explicitly.
    for I := 0 to Tokens.Count - 1 do
      Tokens[I] := MIUnquoteToken(Tokens[I]);

    if not ACoordSys.IsEarth then
    begin
      if Tokens.Count < 1 then Exit;
      ACoordSys.UnitsName := Tokens[0];
      Result := ACoordSys.UnitsName <> '';
      Exit;
    end;

    if Tokens.Count < 2 then Exit;
    if not TryStrToInt(Tokens[0],ACoordSys.ProjectionID) then Exit;
    // Legacy MITAB accepts 1000/2000/3000-prefixed projection numbers and
    // normalizes them to the base MapInfo projection method.
    if ACoordSys.ProjectionID >= 3000 then
      Dec(ACoordSys.ProjectionID,3000)
    else if ACoordSys.ProjectionID >= 2000 then
      Dec(ACoordSys.ProjectionID,2000)
    else if ACoordSys.ProjectionID >= 1000 then
      Dec(ACoordSys.ProjectionID,1000);
    if not TryStrToInt(Tokens[1],ACoordSys.DatumID) then Exit;
    Index := 2;

    DatumCount := 0;
    if (ACoordSys.DatumID = 999) or (ACoordSys.DatumID = 9999) then
    begin
      // A custom datum is not valid without its ellipsoid and the complete
      // 3- or 8-value parameter set.  Do not let a malformed datum shift the
      // units/projection fields and accidentally parse as another CoordSys.
      if (Index >= Tokens.Count) or
         not TryStrToInt(Tokens[Index],ACoordSys.EllipsoidID) then Exit;
      Inc(Index);
      if ACoordSys.DatumID = 999 then DatumCount := 3 else DatumCount := 8;
      SetLength(ACoordSys.DatumParameters,DatumCount);
      for I := 0 to DatumCount - 1 do
      begin
        if (Index >= Tokens.Count) or
           not MITryFloat(Tokens[Index],ACoordSys.DatumParameters[I]) then Exit;
        Inc(Index);
      end;
    end;

    if ACoordSys.ProjectionID <> 1 then
    begin
      if Index >= Tokens.Count then Exit;
      ACoordSys.UnitsName := Tokens[Index];
      Inc(Index);
      if ACoordSys.UnitsName = '' then Exit;
    end;

    SetLength(ACoordSys.ProjectionParameters,Tokens.Count-Index);
    I := 0;
    while Index < Tokens.Count do
    begin
      if not MITryFloat(Tokens[Index],V) then Exit;
      ACoordSys.ProjectionParameters[I] := V;
      Inc(I);
      Inc(Index);
    end;
    Result := True;
  finally
    Tokens.Free;
  end;
end;

function MapInfoCoordSysToString(const ACoordSys: TMapInfoCoordSys;
  const IncludeCoordSysKeyword: Boolean): string;
var
  I: Integer;
begin
  if not ACoordSys.IsEarth then
  begin
    Result := 'NonEarth Units ' + MIQuote(ACoordSys.UnitsName);
    if ACoordSys.HasBounds then
      Result := Result + ' Bounds (' + MIInvariantFloat(ACoordSys.BoundsMinX) + ', ' +
        MIInvariantFloat(ACoordSys.BoundsMinY) + ') (' +
        MIInvariantFloat(ACoordSys.BoundsMaxX) + ', ' +
        MIInvariantFloat(ACoordSys.BoundsMaxY) + ')';
    if IncludeCoordSysKeyword then Result := 'CoordSys ' + Result;
    Exit;
  end;

  Result := Format('Earth Projection %d, %d',[ACoordSys.ProjectionID,ACoordSys.DatumID]);
  if ((ACoordSys.DatumID = 999) or (ACoordSys.DatumID = 9999)) and
     (ACoordSys.EllipsoidID >= 0) then
  begin
    Result := Result + ', ' + IntToStr(ACoordSys.EllipsoidID);
    for I := 0 to Length(ACoordSys.DatumParameters)-1 do
      Result := Result + ', ' + MIInvariantFloat(ACoordSys.DatumParameters[I]);
  end;

  if ACoordSys.ProjectionID <> 1 then
    Result := Result + ', ' + MIQuote(ACoordSys.UnitsName);
  for I := 0 to Length(ACoordSys.ProjectionParameters)-1 do
    Result := Result + ', ' + MIInvariantFloat(ACoordSys.ProjectionParameters[I]);

  if ACoordSys.HasBounds then
    Result := Result + ' Bounds (' + MIInvariantFloat(ACoordSys.BoundsMinX) + ', ' +
      MIInvariantFloat(ACoordSys.BoundsMinY) + ') (' +
      MIInvariantFloat(ACoordSys.BoundsMaxX) + ', ' +
      MIInvariantFloat(ACoordSys.BoundsMaxY) + ')';
  if IncludeCoordSysKeyword then Result := 'CoordSys ' + Result;
end;

procedure MIAddProjParam(AList: TStrings; const AName: string; const AValue: Double);
begin
  AList.Add('+'+AName+'='+MIInvariantFloat(AValue));
end;

function MIProjectionParamCount(const ProjectionID: Integer): Integer;
begin
  // Canonical parameter counts produced by legacy MITAB/GDAL 2.4.
  case ProjectionID of
    1: Result := 0;
    2: Result := 2;
    3,6,9,19,32: Result := 6;
    4,5,28,29: Result := 3;
    7: Result := 6;
    8,20,21,22,23,24,31,34: Result := 5;
    10,11,12,13,14,15,16,17: Result := 1;
    18,25,27,30,33: Result := 4;
    26: Result := 2;
    35: Result := 7;
  else
    Result := -1;
  end;
end;

function MIProjectionParamsValid(const ProjectionID, ParamCount: Integer): Boolean;
begin
  // Some old MIF writers include optional values that legacy GDAL can read
  // although it does not emit them itself.  Accept those variants while
  // keeping a single canonical representation on output.
  case ProjectionID of
    2:
      Result := (ParamCount = 2) or (ParamCount = 4);
    4,5,28,29:
      Result := (ParamCount = 2) or (ParamCount = 3);
  else
    Result := (MIProjectionParamCount(ProjectionID) >= 0) and
              (ParamCount = MIProjectionParamCount(ProjectionID));
  end;
end;

function MIProjectionToProj(const C: TMapInfoCoordSys; AList: TStrings;
  const UnitToMeters: Double): Boolean;
var
  P: TArray<Double>;
  F: Double;
begin
  Result := False;
  P := C.ProjectionParameters;
  if not MIProjectionParamsValid(C.ProjectionID,Length(P)) then Exit;
  Result := True;
  F := UnitToMeters;
  if F <= 0 then F := 1.0;
  case C.ProjectionID of
    1: AList.Add('+proj=longlat');
    2: begin
      AList.Add('+proj=cea');
      MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0));
      MIAddProjParam(AList,'lat_ts',MIDoubleArrayValue(P,1));
      if Length(P) = 4 then
      begin
        MIAddProjParam(AList,'x_0',P[2]*F);
        MIAddProjParam(AList,'y_0',P[3]*F);
      end;
    end;
    3,19: begin AList.Add('+proj=lcc'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'lat_1',MIDoubleArrayValue(P,2)); MIAddProjParam(AList,'lat_2',MIDoubleArrayValue(P,3)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,4)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,5)*F); end;
    4,29: begin
      if (Length(P) = 3) and not SameValue(P[2],90.0,1E-10) then Exit(False);
      AList.Add('+proj=laea');
      MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0));
      MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1));
    end;
    5,28: begin
      if (Length(P) = 3) and not SameValue(P[2],90.0,1E-10) then Exit(False);
      AList.Add('+proj=aeqd');
      MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0));
      MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1));
    end;
    6: begin AList.Add('+proj=eqdc'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'lat_1',MIDoubleArrayValue(P,2)); MIAddProjParam(AList,'lat_2',MIDoubleArrayValue(P,3)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,4)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,5)*F); end;
    7,35: begin
      AList.Add('+proj=omerc'); MIAddProjParam(AList,'lonc',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'alpha',MIDoubleArrayValue(P,2));
      if C.ProjectionID=35 then begin MIAddProjParam(AList,'gamma',MIDoubleArrayValue(P,3)); MIAddProjParam(AList,'k_0',MIDoubleArrayValue(P,4,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,5)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,6)*F); end
      else begin MIAddProjParam(AList,'gamma',90); MIAddProjParam(AList,'k_0',MIDoubleArrayValue(P,3,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,4)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,5)*F); end;
    end;
    8,21,22,23,24,34: begin if C.ProjectionID=34 then AList.Add('+proj=etmerc') else AList.Add('+proj=tmerc'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'k_0',MIDoubleArrayValue(P,2,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,3)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,4)*F); end;
    9: begin AList.Add('+proj=aea'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'lat_1',MIDoubleArrayValue(P,2)); MIAddProjParam(AList,'lat_2',MIDoubleArrayValue(P,3)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,4)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,5)*F); end;
    10,26: begin AList.Add('+proj=merc'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); if C.ProjectionID=26 then MIAddProjParam(AList,'lat_ts',MIDoubleArrayValue(P,1)) else begin MIAddProjParam(AList,'lat_0',0); MIAddProjParam(AList,'k_0',1); end; end;
    11: begin AList.Add('+proj=mill'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    12: begin AList.Add('+proj=robin'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    13: begin AList.Add('+proj=moll'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    14: begin AList.Add('+proj=eck4'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    15: begin AList.Add('+proj=eck6'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    16: begin AList.Add('+proj=sinu'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    17: begin AList.Add('+proj=gall'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); end;
    18: begin AList.Add('+proj=nzmg'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,2)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,3)*F); end;
    20,31: begin if C.ProjectionID=31 then AList.Add('+proj=sterea') else AList.Add('+proj=stere'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'k_0',MIDoubleArrayValue(P,2,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,3)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,4)*F); end;
    25: begin AList.Add('+proj=somerc'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,2)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,3)*F); end;
    27: begin AList.Add('+proj=poly'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,2)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,3)*F); end;
    30: begin AList.Add('+proj=cass'); MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0)); MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1)); MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,2)*F); MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,3)*F); end;
    32: begin
      // PROJ 5.2 Krovak cannot vary these MapInfo metadata parameters.
      // Accept only values compatible with the GDAL 2.4 legacy Krovak form.
      if not SameValue(P[2],MAPINFO_KROVAK_PSEUDO_STD_PARALLEL,
                       MAPINFO_KROVAK_PARAM_TOLERANCE) or
         not SameValue(P[3],MAPINFO_KROVAK_AZIMUTH,
                       MAPINFO_KROVAK_PARAM_TOLERANCE) then Exit(False);
      AList.Add('+proj=krovak');
      MIAddProjParam(AList,'lon_0',P[0]);
      MIAddProjParam(AList,'lat_0',P[1]);
      // Alpha is retained as WKT/round-trip metadata. PROJ 5.2 ignores it.
      MIAddProjParam(AList,'alpha',MAPINFO_KROVAK_AZIMUTH);
      // MapInfo/GDAL 2.4 projection 32 uses scale 1.0.  Explicitly set it
      // because bare PROJ 5.2 Krovak defaults to 0.9999.
      MIAddProjParam(AList,'k_0',1);
      MIAddProjParam(AList,'x_0',P[4]*F);
      MIAddProjParam(AList,'y_0',P[5]*F);
    end;
    33: begin
      AList.Add('+proj=eqc');
      MIAddProjParam(AList,'lon_0',MIDoubleArrayValue(P,0));
      MIAddProjParam(AList,'lat_0',MIDoubleArrayValue(P,1));
      MIAddProjParam(AList,'lat_ts',0);
      MIAddProjParam(AList,'x_0',MIDoubleArrayValue(P,2)*F);
      MIAddProjParam(AList,'y_0',MIDoubleArrayValue(P,3)*F);
    end;
  else Result := False;
  end;
end;

function MIAddDatumToProj(const C: TMapInfoCoordSys; AList: TStrings): Boolean;
var
  D: pjMapInfoDatumInfo;
  E: pjMapInfoSpheroidInfo;
  P: TArray<Double>;
  Towgs: string;
begin
  Result := False;

  // MapInfo datum 157 is the historical Google/Pseudo-Mercator special
  // case. The projection uses the WGS84 semi-major radius as a sphere; the
  // regular MapInfo spheroid 54 values must not be emitted here.
  if (C.ProjectionID = 10) and (C.DatumID = 157) then
  begin
    AList.Add('+lat_ts=0');
    AList.Add('+a=6378137');
    AList.Add('+b=6378137');
    AList.Add('+nadgrids=@null');
    AList.Add('+wktext');
    Exit(True);
  end;

  if C.DatumID = 104 then
  begin
    AList.Add('+datum=WGS84');
    Exit(True);
  end;

  if (C.DatumID=999) or (C.DatumID=9999) then
  begin
    if (C.EllipsoidID < 0) or not MIMapInfoSpheroidByID(C.EllipsoidID,E) then Exit;
    if ((C.DatumID=999) and (Length(C.DatumParameters)<>3)) or
       ((C.DatumID=9999) and (Length(C.DatumParameters)<>8)) then Exit;

    AList.Add('+a='+MIInvariantFloat(E.A));
    if SameValue(E.RF,0.0) then
      AList.Add('+b='+MIInvariantFloat(E.A))
    else
      AList.Add('+rf='+MIInvariantFloat(E.RF));

    P := C.DatumParameters;
    Towgs := MIInvariantFloat(P[0])+','+MIInvariantFloat(P[1])+','+MIInvariantFloat(P[2]);
    if C.DatumID=9999 then
      Towgs := Towgs+','+MIInvariantFloat(-P[3])+','+MIInvariantFloat(-P[4])+','+
        MIInvariantFloat(-P[5])+','+MIInvariantFloat(P[6]);
    AList.Add('+towgs84='+Towgs);
    if (C.DatumID=9999) and not SameValue(P[7],0.0) then
      AList.Add('+pm='+MIInvariantFloat(P[7]));
    Exit(True);
  end;

  if not MIMapInfoDatumByID(C.DatumID,D) then Exit;
  if not MIMapInfoSpheroidByID(D.EllID,E) then Exit;

  AList.Add('+a='+MIInvariantFloat(E.A));
  if SameValue(E.RF,0.0) then
    AList.Add('+b='+MIInvariantFloat(E.A))
  else
    AList.Add('+rf='+MIInvariantFloat(E.RF));

  Towgs := MIInvariantFloat(D.dX)+','+MIInvariantFloat(D.dY)+','+MIInvariantFloat(D.dZ);
  if not SameValue(D.rX,0.0) or not SameValue(D.rY,0.0) or
     not SameValue(D.rZ,0.0) or not SameValue(D.ScaleFactor,0.0) then
    Towgs := Towgs+','+MIInvariantFloat(-D.rX)+','+MIInvariantFloat(-D.rY)+','+
      MIInvariantFloat(-D.rZ)+','+MIInvariantFloat(D.ScaleFactor);
  AList.Add('+towgs84='+Towgs);
  if not SameValue(D.PrimeMeridian,0.0) then
    AList.Add('+pm='+MIInvariantFloat(D.PrimeMeridian));
  Result := True;
end;

function MapInfoCoordSysToLibProjDefn(const ACoordSys: TMapInfoCoordSys): string;
var
  L: TStringList;
  ProjUnit: string;
  UnitToMeters: Double;
begin
  Result := '';
  if not ACoordSys.IsEarth then Exit;
  ProjUnit := ''; UnitToMeters := 1.0;
  if (ACoordSys.ProjectionID<>1) and not MIUnitInfoByName(ACoordSys.UnitsName,ProjUnit,UnitToMeters) then Exit;
  L := TStringList.Create;
  try
    if not MIProjectionToProj(ACoordSys,L,UnitToMeters) then Exit;
    if not MIAddDatumToProj(ACoordSys,L) then Exit;
    if ACoordSys.ProjectionID<>1 then
      if ProjUnit<>'' then L.Add('+units='+ProjUnit) else L.Add('+to_meter='+MIUnitFactorText(ACoordSys.UnitsName,UnitToMeters));
    L.Add('+no_defs');
    L.LineBreak := ' ';
    Result := Trim(L.Text);
  finally
    L.Free;
  end;
end;

function MapInfoCoordSysToLibProjDefn(const ACoordSys: string): string;
var C: TMapInfoCoordSys;
begin
  if TryParseMapInfoCoordSys(ACoordSys,C) then Result := MapInfoCoordSysToLibProjDefn(C) else Result := '';
end;

function MIProjPairDouble(Pairs: TStrings; const Name: string; const Default: Double = 0.0): Double;
var S: string;
begin
  Result := Default;
  if TryFindProjPairValue(Pairs,Name,S,'') then MITryFloat(S,Result);
end;

function MIProjScale(Pairs: TStrings; const Default: Double = 1.0): Double;
var
  S: string;
begin
  Result := Default;
  if TryFindProjPairValue(Pairs,'k_0',S,'') then
    MITryFloat(S,Result)
  else if TryFindProjPairValue(Pairs,'k',S,'') then
    MITryFloat(S,Result);
end;

function MIResolveProjEllipsoid(Pairs: TStrings; out A,RF: Double): Boolean;
var
  S,Name,EllName,EA,ERF: string;
  B,F,ES,Ecc: Double;
  DInfo: PpjDatum;
  HaveBaseShape: Boolean;
  I: Integer;
const
  SpherificationKeys: array[0..6] of string =
    ('R_A','R_V','R_a','R_g','R_h','R_lat_a','R_lat_g');
begin
  Result := False;
  A := 0.0;
  RF := 0.0;
  HaveBaseShape := False;

  // +R has absolute precedence in PROJ 5.2 and turns the definition into a
  // sphere regardless of ellps/datum/shape parameters.
  if TryFindProjPairValue(Pairs,'R',S,'') then
  begin
    Result := MITryFloat(S,A) and (A > 0.0);
    if Result then RF := 0.0;
    Exit;
  end;

  // Spherification parameters alter the effective radius using formulas in
  // pj_ell_set.c.  They cannot be represented by a MapInfo datum/ellipsoid
  // without evaluating the exact PROJ operation, so reject instead of
  // silently exporting the original ellipsoid.
  for I := Low(SpherificationKeys) to High(SpherificationKeys) do
    if TryFindProjPairValue(Pairs,SpherificationKeys[I],S,'') then
      Exit(False);

  // Establish the inherited size/shape from an explicit ellipsoid first,
  // otherwise from the named datum. Explicit +a and shape parameters below
  // are allowed to override that base, matching PROJ 5.2 pj_ellipsoid().
  if TryFindProjPairValue(Pairs,'ellps',Name,'') then
  begin
    if not EllipseInfo(Name,EllName,EA,ERF) or
       not MITryFloat(EA,A) or not MITryFloat(ERF,RF) then Exit(False);
    HaveBaseShape := True;
  end
  else if TryFindProjPairValue(Pairs,'datum',Name,'') then
  begin
    if not DatumInfo(Name,DInfo) or
       not EllipseInfo(DInfo.pjEllKey,EllName,EA,ERF) or
       not MITryFloat(EA,A) or not MITryFloat(ERF,RF) then Exit(False);
    HaveBaseShape := True;
  end;

  if TryFindProjPairValue(Pairs,'a',S,'') then
  begin
    if not MITryFloat(S,A) or (A <= 0.0) then Exit(False);
    // +a alone defines a sphere unless shape was inherited from ellps/datum.
    if not HaveBaseShape then RF := 0.0;
  end
  else if A <= 0.0 then
    Exit(False);

  // PROJ 5.2 shape-parameter precedence: rf, f, es, e, b.
  if TryFindProjPairValue(Pairs,'rf',S,'') then
  begin
    if not MITryFloat(S,RF) or (RF <= 0.0) then Exit(False);
  end
  else if TryFindProjPairValue(Pairs,'f',S,'') then
  begin
    if not MITryFloat(S,F) or (F <= 0.0) or (F >= 1.0) then Exit(False);
    RF := 1.0/F;
  end
  else if TryFindProjPairValue(Pairs,'es',S,'') then
  begin
    if not MITryFloat(S,ES) or (ES < 0.0) or (ES >= 1.0) then Exit(False);
    if SameValue(ES,0.0) then RF := 0.0
    else
    begin
      F := 1.0-Sqrt(1.0-ES);
      if F <= 0.0 then RF := 0.0 else RF := 1.0/F;
    end;
  end
  else if TryFindProjPairValue(Pairs,'e',S,'') then
  begin
    if not MITryFloat(S,Ecc) or (Ecc < 0.0) or (Ecc >= 1.0) then Exit(False);
    ES := Ecc*Ecc;
    if SameValue(ES,0.0) then RF := 0.0
    else
    begin
      F := 1.0-Sqrt(1.0-ES);
      if F <= 0.0 then RF := 0.0 else RF := 1.0/F;
    end;
  end
  else if TryFindProjPairValue(Pairs,'b',S,'') then
  begin
    if not MITryFloat(S,B) or (B <= 0.0) or (B > A) then Exit(False);
    if SameValue(A,B) then RF := 0.0 else RF := A/(A-B);
  end;

  Result := (A > 0.0) and (RF >= 0.0);
end;

function MIParseDoubleList(const S: string; out V: TArray<Double>): Boolean;
var
  L: TStringList;
  I: Integer;
begin
  SetLength(V,0);
  L := TStringList.Create;
  try
    L.Delimiter := ',';
    L.StrictDelimiter := True;
    L.DelimitedText := S;
    if (L.Count<>3) and (L.Count<>7) then Exit(False);
    SetLength(V,L.Count);
    for I := 0 to L.Count-1 do
      if not MITryFloat(L[I],V[I]) then
      begin
        SetLength(V,0);
        Exit(False);
      end;
    Result := True;
  finally
    L.Free;
  end;
end;

function MIParseTowgs(Pairs: TStrings; out V: TArray<Double>): Boolean;
var
  S: string;
begin
  SetLength(V,0);
  Result := TryFindProjPairValue(Pairs,'towgs84',S,'') and MIParseDoubleList(S,V);
end;

function MIDatumMatches(const D: pjMapInfoDatumInfo; const EID: Integer; const Towgs: TArray<Double>; const PM: Double): Boolean;
begin
  Result := (D.EllID=EID) and SameValue(D.dX,MIDoubleArrayValue(Towgs,0),1E-8) and SameValue(D.dY,MIDoubleArrayValue(Towgs,1),1E-8) and SameValue(D.dZ,MIDoubleArrayValue(Towgs,2),1E-8) and SameValue(-D.rX,MIDoubleArrayValue(Towgs,3),1E-8) and SameValue(-D.rY,MIDoubleArrayValue(Towgs,4),1E-8) and SameValue(-D.rZ,MIDoubleArrayValue(Towgs,5),1E-8) and SameValue(D.ScaleFactor,MIDoubleArrayValue(Towgs,6),1E-8) and SameValue(D.PrimeMeridian,PM,1E-8);
end;

procedure MIProjDatumToMapInfo(Pairs: TStrings; var C: TMapInfoCoordSys);
var
  S,PMName: string;
  A,RF,PM: Double;
  E: pjMapInfoSpheroidInfo;
  Towgs: TArray<Double>;
  I: Integer;
  DInfo: PpjDatum;
  HasNamedDatum,HasTowgs: Boolean;
  function IsZeroTowgs: Boolean;
  var J: Integer;
  begin
    Result := True;
    for J := 0 to Length(Towgs)-1 do
      if not SameValue(Towgs[J],0.0,1E-12) then
        Exit(False);
  end;
begin
  C.DatumID := -1;
  C.EllipsoidID := -1;
  SetLength(C.DatumParameters,0);

  DInfo := nil;
  HasNamedDatum := TryFindProjPairValue(Pairs,'datum',S,'');
  if HasNamedDatum and not DatumInfo(S,DInfo) then
    Exit;

  if not MIResolveProjEllipsoid(Pairs,A,RF) then
    Exit;

  PM := 0.0;
  if TryFindProjPairValue(Pairs,'pm',S,'') then
  begin
    if not MITryFloat(S,PM) then
    begin
      PMName := '';
      if not PrimeInfo(S,PMName,PM) then Exit;
    end;
  end;

  HasTowgs := TryFindProjPairValue(Pairs,'towgs84',S,'');
  if HasTowgs then
  begin
    // A present but malformed +towgs84 is an error, not an absent transform.
    if not MIParseDoubleList(S,Towgs) then Exit;
  end
  else if HasNamedDatum and Assigned(DInfo) and (DInfo.toWgs <> '') then
  begin
    if not MIParseDoubleList(DInfo.toWgs,Towgs) then Exit;
  end
  else
  begin
    SetLength(Towgs,7);
    for I := 0 to 6 do Towgs[I] := 0.0;
  end;

  // MapInfo datum 157 is a special Google/Pseudo-Mercator case. Its table
  // ellipsoid (54) is not the actual projection sphere: legacy GDAL exports
  // it as a=b=6378137. Detect the effective PROJ definition before generic
  // MapInfo spheroid matching.
  if (C.ProjectionID = 10) and
     SameValue(A,6378137.0,1E-7) and SameValue(RF,0.0,1E-12) and
     SameValue(PM,0.0,1E-12) and IsZeroTowgs then
  begin
    C.DatumID := 157;
    C.EllipsoidID := 54;
    Exit;
  end;

  if not MIMapInfoSpheroidByARF(A,RF,E) then
    Exit;

  for I := Low(pjMapInfoDatumInfoList) to High(pjMapInfoDatumInfoList) do
    if (pjMapInfoDatumInfoList[I].ID >= 0) and
       (pjMapInfoDatumInfoList[I].ID <> 999) and
       (pjMapInfoDatumInfoList[I].ID <> 9999) and
       MIDatumMatches(pjMapInfoDatumInfoList[I],E.ID,Towgs,PM) then
    begin
      C.DatumID := pjMapInfoDatumInfoList[I].ID;
      C.EllipsoidID := E.ID;
      Exit;
    end;

  C.EllipsoidID := E.ID;
  if not SameValue(MIDoubleArrayValue(Towgs,3),0.0) or
     not SameValue(MIDoubleArrayValue(Towgs,4),0.0) or
     not SameValue(MIDoubleArrayValue(Towgs,5),0.0) or
     not SameValue(MIDoubleArrayValue(Towgs,6),0.0) or
     not SameValue(PM,0.0) then
  begin
    C.DatumID := 9999;
    SetLength(C.DatumParameters,8);
    C.DatumParameters[0] := MIDoubleArrayValue(Towgs,0);
    C.DatumParameters[1] := MIDoubleArrayValue(Towgs,1);
    C.DatumParameters[2] := MIDoubleArrayValue(Towgs,2);
    C.DatumParameters[3] := -MIDoubleArrayValue(Towgs,3);
    C.DatumParameters[4] := -MIDoubleArrayValue(Towgs,4);
    C.DatumParameters[5] := -MIDoubleArrayValue(Towgs,5);
    C.DatumParameters[6] := MIDoubleArrayValue(Towgs,6);
    C.DatumParameters[7] := PM;
  end
  else
  begin
    C.DatumID := 999;
    SetLength(C.DatumParameters,3);
    C.DatumParameters[0] := MIDoubleArrayValue(Towgs,0);
    C.DatumParameters[1] := MIDoubleArrayValue(Towgs,1);
    C.DatumParameters[2] := MIDoubleArrayValue(Towgs,2);
  end;
end;

function MIValidateProjNumericParams(Pairs: TStrings): Boolean;
const
  NumericKeys: array[0..20] of string =
    ('lon_0','lat_0','lat_1','lat_2','lat_ts','x_0','y_0','k','k_0',
     'lonc','alpha','gamma','a','b','rf','f','e','es','R','to_meter','zone');
var
  I,J: Integer;
  S: string;
  V: Double;
begin
  Result := False;
  for I := Low(NumericKeys) to High(NumericKeys) do
    if TryFindProjPairValue(Pairs,NumericKeys[I],S,'') then
    begin
      if SameText(NumericKeys[I],'zone') then
      begin
        if not TryStrToInt(S,J) then Exit;
      end
      else if not MITryProjFactor(S,V) then
        Exit;
    end;
  Result := True;
end;

function LibProjDefnToMapinfoCoordSys(const ADefn: string; out ACoordSys: TMapInfoCoordSys): Boolean;
var
  Pairs: TStringList;
  Proj,Units,S: string;
  ToMeters,XFactor,Gamma,Alpha,Lat0,K: Double;
  Zone: Integer;
  IsWebMerc: Boolean;

  procedure SetParams(const Values: array of Double);
  var
    J: Integer;
  begin
    SetLength(ACoordSys.ProjectionParameters,Length(Values));
    for J := 0 to High(Values) do
      ACoordSys.ProjectionParameters[J] := Values[J];
  end;

  function ReadDouble(const Name: string; const Default: Double;
    out V: Double): Boolean;
  var
    T: string;
  begin
    V := Default;
    if not TryFindProjPairValue(Pairs,Name,T,'') then Exit(True);
    Result := MITryFloat(T,V);
  end;

  function ReadRequiredDouble(const Name: string; out V: Double): Boolean;
  var
    T: string;
  begin
    Result := TryFindProjPairValue(Pairs,Name,T,'') and MITryFloat(T,V);
  end;

  function ReadScale(const Default: Double; out V: Double;
    out WasSpecified: Boolean): Boolean;
  var
    T: string;
  begin
    V := Default;
    WasSpecified := False;
    if TryFindProjPairValue(Pairs,'k_0',T,'') then
    begin
      WasSpecified := True;
      Exit(MITryFloat(T,V));
    end;
    if TryFindProjPairValue(Pairs,'k',T,'') then
    begin
      WasSpecified := True;
      Exit(MITryFloat(T,V));
    end;
    Result := True;
  end;

  function IsZeroParam(const Name: string): Boolean;
  var
    V: Double;
  begin
    Result := ReadDouble(Name,0.0,V) and SameValue(V,0.0,1E-12);
  end;

  function HasFlag(const Name: string): Boolean;
  var
    Dummy: string;
  begin
    Result := TryFindProjPairValue(Pairs,Name,Dummy,'1');
  end;

  function X(const Name: string): Double;
  var
    V: Double;
  begin
    V := 0.0;
    ReadDouble(Name,0.0,V);
    Result := V/XFactor;
  end;

  function ValidateCommonNumericParams: Boolean;
  const
    Names: array[0..18] of string =
      ('lon_0','lat_0','lat_1','lat_2','lat_ts','lonc','alpha','gamma',
       'k','k_0','x_0','y_0','a','b','rf','f','es','e','R');
  var
    J: Integer;
    T: string;
    V: Double;
  begin
    for J := Low(Names) to High(Names) do
      if TryFindProjPairValue(Pairs,Names[J],T,'') and not MITryFloat(T,V) then
        Exit(False);
    Result := True;
  end;

var
  ScaleSpecified: Boolean;
begin
  ACoordSys := Default(TMapInfoCoordSys);
  ACoordSys.IsEarth := True;
  ACoordSys.EllipsoidID := -1;
  Result := False;
  if Trim(ADefn) = '' then Exit;

  Pairs := TStringList.Create;
  try
    Pairs.NameValueSeparator := PROJ_PAIR_DELIMETER;
    if FetchProjPairs(ADefn,Pairs) = 0 then Exit;
    if not TryFindProjPairValue(Pairs,'proj',Proj,'') then Exit;
    if not MIValidateProjNumericParams(Pairs) then Exit;
    if not ValidateCommonNumericParams then Exit;

    // PROJ 5.2 gives +units precedence over +to_meter. Mirror that here.
    Units := 'm';
    ToMeters := 1.0;
    if TryFindProjPairValue(Pairs,'units',S,'') then
    begin
      if not MIUnitInfoByProj(S,0,Units,ToMeters) then Exit;
    end
    else if TryFindProjPairValue(Pairs,'to_meter',S,'') then
    begin
      if not MITryProjFactor(S,ToMeters) or (ToMeters <= 0.0) then Exit;
      if not MIUnitInfoByProj('',ToMeters,Units,XFactor) then Exit;
    end;
    XFactor := ToMeters;
    if XFactor <= 0.0 then Exit;
    ACoordSys.UnitsName := Units;
    IsWebMerc := False;

    if SameText(Proj,'longlat') or SameText(Proj,'latlong') or
       SameText(Proj,'lonlat') or SameText(Proj,'latlon') then
    begin
      ACoordSys.ProjectionID := 1;
      ACoordSys.UnitsName := '';
      SetLength(ACoordSys.ProjectionParameters,0);
    end
    else if SameText(Proj,'cea') then
    begin
      ACoordSys.ProjectionID := 2;
      if IsZeroParam('x_0') and IsZeroParam('y_0') then
        SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_ts')])
      else
        SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_ts'),
                   X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'lcc') then
    begin
      ACoordSys.ProjectionID := 3;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjPairDouble(Pairs,'lat_1'),MIProjPairDouble(Pairs,'lat_2'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'laea') then
    begin
      if not IsZeroParam('x_0') or not IsZeroParam('y_0') then Exit;
      Lat0 := MIProjPairDouble(Pairs,'lat_0');
      if Abs(Abs(Lat0)-90.0) <= 0.001 then ACoordSys.ProjectionID := 4
      else ACoordSys.ProjectionID := 29;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),Lat0,90.0]);
    end
    else if SameText(Proj,'aeqd') then
    begin
      if not IsZeroParam('x_0') or not IsZeroParam('y_0') then Exit;
      Lat0 := MIProjPairDouble(Pairs,'lat_0');
      if Abs(Abs(Lat0)-90.0) <= 0.001 then ACoordSys.ProjectionID := 5
      else ACoordSys.ProjectionID := 28;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),Lat0,90.0]);
    end
    else if SameText(Proj,'eqdc') then
    begin
      ACoordSys.ProjectionID := 6;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjPairDouble(Pairs,'lat_1'),MIProjPairDouble(Pairs,'lat_2'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'omerc') then
    begin
      // MapInfo 7/35 represent the azimuth-center form only.
      if HasFlag('no_off') or HasFlag('no_uoff') or HasFlag('no_rot') then Exit;
      if not ReadRequiredDouble('lonc',Lat0) then Exit;
      if not ReadRequiredDouble('alpha',Alpha) then Exit;
      if not ReadScale(1.0,K,ScaleSpecified) then Exit;
      if TryFindProjPairValue(Pairs,'gamma',S,'') then
      begin
        if not MITryFloat(S,Gamma) then Exit;
      end
      else
        // PJ_omerc.c: when alpha is supplied without gamma, gamma := alpha.
        Gamma := Alpha;

      if SameValue(Gamma,90.0,1E-10) then
      begin
        ACoordSys.ProjectionID := 7;
        SetParams([Lat0,MIProjPairDouble(Pairs,'lat_0'),Alpha,K,X('x_0'),X('y_0')]);
      end
      else
      begin
        ACoordSys.ProjectionID := 35;
        SetParams([Lat0,MIProjPairDouble(Pairs,'lat_0'),Alpha,Gamma,K,
                   X('x_0'),X('y_0')]);
      end;
    end
    else if SameText(Proj,'tmerc') then
    begin
      ACoordSys.ProjectionID := 8;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjScale(Pairs,1),X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'etmerc') then
    begin
      ACoordSys.ProjectionID := 34;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjScale(Pairs,1),X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'utm') then
    begin
      if not TryFindProjPairValue(Pairs,'zone',S,'') or
         not TryStrToInt(S,Zone) or (Zone < 1) or (Zone > 60) then Exit;
      ACoordSys.ProjectionID := 8;
      if HasFlag('south') then
        SetParams([(Zone*6)-183,0,0.9996,500000/XFactor,10000000/XFactor])
      else
        SetParams([(Zone*6)-183,0,0.9996,500000/XFactor,0]);
    end
    else if SameText(Proj,'aea') then
    begin
      ACoordSys.ProjectionID := 9;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjPairDouble(Pairs,'lat_1'),MIProjPairDouble(Pairs,'lat_2'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'merc') then
    begin
      // MapInfo 10/26 do not carry false offsets or a non-zero latitude of
      // origin. Refuse a lossy conversion.
      if not IsZeroParam('x_0') or not IsZeroParam('y_0') or
         not IsZeroParam('lat_0') then Exit;
      if TryFindProjPairValue(Pairs,'lat_ts',S,'') then
      begin
        if not MITryFloat(S,Lat0) then Exit;
        if SameValue(Lat0,0.0,1E-12) then
        begin
          ACoordSys.ProjectionID := 10;
          SetParams([MIProjPairDouble(Pairs,'lon_0')]);
        end
        else
        begin
          ACoordSys.ProjectionID := 26;
          SetParams([MIProjPairDouble(Pairs,'lon_0'),Lat0]);
        end;
      end
      else
      begin
        if not ReadScale(1.0,K,ScaleSpecified) or not SameValue(K,1.0,1E-12) then Exit;
        ACoordSys.ProjectionID := 10;
        SetParams([MIProjPairDouble(Pairs,'lon_0')]);
      end;
    end
    else if SameText(Proj,'webmerc') then
    begin
      // PROJ 5.2 webmerc uses the spherical Mercator formula and forces k0=1.
      if not IsZeroParam('x_0') or not IsZeroParam('y_0') or
         not IsZeroParam('lat_0') then Exit;
      ACoordSys.ProjectionID := 10;
      SetParams([MIProjPairDouble(Pairs,'lon_0')]);
      IsWebMerc := True;
    end
    else if SameText(Proj,'mill') or SameText(Proj,'robin') or
            SameText(Proj,'moll') or SameText(Proj,'eck4') or
            SameText(Proj,'eck6') or SameText(Proj,'sinu') or
            SameText(Proj,'gall') then
    begin
      if not IsZeroParam('x_0') or not IsZeroParam('y_0') then Exit;
      if SameText(Proj,'mill') then ACoordSys.ProjectionID := 11
      else if SameText(Proj,'robin') then ACoordSys.ProjectionID := 12
      else if SameText(Proj,'moll') then ACoordSys.ProjectionID := 13
      else if SameText(Proj,'eck4') then ACoordSys.ProjectionID := 14
      else if SameText(Proj,'eck6') then ACoordSys.ProjectionID := 15
      else if SameText(Proj,'sinu') then ACoordSys.ProjectionID := 16
      else ACoordSys.ProjectionID := 17;
      SetParams([MIProjPairDouble(Pairs,'lon_0')]);
    end
    else if SameText(Proj,'nzmg') then
    begin
      ACoordSys.ProjectionID := 18;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'stere') then
    begin
      ACoordSys.ProjectionID := 20;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjScale(Pairs,1),X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'somerc') then
    begin
      ACoordSys.ProjectionID := 25;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'poly') then
    begin
      ACoordSys.ProjectionID := 27;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'cass') then
    begin
      ACoordSys.ProjectionID := 30;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'sterea') then
    begin
      ACoordSys.ProjectionID := 31;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MIProjScale(Pairs,1),X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'krovak') then
    begin
      if HasFlag('czech') then Exit;
      if TryFindProjPairValue(Pairs,'lat_ts',S,'') then
      begin
        if not MITryFloat(S,Lat0) or
           not SameValue(Lat0,MAPINFO_KROVAK_PSEUDO_STD_PARALLEL,
                         MAPINFO_KROVAK_PARAM_TOLERANCE) then Exit;
      end;
      if TryFindProjPairValue(Pairs,'alpha',S,'') then
      begin
        if not MITryFloat(S,Alpha) or
           not SameValue(Alpha,MAPINFO_KROVAK_AZIMUTH,
                         MAPINFO_KROVAK_PARAM_TOLERANCE) then Exit;
      end;
      if not ReadScale(0.9999,K,ScaleSpecified) or not ScaleSpecified or
         not SameValue(K,1.0,1E-12) then Exit;
      ACoordSys.ProjectionID := 32;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 MAPINFO_KROVAK_PSEUDO_STD_PARALLEL,MAPINFO_KROVAK_AZIMUTH,
                 X('x_0'),X('y_0')]);
    end
    else if SameText(Proj,'eqc') then
    begin
      if not ReadDouble('lat_ts',0.0,Lat0) or not SameValue(Lat0,0.0,1E-12) then Exit;
      ACoordSys.ProjectionID := 33;
      SetParams([MIProjPairDouble(Pairs,'lon_0'),MIProjPairDouble(Pairs,'lat_0'),
                 X('x_0'),X('y_0')]);
    end
    else
      Exit;

    if IsWebMerc then
    begin
      // webmerc's ellipsoidal shape is ignored by the projection itself. It
      // represents the same MapInfo special datum 157 when based on WGS84's
      // semi-major radius and no non-Greenwich datum shift is requested.
      if TryFindProjPairValue(Pairs,'pm',S,'') and not SameText(S,'greenwich') and
         not SameText(S,'0') and not SameText(S,'0.0') then Exit;
      if TryFindProjPairValue(Pairs,'towgs84',S,'') then
      begin
        // Let the generic datum path validate non-zero/invalid transforms.
        MIProjDatumToMapInfo(Pairs,ACoordSys);
        if ACoordSys.DatumID <> 157 then Exit;
      end
      else
      begin
        ACoordSys.DatumID := 157;
        ACoordSys.EllipsoidID := 54;
      end;
    end
    else
    begin
      MIProjDatumToMapInfo(Pairs,ACoordSys);
      if ACoordSys.DatumID < 0 then Exit;
    end;

    Result := True;
  finally
    Pairs.Free;
  end;
end;

function LibProjDefnToMapinfoCoordSys(const ADefn: string): string;
var C: TMapInfoCoordSys;
begin
  if LibProjDefnToMapinfoCoordSys(ADefn,C) then Result := MapInfoCoordSysToString(C,True) else Result := '';
end;

function LibProjKnownParams(var AParams: TArray<TPJParamRec>): Integer;
var
  i: Integer;
begin
  Result := 0;

  SetLength(AParams, pjParamsListMax);

  for i := 0 to pjParamsListMax -1 do
  begin
    if pjParamsList[i].pjKey = 'k' then Continue;

    if (i > 0) and (pjParamsList[Pred(i)].pjKey = pjParamsList[i].pjKey) then Continue;


    AParams[Result].Name := pjParamsList[i].pjKey;
    AParams[Result].DisplayName := pjParamsList[i].Description;
    if AParams[Result].DisplayName = '' then
      AParams[Result].DisplayName := AParams[Result].Name;

    AParams[Result].Desctiption := LibProjParamExplain(AParams[Result].Name);
    AParams[Result].Group := pjParamsList[i].paramGroup;

    Inc(Result);
  end;

  SetLength(AParams,Result);
end;

function LibProjProjectionParamGroup(const AParamName: string): Integer;
var
  i: integer;
begin
  for i := 0 to pjParamsListMax -1 do
  begin
    if SameText(AParamName,pjParamsList[i].pjKey) then
      Exit(pjParamsList[i].paramGroup);

  end;
  Result := PJ_PARAM_GROUP_OTHER;
end;

function LibProjProjectionAllowedParams(const AProjID: string; var AParams: TArray<TPJParamRec>): integer;
var
  i: Integer;
  opt: string;
  pv: {$if compilerversion < 33.0}TArray<string>{$else}System.Types.TStringDynArray{$ifend};
begin
  Result := 0;

  i := FProjDefParams.IndexOfName(AProjID.Trim);
  if i > -1 then
  begin
    for opt in FProjDefParams.ValueFromIndex[i].Trim.Split([','],'"','"',TStringSplitOptions.ExcludeEmpty) do
    begin
      pv := opt.Split(['='],'"','"',TStringSplitOptions.ExcludeEmpty);
      i := Length(pv);
      if i < 1 then Continue;

      if Length(AParams) <= Result then
        SetLength(AParams,Result + 64);

      AParams[Result].Name := pv[0].Trim;
      AParams[Result].DisplayName := LibProjParamDisplayName(AParams[Result].Name);
      AParams[Result].Desctiption := LibProjParamExplain(AParams[Result].Name);
      if i > 1 then
        AParams[Result].Value := pv[1];

      AParams[Result].Group := LibProjProjectionParamGroup(AParams[Result].Name);
      Inc(Result);
    end;
  end;

  SetLength(AParams, Result);
end;

function LibProjParamDisplayName(const AParam: string): string;
var
  parm: TpjParam;
begin
  Result := AParam.Trim;

  for parm in pjParamsList do
  begin
    if SameText(AParam,parm.pjKey) and not parm.Description.IsEmpty then
    begin
      Exit(parm.Description);
    end;
  end;
end;

function LibProjParamExplain(const AKey: string): string;
var
  i: integer;
begin
  Result := AKey;
  i := FProjParamExplain.IndexOfName(AKey);
  if i > -1 then
    Result := FProjParamExplain.ValueFromIndex[i];

  if Result = AKey then
    Result := '';
end;

function LibProjParseParamsString(const ADefn: string; var AParams: TArray<TPJParamRec>): Integer;
var
  i: Integer;
  opt: string;
  pv: {$if compilerversion < 33.0}TArray<string>{$else}System.Types.TStringDynArray{$ifend};
begin
  Result := 0;

  for opt in ADefn.Trim.Split(['+'],'"','"',TStringSplitOptions.ExcludeEmpty) do
  begin
    pv := opt.Trim.Split(['='],'"','"',TStringSplitOptions.ExcludeEmpty);
    i := Length(pv);
    if (i < 1) or (pv[0].Trim = '') then
      Continue;

    if (Length(AParams) <= Result) then
      SetLength(AParams, Result + 64);

    AParams[Result].Name := pv[0].Trim;
    AParams[Result].DisplayName := LibProjParamDisplayName(AParams[Result].Name);
    if i > 1 then
      AParams[Result].Value := pv[1];

    Inc(Result);
  end;

  SetLength(AParams, Result);
end;

initialization
  FWktName2ProjName := TStringList.Create;
  FWktName2ProjName.Text :=
    'Albers_Conic_Equal_Area=aea' + sLineBreak + 'Azimuthal_Equidistant=aeqd' +sLineBreak +
    'Cassini_Soldner=cass' + sLineBreak + 'Cylindrical_Equal_Area=cea' + sLineBreak +
    'Eckert_IV=eck4' + sLineBreak + 'Eckert_VI=eck6' + sLineBreak + 'Equidistant_Conic=eqdc' + sLineBreak +
    'Equirectangular=eqc' + sLineBreak + 'Plate Caree=eqc' + sLineBreak +
    'Extended_Transverse_Mercator=etmerc' + sLineBreak +
    'Transverse_Mercator=tmerc' + sLineBreak + 'Gauss_Kruger=tmerc' + sLineBreak + 'Gauss-Kruger=tmerc' + sLineBreak + 'Gauss Kruger=tmerc' +sLineBreak +
    'Gall_Stereographic=gall' + sLineBreak + 'GEOS=geos' + sLineBreak + 'Gnomonic=gnom' + sLineBreak +
    'hotine_oblique_mercator=omerc' + sLineBreak + 'Krovak=krovak' + sLineBreak + 'Lambert_Azimuthal_Equal_Area=laea' + sLineBreak +
    'Lambert_Conformal_Conic=lcc'+ sLineBreak + 'Lambert_Conformal_Conic_1SP=lcc'+ sLineBreak +'Lambert_Conformal_Conic_2SP=lcc' + sLineBreak +
    'Miller_Cylindrical=mill' + sLineBreak + 'Mollweide=moll' + sLineBreak + 'Mercator_1SP=merc' + sLineBreak +
    'Mercator_2SP=merc' + sLineBreak +'Mercator_Auxiliary_Sphere=merc' + sLineBreak + 'New_Zealand_Map_Grid=nzmg' + sLineBreak +
    'ObliqueMercator_Hotine=omerc'+ sLineBreak+'ObliqueMercator=omerc' + sLineBreak + 'Oblique_Stereographic=sterea' + sLineBreak +
    'Orthographic=ortho' + sLineBreak + 'Polar_Stereographic=stere' + sLineBreak + 'Stereographic=stere' + sLineBreak +
    'Polyconic=poly' + sLineBreak + 'Robinson=robin' + sLineBreak + 'Sinusoidal=sinu' + sLineBreak +
    'Transverse_Mercator_South_Orientated=tmerc' + sLineBreak + 'VanDerGrinten=vandg';

  FWktParamName2Proj := TStringList.Create;
  FWktParamName2Proj.Text := 'false_easting=x_0' + sLineBreak + 'false_northing=y_0' + sLineBreak +
    'scale_factor=k_0' + sLineBreak + 'standard_parallel_1=lat_1' + sLineBreak + 'standard_parallel_2=lat_2' + sLineBreak +
    'longitude_of_center=lon_0' + sLineBreak + 'central_meridian=lon_0' + sLineBreak + 'latitude_of_origin=lat_0' + sLineBreak +
    'latitude_of_center=lat_0' + sLineBreak + 'azimuth=alpha' + sLineBreak +
    'rectified_grid_angle=gamma';

  FWktEll2Proj := TStringList.Create();
  FWktEll2Proj.Text := 'MERIT 1983=MERIT' + sLineBreak + 'Soviet Geodetic System 85=SGS85' + sLineBreak +
    'GRS 1980=GRS80' + sLineBreak + 'IUGG 1980=GRS80' + sLineBreak + 'IAU 1976=IAU76' + sLineBreak +
    'Airy 1830=airy' + sLineBreak + 'Appl. Physics. 1965=APL4.9' + sLineBreak +
    'Naval Weapons Lab., 1965=NWL9D' + sLineBreak + 'Modified Airy=mod_airy' + sLineBreak +
    'Andrae 1876=andrae' + sLineBreak + 'Australian Natl & S. Amer. 1969=aust_SA' + sLineBreak +
    'GRS 67(IUGG 1967)=GRS67' + sLineBreak + 'Bessel 1841=bessel' + sLineBreak + 'Bessel_1841=bessel' + sLineBreak +
    'Bessel1841=bessel' + sLineBreak + 'Bessel 1841 (Namibia)=bess_nam' + sLineBreak + 'Clarke 1866=clrk66' + sLineBreak +
    'Clarke 1880 mod=clrk80' + sLineBreak + 'Comm. des Poids et Mesures 1799=CPM' + sLineBreak +
    'Delambre 1810=delmbr' + sLineBreak + 'Engelis 1985=engelis' + sLineBreak + 'Everest 1830=evrst30' + sLineBreak +
    'Everest 1948=evrst48' + sLineBreak + 'Everest 1956=evrst56' + sLineBreak + 'Everest 1969=evrst69' + sLineBreak +
    'Everest (Sabah & Sarawak)=evrstSS' + sLineBreak + 'Fischer (Mercury Datum) 1960=fschr60' + sLineBreak +
    'Modified Fischer 1960=fschr60m' + sLineBreak + 'Fischer 1968=fschr68' + sLineBreak +
    'Helmert 1906=helmert' + sLineBreak + 'Hough=hough' + sLineBreak + 'International 1909 (Hayford)=intl' + sLineBreak +
    'Krassovsky=krass' + sLineBreak +
    'Krassovsky, 1942=krass' + sLineBreak + 'Krassovsky_1942=krass' + sLineBreak +
    'Krassovsky 1942=krass' + sLineBreak + 'Kaula 1961=kaula' + sLineBreak + 'Lerch 1979=lerch' + sLineBreak +
    'Maupertius 1738=mprts' + sLineBreak + 'New International 1967=new_intl' + sLineBreak +
    'Plessis 1817 (France)=plessis' + sLineBreak + 'Southeast Asia=SEasia' + sLineBreak + 'Walbeck=walbeck' + sLineBreak +
    'WGS 60=WGS60' + sLineBreak + 'WGS 66=WGS66' + sLineBreak + 'WGS 72=WGS72' + sLineBreak + 'WGS 84=WGS84' + sLineBreak +
    'WGS_84=WGS84' + sLineBreak + 'WGS84=WGS84' + sLineBreak + 'WGS1984=WGS84' + sLineBreak +
    'WGS 1984=WGS84' + sLineBreak + 'WGS_1984=WGS84' + sLineBreak + 'Normal Sphere (r=6370997)=sphere';

  FProjDefParams := TStringList.Create;
  FProjDefParams.Text :=
    'aea=lat_1,lat_2,lon_0,ellps,R,x_0,y_0,a,rf,axis'+sLineBreak+
    'aeqd=k_0,lat_ts,lat_0,lon_0,x_0,y_0,ellps,R,a,rf,axis'+sLineBreak+
    'airy=lat_0,lon_0,x_0,y_0,R,axis'+sLineBreak+
    'aitoff=lon_0,R,x_0,y_0,ellps,a,rf,axis'+sLineBreak+
    'alsk=x_0,y_0,ellps,R,a,rf,axis'+sLineBreak+
    'apian=lat_0,lon_0,R,x_0,y_0,axis'+sLineBreak+
    'calcofi=ellps,R,a,rf,axis'+sLineBreak+
    'cass=lat_0,lon_0,x_0,y_0,ellps,R,a,rf,axis'+sLineBreak+
    'ccon=lat_1,lon_0,R,x_0,y_0,a,rf,axis'+sLineBreak+
    'eqc=lon_0,lat_0,lat_ts,x_0,y_0,ellps,R,a,rf,axis,over'+sLineBreak+
    'gall=lon_0,R,x_0,y_0,ellps,a,rf,axis'+sLineBreak+
    'geos=sweep,h,lon_0,R,ellps,x_0,y_0,a,rf,axis'+sLineBreak+
    'hatano=lon_0,R,x_0,y_0,axis'+sLineBreak+
    'healpix=lon_0,x_0,y_0,ellps,R,a,rf,axis'+sLineBreak+
    'merc=lat_ts,k_0,lon_0,x_0,y_0,ellps,R,a,rf,axis'+sLineBreak+
    'mill=lon_0,R,x_0,y_0,axis'+sLineBreak+
    'natearth=lon_0,R,x_0,y_0,axis'+sLineBreak+
    'nsper=h,lon_0,R,x_0,y_0,axis'+sLineBreak+
    'ortho=lon_0,lat_0,R,x_0,y_0,axis'+sLineBreak+
    'qsc=lon_0,lat_0,ellps,x_0,y_0,a,rf,axis'+sLineBreak+
    'rhealpix=lon_0,ellps,x_0,y_0,a,rf,axis'+sLineBreak+
    'tmerc=lon_0,lat_0,ellps,R,k_0,x_0,y_0,a,rf,axis'+sLineBreak+
    'etmerc=lon_0,lat_0,ellps,R,k_0,x_0,y_0,a,rf,axis'+sLineBreak+
    'tpers=azi,tilt,h,lon_0,lat_0,R,x_0,y_0,axis'+sLineBreak+
    'utm=zone,k_0,lon_0,lat_0,x_0,y_0,R,a,rf,axis'+sLineBreak+
    'webmerc=R,x_0,y_0,axis';

  FProjParamExplain := TStringList.Create(dupAccept,False,True);
  FProjParamExplain.Text :=
{$ifndef __proj_ru_explain}
    'lat_1=First standard parallel.'+sLinebreak+
    'lat_2=Second standard parallel.'+sLinebreak+
    'lon_0=Longitude of projection center.'+sLinebreak+
    'ellps=The name of a built-in ellipsoid definition. If not set default value is "WGS84"'+sLinebreak+
    'R=Radius of the sphere, given in meters. If used in conjunction with +ellps, param +R takes precedence.'+sLinebreak+
    'x_0=False easting.'+sLinebreak+
    'y_0=False northing.'+sLinebreak+
    'guam=Use Guam ellipsoidal formulas. Only accurate near the Island of Guam(\lambda\approx 144.5, \phi\approx 13.5)'+sLinebreak+
    'lat_0=Latitude of projection center.'+sLinebreak+
    'lat_b=Angular distance from tangency point of the plane ( \lambda_0, \phi_0 )where the error is kept at minimum.'+sLinebreak+
    'no_cut=Do not cut at hemisphere limit'+sLinebreak+
    'ns=Return non-skewed cartesian coordinates.'+sLinebreak+
    'hyperbolic=Use modified form of the standard Cassini-Soldner projection known as the Hyperbolic Cassini-Soldner.'+sLinebreak+
    'lat_ts=Latitude of true scale. Defines the latitude where scale is not distorted.'+sLinebreak+
    'k_0=Scale factor. Determines scale factor used in the projection.'+sLinebreak+
    'lon_1=Longitude of the first control point.'+sLinebreak+
    'lon_2=Longitude of the second control point.'+sLinebreak+
    'lat_3=Latitude of the third control point.'+sLinebreak+
    'lon_3=Longitude of the third control point.'+sLinebreak+
    'n=Weighting factor. Value should be in the interval 0-1.'+sLinebreak+
    'sweep=Sweep angle axis of the viewing instrument. Valid options are "x" and "y".'+sLinebreak+
    'h=Height of the view point above the Earth and must be in the same units as'+sLinebreak+
    'h=Set to 0.5 for the Hammer projection and 0.25 for the Eckert-Greifendorffprojection. param +W has to be larger than zero.'+sLinebreak+
    'M=param +M has to be larger than zero.'+sLinebreak+
    'rot_xy=Rotation of the HEALPix map in degrees. A positive value results in a clockwiserotation around (x_0, y_0) in the cartesian / projected coordinate space.'+sLinebreak+
    'orient=Can be set to either isea or pole.  See Snyder''s Figure 12 for pole orientation :cite:Snyder1992.'+sLinebreak+
    'azi=Azimuth.'+sLinebreak+
    'aperture=Defaults to 3.0'+sLinebreak+
    'resolution=Defaults to 4.0'+sLinebreak+
    'mode=Can be either plane, di, dd or hex.'+sLinebreak+
    'czech=Reverse the sign of the output coordinates, as is tradition in theCzech Republic.'+sLinebreak+
    'south=Sets the second standard parallel to 90°S. When the flag is off the secondstandard parallel is set to 90°N.'+sLinebreak+
    'lsat=Landsat satellite used for the projection. Value between 1 and 5.'+sLinebreak+
    'path=Selected path of satellite. Value between 1 and 253 when param +lsat isset to 1,2 or 3, otherwise valid input is between 1 and 233.'+sLinebreak+
    'o_proj="Oblique projection. In addition to specifying an oblique projection, how to rotate the projection should be specified.'+sLineBreak+
      'This is done in one of three ways:'+sLineBreak+
      ' - Define a new pole;'+sLineBreak+
      ' - Rotate the projection about a given point'+sLineBreak+
      ' - Define a new "equator" spanned by two points on the sphere."'+sLinebreak+
    'o_lat_p=Latitude of the North pole of the unrotated source CRS, expressed in the rotated geographic CRS.'+sLinebreak+
    'o_lon_p=Longitude of the North pole of the unrotated source CRS, expressed in the rotated geographic CRS.Rotate about point'+sLinebreak+
    'o_alpha=Angle to rotate the projection with.'+sLinebreak+
    'o_lon_c=Longitude of the point the projection will be rotated about.'+sLinebreak+
    'o_lat_c=Latitude of the point the projection will be rotated about.New "equator" points'+sLinebreak+
    'o_lon_1=Longitude of first point.'+sLinebreak+
    'o_lat_1=Latitude of first point.'+sLinebreak+
    'o_lon_2=Longitude of second point.'+sLinebreak+
    'o_lat_2=Latitude of second point.'+sLinebreak+
    'lonc=Longitude of rotational pole point.'+sLinebreak+
    'alpha=Angle of rotational pole.Two points'+sLinebreak+
    'gamma=Azimuth of centerline clockwise from north of the rectifiedbearing of centre line. If param +alpha is not given, thenparam +gamma is used to determine param +alpha.'+sLinebreak+
    'no_rot="No rectification (not "no rotation" as one may well assume).Do not take the last step from the skew uv-plane to the mapXY plane. This option is probably only marginally useful, but remains for (mostly) historical reasons."'+sLinebreak+
    'no_off=Do not offset origin to center of projection.'+sLinebreak+
    'north_square=Position of the north polar square. Valid inputs are 0--3.'+sLinebreak+
    'south_square=Position of the south polar square. Valid inputs are 0--3.'+sLinebreak+
    'approx=Use the Evenden-Snyder algorithm described below under "Legacyellipsoidal form".  It is faster than the default algorithm, but isless accurate and diverges beyond 3° from the central meridian.'+sLinebreak+
    'tilt=Angle in degrees away from nadir.'+sLinebreak+
    'q=Set the q constant.'+sLinebreak+
    'zone=Select which UTM zone to use. Can be a value between 1-60.';
{$else}
    'a=Задает радиус большой полуоси в метрах'+ sLineBreak +
    'b=Задает радиус малой полуоси в метрах'+ sLineBreak +
    'lat_1=Задает первую стандартную параллель проекции.'+sLineBreak+
    'lat_2=Задает вторую стандартную параллель проекции.'+sLineBreak+
    'lon_0=Задает долготу центра проекции.'+sLineBreak+
    'ellps=Имя встроенного определения эллипсоида. Если не задано, значение по умолчанию - "WGS84"'+sLineBreak+
    'R=Радиус сферы, заданный в метрах. Если используется в сочетании с <ellps>, параметр <R> имеет приоритет.'+sLineBreak+
    'x_0=Задает дополнительное смещение координаты x.'+sLineBreak+
    'y_0=Задает дополнительное смещение координаты y.'+sLineBreak+
    'guam=Использовать формулы эллипсоида Гуама. Точные только вблизи острова Гуам.'+sLineBreak+
    'lat_0=Задает широту центра проекции.'+sLineBreak+
    'lat_b=Угловое расстояние от точки касания плоскости, где ошибка минимальна.'+sLineBreak+
    'no_cut=Не обрезать координаты по границам полусферы'+sLineBreak+
    'ns=Возвращает неискаженные декартовы координаты.'+sLineBreak+
    'hyperbolic=Использовать модифицированную форму стандартной проекции Кассини-Солднера (гиперболическая проекция Кассини-Солднера).'+sLineBreak+
    'lat_ts=Определяет широту, на которой масштаб не искажается.'+sLineBreak+
    'k_0=Определяет масштабный коэффициент, используемый при проецировании.'+sLineBreak+
    'lon_1=Долгота первой контрольной точки.'+sLineBreak+
    'lon_2=Долгота второй контрольной точки.'+sLineBreak+
    'lat_3=Широта третьей контрольной точки.'+sLineBreak+
    'lon_3=Долгота третьей контрольной точки.'+sLineBreak+
    'n=Весовой коэффициент. Значение должно находиться в интервале 0-1.'+sLineBreak+
    'sweep=Ось угла размаха инструмента наблюдения. Допустимые значения: "x" и "y".'+sLineBreak+
    'h=Высота точки обзора над Землей и должна быть в тех же единицах, что и'+sLineBreak+
    'W=Установите значение 0,5 для проекции Хаммера и 0,25 для проекции Эккерта-Грейфендорфа. W должен быть больше нуля.'+sLineBreak+
    'M=Параметр M должен быть больше нуля.'+sLineBreak+
    'rot_xy=Вращение карты в градусах. Положительное значение приводит к вращению по часовой стрелке вокруг (<x_0>, <y_0>) в проецируемом координатном пространстве.'+sLineBreak+
    'orient=Можно установить либо isea, либо pole.'+sLineBreak+
    'azi=Азимут.'+sLineBreak+
    'aperture=По умолчанию 3.0'+sLineBreak+
    'resolution=По умолчанию 4.0'+sLineBreak+
    'mode=Вид плоскости может быть, di, dd or hex.'+sLineBreak+
    'czech=Меняет знак выходных координат, так как это принято в Чехии.'+sLineBreak+
    'south=Устанавливает вторую стандартную параллель на 90В° ю.ш. Когда флаг выключен, вторая стандартная параллель устанавливается на 90В° с.ш.'+sLineBreak+
    'lsat=Спутник Landsat, используемый для проецирования. Значение от 1 до 5.'+sLineBreak+
    'path=Выбранный путь спутника. Значение от 1 до 253, если параметр +lsat установлен на 1,2 или 3, в противном случае допустимо значение от 1 до 233.'+sLineBreak+
    'o_proj="Косая проекция. '+sLineBreak+
            'В дополнение к указанию косой проекции необходимо указать, как вращать проекцию.'+sLineBreak+
            'Это можно сделать одним из трех способов:'+sLineBreak+
            '- определить новый полюс'+sLineBreak+
            '- повернуть проекцию вокруг заданной точки'+sLineBreak+
            '- определить новый "экватор", охватывающий две точки на сфере."'+sLineBreak+
    'o_lat_p=Широта северного полюса невращенной исходной CRS, выраженная во вращенной географической CRS.'+sLineBreak+
    'o_lon_p=Долгота северного полюса невращенной исходной CRS, выраженная во вращенной географической CRS.Вращение вокруг точки'+sLineBreak+
    'o_alpha=Угол, на который нужно повернуть проекцию.'+sLineBreak+
    'o_lon_c=Долгота точки, относительно которой будет повернута проекция.'+sLineBreak+
    'o_lat_c=Широта точки, относительно которой будет повернута проекция. Новые точки "экватора"'+sLineBreak+
    'o_lon_1=Долгота первой точки.'+sLineBreak+
    'o_lat_1=Широта первой точки.'+sLineBreak+
    'o_lon_2=Долгота второй точки.'+sLineBreak+
    'o_lat_2=Широта второй точки.'+sLineBreak+
    'lonc=Долгота точки полюса вращения.'+sLineBreak+
    'alpha=Угол полюса вращения.Две точки'+sLineBreak+
    'gamma=Азимут осевой линии по часовой стрелке с севера от осевой линии. Если параметр <alpha> не задан, то для определения параметра <alpha> используется параметр <gamma>.'+sLineBreak+
    'no_rot=Не делать последний шаг при переводе координат uv-плоскости к плоскости mapXY.'+sLineBreak+
    'no_off=Не смещать начало координат относительно центра проекции.'+sLineBreak+
    'north_square=Положение северного полярного квадрата. Допустимые значения 0-3.'+sLineBreak+
    'south_square=Положение южного полярного квадрата. Допустимые значения 0-3.'+sLineBreak+
    'tilt=Угол в градусах от надира.'+sLineBreak+
    'q=Устанавливает постоянную величину q.'+sLineBreak+
    'zone=Устанавливает зону UTM, которую следует использовать. Допустимые значения в диапазоне 1-60.'+sLineBreak+
    'towgs84=Устанавливает параметры ориентации относительно общемирового эллипсоида'+sLineBreak+
    'e=Эксцентриситет'+ sLineBreak +
    'es=Квадрат эксцентриситета'+ sLineBreak +
    'ellps=Название эллипсоида'+ sLineBreak +
    'no_defs=Игнорировать значения по умолчанию'+ sLineBreak +
    'datum='+'Имя встроенного определения датума (по умолчанию  "WGS84")'+ sLineBreak +
    'proj=Название проекции'+ sLineBreak +
    'rf=Обратное сжатие (1/f)'+ sLineBreak +
    'over=Не ограничивать по долготе'+ sLineBreak +
	  'units=Единицы системы координат'+ sLineBreak +
    'to_meter=Коэфициент приведения единиц системы координат в метры'+ sLineBreak +
    'vto_meter=Коэфициент приведения единиц вертикальной системы координат в метры'+ sLineBreak +
    'R_A=R_A'+ sLineBreak +
    'R_V=R_V'+ sLineBreak +
    'R_a=R_a'+ sLineBreak +
    'R_g=R_g'+ sLineBreak +
    'R_h=R_h'+ sLineBreak +
    'R_lat_a=R_lat_a'+ sLineBreak +
    'R_lat_g=R_lat_g'
{$endif}

    ;

finalization
  FreeAndNil(FWktName2ProjName);
  FreeAndNil(FWktParamName2Proj);
  FreeAndNil(FWktEll2Proj);
  FreeAndNil(FProjDefParams);
  FreeAndNil(FProjParamExplain);
end.
