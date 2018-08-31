/// <summary>
/// Projections definition
/// </summary>
unit LibProjProjections;

interface

uses
	Classes, SysUtils, Contnrs, Math,
	WKTProjections;

type
	ENotSupportedProjection = class(ENotSupportedException)end;

function LibProjDefnFromEpsgCode(const Code: Integer): string;
function LibProjDefnToWKTProjection(const Source: string; PrettyWKT: Boolean): string;

implementation

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
	end;


	TpjParam = packed record
		pjKey: string;
		wktName: string;
		epsgCode: Integer;
		Description: string
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
		(pjKey:'aea'; wktName: 'Albers_Conic_Equal_Area'; Description: 'Albers Equal Area'),
		(pjKey:'aea'; wktName: 'Albers'; Description: '[ESRI] Albers Equal Area'),
		(pjKey:'aeqd'; wktName: 'Azimuthal_Equidistant'; Description: 'Azimuthal Equidistant'),
		(pjKey:'airy'; wktName: 'Airy'; Description: 'Airy'),
		(pjKey:'aitoff'; wktName: 'Aitoff'; Description: '[ESRI] Aitoff'),
		(pjKey:'alsk'; wktName: 'Mod_Stererographics_of_Alaska';Description: 'Mod. Stererographics of Alaska'),
		(pjKey:'apian'; wktName: 'Apian_Globular_I'; Description: 'Apian Globular I'),
		(pjKey:'august'; wktName: 'August_Epicycloidal'; Description: 'August Epicycloidal'),
		(pjKey:'bacon'; wktName: 'Bacon_Globular'; Description: 'Bacon Globular'),
		(pjKey:'bipc'; wktName: 'Bipolar_conic_of_western_hemisphere'; Description: 'Bipolar conic of western hemisphere'),
		(pjKey:'boggs'; wktName: 'Boggs_Eumorphic'; Description: ' Boggs Eumorphic'),
		(pjKey:'bonne'; wktName: 'Bonne'; Description: 'Bonne (Werner lat_1=90)'),
		(pjKey:'cass'; wktName: 'Cassini_Soldner'; Description: 'Cassini'),
		(pjKey:'cass'; wktName: 'Cassini'; Description: '[ESRI] Cassini'),
		(pjKey:'cc'; wktName: 'Central_Cylindrical'; Description: 'Central Cylindrical'),
		(pjKey:'cea'; wktName: 'Cylindrical_Equal_Area'; Description: 'Equal Area Cylindrical, alias: Lambert Cyl.Eq.A., Normal Authalic Cyl. (FME), Behrmann (SP=30), Gall Orthogr. (SP=45)'),
		(pjKey:'cea'; wktName: 'Behrmann'; Description: '[ESRI] Behrmann (standard parallel = 30)'),
		(pjKey:'chamb'; wktName: 'Chamberlin_Trimetric'; Description: 'Chamberlin Trimetric'),
		(pjKey:'collg'; wktName: 'Collignon'; Description: 'Collignon'),
		(pjKey:'crast'; wktName: 'Craster_Parabolic'; Description: '[ESRI] Craster Parabolic (Putnins P4)'),
		(pjKey:'denoy'; wktName: 'Denoyer_Semi_Elliptical'; Description: 'Denoyer Semi-Elliptical'),
		(pjKey:'eck1'; wktName: 'Eckert_I'; Description: 'Eckert I'),
		(pjKey:'eck2'; wktName: 'Eckert_II'; Description: 'Eckert II'),
		(pjKey:'eck3'; wktName: 'Eckert_III'; Description: 'Eckert III'),
		(pjKey:'eck4'; wktName: 'Eckert_IV'; Description: 'Eckert IV'),
		(pjKey:'eck5'; wktName: 'Eckert_V'; Description: 'Eckert V'),
		(pjKey:'eck6'; wktName: 'Eckert_VI'; Description: 'Eckert VI'),
		(pjKey:'eqc'; wktName: 'Equirectangular'; Description: 'Equidistant Cylindrical (Plate Caree)'),
		(pjKey:'eqc'; wktName: 'Equidistant_Cylindrical'; Description: '[ESRI] Equidistant Cylindrical (Plate Caree)'),
		(pjKey:'eqc'; wktName: 'Plate_Carree'; Description: '[ESRI] Equidistant Cylindrical (Plate Caree)'),
		(pjKey:'eqdc'; wktName: 'Equidistant_Conic'; Description: 'Equidistant Conic'),
		(pjKey:'euler'; wktName: 'Euler'; Description: 'Euler'),
		(pjKey:'etmerc'; wktName: 'Extended_Transverse_Mercator'; Description: 'Extended Transverse Mercator'),
		(pjKey:'fahey'; wktName: 'Fahey'; Description: 'Fahey'),
		(pjKey:'fouc'; wktName: 'Foucault'; Description: ' Foucaut'),
		(pjKey:'fouc_s'; wktName: 'Foucault_Sinusoidal'; Description: 'Foucaut Sinusoidal'),
		(pjKey:'gall'; wktName: 'Gall_Stereographic'; Description: 'Gall (Gall Stereographic)'),
		(pjKey:'geocent'; wktName: 'Geocentric'; Description: 'Geocentric'),
		(pjKey:'geos'; wktName: 'GEOS'; Description: 'Geostationary Satellite View'),
		(pjKey:'gins8'; wktName: 'Ginsburg_VIII'; Description: 'Ginsburg VIII (TsNIIGAiK)'),
		(pjKey:'gn_sinu'; wktName: 'General_Sinusoidal_Series'; Description: 'General Sinusoidal Series'),
		(pjKey:'gnom'; wktName: 'Gnomonic'; Description: 'Gnomonic'),
		(pjKey:'goode'; wktName: 'Goode_Homolosine'; Description: 'Goode Homolosine'),
		(pjKey:'gs48'; wktName: 'Mod_Stererographics_48'; Description: 'Mod. Stererographics of 48 U.S.'),
		(pjKey:'gs50'; wktName: 'Mod_Stererographics_50'; Description: 'Mod. Stererographics of 50 U.S.'),
		(pjKey:'hammer'; wktName: 'Hammer_Eckert_Greifendorff'; Description: 'Hammer & Eckert-Greifendorff'),
		(pjKey:'hatano'; wktName: 'Hatano_Asymmetrical_Equal_Area'; Description: 'Hatano Asymmetrical Equal Area'),
		(pjKey:'igh'; wktName: 'World_Goode_Homolosine_Land'; Description: 'Interrupted Goode Homolosine'),
		(pjKey:'imw_p'; wktName: 'International_Map_of_the_World_Polyconic'; Description: 'International Map of the World Polyconic'),
		(pjKey:'kav5'; wktName: 'Kavraisky_V'; Description: 'Kavraisky V'),
		(pjKey:'kav7'; wktName: 'Kavraisky_VII'; Description: 'Kavraisky VII'),
		(pjKey:'krovak'; wktName: 'Krovak'; Description: 'Krovak'),
		(pjKey:'labrd'; wktName: 'Laborde_Oblique_Mercator'; Description: 'Laborde'),
		(pjKey:'laea'; wktName: 'Lambert_Azimuthal_Equal_Area'; Description: 'Lambert Azimuthal Equal Area'),
		(pjKey:'lagrng'; wktName: 'Lagrange'; Description: 'Lagrange'),
		(pjKey:'larr'; wktName: 'Larrivee'; Description: 'Larrivee'),
		(pjKey:'lask'; wktName: 'Laskowski'; Description: 'Laskowski'),
		(pjKey:'latlon'; wktName: 'Geodetic'; Description: 'Lat/long'),
		(pjKey:'latlong'; wktName: 'Geodetic'; Description: 'Lat/long'),
		(pjKey:'longlat'; wktName: 'Geodetic'; Description: 'Long/Lat'),
		(pjKey:'lonlat'; wktName: 'Geodetic'; Description: 'Long/Lat'),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic_1SP'; Description: 'Lambert Conformal Conic (1 standard parallel)'),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic_2SP'; Description: 'Lambert Conformal Conic (2 standard parallels)'),
		(pjKey:'lcc'; wktName: 'Lambert_Conformal_Conic'; Description: 'Lambert Conformal Conic'),
		(pjKey:'lcca'; wktName: 'Lambert_Conformal_Conic_Alternative'; Description: 'Lambert Conformal Conic Alternative'),
		(pjKey:'leac'; wktName: 'Lambert_Equal_Area_Conic'; Description: 'Lambert Equal Area Conic'),
		(pjKey:'lee_os'; wktName: 'Lee_Oblated_Stereographic'; Description: 'Lee Oblated Stereographic'),
		(pjKey:'loxim'; wktName: 'Loximuthal'; Description: '[ESRI] Loximuthal'),
		(pjKey:'lsat'; wktName: 'Space_oblique_for_LANDSAT'; Description: 'Space oblique for LANDSAT'),
		(pjKey:'mbt_s'; wktName: 'McBryde_Thomas_Flat_Polar_Sine'; Description: 'McBryde-Thomas Flat-Polar Sine'),
		(pjKey:'mbt_fps'; wktName: 'McBryde_Thomas_Flat_Polar_Sine_2'; Description: 'McBryde-Thomas Flat-Pole Sine (No. 2)'),
		(pjKey:'mbtfpp'; wktName: 'McBryde_Thomas_Flat_Polar_Parabolic'; Description: 'McBride-Thomas Flat-Polar Parabolic'),
		(pjKey:'mbtfpq'; wktName: 'Flat_Polar_Quartic'; Description: '[ESRI] McBryde-Thomas Flat-Polar Quartic'),
		(pjKey:'mbtfps'; wktName: 'McBryde_Thomas_Flat_Polar_Sinusoidal'; Description: 'McBryde-Thomas Flat-Polar Sinusoidal'),
		(pjKey:'merc'; wktName: 'Mercator'; Description: '[ESRI] Mercator'),
		(pjKey:'merc'; wktName: 'Mercator_1SP'; Description: 'Mercator (1 standard parallel)'),
		(pjKey:'merc'; wktName: 'Mercator_2SP'; Description: 'Mercator (2 standard parallels)'),
		(pjKey:'mil_os'; wktName: 'Miller_Oblated_Stereographic'; Description: 'Miller Oblated Stereographic'),
		(pjKey:'mill'; wktName: 'Miller_Cylindrical'; Description: 'Miller Cylindrical'),
		(pjKey:'moll'; wktName: 'Mollweide'; Description: 'Mollweide'),
		(pjKey:'murd1'; wktName: 'Murdoch_I'; Description: 'Murdoch I'),
		(pjKey:'murd2'; wktName: 'Murdoch_II'; Description: 'Murdoch II'),
		(pjKey:'murd3'; wktName: 'Murdoch_III'; Description: 'Murdoch III'),
		(pjKey:'nell'; wktName: 'Nell'; Description: 'Nell'),
		(pjKey:'nell_h'; wktName: 'Nell_Hammer'; Description: 'Nell-Hammer'),
		(pjKey:'nicol'; wktName: 'Nicolosi_Globular'; Description: 'Nicolosi Globular'),
		(pjKey:'nsper'; wktName: 'Near_sided_perspective'; Description: 'Near-sided perspective'),
		(pjKey:'nzmg'; wktName: 'New_Zealand_Map_Grid'; Description: 'New Zealand Map Grid'),
		(pjKey:'ob_tran'; wktName: 'General_Oblique_Transformation'; Description: 'General Oblique Transformation'),
		(pjKey:'ocea'; wktName: 'Oblique_Cylindrical_Equal_Area'; Description: 'Oblique Cylindrical Equal Area'),
		(pjKey:'oea'; wktName: 'Oblated_Equal_Area'; Description: 'Oblated Equal Area'),
		(pjKey:'omerc'; wktName: 'Hotine_Oblique_Mercator'; Description: 'Oblique Mercator'),
		(pjKey:'omerc'; wktName: 'Oblique_Mercator'; Description: 'Oblique Mercator'),
		(pjKey:'ortel'; wktName: 'Ortelius_Oval'; Description: 'Ortelius Oval'),
		(pjKey:'ortho'; wktName: 'Orthographic'; Description: 'Orthographic (ESRI: World from Space)'),
		(pjKey:'pconic'; wktName: 'Perspective_Conic'; Description: 'Perspective Conic'),
		(pjKey:'poly'; wktName: 'Polyconic'; Description: 'Polyconic (American)'),
		(pjKey:'putp1'; wktName: 'Putnins_P1'; Description: 'Putnins P1'),
		(pjKey:'putp2'; wktName: 'Putnins_P2'; Description: 'Putnins P2'),
		(pjKey:'putp3'; wktName: 'Putnins_P3'; Description: 'Putnins P3'),
		(pjKey:'putp3p'; wktName: 'Putnins_P3'''; Description: 'Putnins P3'''),
		(pjKey:'putp4p'; wktName: 'Putnins_P4'''; Description: 'Putnins P4'''),
		(pjKey:'putp5'; wktName: 'Putnins_P5'; Description: 'Putnins P5'),
		(pjKey:'putp5p'; wktName: 'Putnins_P5'''; Description: 'Putnins P5'''),
		(pjKey:'putp6'; wktName: 'Putnins_P6'; Description: 'Putnins P6'),
		(pjKey:'putp6p'; wktName: 'Putnins_P6'''; Description: 'Putnins P6'''),
		(pjKey:'qua_aut'; wktName: 'Quartic_Authalic'; Description: '[ESRI] Quartic Authalic'),
		(pjKey:'robin'; wktName: 'Robinson'; Description: 'Robinson'),
		(pjKey:'rouss'; wktName: 'Roussilhe_Stereographic'; Description: 'Roussilhe Stereographic'),
		(pjKey:'rpoly'; wktName: 'Rectangular_Polyconic'; Description: 'Rectangular Polyconic'),
		(pjKey:'sinu'; wktName: 'Sinusoidal'; Description: 'Sinusoidal (Sanson-Flamsteed)'),
		(pjKey:'somerc'; wktName: 'Hotine_Oblique_Mercator'; Description: 'Swiss Oblique Mercator'),
		(pjKey:'somerc'; wktName: 'Swiss_Oblique_Cylindrical'; Description: 'Swiss Oblique Cylindrical'),
		(pjKey:'somerc'; wktName: 'Hotine_Oblique_Mercator_Azimuth_Center'; Description: '[ESRI] Swiss Oblique Mercator/Cylindrical'),
		(pjKey:'stere'; wktName: 'Polar_Stereographic'; Description: 'Stereographic'),
		(pjKey:'stere'; wktName: 'Stereographic'; Description: '[ESRI] Stereographic'),
		(pjKey:'sterea'; wktName: 'Oblique_Stereographic'; Description: 'Oblique Stereographic Alternative'),
		(pjKey:'gstmerc'; wktName: 'Gauss_Schreiber_Transverse_Mercator'; Description: 'Gauss-Schreiber Transverse Mercator (aka Gauss-Laborde Reunion)'),
		(pjKey:'tcc'; wktName: 'Transverse_Central_Cylindrical'; Description: 'Transverse Central Cylindrical'),
		(pjKey:'tcea'; wktName: 'Transverse_Cylindrical_Equal_Area'; Description: 'Transverse Cylindrical Equal Area'),
		(pjKey:'tissot'; wktName: 'Tissot_Conic'; Description: 'Tissot Conic'),
		(pjKey:'tmerc'; wktName: 'Transverse_Mercator'; Description: 'Transverse Mercator'),
		(pjKey:'tmerc'; wktName: 'Gauss_Kruger'; Description: 'Gauss Kruger'),
		(pjKey:'tpeqd'; wktName: 'Two_Point_Equidistant'; Description: 'Two Point Equidistant'),
		(pjKey:'tpers'; wktName: 'Tilted_perspective'; Description: 'Tilted perspective'),
		(pjKey:'ups'; wktName: 'Universal_Polar_Stereographic'; Description: 'Universal Polar Stereographic'),
		(pjKey:'urm5'; wktName: 'Urmaev_V'; Description: 'Urmaev V'),
		(pjKey:'urmfps'; wktName: 'Urmaev_Flat_Polar_Sinusoidal'; Description: 'Urmaev Flat-Polar Sinusoidal'),
		(pjKey:'utm'; wktName: 'Transverse_Mercator'; Description: 'Universal Transverse Mercator (UTM)'),
		(pjKey:'vandg'; wktName: 'Van_Der_Grinten_I'; Description: '[ESRI] van der Grinten (I)'),
		(pjKey:'vandg'; wktName: 'VanDerGrinten'; Description: 'van der Grinten (I)'),
		(pjKey:'vandg2'; wktName: 'VanDerGrinten_II'; Description: 'van der Grinten II'),
		(pjKey:'vandg3'; wktName: 'VanDerGrinten_III'; Description: 'van der Grinten III'),
		(pjKey:'vandg4'; wktName: 'VanDerGrinten_IV'; Description: 'van der Grinten IV'),
		(pjKey:'vitk1'; wktName: 'Vitkovsky_I'; Description: 'Vitkovsky I'),
		(pjKey:'wag1'; wktName: 'Wagner_I'; Description: 'Wagner I (Kavraisky VI)'),
		(pjKey:'wag2'; wktName: 'Wagner_II'; Description: 'Wagner II'),
		(pjKey:'wag3'; wktName: 'Wagner_III'; Description: 'Wagner III'),
		(pjKey:'wag4'; wktName: 'Wagner_IV'; Description: 'Wagner IV'),
		(pjKey:'wag5'; wktName: 'Wagner_V'; Description: 'Wagner V'),
		(pjKey:'wag6'; wktName: 'Wagner_VI'; Description: 'Wagner VI'),
		(pjKey:'wag7'; wktName: 'Wagner_VII'; Description: 'Wagner VII'),
		(pjKey:'weren'; wktName: 'Werenskiold_I'; Description: 'Werenskiold I'),
		(pjKey:'wink1'; wktName: 'Winkel_I'; Description: 'Winkel I'),
		(pjKey:'wink2'; wktName: 'Winkel_II'; Description: 'Winkel II'),
		(pjKey:'wintri'; wktName: 'Winkel_Tripel'; Description: 'Winkel Tripel')
	 );

 pjParamsList: array[0..64] of TpjParam =
 (
		// General Projection Parameters'),
		(pjKey:'k'; wktName: 'scale_factor'; Description: 'Scaling factor'),
		(pjKey:'k_0'; wktName: 'scale_factor'; Description: 'Scaling factor'),
		(pjKey:'lat_0'; wktName: 'latitude_of_origin'; Description: 'Latitude of origin'),
		(pjKey:'lat_0'; wktName: 'latitude_of_center'; Description: 'Latitude of center'),
		(pjKey:'lat_0'; wktName: 'central_parallel'; Description: '[ESRI] Latitude of center'),
		(pjKey:'lat_1'; wktName: 'standard_parallel_1'; Description: 'Latitude of first standard parallel'),
		(pjKey:'lat_2'; wktName: 'standard_parallel_2'; Description: 'Latitude of second standard parallel'),
		(pjKey:'lat_ts'; wktName: 'latitude_of_origin'; Description: 'Latitude of true scale'),
		(pjKey:'lon_0'; wktName: 'central_meridian'; Description: 'Central meridian'),
		(pjKey:'lon_0'; wktName: 'longitude_of_center'; Description: 'Longitude of center'),
		(pjKey:'lonc'; wktName: 'longitude_of_center'; Description: 'Longitude of projection center'),
		(pjKey:'x_0'; wktName: 'false_easting'; Description: 'False easting'),
		(pjKey:'y_0'; wktName: 'false_northing'; Description: 'False northing'),
		// Additional Projection Parameters'),
		(pjKey:'alpha'; wktName: 'azimuth'; Description: 'Azimuth of initial line'),
		(pjKey:'azi'; wktName: ''; Description: ''),
		(pjKey:'belgium'; wktName: ''; Description: ''),
		(pjKey:'beta'; wktName: ''; Description: ''),
		(pjKey:'czech'; wktName: ''; Description: ''),
		(pjKey:'gamma'; wktName: ''; Description: ''),
		(pjKey:'geoc'; wktName: ''; Description: ''),
		(pjKey:'guam'; wktName: ''; Description: ''),
		(pjKey:'h'; wktName: 'satellite_height'; Description: 'Satellite height'),
		(pjKey:'lat_b'; wktName: ''; Description: ''),
		(pjKey:'lat_t'; wktName: ''; Description: ''),
		(pjKey:'lon_1'; wktName: ''; Description: ''),
		(pjKey:'lon_2'; wktName: ''; Description: ''),
		(pjKey:'lsat'; wktName: ''; Description: ''),
		(pjKey:'m'; wktName: ''; Description: ''),
		(pjKey:'M'; wktName: ''; Description: ''),
		(pjKey:'n'; wktName: ''; Description: ''),
		(pjKey:'no_cut'; wktName: ''; Description: ''),
		(pjKey:'no_off'; wktName: ''; Description: ''),
		(pjKey:'no_rot'; wktName: ''; Description: ''),
		(pjKey:'ns'; wktName: ''; Description: ''),
		(pjKey:'o_alpha'; wktName: ''; Description: ''),
		(pjKey:'o_lat_1'; wktName: ''; Description: ''),
		(pjKey:'o_lat_2'; wktName: ''; Description: ''),
		(pjKey:'o_lat_c'; wktName: ''; Description: ''),
		(pjKey:'o_lat_p'; wktName: ''; Description: ''),
		(pjKey:'o_lon_1'; wktName: ''; Description: ''),
		(pjKey:'o_lon_2'; wktName: ''; Description: ''),
		(pjKey:'o_lon_c'; wktName: ''; Description: ''),
		(pjKey:'o_lon_p'; wktName: ''; Description: ''),
		(pjKey:'o_proj'; wktName: ''; Description: ''),
		(pjKey:'over'; wktName: ''; Description: ''),
		(pjKey:'p'; wktName: ''; Description: ''),
		(pjKey:'path'; wktName: ''; Description: ''),
		(pjKey:'q'; wktName: ''; Description: ''),
		(pjKey:'R'; wktName: ''; Description: ''),
		(pjKey:'R_a'; wktName: ''; Description: ''),
		(pjKey:'R_A'; wktName: ''; Description: ''),
		(pjKey:'R_g'; wktName: ''; Description: ''),
		(pjKey:'R_h'; wktName: ''; Description: ''),
		(pjKey:'R_lat_a'; wktName: ''; Description: ''),
		(pjKey:'R_lat_g'; wktName: ''; Description: ''),
		(pjKey:'rot'; wktName: ''; Description: ''),
		(pjKey:'R_V'; wktName: ''; Description: ''),
		(pjKey:'s'; wktName: ''; Description: ''),
		(pjKey:'sym'; wktName: ''; Description: ''),
		(pjKey:'t'; wktName: ''; Description: ''),
		(pjKey:'theta'; wktName: ''; Description: ''),
		(pjKey:'tilt'; wktName: ''; Description: ''),
		(pjKey:'vopt'; wktName: ''; Description: ''),
		(pjKey:'W'; wktName: ''; Description: ''),
		(pjKey:'westo'; wktName: ''; Description: '')
	);

var
	FWktName2ProjName,FWktEll2Proj,FWktParamName2Proj: TStrings;

const PROJ_PAIR_SEPARATOR: Char = '+';
			PROJ_PAIR_DELIMETER: Char = '=';

function LibProjDefnFromEpsgCode(const Code: Integer): string;
const
	gk_tpl = '+proj=tmerc +lat_0=0 +lon_0=%d +k=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs';
	utm_tpl = '+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
var
	GKZoneOffset: Integer;
begin
	case Code of
		// Sphere Mercator ESRI:53004
		53004:
			Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';
		// Popular Visualisation CRS / Mercator
		3785:
			Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
		900913:
			Result := '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs';
		// WGS 84 / World Mercator
		3395:
			Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
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
		A := AF.ToString;
		F := FF.ToString;
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

	PrimeLon := Lon.ToString;
end;

procedure UnitsInfo(const AKey: string; var wktName,toMerersFactor: string);
var
	Idx: Integer;
begin
	for Idx := 0 to Length(pjLinearUnitsList) -1 do
	begin
		if SameText(pjLinearUnitsList[Idx].pjKey,AKey) then
		begin
			wktName := pjLinearUnitsList[Idx].wktName;
			toMerersFactor := pjLinearUnitsList[Idx].toMeters.ToString;
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
	Result := Index > -1;
	if Result then
	begin
		PairValue := Pairs.ValueFromIndex[Index];
		if PairValue = '' then
			PairValue := DefaultValue;
	end;
end;

function LibProjDefnToWKTProjection(const Source: string; PrettyWKT: Boolean): string;
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
	begin
		if TryFindProjPairValue(ProjPairs, 'pm', FPrimeName, 'greenwich') then
			PrimeInfo(FPrimeName,FPrimeName,FPrimeLon)
		else
			PrimeInfo('greenwich',FPrimeName,FPrimeLon)
	end;

	procedure FetchEllipse();
	var
		EA, EF: double;
	begin
		FEllipseName := 'unnamed';
		FDatumName := 'unknown';
		FDatumToWgs := '';
		FEllipsePrms := '';
		FEllipseA := '';
		FEllipseF := '';

		if TryFindProjPairValue(ProjPairs,'a',FEllipseA,DEF_ELL_A) then
		begin
			TryFindProjPairValue(ProjPairs,'R',FEllipseA,FEllipseA); // "R" takes precedence over "a"

			if not TryFindProjPairValue(ProjPairs,'f',FEllipseF, DEF_ELL_F) then
			begin
				if not TryFindProjPairValue(ProjPairs,'b',FEllipseF,DEF_ELL_B) then
					FEllipseF := DEF_ELL_F
				else
				begin
					if EA.TryParse(FEllipseA, EA) and EF.TryParse(FEllipseA, EF) then
					begin
						EF := (EA - EF) / EA;
						if EF = 0 then
							begin
								FEllipseName := FEllipseName + ' sphere';
								FGeoCSName := 'Sphere based';
							end
						else
							EF := 1 / EF;

						FEllipseA := EA.ToString();
						FEllipseF := EF.ToString();
					end
					else
					begin
            FEllipseName := DEF_ELL_N;
						FEllipseA := DEF_ELL_A;
						FEllipseF := DEF_ELL_F;
          end;
				end;
			end;
		end
		else
		begin
			if TryFindProjPairValue(ProjPairs,'ellps',FEllipseName,DEF_ELL_N) then
			begin
				if not EllipseInfo(FEllipseName,FEllipseName,FEllipseA,FEllipseF) then
				begin
					FEllipseA := DEF_ELL_A;
					FEllipseF := DEF_ELL_F;
				end;
			end
			else
			if TryFindProjPairValue(ProjPairs, 'datum', FDatumName, 'unknown') then
			begin
				if DatumInfo(FDatumName,DtmInfo) then
				begin
					FDatumName := DtmInfo.dName;
					// if datum set then change ellipsoid
					EllipseInfo(DtmInfo.pjEllKey,FEllipseName,FEllipseA,FEllipseF);
					FDatumToWgs := DtmInfo.toWgs;
				end;
			end
			else
			begin
				FEllipseName := DEF_ELL_N;
				FDatumName := DEF_DATUM_N;
				FEllipseA := DEF_ELL_A;
				FEllipseF := DEF_ELL_F;
			end;
		end;

		TryFindProjPairValue(ProjPairs,'towgs84',FDatumToWgs,DEF_ELL_TOWGS);

		if (FEllipseA <> '') and (FEllipseF <> '') then
			FEllipsePrms := FEllipseA + ','+FEllipseF;
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
	end;

	procedure FetchProjectionName();
	begin
		if not TranslateProjectionName(FProjID,FProjectionName) then
			InvalidProjection(FProjID,Source);

		FProjCSName := 'unnamed';
	end;

	procedure FetchProjectionUnits();
	begin
		FProjUnits := 'Meter';
		FFProjUnitsFactor := '1';

		if TryFindProjPairValue(ProjPairs,'units',FProjUnits,'m') then
			UnitsInfo(FProjUnits,FProjUnits,FFProjUnitsFactor);

		TryFindProjPairValue(ProjPairs,'to_meter',FFProjUnitsFactor,'1');
	end;

	procedure FetchUtm();
	const
		pjTmParams: array[0..4] of string = (
			'k_0','lat_0','lon_0','x_0','y_0');
		var
			Zone,South: string;
			SouthHemesphere: Boolean;
			Code: Integer;
			pjTmParamsValues: array[0..4] of string;
	begin
		if TryFindProjPairValue(ProjPairs,'zone',Zone,'') then
		begin
			if Code.TryParse(Zone,Code) then
			begin
				SouthHemesphere := TryFindProjPairValue(ProjPairs,'south',South,'1') and (South = '1');
				if (Code > 0) and (Code < 60) then
				begin
					FProjCSName := 'UTM Zone '+Zone+', ';
					if  SouthHemesphere then
						begin
							pjTmParamsValues[4] := '10000000';
							FProjCSName := FProjCSName + 'Southern Hemisphere'
						end
					else
					begin
						pjTmParamsValues[4] := '0';
						FProjCSName := FProjCSName + 'Northern Hemisphere';
					end;

					pjTmParamsValues[0] := '0.9996';
					pjTmParamsValues[1] := '0';
					pjTmParamsValues[2] := ((Code * 6) - 183).ToString;
					pjTmParamsValues[3] := '500000';

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
		pjGenParams: array[0..9] of string = (
			'k','k_0','lat_0','lat_1','lat_2','lat_ts','lon_0','lonc','x_0','y_0');
	var
		ParamName,ParamValue: string;
		Idx: Integer;
	begin
		if FProjCSParameters = nil then
			FProjCSParameters := TStringList.Create
		else
			FProjCSParameters.Clear;

		if SameText(FProjID,'utm') then
		begin
			FetchUtm();
		end
		else
		begin
			for Idx := 0 to Length(pjGenParams) -1 do
			begin
				ParamName := pjGenParams[Idx];
				if TryFindProjPairValue(ProjPairs,ParamName,ParamValue,'') and
					 TranslateProjectionParam(ParamName,ParamName) then
				begin
					FProjCSParameters.Add(ParamName+'='+ParamValue);
				end;
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
		FetchProjectionParameters();
		FetchProjectionUnits();
	end;

begin
	if Source = '' then
		Exit('');

	ProjPairs := TStringList.Create();
	ProjPairs.NameValueSeparator := PROJ_PAIR_DELIMETER;
	try
		Cnt := FetchProjPairs(Source, ProjPairs);
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
		else if WtkDefn is TWKTProjectedCRS then
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
		if FWktName2ProjName = nil then
		begin
			FWktName2ProjName := TStringList.Create;

			FWktName2ProjName.Text := 'Albers_Conic_Equal_Area=aea' + sLineBreak + 'Azimuthal_Equidistant=aeqd' +
				sLineBreak + 'Cassini_Soldner=cass' + sLineBreak + 'Cylindrical_Equal_Area=cea' + sLineBreak +
				'Eckert_IV=eck4' + sLineBreak + 'Eckert_VI=eck6' + sLineBreak + 'Equidistant_Conic=eqdc' + sLineBreak +
				'Equirectangular=eqc' + sLineBreak + 'Plate Caree=eqc' + sLineBreak + 'Transverse_Mercator=tmerc' +
				sLineBreak + 'Gauss_Kruger=tmerc' + sLineBreak + 'Gauss-Kruger=tmerc' + sLineBreak + 'Gauss Kruger=tmerc' +
				sLineBreak + 'Gall_Stereographic=gall' + sLineBreak + 'GEOS=geos' + sLineBreak + 'Gnomonic=gnom' +
				sLineBreak + 'hotine_oblique_mercator' + sLineBreak + 'Krovak=krovak' + sLineBreak +
				'Lambert_Azimuthal_Equal_Area=laea' + sLineBreak + 'Lambert_Conformal_Conic_1SP=lcc' + sLineBreak +
				'Lambert_Conformal_Conic_2SP=lcc' + sLineBreak + 'Miller_Cylindrical=mill' + sLineBreak + 'Mollweide=moll'
				+ sLineBreak + 'Mercator_1SP=merc' + sLineBreak + 'Mercator_2SP=merc' + sLineBreak +
				'New_Zealand_Map_Grid=nzmg' + sLineBreak + 'ObliqueMercator_Hotine=omerc' + sLineBreak +
				'ObliqueMercator=omerc' + sLineBreak + 'Oblique_Stereographic=sterea' + sLineBreak + 'Orthographic=ortho' +
				sLineBreak + 'Polar_Stereographic=stere' + sLineBreak + 'Stereographic=stere' + sLineBreak +
				'Polyconic=stere' + sLineBreak + 'Robinson=robin' + sLineBreak + 'Sinusoidal=sinu' + sLineBreak +
				'Transverse_Mercator_South_Orientated=tmerc' + sLineBreak + 'VanDerGrinten=vandg';
		end;

		Index := FWktParamName2Proj.IndexOf(WktName);

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
		if FWktParamName2Proj = nil then
		begin
			FWktParamName2Proj := TStringList.Create;
			FWktParamName2Proj.Text := 'false_easting=+x_0' + sLineBreak + 'false_northing=+y_0' + sLineBreak +
				'scale_factor=+k_0' + sLineBreak + 'standard_parallel_1=+lat_1' + sLineBreak + 'standard_parallel_2=+lat_2'
				+ sLineBreak + 'longitude_of_center=+lon_0' + sLineBreak + 'central_meridian=+lon_0' + sLineBreak +
				'latitude_of_origin=+lat_0' + sLineBreak + 'latitude_of_center=+lat_0' + sLineBreak +
				'central_meridian=+lon_0';
		end;

		Index := FWktParamName2Proj.IndexOf(WktName);

		Result := Index > -1;
		if Result then
			ProjName := FWktParamName2Proj.ValueFromIndex[Index]
		else
			ProjName := '';
	end;

	function WktDatum2ProjDatum(const WktName: string; out ProjName: string): Boolean;
	// proj/src/pj_ellps.c.
	var
		Index: Integer;
	begin
		if FWktEll2Proj = nil then
		begin
			FWktEll2Proj := TStringList.Create();
			FWktEll2Proj.Text := 'MERIT 1983=MERIT' + sLineBreak + 'Soviet Geodetic System 85=SGS85' + sLineBreak +
				'GRS 1980=GRS80' + sLineBreak + 'IUGG 1980=GRS80' + sLineBreak + 'IAU 1976=IAU76' + sLineBreak +
				'Airy 1830=airy' + sLineBreak + 'Appl. Physics. 1965=APL4.9' + sLineBreak +
				'Naval Weapons Lab., 1965=NWL9D' + sLineBreak + 'Modified Airy=mod_airy' + sLineBreak +
				'Andrae 1876=andrae' + sLineBreak + 'Australian Natl & S. Amer. 1969=aust_SA' + sLineBreak +
				'GRS 67(IUGG 1967)=GRS67' + sLineBreak + 'Bessel 1841=bessel' + sLineBreak + 'Bessel_1841=bessel' +
				sLineBreak + 'Bessel1841=bessel' + sLineBreak + 'Bessel 1841 (Namibia)=bess_nam' + sLineBreak +
				'Clarke 1866=clrk66' + sLineBreak + 'Clarke 1880 mod=clrk80' + sLineBreak +
				'Comm. des Poids et Mesures 1799=CPM' + sLineBreak + 'Delambre 1810=delmbr' + sLineBreak +
				'Engelis 1985=engelis' + sLineBreak + 'Everest 1830=evrst30' + sLineBreak + 'Everest 1948=evrst48' +
				sLineBreak + 'Everest 1956=evrst56' + sLineBreak + 'Everest 196=evrst69' + sLineBreak +
				'Everest (Sabah & Sarawak)=evrstSS' + sLineBreak + 'Fischer (Mercury Datum) 1960=fschr60' + sLineBreak +
				'Modified Fischer 1960=fschr60m' + sLineBreak + 'Fischer 1968=fschr68' + sLineBreak +
				'Helmert 1906=helmert' + sLineBreak + 'Hough=hough' + sLineBreak + 'International 1909 (Hayford)=intl' +
				sLineBreak + 'Krassovsky, 1942=krass' + sLineBreak + 'Krassovsky_1942=krass' + sLineBreak +
				'Krassovsky 1942=krass' + sLineBreak + 'Kaula 1961=kaula' + sLineBreak + 'Lerch 1979=lerch' + sLineBreak +
				'Maupertius 1738=mprts' + sLineBreak + 'New International 1967=new_intl' + sLineBreak +
				'Plessis 1817 (France)=plessis' + sLineBreak + 'Southeast Asia=SEasia' + sLineBreak + 'Walbeck=walbeck' +
				sLineBreak + 'WGS 60=WGS60' + sLineBreak + 'WGS 66=WGS66' + sLineBreak + 'WGS 72=WGS72' + sLineBreak +
				'WGS 84=WGS84' + sLineBreak + 'WGS_84=WGS84' + sLineBreak + 'WGS84=WGS84' + sLineBreak + 'WGS1984=WGS84' +
				sLineBreak + 'WGS 1984=WGS84' + sLineBreak + 'WGS_1984=WGS84' + sLineBreak +
				'Normal Sphere (r=6370997)=sphere';
		end;

		Index := FWktEll2Proj.IndexOf(WktName);

		Result := Index > -1;
		if Result then
			ProjName := FWktEll2Proj.ValueFromIndex[Index]
		else
			ProjName := '';
	end;

initialization

	finalization

	FreeAndNil(FWktName2ProjName); FreeAndNil(FWktParamName2Proj); FreeAndNil(FWktEll2Proj);

end.
