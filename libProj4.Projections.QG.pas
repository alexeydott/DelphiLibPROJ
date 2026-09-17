unit libProj4.Projections.QG;

interface

procedure RunLibProjProjectionRoundTripQG;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  libProj4.Projections,
  libProj4.WellKnownText;

const
  QG_EPS = 1E-9;

type
  EQGFailure = class(Exception);

procedure QGFail(const Msg: string);
begin
  raise EQGFailure.Create(Msg);
end;

procedure QGCheck(const Condition: Boolean; const Msg: string);
begin
  if not Condition then
    QGFail(Msg);
end;

procedure QGCheckContains(const S, SubString, Msg: string);
begin
  QGCheck(Pos(SubString,S) > 0, Msg + ': [' + SubString + '] not found in [' + S + ']');
end;

procedure QGCheckFloat(const Expected, Actual: Double; const Msg: string;
  const Epsilon: Double = QG_EPS);
begin
  QGCheck(SameValue(Expected,Actual,Epsilon),
    Format('%s: expected %.15g, actual %.15g',[Msg,Expected,Actual],TFormatSettings.Invariant));
end;

procedure QGCheckParams(const A, B: TArray<Double>; const Msg: string);
var
  I: Integer;
begin
  QGCheck(Length(A)=Length(B),Msg + ': parameter count differs');
  for I := 0 to Length(A)-1 do
    QGCheckFloat(A[I],B[I],Msg + ': parameter ' + IntToStr(I));
end;

procedure CheckMapInfoParserGenerator;
const
  INPUT = 'CoordSys Earth Projection 34, 104, "m", -177, 0, 0.9996, 500000, 0 Bounds (-7919730.23343, -9997964.94302) (8919730.23343, 9997964.94302)';
var
  A,B: TMapInfoCoordSys;
  S: string;
begin
  QGCheck(TryParseMapInfoCoordSys(INPUT,A),'MapInfo parser rejected projection 34');
  QGCheck(A.IsEarth,'Projection 34 must be Earth');
  QGCheck(A.ProjectionID=34,'Projection 34 parser ID');
  QGCheck(A.DatumID=104,'Projection 34 parser datum');
  QGCheck(SameText(A.UnitsName,'m'),'Projection 34 parser units');
  QGCheck(A.HasBounds,'Projection 34 parser Bounds');
  QGCheckParams(A.ProjectionParameters,TArray<Double>.Create(-177,0,0.9996,500000,0),
    'Projection 34 parser parameters');

  S := MapInfoCoordSysToString(A,True);
  QGCheck(TryParseMapInfoCoordSys(S,B),'Generated MapInfo CoordSys is not parseable');
  QGCheck(B.ProjectionID=A.ProjectionID,'MapInfo parser/generator projection ID');
  QGCheck(B.DatumID=A.DatumID,'MapInfo parser/generator datum ID');
  QGCheckParams(A.ProjectionParameters,B.ProjectionParameters,'MapInfo parser/generator parameters');
  QGCheck(B.HasBounds,'MapInfo parser/generator lost Bounds');
  QGCheckFloat(A.BoundsMinX,B.BoundsMinX,'Bounds MinX');
  QGCheckFloat(A.BoundsMinY,B.BoundsMinY,'Bounds MinY');
  QGCheckFloat(A.BoundsMaxX,B.BoundsMaxX,'Bounds MaxX');
  QGCheckFloat(A.BoundsMaxY,B.BoundsMaxY,'Bounds MaxY');
end;

procedure CheckOneProjection(const MapInfoText, ExpectedProjToken: string;
  const ExpectedBackID: Integer; const CheckParameters: Boolean = True);
var
  A,B: TMapInfoCoordSys;
  P,S: string;
begin
  QGCheck(TryParseMapInfoCoordSys(MapInfoText,A),'Cannot parse test CoordSys: ' + MapInfoText);
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheck(P<>'','MapInfo -> PROJ failed: ' + MapInfoText);
  QGCheckContains(P,ExpectedProjToken,'Wrong PROJ method');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'PROJ -> MapInfo failed: ' + P);
  QGCheck(B.ProjectionID=ExpectedBackID,
    Format('Unexpected round-trip projection ID: source=%d expected=%d actual=%d',
      [A.ProjectionID,ExpectedBackID,B.ProjectionID]));
  QGCheck(B.DatumID=A.DatumID,'Datum ID changed in round-trip');
  if (A.ProjectionID<>1) and (B.ProjectionID<>1) then
    QGCheck(SameText(A.UnitsName,B.UnitsName),'Units changed in round-trip');
  if CheckParameters then
    QGCheckParams(A.ProjectionParameters,B.ProjectionParameters,
      'Projection ' + IntToStr(A.ProjectionID) + ' parameters');

  S := LibProjDefnToMapinfoCoordSys(P);
  QGCheck(S<>'','String PROJ -> MapInfo overload returned empty');
end;

procedure CheckProjectionMatrix;
begin
  CheckOneProjection('CoordSys Earth Projection 1, 104','+proj=longlat',1);
  CheckOneProjection('CoordSys Earth Projection 2, 104, "m", 0, 30, 100, 200','+proj=cea',2);
  CheckOneProjection('CoordSys Earth Projection 3, 104, "m", 3, 46.5, 44, 49, 700000, 6600000','+proj=lcc',3);
  { MapInfo 4/5 are the polar forms. Legacy GDAL 2.4 exports them only
    when the latitude of center is within 0.001 degree of a pole. }
  CheckOneProjection('CoordSys Earth Projection 4, 104, "m", 10, 90, 90','+proj=laea',4);
  CheckOneProjection('CoordSys Earth Projection 5, 104, "m", 10, -90, 90','+proj=aeqd',5);
  CheckOneProjection('CoordSys Earth Projection 6, 104, "m", 3, 46.5, 44, 49, 700000, 6600000','+proj=eqdc',6);
  CheckOneProjection('CoordSys Earth Projection 7, 104, "m", 10, 52, 30, 0.9996, 1000, 2000','+proj=omerc',7);
  CheckOneProjection('CoordSys Earth Projection 8, 104, "m", 39, 0, 1, 7500000, 0','+proj=tmerc',8);
  CheckOneProjection('CoordSys Earth Projection 9, 104, "m", 3, 46.5, 44, 49, 700000, 6600000','+proj=aea',9);
  CheckOneProjection('CoordSys Earth Projection 10, 104, "m", 0','+proj=merc',10);
  CheckOneProjection('CoordSys Earth Projection 11, 104, "m", 0','+proj=mill',11);
  CheckOneProjection('CoordSys Earth Projection 12, 104, "m", 0','+proj=robin',12);
  CheckOneProjection('CoordSys Earth Projection 13, 104, "m", 0','+proj=moll',13);
  CheckOneProjection('CoordSys Earth Projection 14, 104, "m", 0','+proj=eck4',14);
  CheckOneProjection('CoordSys Earth Projection 15, 104, "m", 0','+proj=eck6',15);
  CheckOneProjection('CoordSys Earth Projection 16, 104, "m", 0','+proj=sinu',16);
  CheckOneProjection('CoordSys Earth Projection 17, 104, "m", 0','+proj=gall',17);
  CheckOneProjection('CoordSys Earth Projection 18, 104, "m", 173, -41, 2510000, 6023150','+proj=nzmg',18);

  { Projection 19 is the MapInfo Belgium LCC variant. PROJ 5.2 exposes lcc,
    so the variant ID cannot be reconstructed from a plain PROJ string. }
  CheckOneProjection('CoordSys Earth Projection 19, 104, "m", 4.3674867, 90, 49.8333339, 51.1666672, 150000.013, 5400088.438','+proj=lcc',3);

  CheckOneProjection('CoordSys Earth Projection 20, 104, "m", 0, 90, 0.994, 2000000, 2000000','+proj=stere',20);

  { MapInfo 21..24 are distinct modified-TM variants but PROJ 5.2 tmerc has no
    variant identifier. Their projection parameters survive; the ID becomes 8. }
  CheckOneProjection('CoordSys Earth Projection 21, 104, "m", 9, 0, 0.9996, 500000, 0','+proj=tmerc',8);
  CheckOneProjection('CoordSys Earth Projection 22, 104, "m", 9, 0, 0.9996, 500000, 0','+proj=tmerc',8);
  CheckOneProjection('CoordSys Earth Projection 23, 104, "m", 15, 0, 0.9996, 500000, 0','+proj=tmerc',8);
  CheckOneProjection('CoordSys Earth Projection 24, 104, "m", 27, 0, 1, 3500000, 0','+proj=tmerc',8);

  CheckOneProjection('CoordSys Earth Projection 25, 104, "m", 7.439583333333, 46.952405555556, 600000, 200000','+proj=somerc',25);
  CheckOneProjection('CoordSys Earth Projection 26, 104, "m", 0, 33','+proj=merc',26);
  CheckOneProjection('CoordSys Earth Projection 27, 104, "m", -54, -22, 500000, 1000000','+proj=poly',27);

  { 28/29 are the general-aspect forms. Legacy GDAL 2.4 chooses 5/4 only
    for polar centers; a non-polar center must stay 28/29. }
  CheckOneProjection('CoordSys Earth Projection 28, 104, "m", 10, 52, 90','+proj=aeqd',28);
  CheckOneProjection('CoordSys Earth Projection 29, 104, "m", 10, 52, 90','+proj=laea',29);

  CheckOneProjection('CoordSys Earth Projection 30, 104, "m", 10, 52, 1000, 2000','+proj=cass',30);
  CheckOneProjection('CoordSys Earth Projection 31, 104, "m", 10, 52, 0.9999, 1000, 2000','+proj=sterea',31);

  { PROJ 5.2 Krovak hard-codes S0/UQ internally and ignores an external
    alpha value.  Strict GDAL 2.4 PROJ import materializes the Krovak WKT
    azimuth as 30.28813972222222 and pseudo-standard-parallel as 78.5.
    Use those legacy metadata values for an exact MapInfo -> PROJ -> MapInfo
    round-trip.  Do not derive alpha as 90deg-UQ (30.288139752778), since
    GDAL 2.4 deliberately uses the slightly different legacy constant. }
  CheckOneProjection('CoordSys Earth Projection 32, 104, "m", 24.8333333333333, 49.5, 78.5, 30.28813972222222, 0, 0','+proj=krovak',32);

  CheckOneProjection('CoordSys Earth Projection 33, 104, "m", 0, 0, 0, 0','+proj=eqc',33);

  { Critical case: MapInfo Projection 34 must remain etmerc, never tmerc. }
  CheckOneProjection('CoordSys Earth Projection 34, 104, "m", -177, 0, 0.9996, 500000, 0','+proj=etmerc',34);

  CheckOneProjection('CoordSys Earth Projection 35, 104, "m", 102.25, 4, 323.025796466667, 80, 0.99984, 804671, 123456','+proj=omerc',35);
end;

procedure CheckAzimuthalAliasCanonicalization;
var
  A,B: TMapInfoCoordSys;
  P: string;
begin
  { Legacy GDAL accepts the historical polar IDs with a non-polar latitude,
    but once represented as a projection method the original alias is lost.
    On export it canonicalizes such definitions to the general IDs 28/29. }
  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 4, 104, "m", 10, 52, 90',A),
    'Cannot parse non-polar Projection 4 alias test');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheck(P<>'','Projection 4 alias -> PROJ failed');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'PROJ -> MapInfo failed for Projection 4 alias');
  QGCheck(B.ProjectionID=29,'Non-polar Projection 4 must canonicalize to 29');

  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 5, 104, "m", 10, 52, 90',A),
    'Cannot parse non-polar Projection 5 alias test');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheck(P<>'','Projection 5 alias -> PROJ failed');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'PROJ -> MapInfo failed for Projection 5 alias');
  QGCheck(B.ProjectionID=28,'Non-polar Projection 5 must canonicalize to 28');
end;

procedure CheckProjection34UnitsAndDatums;
var
  A,B: TMapInfoCoordSys;
  P: string;
begin
  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 104, "ft", -177, 0, 0.9996, 500000, 0',A),
    'Cannot parse projection 34 / ft');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+proj=etmerc','Projection 34 / ft method');
  QGCheckContains(P,'+x_0=152400','Projection 34 false easting must be converted to metres');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'Projection 34 / ft reverse conversion');
  QGCheck(SameText(B.UnitsName,'ft'),'Projection 34 / ft units lost');
  QGCheckFloat(500000,B.ProjectionParameters[3],'Projection 34 / ft false easting round-trip',1E-7);

  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 1001, "m", 39, 0, 1, 7500000, 0',A),
    'Cannot parse projection 34 / Pulkovo 1942');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+proj=etmerc','Projection 34 / Pulkovo method');
  QGCheckContains(P,'+towgs84=24,-123,-94','Pulkovo datum transform');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'Projection 34 / Pulkovo reverse');
  QGCheck(B.DatumID=1001,'Pulkovo datum ID round-trip');

  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 9999, 28, 1, 2, 3, 4, 5, 6, 7, 2.337229166667, "m", -177, 0, 0.9996, 500000, 0',A),
    'Cannot parse custom 9999 datum');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+towgs84=1,2,3,-4,-5,-6,7','Custom datum rotation convention');
  QGCheckContains(P,'+pm=2.337229166667','Custom datum prime meridian');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'Custom datum reverse conversion');
  QGCheck(B.DatumID=9999,'Custom datum ID round-trip');
  QGCheck(B.EllipsoidID=28,'Custom datum ellipsoid round-trip');
  QGCheckParams(A.DatumParameters,B.DatumParameters,'Custom datum parameters');
end;

procedure CheckMapInfoSpecialUnits;
var
  A,B: TMapInfoCoordSys;
  P: string;
begin
  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 104, "li", -177, 0, 0.9996, 1000, 2000',A),
    'Cannot parse MapInfo link units');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+to_meter=0.20116684023368047','MapInfo link must use its exact metre factor');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'MapInfo link reverse');
  QGCheck(SameText(B.UnitsName,'li'),'MapInfo link unit round-trip');
  QGCheckParams(A.ProjectionParameters,B.ProjectionParameters,'MapInfo link parameters');

  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 104, "ch", -177, 0, 0.9996, 1000, 2000',A),
    'Cannot parse MapInfo chain units');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+to_meter=20.116684023368047','MapInfo chain must use its exact metre factor');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'MapInfo chain reverse');
  QGCheck(SameText(B.UnitsName,'ch'),'MapInfo chain unit round-trip');

  QGCheck(TryParseMapInfoCoordSys(
    'CoordSys Earth Projection 34, 104, "rd", -177, 0, 0.9996, 1000, 2000',A),
    'Cannot parse MapInfo rod units');
  P := MapInfoCoordSysToLibProjDefn(A);
  QGCheckContains(P,'+to_meter=5.02921005842012','MapInfo rod exact metre factor');
  QGCheck(LibProjDefnToMapinfoCoordSys(P,B),'MapInfo rod reverse');
  QGCheck(SameText(B.UnitsName,'rd'),'MapInfo rod unit round-trip');
end;

procedure CheckWKTExternalFallback;
const
  ETM_WKT =
    'PROJCS["ETM",'+
    'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS_84",6378137,298.257223563],TOWGS84[0,0,0,0,0,0,0]],'+
    'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],'+
    'PROJECTION["Extended_Transverse_Mercator"],'+
    'PARAMETER["central_meridian",-177],PARAMETER["latitude_of_origin",0],'+
    'PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],'+
    'PARAMETER["false_northing",0],UNIT["Meter",1]]';
  CUSTOM_GEOG_WKT =
    'GEOGCS["Custom",DATUM["Custom_Datum",SPHEROID["Custom_Spheroid",6378137,298.257223563],'+
    'TOWGS84[1,2,3,4,5,6,7]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]';
var
  P,W: string;
begin
  P := WKTProjectionToLibProjDefn(ETM_WKT);
  QGCheckContains(P,'+proj=etmerc','External WKT ETM method');
  QGCheckContains(P,'+lon_0=-177','External WKT ETM central meridian');
  QGCheckContains(P,'+k_0=0.9996','External WKT ETM scale');

  P := WKTProjectionToLibProjDefn(CUSTOM_GEOG_WKT);
  QGCheckContains(P,'+proj=longlat','External WKT geographic method');
  QGCheckContains(P,'+a=6378137','External WKT custom spheroid semi-major');
  QGCheckContains(P,'+b=6356752','External WKT custom spheroid semi-minor');
  QGCheckContains(P,'+towgs84=1,2,3,4,5,6,7','External WKT TOWGS84 7 parameters');

  W := LibProjDefnToWKTProjection(
    '+proj=longlat +a=6378137 +rf=298.257223563 +pm=2.337229166667 +towgs84=1,2,3,4,5,6,7 +no_defs',False);
  QGCheckContains(W,'PRIMEM["unnamed",2.337229166667]','Numeric PROJ prime meridian -> WKT');
  QGCheckContains(W,'TOWGS84[1,2,3,4,5,6,7]','PROJ TOWGS84 -> WKT');
end;

procedure CheckWKTExtensionRoundTrip;
var
  P,W,P2: string;
begin
  P := '+proj=etmerc +lon_0=-177 +lat_0=0 +k_0=0.9996 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs';
  W := LibProjDefnToWKTProjection(P,False);
  QGCheckContains(W,'PROJECTION["Transverse_Mercator"]','PROJ etmerc -> legacy GDAL 2.4 WKT method');
  QGCheckContains(W,'EXTENSION["PROJ4"','PROJ WKT exact extension');
  P2 := WKTProjectionToLibProjDefn(W);
  QGCheck(P2=P,'PROJ -> WKT -> PROJ must preserve exact EXTENSION definition');
end;

procedure CheckWKTCanonicalGeneration;
var
  W: string;
begin
  W := LibProjDefnToWKTProjection(
    '+proj=merc +ellps=WGS84 +datum=WGS84 +units=m +no_defs',False);
  QGCheck(W<>'','Mercator WKT generation failed');
  QGCheckContains(W,'GEOGCS["unnamed"','Projected CRS must contain GEOGCS, not GEOCCS');
  QGCheck(Pos('GEOCCS[',W)=0,'Projected CRS incorrectly contains GEOCCS');
  QGCheckContains(W,'PROJECTION["Mercator_1SP"]','Mercator without +lat_ts must be Mercator_1SP');
  QGCheckContains(W,'PARAMETER["latitude_of_origin",0]','Mercator_1SP default latitude_of_origin');
  QGCheckContains(W,'PARAMETER["central_meridian",0]','Mercator_1SP default central_meridian');
  QGCheckContains(W,'PARAMETER["scale_factor",1]','Mercator_1SP default scale_factor');
  QGCheckContains(W,'PARAMETER["false_easting",0]','Mercator_1SP default false_easting');
  QGCheckContains(W,'PARAMETER["false_northing",0]','Mercator_1SP default false_northing');

  W := LibProjDefnToWKTProjection(
    '+proj=merc +lat_ts=33 +lon_0=10 +x_0=100 +y_0=200 +ellps=WGS84 +units=m +no_defs',False);
  QGCheckContains(W,'PROJECTION["Mercator_2SP"]','Mercator with +lat_ts must be Mercator_2SP');
  QGCheckContains(W,'PARAMETER["standard_parallel_1",33]','Mercator_2SP standard parallel');
  QGCheckContains(W,'PARAMETER["latitude_of_origin",0]','Mercator_2SP latitude_of_origin');
  QGCheckContains(W,'PARAMETER["central_meridian",10]','Mercator_2SP central_meridian');
  QGCheckContains(W,'PARAMETER["false_easting",100]','Mercator_2SP false_easting');
  QGCheckContains(W,'PARAMETER["false_northing",200]','Mercator_2SP false_northing');
end;

procedure CheckRussianProjectionCaptions;
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := Low(cSupportedProjectionsList) to High(cSupportedProjectionsList) do
    if SameText(cSupportedProjectionsList[I].Key,'aea') then
    begin
      QGCheck(cSupportedProjectionsList[I].Desc='Коническая равновеликая Альберса',
        'Russian projection caption for aea was lost');
      Found := True;
      Break;
    end;
  QGCheck(Found,'aea projection metadata not found');
  QGCheck(LibProjParamDisplayName('lon_0')='Центральный меридиан',
    'Russian PROJ parameter display name was lost');
end;

procedure CheckWKTModelAndStream;
const
  SOURCE_WKT =
    'GEOGCS["Custom",DATUM["Custom_Datum",SPHEROID["Custom_Spheroid",6378137,298.257223563]],'+
    'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]';
var
  G1,G2: TWKTGeoCRS;
  M: TMemoryStream;
  N: Integer;
begin
  G1 := TWKTCRSDefinition.NewFromString(SOURCE_WKT) as TWKTGeoCRS;
  QGCheck(Assigned(G1),'WKT model parse');
  try
    QGCheck(G1.Valid,'Parsed WKT GeoCRS must validate');
    QGCheck(G1.SpheroidAF='6378137,298.257223563','SPHEROID a/rf extraction');
    G1.Name := 'Custom "quoted" CRS';
    QGCheck(Pos('Custom ""quoted"" CRS',G1.SaveToString(False)) > 0,
      'Embedded WKT quote serialization');
    G1.ToWGS := '1,2,3,4,5,6,7';
    QGCheck(G1.ToWGS='1,2,3,4,5,6,7','TOWGS84 model setter/getter');

    M := TMemoryStream.Create;
    try
      N := G1.SaveToStream(M);
      QGCheck(N=M.Size,'SaveToStream returned byte count differs from stream size');
      QGCheck(M.Size>Length(SOURCE_WKT) div 2,'SaveToStream copied only a node count instead of full WKT');
      M.Position := 0;
      G2 := TWKTGeoCRS.Create(nil);
      try
        G2.LoadFromStream(M);
        QGCheck(G2.Valid,'Reloaded WKT GeoCRS must validate');
        QGCheck(G2.ToWGS='1,2,3,4,5,6,7','TOWGS84 lost after stream round-trip');
        QGCheck(G2.SpheroidAF='6378137,298.257223563','SPHEROID a/rf lost after stream round-trip');
      finally
        G2.Free;
      end;
    finally
      M.Free;
    end;
  finally
    G1.Free;
  end;
end;

procedure CheckWKTParameterCache;
const
  SOURCE_WKT =
    'PROJCS["TM",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS_84",6378137,298.257223563]],'+
    'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],'+
    'PARAMETER["central_meridian",3],PARAMETER["latitude_of_origin",0],PARAMETER["scale_factor",1],'+
    'PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1]]';
var
  D: TWKTCRSDefinition;
  P: TWKTProjectedCRS;
  CountBefore: Integer;
begin
  D := TWKTCRSDefinition.NewFromString(SOURCE_WKT);
  QGCheck(Assigned(D) and (D is TWKTProjectedCRS),'Projected WKT parse');
  try
    P := TWKTProjectedCRS(D);
    CountBefore := P.ProjectionParameters.Count;
    P.Parameter['central_meridian'] := '4';
    QGCheck(P.Parameter['central_meridian']='4','Projection parameter cache update');
    QGCheck(P.ProjectionParameters.Count=CountBefore,'Projection parameter count after update');
    P.Parameter['false_northing'] := '';
    QGCheck(P.ProjectionParameters.Count=CountBefore-1,'Projection parameter cache after remove');
  finally
    D.Free;
  end;
end;

{$IFDEF LIBPROJ_QG_EXCEPTION_TESTS}
{ This is an intentional exception-path test. Delphi's debugger normally
  breaks when EWKTError is raised even though the exception is caught below.
  Keep it opt-in so the normal QG run is uninterrupted under the IDE. }
procedure CheckWKTUnterminatedQuote;
var
  G: TWKTGeoCRS;
  Raised: Boolean;
begin
  G := TWKTGeoCRS.Create(nil);
  try
    Raised := False;
    try
      G.LoadFromString('GEOGCS["Broken CRS');
    except
      on E: EWKTError do
        Raised := True;
    end;
    QGCheck(Raised,'Unterminated WKT quote must raise EWKTError');
  finally
    G.Free;
  end;
end;
{$ENDIF}

procedure CheckMalformedInputRejected;
var
  C: TMapInfoCoordSys;
begin
  QGCheck(MapInfoCoordSysToLibProjDefn(
    'CoordSys Earth Projection 34, 104, "m", -177, 0, 0.9996, 500000')='',
    'Projection 34 with missing parameter must be rejected');
  QGCheck(not TryParseMapInfoCoordSys('CoordSys Earth Projection X, 104, "m"',C),
    'Non-numeric projection ID must be rejected');
  QGCheck(MapInfoCoordSysToLibProjDefn(
    'CoordSys Earth Projection 34, 9999, 28, 1, 2, 3, "m", -177, 0, 0.9996, 500000, 0')='',
    'Incomplete custom datum must be rejected');
  QGCheck(LibProjDefnToMapinfoCoordSys(
    '+proj=etmerc +lon_0=bad +lat_0=0 +k_0=0.9996 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs')='',
    'Malformed numeric PROJ parameter must be rejected');
  QGCheck(WKTProjectionToLibProjDefn(
    'GEOGCS["Bad",DATUM["Bad",SPHEROID["Bad",6378137,broken]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]]')='',
    'Malformed WKT spheroid must be rejected');
end;

procedure RunLibProjProjectionRoundTripQG;
begin
  CheckMapInfoParserGenerator;
  CheckProjectionMatrix;
  CheckAzimuthalAliasCanonicalization;
  CheckProjection34UnitsAndDatums;
  CheckMapInfoSpecialUnits;
  CheckWKTExternalFallback;
  CheckWKTExtensionRoundTrip;
  CheckWKTCanonicalGeneration;
  CheckRussianProjectionCaptions;
  CheckWKTModelAndStream;
  CheckWKTParameterCache;
{$IFDEF LIBPROJ_QG_EXCEPTION_TESTS}
  CheckWKTUnterminatedQuote;
{$ENDIF}
  CheckMalformedInputRejected;
end;

end.
