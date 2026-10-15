unit libProj4.Classes;

interface
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsweakpackaging_xml.html
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
{$IFDEF USER_DEFINES_INC}{$I user_defines.inc}{$ENDIF}
{$ifdef __proj4mappl}{$I ..\mappl_config.inc}{$endif}
uses
  System.Types, System.Classes, Sysutils, System.SyncObjs,
  System.Generics.Collections, libProj4.Types, libProj4.Intf;

type
  TProjection = class(TContainedObject, IProjection)
  private
    FProj4Defn: string;
    FProj4LastError: string;
    FHandle: TPJ;
    FLock: TCriticalSection;
    FToMeters: Double;
    FGeodesic: TGeodesic;
    FGeodesicValid: Boolean;
    [weak] FGeoProjection: IProjection;
    function GetHandle: TPJ;
    function Manager: IProjectionsManager;
    procedure InitGeodesic;
    class function MetricMetersPerMapUnit(const Metric: TProjectionLocalMetric; const Mode: TProjectionScaleMode): Double; static;
    function GeodesicDistance(const Lat1,Lon1,Lat2,Lon2: Double): Double;
    function ComputeGeodesicPolygon(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double; Reversed,Signed: Boolean): Boolean;
    function GeodesicPolygonArea(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double): Boolean;
    function GetLocalMetricInternal(const X,Y: Double; const NeedArea: Boolean; out Metric: TProjectionLocalMetric): Boolean;
    procedure Lock;
    procedure UnLock;
  protected
     procedure CreateHandle;
     procedure DestroyHandle;
     procedure SetProj4Definition(const Value: string);
     function DumpParams(var ADest: TArray<TPJParamRec>): Integer;
     function GetDisplayName(): string; virtual;
     function GetParamDisplayName(const AParam: string): string; virtual;
     function GetDefaultParams(): string; virtual;
     procedure Init; virtual;
     procedure DeInit; virtual;
     procedure SetDefaults; virtual;
    function IsEquals(Obj: TProjection): Boolean; overload;
  public
    constructor Create(Controller: IProjectionsManager); overload;
    constructor Create(Controller: IProjectionsManager; const AHandle: TPJ); overload;
    constructor Create(Controller: IProjectionsManager; const ADefn: string); overload;
    procedure AfterConstruction; override; final;
    procedure BeforeDestruction; override; final;
    function Equals(Obj: TObject): Boolean; override;
    function IsEquals(const PJ: IProjection): Boolean; overload;
    function Geographic: IProjection;
    function IsGeocentric: Boolean;
    function IsGeographic: Boolean;
    function HandleAllocated: Boolean;
    function MapinfoDefinition: string;
    function Proj4Definition: string;
    function SpheroidDefinition(var a,b,es: Double): Boolean;
    function ToMetersFactor: Double;
    function GetProjectionFactors(const X,Y: Double; out Factors: TPJFactors): Boolean;
    function GetConformalityAt(const X,Y: Double; const AngularToleranceRad: Double = PROJ_DEFAULT_CONFORMAL_TOLERANCE_RAD): TProjectionConformality;
    function GetLocalMetric(const X,Y: Double; out Metric: TProjectionLocalMetric): Boolean;
    function GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixelX,GroundMetersPerPixelY: Double): Boolean; overload;
    function GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixel: Double; const Mode: TProjectionScaleMode = psmHorizontal): Boolean; overload;
    function GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double = PROJ_DEFAULT_PIXEL_SIZE_MM): Boolean; overload;
    function GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean; overload;
    function GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double = PROJ_DEFAULT_PIXEL_SIZE_MM): Boolean; overload;
    function GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean; overload;
    function ToString: string; override;
    function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean;
    function TransformPoints(Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
    function WKTDefinition(const PrettyPrint: Boolean): string;

    property Handle: TPJ read GetHandle;
  end;

  TProjectionsManager = class(TComponent, IProjectionsManager)
  private
  class var FGLobalInstance: TProjectionsManager;
  var
    FLock: TCriticalSection;
    FContext: TPJCtx;
    FList: TThreadList<TPair<string, TProjection>>;
    FSearchPaths: string;
    // Per-manager cache. The projection is owned by FList; do not store it as
    // an interface here, because its lifetime belongs to this manager.
    FWgs84Projection: TProjection;
    procedure ListNotify(Sender: TObject; const Item: TPair<string,TProjection>; Action: TCollectionNotification);
    procedure Lock;
    procedure Unlock;
    procedure CreateContext;
    procedure DestroyContext;
    procedure SetSearchPaths(const Value: string);
    function GetWgs84Projection: TProjection;
  protected
    function FindIndex(const Defn: string): Integer;
    procedure Init; virtual;
    procedure DeInit; virtual;
    function RegisterProjection(const AProjection: TProjection): Integer;
    procedure ChangeDefinition(AOld,ANew: string);
    function GetProjectionByDefinition(const ADefn: string): IProjection; virtual;
    function GetProjectionByCode(const EpsgCode: Integer): IProjection;
  public
    procedure AfterConstruction; override; final;
    procedure BeforeDestruction; override; final;
    function Context: TPJCtx;
    function TransformPoints(const Src,Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
    function TransformPoint(const Src,Dst: IProjection; var X,Y: Double; AngularUnits: Boolean): Boolean;
    function TransformPointsXY(const Src,Dst: IProjection;  const X,Y: PDouble; Count: Integer): Boolean;
    function TransformPointXY(const Src,Dst: IProjection; var X,Y: Double): Boolean;
    function GetGeodeticDistance(const Lat1,Lon1, Lat2,Lon2: Double): Double;
    function ComputeGeodeticPolygon(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double; Reversed: Boolean = False; Signed: Boolean = False): Boolean;
    function GetGeodeticPolygonArea(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double): Boolean;
    class function GlobalInstance: IProjectionsManager; virtual;
		property ProjectionByDefinition[const Defn: string]: IProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: IProjection read GetProjectionByCode;
    property SearchPaths: string read FSearchPaths write SetSearchPaths;
  end;

implementation

uses
  libProj4.Api, libProj4.Projections;

procedure TProjection.AfterConstruction;
begin
  inherited;
  FLock := TCriticalSection.Create;
  Lock;
  try
    Init;
  finally
    Unlock;
  end;
end;

procedure TProjection.BeforeDestruction;
begin
  Lock;
  try
    Deinit;
  finally
    Unlock;
  end;
  FreeAndNil(FLock);
  inherited;
end;

constructor TProjection.Create(Controller: IProjectionsManager; const ADefn: string);
begin
  if ADefn.IsEmpty then
    raise EEmptyProjectionDefinition.Create('empty projection definition');
  Create(Controller);
  FProj4Defn := ADefn;
end;

constructor TProjection.Create(Controller: IProjectionsManager; const AHandle: TPJ);
begin
  Create(Controller);
  FHandle := AHandle;
end;

constructor TProjection.Create(Controller: IProjectionsManager);
begin
  inherited Create(Controller);
  SetDefaults;
end;

procedure TProjection.CreateHandle;
begin
  Lock;
  try
    if FHandle = nil then
      FHandle := CreatePJ(FProj4Defn,nil,@FProj4LastError);

    // Handles supplied through Create(Controller, AHandle) also pass here.
    // Therefore geodesic initialization must not depend on this method having
    // created the PJ handle itself.
    if (FHandle <> nil) and not FGeodesicValid then
      InitGeodesic;
  finally
    UnLock;
  end;
end;

procedure TProjection.InitGeodesic;
var
  a,b,es,f: Double;
begin
  FGeodesic := Default(TGeodesic);
  FGeodesicValid := False;

  if FHandle = nil then
    Exit;

  if not GetSpheroidInfo(FHandle,a,b,es) then
    Exit;

  if (a <= 0.0) or (b <= 0.0) then
    Exit;

  // geod_init expects flattening f, not eccentricity squared es.
  f := (a - b) / a;
  if f.IsNan or f.IsInfinity then
    Exit;

  libProj4.Api.GeodesicInit(FGeodesic,a,f);
  FGeodesicValid := True;
end;

procedure TProjection.DestroyHandle;
begin
  Lock;
  try
    FreePJ(FHandle);
    FGeodesic := Default(TGeodesic);
    FGeodesicValid := False;
  finally
    UnLock;
  end;
end;

function TProjection.GeodesicDistance(const Lat1,Lon1,Lat2,Lon2: Double): Double;
var
  Azi12,Azi21: Double;
begin
  Result := 0.0;
  Lock;
  try
    if not FGeodesicValid then
      Exit;

    libProj4.Api.GeodesicInverse(FGeodesic,Lat1,Lon1,Lat2,Lon2,Result,Azi12,Azi21);
    if Result.IsNan or Result.IsInfinity then
      Result := 0.0;
  finally
    Unlock;
  end;
end;

function TProjection.ComputeGeodesicPolygon(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double; Reversed,Signed: Boolean): Boolean;
var
  p: TGeodesicPolygon;
begin
  AArea := 0.0;
  APerimeter := 0.0;
  Result := False;

  Lock;
  try
    if not FGeodesicValid then
      Exit;

    p := GeodesicPolygonCreate(FGeodesic,yLats,xLons);
    Result := GeodesicPolygonCompute(FGeodesic,p,Reversed,Signed,AArea,APerimeter) > 2;

    if not Result or AArea.IsInfinity or AArea.IsNan then
      AArea := 0.0;
    if not Result or APerimeter.IsInfinity or APerimeter.IsNan then
      APerimeter := 0.0;
  finally
    Unlock;
  end;
end;

function TProjection.GeodesicPolygonArea(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double): Boolean;
begin
  AArea := 0.0;
  APerimeter := 0.0;
  Result := False;

  Lock;
  try
    if not FGeodesicValid then
      Exit;

    libProj4.Api.GeodesicPolygonArea(FGeodesic,yLats,xLons,AArea,APerimeter);
    Result := (AArea > 0.0) and not AArea.IsNan and not AArea.IsInfinity and
      not APerimeter.IsNan and not APerimeter.IsInfinity;
    if not Result then
    begin
      AArea := 0.0;
      APerimeter := 0.0;
    end;
  finally
    Unlock;
  end;
end;

function TProjection.DumpParams(var ADest: TArray<TPJParamRec>): Integer;
begin
  Result := LibProjParseParamsString(Proj4Definition,ADest);
end;

function TProjection.IsEquals(const PJ: IProjection): Boolean;
begin
  Result := Assigned(PJ) and IsEquals(TProjection(PJ));
end;

function TProjection.IsEquals(Obj: TProjection): Boolean;
begin
  Result := Assigned(Obj) and Self.HandleAllocated and Obj.HandleAllocated and SameText(Self.ToString, TProjection(Obj).ToString);
end;

function TProjection.Equals(Obj: TObject): Boolean;
begin
  Result := inherited Equals(Obj) and Assigned(Self) and Assigned(Obj) and (Obj is TProjection);
  if Result then
    Result := IsEquals(TProjection(Obj))
end;

function TProjection.Geographic: IProjection;
var
  latLong: TPJ;
begin
  Lock;
  try
    if (FGeoProjection = nil) then
    begin
      if GetLatLong(Self.Handle,latLong) then
        FGeoProjection := TProjection.Create(Manager, latLong);
    end;

    Result := FGeoProjection;
  finally
    Unlock;
  end;
end;

function TProjection.GetDefaultParams: string;
begin
   Result := ''; //todo// LibProjProjectionAllowedParams(Handle.Id);
end;

function TProjection.GetDisplayName: string;
begin
  Result := Handle.Id;
end;

function TProjection.GetHandle: TPJ;
begin
  Lock;
  try
    CreateHandle;
    Result := FHandle;
  finally
    UnLock;
  end;
end;

function TProjection.GetParamDisplayName(const AParam: string): string;
begin
  Result := LibProjParamDisplayName(AParam);
end;

function TProjection.HandleAllocated: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TProjection.Init;
begin
  Lock;
  try
    CreateHandle;
    if Assigned(Controller) then
      TProjectionsManager(Manager).RegisterProjection(Self);
  finally
    Unlock;
  end;
end;

function TProjection.IsGeocentric: Boolean;
begin
  Lock;
  try
    Result := Handle.IsGeocentric;
  finally
    UnLock;
  end;
end;

function TProjection.IsGeographic: Boolean;
begin
  Lock;
  try
    Result := Handle.IsGeographic;
  finally
    Unlock;
  end;
end;

procedure TProjection.Lock;
begin
  FLock.Enter;
end;

function TProjection.Manager: IProjectionsManager;
begin
  Result := IProjectionsManager(Controller);
end;

function TProjection.MapinfoDefinition: string;
begin
  Result := '';
  Lock;
  try
    if FHandle.IsValid then
      Result := LibProjDefnToMapinfoCoordSys(Self.Proj4Definition);
  finally
    Unlock;
  end;
end;

function TProjection.Proj4Definition: string;
begin
  Lock;
  try
    if (FProj4Defn = '') and FHandle.IsValid then
      FProj4Defn := FHandle.Definition;

    Result := FProj4Defn;
  finally
    Unlock;
  end;
end;

procedure TProjection.SetDefaults;
begin
  FToMeters := Double.NaN;
  FGeoProjection := nil;
  FProj4Defn := '';
  FHandle := nil;
  FGeodesic := Default(TGeodesic);
  FGeodesicValid := False;
end;

procedure TProjection.SetProj4Definition(const Value: string);
begin
  Lock;
  try
    if SameText(Value, FProj4Defn) then Exit;

    DestroyHandle;
    SetDefaults;
    FProj4Defn := Value;

  finally
    Unlock;
  end;
end;

function TProjection.SpheroidDefinition(var a,b,es: Double): Boolean;
begin
  Lock;
  try
    Result := GetSpheroidInfo(Handle,a,b,es);
  finally
    Unlock;
  end;
end;

class function TProjection.MetricMetersPerMapUnit(const Metric: TProjectionLocalMetric; const Mode: TProjectionScaleMode): Double;
begin
  case Mode of
    psmHorizontal:
      Result := Metric.MetersPerMapUnitX;
    psmVertical:
      Result := Metric.MetersPerMapUnitY;
    psmAreaEquivalent:
      Result := Metric.AreaEquivalentMetersPerMapUnit;
  else
    Result := Double.NaN;
  end;
end;

function TProjection.GetProjectionFactors(const X,Y: Double; out Factors: TPJFactors): Boolean;
var
  Geo: IProjection;
  Lon,Lat: Double;
  Coord: TPJcoord_t;
begin
  Factors := Default(TPJFactors);
  Result := False;

  // Cartographic factors are defined for a geodetic-to-plane mapping.
  if IsGeographic or IsGeocentric then
    Exit;

  Geo := Geographic;
  if not Assigned(Geo) then
    Exit;

  Lon := X;
  Lat := Y;
  if not TransformPoint(Geo,Lon,Lat,False) then
    Exit;

  if Lon.IsNan or Lon.IsInfinity or Lat.IsNan or Lat.IsInfinity or
     (Abs(Lat) > 90.0) then
    Exit;

  Coord := Default(TPJcoord_t);
  Coord.lp.lam := libProj4.Api.PJ_DegToRad(Lon);
  Coord.lp.phi := libProj4.Api.PJ_DegToRad(Lat);

  Lock;
  try
    CreateHandle;
    if not Assigned(FHandle) then
      Exit;
    Factors := libProj4.Api.GetPJFactors(FHandle,Lon, Lat);
  finally
    Unlock;
  end;

  // At singularities proj_factors may return unusable values. Validate the
  // complete public factor set rather than changing the shared PROJ context
  // error state here.
  Result :=
    not Factors.h.IsNan and not Factors.h.IsInfinity and (Factors.h > 0.0) and
    not Factors.k.IsNan and not Factors.k.IsInfinity and (Factors.k > 0.0) and
    not Factors.s.IsNan and not Factors.s.IsInfinity and
    not Factors.omega.IsNan and not Factors.omega.IsInfinity and
    not Factors.thetaPrime.IsNan and not Factors.thetaPrime.IsInfinity and
    not Factors.alpha.IsNan and not Factors.alpha.IsInfinity and
    not Factors.a.IsNan and not Factors.a.IsInfinity and (Factors.a >= 0.0) and
    not Factors.b.IsNan and not Factors.b.IsInfinity and (Factors.b >= 0.0) and
    not Factors.dx_dlam.IsNan and not Factors.dx_dlam.IsInfinity and
    not Factors.dx_dphi.IsNan and not Factors.dx_dphi.IsInfinity and
    not Factors.dy_dlam.IsNan and not Factors.dy_dlam.IsInfinity and
    not Factors.dy_dphi.IsNan and not Factors.dy_dphi.IsInfinity;
end;

function TProjection.GetConformalityAt(const X,Y: Double; const AngularToleranceRad: Double): TProjectionConformality;
var
  Factors: TPJFactors;
  Tol: Double;
begin
  if IsGeographic or IsGeocentric then
    Exit(pcNotApplicable);

  Tol := Abs(AngularToleranceRad);
  if Tol.IsNan or Tol.IsInfinity then
    Exit(pcUnknown);

  if not GetProjectionFactors(X,Y,Factors) then
    Exit(pcUnknown);

  // PROJ 5.2 defines omega as Tissot angular distortion:
  // omega = 2*asin((a-b)/(a+b)). A locally conformal mapping has a=b, hence
  // omega=0. This detects shear as well as unequal axis scales.
  if Abs(Factors.omega) <= Tol then
    Result := pcConformal
  else
    Result := pcNonConformal;
end;

function TProjection.GetLocalMetricInternal(const X,Y: Double; const NeedArea: Boolean; out Metric: TProjectionLocalMetric): Boolean;
const
  LOCAL_SAMPLE_METERS = 10.0;
  LOCAL_SAMPLE_DEGREES = 0.0001;
var
  d,halfD,toMeters,dx,dy,groundArea,groundPerimeter: Double;
  sampleCount: Integer;
  xs,ys: array[0..7] of Double;
  polygonLats,polygonLons: TArray<Double>;
  Wgs84: IProjection;
  Wgs84Projection: TProjection;
begin
  Metric := Default(TProjectionLocalMetric);
  Result := False;

  if IsGeocentric or (Manager = nil) then
    Exit;

  if IsGeographic then
    d := LOCAL_SAMPLE_DEGREES
  else
  begin
    toMeters := ToMetersFactor;
    if toMeters.IsNan or toMeters.IsInfinity or (toMeters <= 0.0) then
      Exit;
    d := LOCAL_SAMPLE_METERS / toMeters;
  end;

  if d.IsNan or d.IsInfinity or (d <= 0.0) then
    Exit;

  halfD := 0.5 * d;

  // X-axis sample
  xs[0] := X - halfD;
  ys[0] := Y;
  xs[1] := X + halfD;
  ys[1] := Y;

  // Y-axis sample
  xs[2] := X;
  ys[2] := Y - halfD;
  xs[3] := X;
  ys[3] := Y + halfD;

  sampleCount := 4;
  if NeedArea then
  begin
    // Square sample for the true local area metric. Do not derive area from
    // MetersPerMapUnitX * MetersPerMapUnitY: in a non-orthogonal projection
    // that product does not include the angle/shear term.
    xs[4] := X - halfD;
    ys[4] := Y - halfD;
    xs[5] := X + halfD;
    ys[5] := Y - halfD;
    xs[6] := X + halfD;
    ys[6] := Y + halfD;
    xs[7] := X - halfD;
    ys[7] := Y + halfD;
    sampleCount := 8;
  end;

  // Use the manager's cached WGS84 projection both as the common geographic
  // reference and as the owner of the WGS84 geodesic.
  Wgs84Projection := TProjectionsManager(Manager).GetWgs84Projection;
  if not Assigned(Wgs84Projection) then
    Exit;

  if Self <> Wgs84Projection then
  begin
    Wgs84 := Wgs84Projection;
    if not TransformPoints(Wgs84,@xs[0],@ys[0],sampleCount,False) then
      Exit;
  end;

  dx := Wgs84Projection.GeodesicDistance(ys[0],xs[0],ys[1],xs[1]);
  dy := Wgs84Projection.GeodesicDistance(ys[2],xs[2],ys[3],xs[3]);

  if dx.IsNan or dx.IsInfinity or (dx <= 0.0) or
     dy.IsNan or dy.IsInfinity or (dy <= 0.0) then
    Exit;

  Metric.MetersPerMapUnitX := dx / d;
  Metric.MetersPerMapUnitY := dy / d;
  Metric.ScaleRatioYX := Metric.MetersPerMapUnitY / Metric.MetersPerMapUnitX;

  Result :=
    not Metric.MetersPerMapUnitX.IsNan and not Metric.MetersPerMapUnitX.IsInfinity and
    not Metric.MetersPerMapUnitY.IsNan and not Metric.MetersPerMapUnitY.IsInfinity and
    not Metric.ScaleRatioYX.IsNan and not Metric.ScaleRatioYX.IsInfinity;
  if not Result then
    Exit;

  if NeedArea then
  begin
    SetLength(polygonLats,4);
    SetLength(polygonLons,4);
    polygonLats[0] := ys[4]; polygonLons[0] := xs[4];
    polygonLats[1] := ys[5]; polygonLons[1] := xs[5];
    polygonLats[2] := ys[6]; polygonLons[2] := xs[6];
    polygonLats[3] := ys[7]; polygonLons[3] := xs[7];

    if not Wgs84Projection.ComputeGeodesicPolygon(
      polygonLats,polygonLons,groundArea,groundPerimeter,False,False
    ) then
      Exit(False);

    groundArea := Abs(groundArea);
    if groundArea.IsNan or groundArea.IsInfinity or (groundArea <= 0.0) then
      Exit(False);

    Metric.GroundAreaPerMapUnit2 := groundArea / (d * d);
    Metric.AreaEquivalentMetersPerMapUnit := Sqrt(Metric.GroundAreaPerMapUnit2);

    Result :=
      not Metric.GroundAreaPerMapUnit2.IsNan and
      not Metric.GroundAreaPerMapUnit2.IsInfinity and
      (Metric.GroundAreaPerMapUnit2 > 0.0) and
      not Metric.AreaEquivalentMetersPerMapUnit.IsNan and
      not Metric.AreaEquivalentMetersPerMapUnit.IsInfinity and
      (Metric.AreaEquivalentMetersPerMapUnit > 0.0);
  end;
end;

function TProjection.GetLocalMetric(const X,Y: Double; out Metric: TProjectionLocalMetric): Boolean;
begin
  // Public metric is complete, including the true geodesic area metric.
  Result := GetLocalMetricInternal(X,Y,True,Metric);
end;

function TProjection.GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixelX,GroundMetersPerPixelY: Double): Boolean;
var
  Metric: TProjectionLocalMetric;
begin
  GroundMetersPerPixelX := 0.0;
  GroundMetersPerPixelY := 0.0;

  Result :=
    not MapUnitsPerPixel.IsNan and not MapUnitsPerPixel.IsInfinity and
    (MapUnitsPerPixel > 0.0) and GetLocalMetricInternal(X,Y,False,Metric);
  if not Result then
    Exit;

  GroundMetersPerPixelX := MapUnitsPerPixel * Metric.MetersPerMapUnitX;
  GroundMetersPerPixelY := MapUnitsPerPixel * Metric.MetersPerMapUnitY;
  Result :=
    not GroundMetersPerPixelX.IsNan and not GroundMetersPerPixelX.IsInfinity and
    (GroundMetersPerPixelX > 0.0) and
    not GroundMetersPerPixelY.IsNan and not GroundMetersPerPixelY.IsInfinity and
    (GroundMetersPerPixelY > 0.0);
end;

function TProjection.GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixel: Double; const Mode: TProjectionScaleMode): Boolean;
var
  Metric: TProjectionLocalMetric;
  MetersPerMapUnit,Resolution: Double;
begin
  GroundMetersPerPixel := 0.0;

  Result :=
    not MapUnitsPerPixel.IsNan and not MapUnitsPerPixel.IsInfinity and
    (MapUnitsPerPixel > 0.0) and
    GetLocalMetricInternal(X,Y,Mode = psmAreaEquivalent,Metric);
  if not Result then
    Exit;

  MetersPerMapUnit := MetricMetersPerMapUnit(Metric,Mode);
  Result :=
    not MetersPerMapUnit.IsNan and not MetersPerMapUnit.IsInfinity and
    (MetersPerMapUnit > 0.0);
  if not Result then
    Exit;

  // Reject overflow before multiplying, including when FP exceptions are enabled.
  if (MetersPerMapUnit > 1.0) and
     (MapUnitsPerPixel > Double.MaxValue / MetersPerMapUnit) then
    Exit(False);

  Resolution := MapUnitsPerPixel * MetersPerMapUnit;
  Result := not Resolution.IsNan and not Resolution.IsInfinity and (Resolution > 0.0);
  if Result then
    GroundMetersPerPixel := Resolution;
end;

function TProjection.GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double): Boolean;
begin
  Result := GetScaleDenominator(
    X,Y,MapUnitsPerPixel,ScaleDenominator,PixelSizeMM,psmHorizontal
  );
end;

function TProjection.GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean;
var
  GroundMetersPerPixel: Double;
begin
  ScaleDenominator := 0.0;
  Result :=
    not PixelSizeMM.IsNan and not PixelSizeMM.IsInfinity and (PixelSizeMM > 0.0) and
    GetGroundResolution(X,Y,MapUnitsPerPixel,GroundMetersPerPixel,Mode);
  if not Result then
    Exit;

  ScaleDenominator := GroundMetersPerPixel / (PixelSizeMM / 1000.0);
  Result := not ScaleDenominator.IsNan and not ScaleDenominator.IsInfinity and
    (ScaleDenominator > 0.0);
end;

function TProjection.GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double): Boolean;
begin
  Result := GetMapUnitsPerPixelForScale(
    X,Y,ScaleDenominator,MapUnitsPerPixel,PixelSizeMM,psmHorizontal
  );
end;

function TProjection.GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean;
var
  Metric: TProjectionLocalMetric;
  TargetGroundMetersPerPixel,MetersPerMapUnit: Double;
begin
  MapUnitsPerPixel := 0.0;

  Result :=
    not ScaleDenominator.IsNan and not ScaleDenominator.IsInfinity and (ScaleDenominator > 0.0) and
    not PixelSizeMM.IsNan and not PixelSizeMM.IsInfinity and (PixelSizeMM > 0.0) and
    GetLocalMetricInternal(X,Y,Mode = psmAreaEquivalent,Metric);
  if not Result then
    Exit;

  MetersPerMapUnit := MetricMetersPerMapUnit(Metric,Mode);
  Result :=
    not MetersPerMapUnit.IsNan and not MetersPerMapUnit.IsInfinity and
    (MetersPerMapUnit > 0.0);
  if not Result then
    Exit;

  TargetGroundMetersPerPixel := ScaleDenominator * PixelSizeMM / 1000.0;
  MapUnitsPerPixel := TargetGroundMetersPerPixel / MetersPerMapUnit;

  Result := not MapUnitsPerPixel.IsNan and not MapUnitsPerPixel.IsInfinity and
    (MapUnitsPerPixel > 0.0);
end;

function TProjection.ToMetersFactor: Double;
begin
  Lock;
  try
    if FToMeters.IsNan then
      FToMeters := Handle.GetToMetersFactor;

    Result := FToMeters;
  finally
    Unlock;
  end;
end;

function TProjection.ToString: string;
begin
  Result := Proj4Definition;
end;

function TProjection.TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean;
begin
  Result := Assigned(Controller);
  if Result then
    Result := TProjectionsManager(Manager).TransformPoint(Self,Dst,X,Y,AngularUnits);
end;

function TProjection.TransformPoints(Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
begin
  Result := Assigned(Controller);
  if Result then
    Result := Manager.TransformPoints(Self,Dst,X,Y,Count,AngularUnits);
end;

procedure TProjection.UnLock;
begin
  FLock.Leave;
end;

procedure TProjection.Deinit;
begin
  Lock;
  try
    DestroyHandle;
  finally
    Unlock;
  end;
end;

function TProjection.WKTDefinition(const PrettyPrint: Boolean): string;
begin
  Lock;
  try
    Result := LibProjDefnToWKTProjection(Proj4Definition, PrettyPrint);
  finally
    Unlock;
  end;
end;

procedure TProjectionsManager.AfterConstruction;
begin
  inherited;
  Init;
end;

procedure TProjectionsManager.BeforeDestruction;
begin
  inherited;
  DeInit;
end;

procedure TProjectionsManager.ChangeDefinition(AOld, ANew: string);
var
  i,j: Integer;
  lst: TList<TPair<string,TProjection>>;
  item: TPair<string, TProjection>;
begin
  if AOld.IsEmpty or ANew.IsEmpty or SameText(AOld,ANew) then
    Exit;

  lst := FList.LockList;
  try
    item.Key := AOld;
    item.Value := nil;
    j := -1;

    for i := 0 to lst.Count -1 do
    begin
      if SameText(lst[i].Key, item.Key) then
      begin
        item.Value := lst[i].Value;
        j := i;
        Break;
      end;
    end;

    if j < 0 then Exit;
    
{$if compilerversion < 33.0}
    item := lst.Extract(lst.Items[j]);
{$else}
    item := lst.ExtractAt(j);
{$ifend}
    item.Key := ANew;
    item.Value.SetProj4Definition(item.Key);
    lst.Add(item);

  finally
    FList.UnlockList;
  end;

end;

function TProjectionsManager.ComputeGeodeticPolygon(const yLats, xLons: TArray<Double>; var AArea,APerimeter: Double; Reversed: Boolean; Signed: Boolean): Boolean;
var P: TProjection;
begin
  P := GetWgs84Projection;
  Result := Assigned(P) and P.ComputeGeodesicPolygon(yLats,xLons,AArea,APerimeter,Reversed,Signed);
  if not Result then
  begin
    AArea := 0.0;
    APerimeter := 0.0;
  end;
end;

function TProjectionsManager.Context: TPJCtx;
begin
  Lock;
  try
    CreateContext;
    Result := FContext;
  finally
    Unlock;
  end;
end;

procedure TProjectionsManager.CreateContext;
begin
  Lock;
  try
    if FContext = nil then
      FContext := CreatePJContext(Self);
  finally
    Unlock;
  end;
end;

procedure TProjectionsManager.DeInit;
begin
  // FList owns projection objects. Drop the non-owning cache before the list
  // starts releasing them.
  FWgs84Projection := nil;
  FreeAndNil(FList);
  DestroyContext;
  FreeAndNil(FLock);
end;

procedure TProjectionsManager.DestroyContext;
begin
  Lock;
  try
    FreePJContext(FContext);
  finally
    Unlock;
  end;
end;

function TProjectionsManager.FindIndex(const Defn: string): Integer;
var
  lst: TList<TPair<string, TProjection>>;
begin
  if Defn <> '' then
  begin
    lst := FList.LockList;
    try
      for Result := 0 to lst.Count -1 do
      begin
        if SameText(lst[Result].Key,Defn) then
          Exit;
      end;
    finally
      FList.UnlockList;
    end;
  end;
  Result := -1;
end;

function TProjectionsManager.GetWgs84Projection: TProjection;
var
  P: IProjection;
begin
  // Fast path: WGS84 is immutable for normal manager usage and is requested
  // very frequently by measurements and local-scale calculations.
  Result := FWgs84Projection;
  if Assigned(Result) then
    Exit;

  // Serialize first creation. GetProjectionByDefinition has its own list lock;
  // TCriticalSection is recursive for the current thread.
  Lock;
  try
    if not Assigned(FWgs84Projection) then
    begin
      P := GetProjectionByCode(4326);
      if not Assigned(P) then
        P := GetProjectionByDefinition('+proj=longlat +datum=WGS84 +no_defs');
      if Assigned(P) then
        FWgs84Projection := TProjection(P);
    end;

    Result := FWgs84Projection;
  finally
    Unlock;
  end;
end;

function TProjectionsManager.GetGeodeticDistance(const Lat1, Lon1, Lat2, Lon2: Double): Double;
var
  P: TProjection;
begin
  P := GetWgs84Projection;
  if Assigned(P) then
    Result := P.GeodesicDistance(Lat1,Lon1,Lat2,Lon2)
  else
    Result := 0.0;
end;

function TProjectionsManager.GetGeodeticPolygonArea(const yLats, xLons: TArray<Double>; var AArea, APerimeter: Double): Boolean;
var
  P: TProjection;
begin
  P := GetWgs84Projection;
  Result := Assigned(P) and P.GeodesicPolygonArea(yLats,xLons,AArea,APerimeter);
  if not Result then
  begin
    AArea := 0.0;
    APerimeter := 0.0;
  end;
end;

function TProjectionsManager.GetProjectionByCode(const EpsgCode: Integer): IProjection;
begin
  Result := GetProjectionByDefinition(LibProjDefnFromEpsgCode(EpsgCode));
end;

function TProjectionsManager.GetProjectionByDefinition(const ADefn: string): IProjection;
label
  DefnEmpty;
var
  defn: string;
  lst: TList<TPair<string, TProjection>>;
  item: TPair<string, TProjection>;
begin
  Result := nil;
  // handle wkt projectrion strings
  if ADefn.StartsWith('PROJCS[',True) or ADefn.StartsWith('GEOGCS[',True) then
    defn := WKTProjectionToLibProjDefn(ADefn)
  else
    defn := ADefn;

  if defn.IsEmpty then Exit;

  lst := FList.LockList;
  try
    for item in lst do
    begin
      if SameText(item.Key,defn) then
      begin
        Result := item.Value;
        Exit;
      end;
    end;
    Result := TProjection.Create(Self,defn);
  finally
    FList.UnlockList;
  end;
end;

class function TProjectionsManager.GlobalInstance: IProjectionsManager;
begin
  if FGLobalInstance = nil then
    FGLobalInstance := TProjectionsManager.Create(nil);

  Result := FGLobalInstance;
end;

procedure TProjectionsManager.Init;
begin
  FWgs84Projection := nil;
  FLock := TCriticalSection.Create;
  CreateContext;
  FList := TThreadList<TPair<string,TProjection>>.Create;
  FList.LockList.OnNotify := ListNotify;
  FList.UnlockList;
end;

procedure TProjectionsManager.ListNotify(Sender: TObject; const Item: TPair<string, TProjection>; Action: TCollectionNotification);
begin
  case Action of
    cnExtracted:
      // Extract does not free the object, but it is no longer a stable member
      // of this manager's projection cache.
      if Item.Value = FWgs84Projection then
        FWgs84Projection := nil;

    cnRemoved:
    begin
      // The cache is non-owning. A replacement/removal of the cached list item
      // must invalidate it before the projection object is freed.
      if Item.Value = FWgs84Projection then
        FWgs84Projection := nil;
      Item.Value.Free;
    end;
  end;
end;

procedure TProjectionsManager.Lock;
begin
  FLock.Enter;
end;

function TProjectionsManager.RegisterProjection(const AProjection: TProjection): Integer;
var
  defn: string;
  i: Integer;
  item: TPair<string, TProjection>;
  lst: TList<TPair<string, TProjection>>;
begin
  Result := -1;
  if not Assigned(AProjection) then
    Exit;

  defn := AProjection.Proj4Definition;
  if defn = '' then
    raise EEmptyProjectionDefinition.Create('empty projection definition')
  else
  if not AProjection.HandleAllocated then
    raise EInvalidProjectionDefinition.Create('invalid projection definition' + sLineBreak + AProjection.FProj4LastError );

  lst := FList.LockList;
  try
    item.Key := defn;
    item.Value := AProjection;

    for i := 0 to lst.Count -1 do
    begin
      if SameText(lst[i].Key, item.Key) then
      begin
        Result := i;
        Break;
      end;
    end;

    if Result > -1 then
      lst[Result] := item
    else
      Result := lst.Add(item);

  finally
    FList.UnlockList;
  end;
end;

procedure TProjectionsManager.SetSearchPaths(const Value: string);
begin
  if FSearchPaths <> Value then
  begin
    FSearchPaths := Value;
    SetPROJDataSearchPaths(FSearchPaths.Split([';'],'"'));
  end;
end;

function TProjectionsManager.TransformPoint(const Src, Dst: IProjection; var X, Y: Double; AngularUnits: Boolean): Boolean;
var
  x_,y_: Double;
begin
  x_ := X;
  y_ := Y;
  Result := TransformPoints(Src,Dst,@x_,@y_,1,AngularUnits) and not (x_.IsNan or y_.IsNan or x_.IsInfinity or y_.IsInfinity);
  if Result then
  begin
   X := x_;
   Y := y_;
  end;
end;

function TProjectionsManager.TransformPoints(const Src, Dst: IProjection; const X, Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
begin
  Result := Assigned(Src) and Assigned(Dst) {and Assigned(Src.Handle) and Assigned(dst.Handle) } and Assigned(X) and Assigned(Y) and (Count > 0);
  if not Result then Exit;
    Result := libProj4.Api.TransformPoints2D(Src.Handle,Dst.Handle,X,Y, Count, AngularUnits) = 0;
end;

function TProjectionsManager.TransformPointsXY(const Src,Dst: IProjection; const X,Y: PDouble; Count: Integer): Boolean;
begin
  Result := TransformPoints(Src,Dst,X,Y,Count,False);
end;

function TProjectionsManager.TransformPointXY(const Src, Dst: IProjection; var X, Y: Double): Boolean;
begin
  Result := TransformPoint(Src, Dst,X,Y,False);
end;

procedure TProjectionsManager.Unlock;
begin
  FLock.Leave;
end;

initialization
  TProjectionsManager.FGLobalInstance := TProjectionsManager.Create(nil);

finalization
  FreeAndNil(TProjectionsManager.FGLobalInstance);

end.
