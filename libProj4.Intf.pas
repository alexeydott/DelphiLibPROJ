unit libProj4.Intf;

interface
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsweakpackaging_xml.html
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
{$IFDEF USER_DEFINES_INC}{$I user_defines.inc}{$ENDIF}
{$ifdef __proj4mappl}{$I ..\mappl_config.inc}{$endif}
uses
  System.Types, System.Classes, System.Sysutils,
  libProj4.Types;


type
  EProj4Error = libProj4.Types.EProj4Error;
  TGeodesic = libProj4.Types.TGeodesic;
  TGeodesicLine = libProj4.Types.TGeodesicLine;
  TGeodesicPolygon = libProj4.Types.TGeodesicPolygon;
  TProjectionLocalMetric = libProj4.Types.TProjectionLocalMetric;
  TProjectionScaleMode = libProj4.Types.TProjectionScaleMode;
  TProjectionConformality = libProj4.Types.TProjectionConformality;
  TPJFactors = libProj4.Types.TPJFactors;

  IProjection = Interface(IInvokable)
  ['{EE6FB2E9-9327-46E3-A0FF-2A55AF324498}']
    function Geographic: IProjection;
    function GetHandle: TPJ;
    function DumpParams(var ADest: TArray<TPJParamRec>): Integer;
    function GetDisplayName(): string;
    function GetParamDisplayName(const AParam: string): string;
    function GetDefaultParams(): string;
    function IsEquals(const PJ: IProjection): Boolean;
    function IsGeocentric: Boolean;
    function IsGeographic(): Boolean;
    function HandleAllocated: Boolean;
    function MapinfoDefinition: string;
    function Proj4Definition: string;
    procedure SetProj4Definition(const Value: string);
    function SpheroidDefinition(var a,b,es: Double): Boolean;
    function ToMetersFactor: Double;
    function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean;
    function TransformPoints(Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
    function WKTDefinition(const PrettyPrint: Boolean): string;
    /// Return intrinsic PROJ cartographic factors at projected map point X,Y.
    /// The point is internally transformed to this projection's own geographic
    /// CRS and passed to PROJ 5.2 proj_factors(). Factors include h, k, areal
    /// scale, angular distortion, convergence and Tissot semiaxes.
    function GetProjectionFactors(const X,Y: Double; out Factors: TPJFactors): Boolean;
    /// Return local conformality at X,Y. For projected CRS this uses the Tissot
    /// angular distortion reported by PROJ. Geographic/geocentric CRS return
    /// pcNotApplicable; calculation failures return pcUnknown.
    function GetConformalityAt(const X,Y: Double; const AngularToleranceRad: Double = PROJ_DEFAULT_CONFORMAL_TOLERANCE_RAD): TProjectionConformality;
    /// Return the local physical ground metric around X,Y. In addition to
    /// X/Y linear factors it includes the geodesic ground area represented by
    /// one square map unit.
    function GetLocalMetric(const X,Y: Double; out Metric: TProjectionLocalMetric): Boolean;
    /// Convert map-units/pixel to physical ground meters/pixel along X and Y.
    function GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixelX,GroundMetersPerPixelY: Double): Boolean; overload;
    /// Convert map-units/pixel to one scalar physical ground resolution selected
    /// by Mode.
    function GetGroundResolution(const X,Y,MapUnitsPerPixel: Double; out GroundMetersPerPixel: Double; const Mode: TProjectionScaleMode): Boolean; overload;
    /// Backward-compatible nominal 1:N scale denominator. Uses psmHorizontal.
    function GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double = PROJ_DEFAULT_PIXEL_SIZE_MM): Boolean; overload;
    /// Nominal 1:N scale denominator using the requested local scale mode.
    function GetScaleDenominator(const X,Y,MapUnitsPerPixel: Double; out ScaleDenominator: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean; overload;
    /// Backward-compatible map-units/pixel for requested 1:N scale. Uses
    /// psmHorizontal.
    function GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double = PROJ_DEFAULT_PIXEL_SIZE_MM): Boolean; overload;
    /// Map-units/pixel required for the requested nominal 1:N scale and mode.
    function GetMapUnitsPerPixelForScale(const X,Y,ScaleDenominator: Double; out MapUnitsPerPixel: Double; const PixelSizeMM: Double; const Mode: TProjectionScaleMode): Boolean; overload;
    property Handle: TPJ read GetHandle;
  end;


  IEPSGDatabase = interface(IInvokable)
  ['{5CA58528-BE0D-4719-A23B-1AF58C1BE475}']
    function FindByEPSG(ACode: Integer; var pjName, pjDefn: string): Boolean;
  end;


  IProjectionsManager = interface(IInvokable)
    ['{85C50F1B-D9C8-4958-885E-3B4A9F2FB61A}']
      function GetProjectionByDefinition(const Defn: string): IProjection;
	  function GetProjectionByCode(const EpsgCode: Integer): IProjection;
      function TransformPoints(const Src,Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
      function TransformPoint(const Src,Dst: IProjection; var X,Y: Double; AngularUnits: Boolean): Boolean;
      function GetGeodeticDistance(const yLat1,xLon1,yLat2,xLon2: Double): Double;
      function ComputeGeodeticPolygon(const yLats, xLons: TArray<Double>; var AArea, APerimeter: Double; Reversed: Boolean = False; Signed: Boolean = False): Boolean;
      function GetGeodeticPolygonArea(const yLats,xLons: TArray<Double>; var AArea,APerimeter: Double): Boolean;
	  property ProjectionByDefinition[const Defn: string]: IProjection read GetProjectionByDefinition;
	  property ProjectionByEpsgCode[const Code: Integer]: IProjection read GetProjectionByCode;
  end;

  EInvalidProjectionDefinition = class(EProj4Error);
  EEmptyProjectionDefinition = class(EInvalidProjectionDefinition);

implementation


end.
