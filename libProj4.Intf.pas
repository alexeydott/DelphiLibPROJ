unit libProj4.Intf;

interface
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsweakpackaging_xml.html
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
uses
  System.Types, System.Classes, System.Sysutils,
  libProj4.Types;


type
  EProj4Error = libProj4.Types.EProj4Error;
  TGeodesic = libProj4.Types.TGeodesic;
  TGeodesicLine = libProj4.Types.TGeodesicLine;
  TGeodesicPolygon = libProj4.Types.TGeodesicPolygon;

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
    function SpheroidDefinition(var a,b,e2: Double): Boolean;
    function ToMetersFactor: Double;
    function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean;
    function TransformPoints(Dst: IProjection;  const X,Y: PDouble; Count: Integer; AngularUnits: Boolean): Boolean;
    function WKTDefinition(const PrettyPrint: Boolean): string;
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
