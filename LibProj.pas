unit LibProj;

interface

uses
	Classes, SysUtils, Generics.Collections,

	LibProjApi;

type
	TProjectionsManager = class;

	IProjection = interface(IUnknown)
	['{EE6FB2E9-9327-46E3-A0FF-2A55AF324498}']
		procedure CreateHandle(); stdcall;
		procedure DestroyHandle(); stdcall;
		/// <param name="a">
		///   MajorAxis
		/// </param>
		/// <param name="b">
		///   MinorAxis
		/// </param>
		/// <param name="e2">
		///   EccentricitySquared
		/// </param>
		function GetSpheroidDefinition(out a,b,e2: Double): Boolean; stdcall;
		function GetCreateDefinition: string; stdcall;
		function GetDefinition: string; stdcall;
		function GetHandle: Pointer; stdcall;
		function GetIsGeocraphic(): Boolean; stdcall;
		function GetIsGeocentric(): Boolean; stdcall;
		function HandleValid(): Boolean; stdcall;

		property Handle: Pointer read GetHandle;
	end;

	TProjection = class(TInterfacedPersistent,IProjection)
	private
		FHandle: Pointer;
		FOwner: TProjectionsManager;
		FDefn: string;
		procedure SetDefinition(const Value: string);
	protected
		procedure CreateHandle(); stdcall;
		procedure DestroyHandle(); stdcall;

		function GetHandle: Pointer; stdcall;
		function GetSpheroidDefinition(out a,b,e2: Double): Boolean; stdcall;
		function GetCreateDefinition: string; stdcall;
		function GetDefinition: string; stdcall;
		function GetIsGeocraphic(): Boolean; stdcall;
		function GetIsGeoCentric(): Boolean; stdcall;

		property Handle: Pointer read GetHandle;
	public
		constructor Create(const ADefn: string); overload;
		constructor CreateOwned(AOwner: TProjectionsManager; const ADefn: string); overload;
		constructor CreateOwned(AOwner: TProjectionsManager); overload;
		destructor Destroy(); override;
		function HandleValid(): Boolean; stdcall;
		property Definition: string read GetDefinition write SetDefinition;
		property IsGeographic: Boolean read GetIsGeocraphic;
		property IsGeocentric: Boolean read GetIsGeocentric;
	end;

//	IPointsTransformer = interface(IUnknown)
//		['{AD17E7B1-B047-4C25-B3A2-345FC4FD2375}']
//		function TransformPointsXY(Dest: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean); stdcall;
//		function TransformPointXY(Dest: IProjection; var X,Y: Double; AngularUnits: Boolean); stdcall;
//	end;

	IProjectionsManager = interface(IUnknown)
	['{85C50F1B-D9C8-4958-885E-3B4A9F2FB61A}']
		function GetContainer(): TObject; stdcall;
		function GetProjectionByDefinition(const Defn: string): TProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): TProjection; stdcall;
		function TryGetProjection(const Defn: string; out Proj: TProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: TProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer; stdcall;
		property ProjectionByDefinition[const Defn: string]: TProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: TProjection read GetProjectionByCode;
	end;

	TProjectionsManager = class(TComponent, IProjectionsManager)
	private
		FContainer: TDictionary<string,TProjection>;
	protected
		function GetContainer: TObject; stdcall;
		function GetProjectionByDefinition(const Defn: string): TProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): TProjection; stdcall;
		function TryGetProjection(const Defn: string; out Proj: TProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: TProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer; stdcall;
	public
		destructor Destroy(); override;
		function KnownProjections: TDictionary<string,TProjection>;
		property ProjectionByDefinition[const Defn: string]: TProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: TProjection read GetProjectionByCode;
	end;


	function LibProjDefnFromEpsgCode(const Code: Integer): string;

implementation

//--------------------------------------
// utility functions

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

{ TProjection }

constructor TProjection.Create(const ADefn: string);
begin
	CreateOwned(nil,ADefn);
end;

procedure TProjection.CreateHandle();
begin
	DestroyHandle();

	FHandle := PJ_init_plus(GetCreateDefinition);
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager);
begin
	CreateOwned(AOwner,'');
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager; const ADefn: string);
begin
	inherited Create();

	FOwner := AOwner;
  FDefn := ADefn;
end;

destructor TProjection.Destroy;
begin
	inherited;
  DestroyHandle();
end;

procedure TProjection.DestroyHandle;
begin
	if FHandle <> nil then
		PJ_free(FHandle,True);
end;

function TProjection.GetCreateDefinition: string;
begin
  Result := FDefn;
end;

function TProjection.GetDefinition: string;
begin
	Result := PJ_get_definition(Handle);
end;

function TProjection.GetHandle: Pointer;
begin
	if FHandle = nil then
		CreateHandle();

	Result := FHandle;
end;

function TProjection.GetIsGeoCentric: Boolean;
begin
  Result := PJ_is_geocentric(Handle);
end;

function TProjection.GetIsGeocraphic: Boolean;
begin
  Result := PJ_is_geographic(Handle);
end;

function TProjection.GetSpheroidDefinition(out a, b, e2: Double): Boolean;
begin
	Result := PJ_get_spheroid_defn(Handle,a,e2);
	if Result then
	begin
		b := a * Sqrt(1 - e2)
	end
	else
		b := a.NegativeInfinity;
end;

function TProjection.HandleValid: Boolean;
begin
  Result := Assigned(Handle)
end;

procedure TProjection.SetDefinition(const Value: string);
begin
	if not SameText(Value,FDefn) then
	begin
	  DestroyHandle;
		FDefn := Value;
  end;
end;

{ TProjectionsManager }

destructor TProjectionsManager.Destroy;
begin
  FreeAndNil(FContainer);
	inherited;
end;

function TProjectionsManager.GetContainer: TObject;
begin
	if FContainer = nil then
		FContainer := TObjectDictionary<string, TProjection>.Create([doOwnsValues]);

	Result := FContainer;
end;

function TProjectionsManager.GetProjectionByCode(const EpsgCode: Integer): TProjection;
begin
	Result := GetProjectionByDefinition(LibProjDefnFromEpsgCode(EpsgCode));
end;

function TProjectionsManager.GetProjectionByDefinition(const Defn: string): TProjection;
begin
	TryGetProjection(Defn,Result);
end;

function TProjectionsManager.KnownProjections: TDictionary<string, TProjection>;
begin
	Result := TDictionary<string, TProjection>(GetContainer);
end;

function TProjectionsManager.MakeNewProjectionFromDefn(const Defn: string; out proj: TProjection): Integer;
begin
	Result := -1;
	if Defn <> '' then
	begin
		proj := TProjection.CreateOwned(Self,Defn);
		if proj.HandleValid then
			Result := 0
		else
		begin
			Result := PJ_get_errno();

			FreeAndNil(proj);
		end;
	end;
end;

function TProjectionsManager.MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer;
begin
	Result := -1;
	if TryGetProjection(LibProjDefnFromEpsgCode(Code),proj) then
		Result := 0;
end;

function TProjectionsManager.TryGetProjection(const Defn: string; out Proj: TProjection): Boolean;
begin
	Result := (Defn <> '') and KnownProjections.TryGetValue(Defn,Proj);
	if not Result then
	begin
		Result := MakeNewProjectionFromDefn(Defn,Proj) = 0;
		if Result then
			KnownProjections.AddOrSetValue(Proj.GetCreateDefinition,Proj);
	end;
end;

end.
