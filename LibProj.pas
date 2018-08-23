unit LibProj;

interface

uses
	Classes, SysUtils, SyncObjs, Generics.Collections,

	LibProjApi, LibProjProjections;

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
		procedure SetHandle(Value: Pointer); stdcall;
		function GetIsGeocraphic(): Boolean; stdcall;
		function GetIsGeocentric(): Boolean; stdcall;
		function HandleValid(): Boolean; stdcall;
		function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean; stdcall;
		function TransformPoints(Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		property Handle: Pointer read GetHandle write SetHandle;
	end;

	TProjection = class(TInterfacedPersistent,IProjection)
	private
		FSourceDefn: string;
		FGeoProjection: IProjection;
		FHandle: Pointer;
		FOwner: TProjectionsManager;
		FOwnGeoProjection: Boolean;
		procedure SetDefinition(const Value: string);
	protected
		procedure CreateHandle(); stdcall;
		procedure DestroyHandle(); stdcall;
		function GetHandle: Pointer; stdcall;
		procedure SetHandle(Value: Pointer); stdcall;
		function GetOwner: TPersistent; override;
		function GetSpheroidDefinition(out a,b,e2: Double): Boolean; stdcall;
		function GetCreateDefinition: string; stdcall;
		function GetDefinition: string; stdcall;
		function GetIsGeocraphic(): Boolean; stdcall;
		function GetIsGeoCentric(): Boolean; stdcall;
		property Handle: Pointer read GetHandle write SetHandle;
	public
		constructor Create(const ADefn: string); overload;
		constructor CreateOwned(AOwner: TProjectionsManager; const ADefn: string); overload;
		constructor CreateOwned(AOwner: TProjectionsManager); overload;
		destructor Destroy(); override;
		function HandleValid(): Boolean; stdcall;
		function GetGeographic: IProjection;
		function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean; stdcall;
		function TransformPoints(Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		property Definition: string read GetDefinition write SetDefinition;
		property IsGeographic: Boolean read GetIsGeocraphic;
		property IsGeocentric: Boolean read GetIsGeocentric;
	end;

	IPointsTransformer = interface(IUnknown)
		['{AD17E7B1-B047-4C25-B3A2-345FC4FD2375}']
		function TransformPointsXY(const Src,Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		function TransformPointXY(const Src,Dst: IProjection; var X,Y: Double; AngularUnits: Boolean): Boolean; stdcall;
	end;

	TPointsTransformer = class(TInterfacedPersistent,IPointsTransformer)
	private
		FOwner: TProjectionsManager;
	protected
		function GetOwner: TPersistent; override;
	public
		constructor CreateOwned(AOwner: TProjectionsManager);
    destructor Destroy(); override;
		function TransformPointsXY(const Src,Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		function TransformPointXY(const Src,Dst: IProjection; var X,Y: Double; AngularUnits: Boolean): Boolean; stdcall;
	end;

	IProjectionsManager = interface(IUnknown)
	['{85C50F1B-D9C8-4958-885E-3B4A9F2FB61A}']
		procedure Changes; stdcall;
		procedure Changed; stdcall;
		function GetContainer(): TObject; stdcall;
		function GetPointsTransformer: TPointsTransformer;
		function GetProjectionByDefinition(const Defn: string): TProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): TProjection; stdcall;
		function GetGeographic(Src: IProjection): IProjection;
		function TryGetProjection(const Defn: string; out Proj: TProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: TProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer; stdcall;

		property ProjectionByDefinition[const Defn: string]: TProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: TProjection read GetProjectionByCode;
	end;

	TProjectionsManager = class(TComponent,
		IProjectionsManager,IPointsTransformer)
	private
		FContainer: TDictionary<string,TProjection>;
		FPointsTransformer: TPointsTransformer;
		FGuard: TCriticalSection;
		FChangesCount: Integer;
		procedure Changes; stdcall;
		procedure Changed; stdcall;
	protected
		function GetContainer: TObject; stdcall;
		function GetPointsTransformer: TPointsTransformer;
		function GetProjectionByDefinition(const Defn: string): TProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): TProjection; stdcall;
		function TryGetProjection(const Defn: string; out Proj: TProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: TProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer; stdcall;
		function GetGeographic(Src: IProjection): IProjection;
		function KnownProjections: TDictionary<string,TProjection>;
	public
		destructor Destroy(); override;
		property ProjectionByDefinition[const Defn: string]: TProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: TProjection read GetProjectionByCode;

		property PointsTransformer: TPointsTransformer read GetPointsTransformer implements IPointsTransformer;
	end;

implementation

{ TProjection }

constructor TProjection.Create(const ADefn: string);
begin
	CreateOwned(nil,ADefn);
end;

procedure TProjection.CreateHandle();
begin
	DestroyHandle();
	if FOwner = nil then
		FHandle := PJ_init_plus(GetCreateDefinition)
	else
	begin
		FOwner.Changes;
		try
			FHandle := PJ_init_plus(GetCreateDefinition);
		finally
			FOwner.Changed;
		end;
	end;
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager);
begin
	CreateOwned(AOwner,'');
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager; const ADefn: string);
begin
	inherited Create();
	FGeoProjection := nil;
	FOwner := AOwner;
	FSourceDefn := ADefn;
end;

destructor TProjection.Destroy;
begin
	DestroyHandle();
	if FOwnGeoProjection then
		FGeoProjection := nil;

	inherited;
end;

procedure TProjection.DestroyHandle;
begin
	if FHandle <> nil then
	begin
		if FOwner <> nil then
		begin
			FOwner.Changes;
			try
				PJ_free(FHandle);
				FHandle := nil;
			finally
				FOwner.Changed;
			end;
		end;
	end;
end;

function TProjection.GetCreateDefinition: string;
begin
  Result := FSourceDefn;
end;

function TProjection.GetDefinition: string;
begin
	Result := PJ_get_definition(Handle);
end;

function TProjection.GetGeographic: IProjection;
var
	GeoHandle: Pointer;
begin
	if FGeoProjection = nil then
	begin
		FOwnGeoProjection := FOwner = nil;
		if FOwnGeoProjection then
		begin
			GeoHandle := PJ_latlong_from_proj(Self.Handle);
			if GeoHandle <> nil then
			begin
				FGeoProjection := TProjection.Create('');
				FGeoProjection.Handle := GeoHandle;

				TProjection( FGeoProjection ).FSourceDefn := FGeoProjection.GetDefinition;
			end;
		end
		else
			Result := FOwner.GetGeographic(Self);
	end;

	Result := FGeoProjection;
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

function TProjection.GetOwner: TPersistent;
begin
  Result := FOwner;
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
	if not SameText(Value,FSourceDefn) then
	begin
	  DestroyHandle;
		FSourceDefn := Value;
  end;
end;

procedure TProjection.SetHandle(Value: Pointer);
begin
	if (Value <> FHandle) then
	begin
		if FOwner = nil then
		begin
			DestroyHandle;
			FHandle := Value;
		end
		else
		begin
			FOwner.Changes;
			try
				DestroyHandle;
				FHandle := Value;
			finally
				FOwner.Changed;
			end;
		end;
	end;
end;

function TProjection.TransformPoint(Dst: IProjection; var X, Y: Double; AngularUnits: Boolean): Boolean;
begin
	Result := TransformPoints(Dst,X,Y,1,AngularUnits);
end;

function TProjection.TransformPoints(Dst: IProjection; var X, Y: Double; Count: Integer;
	AngularUnits: Boolean): Boolean;
var
	Transformer: IPointsTransformer;
begin
	Result := Self.HandleValid and Assigned(FOwner) and
		FOwner.GetInterface(IPointsTransformer,Transformer);

	if Result then
		Result := Transformer.TransformPointsXY(Self,Dst,X,Y,Count,AngularUnits);
end;

{ TProjectionsManager }

procedure TProjectionsManager.Changed;
begin
	Dec(FChangesCount);

	if FChangesCount = 0 then
	begin
		if FGuard <> nil then
			FGuard.Leave;
	end;
end;

procedure TProjectionsManager.Changes;
begin
	Inc(FChangesCount);

	if FChangesCount = 1 then
	begin
		if FGuard = nil then
			FGuard := TCriticalSection.Create;

		FGuard.Enter;
	end;
end;

destructor TProjectionsManager.Destroy;
begin
	FreeAndNil(FPointsTransformer);
	Changes;
	FreeAndNil(FContainer);
	Changed;
	FreeAndNil(FGuard);
	inherited;
end;

function TProjectionsManager.GetContainer: TObject;
begin
	if FContainer = nil then
		FContainer := TObjectDictionary<string, TProjection>.Create([doOwnsValues]);

	Result := FContainer;
end;

function TProjectionsManager.GetGeographic(Src: IProjection): IProjection;
var
	GeoDefn: string;
begin
	Result := nil;
	if Src <> nil then
	begin
		GeoDefn := PJ_get_definition( PJ_latlong_from_proj(Src.Handle));
		if GeoDefn <> '' then
			Result := ProjectionByDefinition[GeoDefn];
	end;
end;

function TProjectionsManager.GetPointsTransformer: TPointsTransformer;
begin
	if FPointsTransformer = nil then
		FPointsTransformer := TPointsTransformer.CreateOwned(Self);

	Result := FPointsTransformer;
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
		Changes;
		proj := TProjection.CreateOwned(Self,Defn);
		if proj.HandleValid then
			Result := 0
		else
		begin
			Result := PJ_get_errno();

			FreeAndNil(proj);
		end;
	  Changed;
	end;
end;

function TProjectionsManager.MakeNewProjectionFromEpsg(const Code: Integer; out proj: TProjection): Integer;
begin
	Result := -1;
	if TryGetProjection(LibProjDefnFromEpsgCode(Code),proj) then
		Result := 0;
end;

function TProjectionsManager.TryGetProjection(const Defn: string;
	out Proj: TProjection): Boolean;
begin
	Proj := nil;

	Result := (Defn <> '') and KnownProjections.TryGetValue(Defn,Proj);
	if not Result then
	begin
		Changes;
		try
			Result := MakeNewProjectionFromDefn(Defn,Proj) = 0;
			if Result then
				KnownProjections.AddOrSetValue(Proj.GetCreateDefinition,Proj);
		finally
			Changed;
    end;
	end;
end;

{ TPointsTransformer }

constructor TPointsTransformer.CreateOwned(AOwner: TProjectionsManager);
begin
	inherited Create;
	FOwner := AOwner;
end;

destructor TPointsTransformer.Destroy;
begin
	inherited Destroy();
	FOwner := nil;
end;

function TPointsTransformer.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPointsTransformer.TransformPointsXY(const Src,Dst: IProjection; var X,Y: Double; Count: Integer;
		AngularUnits: Boolean): Boolean;
begin
	Result := Assigned(Src) and Assigned(Dst) and (Src.HandleValid) and (Dst.HandleValid);
	if Result then
		Result := PJ_transform_points2D(Src.Handle,Dst.Handle,@X,@Y,Count,AngularUnits) = 0;
end;

function TPointsTransformer.TransformPointXY(const Src,Dst: IProjection;
	var X,Y: Double; AngularUnits: Boolean): Boolean;
begin
	Result := TransformPointsXY(Src,Dst,X,Y,1,AngularUnits);
end;

end.
