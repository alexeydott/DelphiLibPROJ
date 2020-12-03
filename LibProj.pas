unit LibProj;

interface

uses
	Classes, SysUtils, SyncObjs, Generics.Collections,

	LibProjProjections;

type
	TProjectionsManager = class;
  TPointsTransformer = class;

	IProjection = interface(IUnknown)
	['{EE6FB2E9-9327-46E3-A0FF-2A55AF324498}']
		procedure CreateHandle(); stdcall;
		procedure DestroyHandle(); stdcall;
    procedure HandleNeeded; stdcall;
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
		function GetWKTDefinition(const PrettyPrint: Boolean): string; stdcall;
		function GetHandle: Pointer; stdcall;
		procedure SetHandle(Value: Pointer); stdcall;
		function GetIsGeocraphic(): Boolean; stdcall;
		function GetIsGeocentric(): Boolean; stdcall;
		function HandleValid(): Boolean; stdcall;
		function TransformPoint(Dst: IProjection; var X,Y: Double; AngularUnits: Boolean = False): Boolean; stdcall;
		function TransformPoints(Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		property Handle: Pointer read GetHandle write SetHandle;
	end;

	TProjection = class(TInterfacedObject,IProjection)
	private
		FSourceDefn: string;
		FGeoProjection: IProjection;
		FHandle: Pointer;
		FOwner: TProjectionsManager;
		FOwnGeoProjection: Boolean;
		procedure SetDefinition(const Value: string);
		function GetWKTDefinition(const PrettyPrint: Boolean): string; stdcall;
    constructor Create; overload;
	protected
		procedure CreateHandle(); stdcall;
		procedure DestroyHandle(); stdcall;
    procedure HandleNeeded; stdcall;
		function GetHandle: Pointer; stdcall;
		procedure SetHandle(Value: Pointer); stdcall;
		function GetOwner: TPersistent;
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

	TPointsTransformer = class(TInterfacedObject,IPointsTransformer)
	private
		FOwner: TProjectionsManager;
	protected
		function GetOwner: TPersistent;
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
		function GetPointsTransformer: IPointsTransformer;
		function GetProjectionByDefinition(const Defn: string): IProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): IProjection; stdcall;
		function GetGeographic(Src: IProjection): IProjection;
		function TryGetProjection(const Defn: string; out Proj: IProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: IProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: IProjection): Integer; stdcall;

		property ProjectionByDefinition[const Defn: string]: IProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: IProjection read GetProjectionByCode;
	end;

	TProjectionsManager = class(TComponent, IProjectionsManager)
	private
		FContainer: TDictionary<string,IProjection>;
		FPointsTransformer: IPointsTransformer;
		FGuard: TCriticalSection;
		[volatile] FChangesCount: Integer;
		procedure Changes; stdcall;
		procedure Changed; stdcall;
	protected
		function GetContainer: TObject; stdcall;
		function GetPointsTransformer: IPointsTransformer;
		function GetProjectionByDefinition(const Defn: string): IProjection; stdcall;
		function GetProjectionByCode(const EpsgCode: Integer): IProjection; stdcall;
		function TryGetProjection(const Defn: string; out Proj: IProjection): Boolean; stdcall;
		function MakeNewProjectionFromDefn(const Defn: string; out proj: IProjection): Integer; stdcall;
		function MakeNewProjectionFromEpsg(const Code: Integer; out proj: IProjection): Integer; stdcall;
		function GetGeographic(Src: IProjection): IProjection;
		function KnownProjections: TDictionary<string,IProjection>;
    { IProjectionsManager }
		property PointsTransformer: IPointsTransformer read GetPointsTransformer;
	public
		destructor Destroy(); override;

    { IPointsTransformer }
		function TransformPointsXY(const Src,Dst: IProjection; var X,Y: Double; Count: Integer; AngularUnits: Boolean): Boolean; stdcall;
		function TransformPointXY(const Src,Dst: IProjection; var X,Y: Double; AngularUnits: Boolean): Boolean; stdcall;

		property ProjectionByDefinition[const Defn: string]: IProjection read GetProjectionByDefinition;
		property ProjectionByEpsgCode[const Code: Integer]: IProjection read GetProjectionByCode;
	end;

implementation

uses
  LibProjApi;

{ TProjection }

constructor TProjection.Create(const ADefn: string);
begin
	CreateOwned(nil,ADefn);
end;

constructor TProjection.Create;
begin
  inherited Create;
end;

procedure TProjection.CreateHandle();
begin
	DestroyHandle();
//  try
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
//  except
//    on E: Exception do
//
//  end;
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager);
begin
	CreateOwned(AOwner,'');
end;

constructor TProjection.CreateOwned(AOwner: TProjectionsManager; const ADefn: string);
begin
	Create();
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

function TProjection.GetWKTDefinition(const PrettyPrint: Boolean): string;
begin
	Result := LibProjDefnToWKTProjection(Definition,PrettyPrint);
end;

procedure TProjection.HandleNeeded;
begin
  if not HandleValid then
    CreateHandle;
end;

function TProjection.HandleValid: Boolean;
begin
  Result := Assigned(FHandle)
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
	Result := Self.HandleValid and Assigned(Dst) and Dst.HandleValid;
  if Result then
  begin
    if not Assigned(FOwner) then
      Transformer := TPointsTransformer.Create()
    else
      Transformer := FOwner.PointsTransformer;

		Result := Transformer.TransformPointsXY(Self,Dst,X,Y,Count,AngularUnits);
  end;


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
	Changes;
  try
    FPointsTransformer := nil;
    FreeAndNil(FContainer);
  finally
    Changed;
  end;
	FreeAndNil(FGuard);
	inherited;
end;

function TProjectionsManager.GetContainer: TObject;
begin
	if FContainer = nil then
		FContainer := TObjectDictionary<string, IProjection>.Create([]);

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

function TProjectionsManager.GetPointsTransformer: IPointsTransformer;
begin
	if FPointsTransformer = nil then
		FPointsTransformer := TPointsTransformer.CreateOwned(Self);

	Result := FPointsTransformer;
end;

function TProjectionsManager.GetProjectionByCode(const EpsgCode: Integer): IProjection;
begin
	Result := GetProjectionByDefinition(LibProjDefnFromEpsgCode(EpsgCode));
end;

function TProjectionsManager.GetProjectionByDefinition(const Defn: string): IProjection;
begin
	TryGetProjection(Defn,Result);
end;

function TProjectionsManager.KnownProjections: TDictionary<string, IProjection>;
begin
	Result := TDictionary<string, IProjection>(GetContainer);
end;

function TProjectionsManager.MakeNewProjectionFromDefn(const Defn: string; out proj: IProjection): Integer;
begin
	Result := -1;
	if Defn <> '' then
	begin
		Changes;
    try
      proj := TProjection.CreateOwned(Self,Defn);
      proj.HandleNeeded;

      if proj.HandleValid then
        Result := 0
      else
      begin
        Result := PJ_get_errno();
        if Result = 0 then // unknown error or exception false
          Result := -1;
        proj := nil;
      end;
    finally
      Changed;
    end;
	end;
end;

function TProjectionsManager.MakeNewProjectionFromEpsg(const Code: Integer; out proj: IProjection): Integer;
begin
	Result := -1;
	if TryGetProjection(LibProjDefnFromEpsgCode(Code),proj) then
		Result := 0;
end;

function TProjectionsManager.TransformPointsXY(const Src, Dst: IProjection;
  var X, Y: Double; Count: Integer; AngularUnits: Boolean): Boolean;
begin
  Result := PointsTransformer.TransformPointsXY(Src, Dst,x,y,count,AngularUnits);
end;

function TProjectionsManager.TransformPointXY(const Src, Dst: IProjection;
  var X, Y: Double; AngularUnits: Boolean): Boolean;
begin
  Result := PointsTransformer.TransformPointXY(Src, Dst,x,y,AngularUnits);
end;

function TProjectionsManager.TryGetProjection(const Defn: string;
	out Proj: IProjection): Boolean;
begin
	Proj := nil;
  Changes;
  try
	  Result := (Defn <> '') and KnownProjections.TryGetValue(Defn,Proj);
    if not Result then
    begin
      Result := MakeNewProjectionFromDefn(Defn,Proj) = 0;

      if Result then
        KnownProjections.AddOrSetValue(Proj.GetCreateDefinition,Proj);
    end;
  finally
    Changed;
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
