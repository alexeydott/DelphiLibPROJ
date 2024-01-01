unit libProj4.Classes;

interface
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsweakpackaging_xml.html
{$IFDEF MSWINDOWS}
  {.$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}
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
    [volatile] FLockCounter: Integer;
    [weak] FGeoProjection: IProjection;
    function GetHandle: TPJ;
    function Manager: IProjectionsManager;
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
    function SpheroidDefinition(var a,b,e2: Double): Boolean;
    function ToMetersFactor: Double;
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
    [volatile] FLockCounter: Integer;
    FContext: TPJCtx;
    FList: TThreadList<TPair<string, TProjection>>;
    FSearchPaths: string;
    procedure ListNotify(Sender: TObject; const Item: TPair<string,TProjection>; Action: TCollectionNotification);
    procedure Lock;
    procedure Unlock;
    procedure CreateContext;
    procedure DestroyContext;
    procedure SetSearchPaths(const Value: string);
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

var
  Wgs84Geodesic: libProj4.Types.TGeodesic;

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
  FLockCounter := 0;
  SetDefaults;
end;

procedure TProjection.CreateHandle;
begin
  Lock;
  try
    if FHandle <> nil then
      Exit;

    FHandle := CreatePJ(FProj4Defn,nil,@FProj4LastError);
  finally
    UnLock;
  end;
end;

procedure TProjection.DestroyHandle;
begin
  Lock;
  try
    FreePJ(FHandle);
  finally
    UnLock;
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
  if TInterlocked.Increment(FLockCounter) = 1 then
    FLock.Enter;
end;

function TProjection.Manager: IProjectionsManager;
begin
  Result := IProjectionsManager(Controller);
end;

function TProjection.MapinfoDefinition: string;
begin
  Result := ''; // todo implementMe!!!

  Lock;
  try
    if FHandle.IsValid then
    begin
      if IsGeographic then
        Result := 'Earth Projection 1, 104'
      else
        Result := 'NonEarth Units "m"';
    end;
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
  FProj4Defn := '';
  FHandle := nil;
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

function TProjection.SpheroidDefinition(var a,b,e2: Double): Boolean;
begin
  Lock;
  try
  	Result := GetSpheroidInfo(Handle ,a,b,e2);
  finally
    Unlock;
  end;
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
  if TInterlocked.Decrement(FLockCounter) = 0 then
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
var p: TGeodesicPolygon;
begin
  p := GeodesicPolygonCreate(Wgs84Geodesic,yLats, xLons);
  Result := GeodesicPolygonCompute(Wgs84Geodesic, p, Reversed, Signed, AArea, APerimeter) > 2;

  if not Result or AArea.IsInfinity or AArea.IsNan then AArea := 0;
  if not Result or APerimeter.IsInfinity or APerimeter.IsNan then APerimeter := 0;
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

function TProjectionsManager.GetGeodeticDistance(const Lat1, Lon1, Lat2, Lon2: Double): Double;
var azi12,azi21: Double;
begin
  GeodesicInverse(Wgs84Geodesic,Lat1, Lon1, Lat2, Lon2, Result,azi12,azi21);
end;

function TProjectionsManager.GetGeodeticPolygonArea(const yLats, xLons: TArray<Double>; var AArea, APerimeter: Double): Boolean;
begin
  GeodesicPolygonArea(Wgs84Geodesic, yLats, xLons, AArea, APerimeter);
  Result := (AArea > 0) and not AArea.IsNan and not AArea.IsInfinity;
  if not Result then
  begin
    AArea := 0;
    APerimeter := 0;
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
  FLock := TCriticalSection.Create;
  CreateContext;
  FList := TThreadList<TPair<string,TProjection>>.Create;
  FList.LockList.OnNotify := ListNotify;
  FList.UnlockList;
end;

procedure TProjectionsManager.ListNotify(Sender: TObject; const Item: TPair<string, TProjection>; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved:
    begin
      Item.Value.Free;
    end;
  end;
end;

procedure TProjectionsManager.Lock;
begin
  if TInterlocked.Increment(FLockCounter) = 1 then
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
  if TInterlocked.Decrement(FLockCounter) = 0 then
    FLock.Leave;
end;

initialization
  GeodesicInit(Wgs84Geodesic, 6378137,1/298.257223563);

  TProjectionsManager.FGLobalInstance := TProjectionsManager.Create(nil);

finalization
  FreeAndNil(TProjectionsManager.FGLobalInstance);

end.
