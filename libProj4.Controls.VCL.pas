unit libProj4.Controls.VCL;

interface

{$IFDEF MSWINDOWS}
{.$WEAKPACKAGEUNIT}
{$ENDIF}
{$I 'libProj4.config.inc'}
uses

  Winapi.Windows, Winapi.Messages,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  VCL.Graphics, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Buttons, VCL.Grids, VCL.ValEdit, VCL.Menus,

  libProj4.Types, libProj4.Intf, libProj4.Classes;

// START resource string wizard section
resourcestring
  rsPROJ4_CommonParams_Caption = {$ifndef __proj_ru_explain}'Common'{$else}'Основные'{$endif};
  rsPROJ4_EllipsoidsParams_Caption = {$ifndef __proj_ru_explain}'Ellipsoid'{$else}'Эллипсоид'{$endif};
  rsPROJ4_DatumsParams_Caption = {$ifndef __proj_ru_explain}'Datum'{$else}'Датум'{$endif};
  rsPROJ4_OtherParaps_Caption = {$ifndef __proj_ru_explain}'Other'{$else}'Прочие'{$endif};
  rsPROJ4_Add_Caption = {$ifndef __proj_ru_explain}'Add'{$else}'Добавить'{$endif};
  rsPROJ4_AddParameter_Caption = {$ifndef __proj_ru_explain}'Add parameter'{$else}'Добавить параметр'{$endif};
  rsPROJ4_Delete_Caption = {$ifndef __proj_ru_explain}'Delete'{$else}'Удалить'{$endif};
  rsPROJ4_DeleteParameter_Caption = {$ifndef __proj_ru_explain}'Delete parameter'{$else}'Удалить параметр'{$endif};
  rsPROJ4_Parameter_Caption = {$ifndef __proj_ru_explain}'Parameter'{$else}'Параметр'{$endif};
  rsPROJ4_Value_Caption = {$ifndef __proj_ru_explain}'Value'{$else}'Значение'{$endif};
  rsPROJ4_CoordDefn_Caption = {$ifndef __proj_ru_explain}'CRS definition'{$else}'Описание системы координат'{$endif};
  rsPROJ4Format_Caption = {$ifndef __proj_ru_explain}'format PROJ4'{$else}'(формат PROJ4): '{$endif};
  rsPROJ4_InputValue_Caption = {$ifndef __proj_ru_explain}'Input value'{$else}'Ввод значения'{$endif};
  rsPROJ4_OK_Caption = {$ifndef __proj_ru_explain}'ОК'{$else}'OK'{$endif};
  rsPROJ4_Cancel_Caption = {$ifndef __proj_ru_explain}'Cancel'{$else}'Отмена'{$endif};
  rsPROJ4_EmptyProjection_Caption = {$ifndef __proj_ru_explain}'Not Selected'{$else}'Не выбрана'{$endif};
  rsPROJ4_CustomProjection_Caption = {$ifndef __proj_ru_explain}'User defined ...'{$else}'Собственная ...'{$endif};
// END resource string wizard section

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TPROJ4CRSParametersEditor = class(TPanel)
  private const
    cMIAddTag = 1000;
    cMIDeleteTag = 2000;
    cMIParamsTag = 3000;

  type
    TPJParamProp = (ppReadOnly);
    TPJParamProps = set of TPJParamProp;
    TProjectionHint = class(TCustomHint);

  var
    FAllowedParams: TArray<TPJParamRec>;
    FButtonsPnl: TPanel;
    FEditor: TValueListEditor;
    FEditorPnl: TPanel;
    FKnownParams: TArray<TPJParamRec>; // all known params
    FDefinitionParams: TArray<TPJParamRec>;
    FDefinition: string;
    FLastCell: TPoint;
    FMIAdd: TMenuItem;
    FMIDelete: TMenuItem;
    FParamsMenuItemsGroups: array [0 .. PJ_PARAM_GROUP_MAX - 1] of TMenuItem;
    FParamsMenuItems: TArray<TMenuItem>;
    FPopupMenu: TPopupMenu;
    FOmmittedParams: TStringDynArray;
    FOnChange: TNotifyEvent;
    function DisplayNameToParam(const ADisplayName: string): string;
    procedure DoOnEditorGetPickList(Sender: TObject; const KeyName: string; Values: TStrings);
    procedure DoOnEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoOnEditorPopupMenuClick(Sender: TObject);
    // procedure DoOnEditorPopupMenuClose(Sender: TObject);
    procedure DoOnEditorPopupMenuPopup(Sender: TObject);
    function ParamOmitted(const AParamName: string): Boolean;
    function GetCRSDefinition: string;
    function GetParamValue(const AName, ADefault: string; IsDisplayName: Boolean): string;
    function GetParamProps(AParamName: string): TPJParamProps;
    function KnownParamIndexFromMenuItemTag(ATag: Integer): Integer;
    function MouseToEditorCell(const P: TPoint): TPoint;
    function ParamAllowed(const ParamName: string): Boolean;
    function ParamDisplayName(const AParamName: string): string;
    function ParamIgnored(const AParamName: string): Boolean;
    function ParamNameExist(const AParamName: string): Boolean;
    procedure SetCRSDefinition(const Value: string);
    procedure SetParamProps(const ADisplayName: string; AProps: TPJParamProps);
    procedure HandleEditorStringsChange(Sender: TObject);
  protected
    function ParamDescription(const AParamName: string): string; virtual;
    procedure CreateControls; virtual;
    procedure ParseDefinition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy(); override;
    function CellHeight: Integer;
    function ParameterCount: Integer;
  published
    property CRSDefinition: string read GetCRSDefinition write SetCRSDefinition;
    property OmittedParams: TStringDynArray read FOmmittedParams write FOmmittedParams;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // from tpanel
    property ShowCaption default False;
    property Visible;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TPROJ4CRSSelector = class(TComboBox)
  private
    FManager: TProjectionsManager;
    FEditor: TPROJ4CRSParametersEditor;
    FHintWnd: THintWindow;
    FKnownCRS: TArray<TPair<string, string>>;
    FStoredItems: TStrings;
    FAllowFiltration: Boolean;
    FShowCRSHint: Boolean;
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
    function GetCRSByIndex(AIndex: Integer): TPair<string, string>;
    function GetSelectedCRS: string;
    function GetSelectedCRSDefn: string;
    procedure StoredItemsChange(Sender: TObject);
    procedure SetAllowFiltration(const Value: Boolean);
    procedure SetSelectedCRSDefn(const Value: string);
  protected
    procedure DropDown; override;
    procedure Change; override;
    procedure CloseUp; override;
    procedure FilterItems; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property AllowFiltration: Boolean read FAllowFiltration write SetAllowFiltration default False;
    property ShowCRSHint: Boolean read FShowCRSHint write FShowCRSHint default False;
    property SelectedCRS: string read GetSelectedCRS;
    property SelectedCRSDefn: string read GetSelectedCRSDefn write SetSelectedCRSDefn;
  published
    property Editor: TPROJ4CRSParametersEditor read FEditor write FEditor;
    property Manager: TProjectionsManager read FManager write FManager;
  end;

function Proj4DefininionQuery(const ADefinition: string): string;
function Proj4ParametersEditDlg(var ADefinition: string): Boolean;
procedure Register;

implementation

uses
  VCL.Forms, System.Math, System.StrUtils, VCL.Dialogs, VCL.Consts,
  libProj4.Api, libProj4.Projections,libProj4.Projections.LCRS
  ;


constructor TPROJ4CRSParametersEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
  Ctl3D := False;
//  AutoSize := True;
  Width := 380;
  Height := 130;
end;

destructor TPROJ4CRSParametersEditor.Destroy;
begin
  inherited;
end;

function TPROJ4CRSParametersEditor.CellHeight: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to FEditor.RowCount - 1 do
    Result := Result + FEditor.RowHeights[I];

  Result := Result div FEditor.RowCount;
end;

procedure TPROJ4CRSParametersEditor.CreateControls;

  procedure UpdateFontSize(VLE: TValueListEditor; Value: integer);
  var
    factor: single;
  begin
    if Value = 0 then
      Value := Application.DefaultFont.Size;
    VLE.Font.Size := Value;
    Value := Abs(VLE.Font.Height);
//      factor := (18{DefaultRowHeight in VCL} * VLE.ScaleFactor)/Value // ??? adt ??? if creating in runtime ScaleFactor = 1
    factor := (18{DefaultRowHeight in VCL} * GetCurrentPPIScreen(Self)/96)/Value;
    Value := Round(Value*factor);
    VLE.DefaultRowHeight := Value;
  end;
begin
  if FPopupMenu = nil then
  begin
    FPopupMenu := TPopupMenu.Create(Self);
    FPopupMenu.AutoHotkeys := maManual;

    FParamsMenuItemsGroups[PJ_PARAM_GROUP_GENERAL] := TMenuItem.Create(FPopupMenu);
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_GENERAL].Tag := cMIParamsTag;
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_GENERAL].Caption := rsPROJ4_CommonParams_Caption;

    FParamsMenuItemsGroups[PJ_PARAM_GROUP_ELLIPSOID] := TMenuItem.Create(FPopupMenu);
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_ELLIPSOID].Tag := cMIParamsTag;
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_ELLIPSOID].Caption := rsPROJ4_EllipsoidsParams_Caption;

    FParamsMenuItemsGroups[PJ_PARAM_GROUP_DATUM] := TMenuItem.Create(FPopupMenu);
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_DATUM].Tag := cMIParamsTag;
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_DATUM].Caption := rsPROJ4_DatumsParams_Caption;

    FParamsMenuItemsGroups[PJ_PARAM_GROUP_OTHER] := TMenuItem.Create(FPopupMenu);
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_OTHER].Tag := cMIParamsTag;
    FParamsMenuItemsGroups[PJ_PARAM_GROUP_OTHER].Caption := rsPROJ4_OtherParaps_Caption;

    FParamsMenuItemsGroups[PJ_PARAM_GROUP_ELLIPSOID].Add(FParamsMenuItemsGroups[PJ_PARAM_GROUP_DATUM]);

    // FPopupMenu.OnClose := DoOnEditorPopupMenuClose;
    FPopupMenu.OnPopup := DoOnEditorPopupMenuPopup;

    FMIAdd := TMenuItem.Create(FPopupMenu);
    FMIAdd.Caption := rsPROJ4_Add_Caption;
    FMIAdd.Hint := rsPROJ4_AddParameter_Caption;
    FMIAdd.Tag := cMIAddTag;
    FMIAdd.OnClick := DoOnEditorPopupMenuClick;

    FMIDelete := TMenuItem.Create(FPopupMenu);
    FMIDelete.Caption := rsPROJ4_Delete_Caption;
    FMIDelete.Hint := rsPROJ4_DeleteParameter_Caption;
    FMIDelete.Tag := cMIDeleteTag;
    FMIDelete.OnClick := DoOnEditorPopupMenuClick;

    FPopupMenu.Items.Add([FMIAdd, VCL.Menus.NewLine, FMIDelete]);
  end;

  if FButtonsPnl = nil then
  begin
    FButtonsPnl := TPanel.Create(Self);
    FButtonsPnl.Align := alBottom;
    FButtonsPnl.BevelOuter := bvNone;
    FButtonsPnl.Name := 'FButtonsPnl';
    FButtonsPnl.AutoSize := True;
    FButtonsPnl.Visible := False;
  end;

  if FEditorPnl = nil then
  begin
    FEditorPnl := TPanel.Create(Self);
    FEditorPnl.Parent := Self;
    FEditorPnl.BevelOuter := bvNone;
    FEditorPnl.Name := 'FEditorPnl';
    FEditorPnl.Caption := '';
    FEditorPnl.Align := alClient;
  end;

  if FEditor = nil then
  begin
    FEditor := TValueListEditor.Create(Self);
    FEditor.PopupMenu := FPopupMenu;
    FEditor.Parent := FEditorPnl;
    FEditor.Align := alClient;
    FEditor.FixedColor := clActiveCaption;
    FEditor.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking, goFixedHotTrack];
    FEditor.Hint := '';
    FEditor.ShowHint := True;
    FEditor.OnGetPickList := DoOnEditorGetPickList;
    FEditor.OnMouseMove := DoOnEditorMouseMove;
    FEditor.TitleCaptions.Text := rsPROJ4_Parameter_Caption + sLineBreak + rsPROJ4_Value_Caption;
    FEditor.OnStringsChange := HandleEditorStringsChange;

    UpdateFontSize(FEditor,0);
  end;
end;

function TPROJ4CRSParametersEditor.DisplayNameToParam(const ADisplayName: string): string;
var
  P: TPJParamRec;
begin

  for P in FKnownParams do
  begin
    if SameText(ADisplayName, P.DisplayName) then
      Exit(P.Name)
  end;

  Result := ADisplayName;
end;

procedure TPROJ4CRSParametersEditor.DoOnEditorGetPickList(Sender: TObject; const KeyName: string; Values: TStrings);
begin

  case System.StrUtils.IndexText(DisplayNameToParam(KeyName), ['ellps', 'datum', 'axis', 'proj', 'units']) of
    0:
      Values.CommaText := 'WGS84,krass,sphere,bessel,MERIT,SGS85,GRS80,IAU76,airy,APL4.9,' + 'NWL9D,mod_airy,andrae,aust_SA,GRS67,bess_nam,clrk66,clrk80,CPMt,delmbr,engelis,' + 'evrst30,evrst48,evrst56,evrst69,evrstSS,fschr60,fschr60m,fschr68,helmert,hough,intl,' +
        'kaula,lerch,mprts,new_intl,plessis,SEasia,Walbeck,WGS60,WGS66,WGS72';

    1:
      Values.CommaText := 'WGS84,GGRS87,NAD38,NAD27,potsdam,carthage,hermannskogel,ire65,nzgd49,OSGB336';

    // e - Easting
    // w - Westing
    // n - Northing
    // s - Southing
    // u - Up
    // d - Down
    2:
      Values.CommaText := 'enu,neu,wnu';
    3:
      Values.CommaText := 'aea,aeqd,airy,aitoff,alsk,apian,august,bacon,bipc,boggs,bonne,cass,cc,ccon,cea,' + 'chamb,collg,comill,crast,denoy,eck1,eck2,eck3,eck4,eck5,eck6,eqc,eqdc,eqearth,euler,etmerc,fahey,fouc,' +
        'fouc_s,gall,geos,gins8,gn_sinu,gnom,goode,gs48,gs50,hammer,hatano,igh,imw_p,isea,kav5,kav7,krovak,labrd,' + 'laea,lagrng,larr,lask,lcc,lcca,leac,lee_os,longlat,latlong,loxim,lsat,mbt_s,mbt_fps,mbtfpp,mbtfpq,mbtfps,' +
        'merc,mil_os,mill,misrsom,moll,murd1,murd2,murd3,natearth,natearth2,nell,nell_h,nicol,nsper,nzmg,ob_tran,' + 'ocea,oea,omerc,ortel,ortho,pconic,patterson,poly,putp1,putp2,putp3,putp3p,putp4p,putp5,putp5p,putp6,' +
        'putp6p,qua_aut,qsc,robin,rouss,rpoly,sinu,somerc,stere,sterea,gstmerc,tcc,tcea,tissot,tmerc,tpeqd,' + 'tpers,ups,urm5,urmfps,utm,vandg,vandg2,vandg3,vandg4,vitk1,wag1,wag2,wag3,wag4,wag5,wag6,wag7,' + 'webmerc,weren,wink1,wink2,wintri';
    4:
      Values.CommaText := 'm,mm,dm,cm,km,ft,us-ft,ind-ft,kmi,mi,us-mi,link,yd,us-yd,ind-yd,in,us-in,fath,ch,us-ch,ind-ch';
  end;
end;

procedure TPROJ4CRSParametersEditor.DoOnEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  c: TPoint;
  maxChars: Integer;
  eHint: string;
begin
  FEditor.MouseToCell(X, Y, c.X, c.Y);
  if (c.X = FLastCell.X) and (c.Y = FLastCell.Y) then
    Exit;

  FLastCell := c;
  if (FLastCell.X = 0) and (FLastCell.Y > -1) then
  begin
    maxChars := Min(FEditor.CellRect(FLastCell.X, FLastCell.Y).Width div FEditor.Canvas.TextWidth('x'), 25);
    eHint := WrapText(LibProjParamExplain(DisplayNameToParam(FEditor.Keys[FLastCell.Y])), maxChars);
    if eHint <> FEditor.Hint then
    begin
      Application.CancelHint;
      FEditor.Hint := eHint;
    end;
  end
  else
    FEditor.Hint := '';
end;

procedure TPROJ4CRSParametersEditor.DoOnEditorPopupMenuClick(Sender: TObject);
const
  cMIParamsTagMax = cMIParamsTag + 100; // pjParamsListMax;
var
  mi: TMenuItem absolute Sender;
  I: Integer;
  c: TPoint;
begin
  case mi.Tag of
    cMIParamsTag .. cMIParamsTagMax:
      begin
        I := KnownParamIndexFromMenuItemTag(mi.Tag);
        if I < 0 then
          Exit;

        if not ParamAllowed(FKnownParams[I].Name) then
          Exit;

        FEditor.Strings.AddPair(FKnownParams[I].DisplayName, FKnownParams[I].Value);
        SetParamProps(FKnownParams[I].DisplayName, GetParamProps(FKnownParams[I].DisplayName));
      end;

    cMIDeleteTag:
      begin
        c := MouseToEditorCell(FPopupMenu.PopupPoint);
        if (c.Y > 0) and (c.Y <= ParameterCount) then
          FEditor.DeleteRow(c.Y);
      end;
  end;
end;

// procedure TPROJ4CRSParametersEditor.DoOnEditorPopupMenuClose(Sender: TObject);
// begin
/// /
// end;

procedure TPROJ4CRSParametersEditor.DoOnEditorPopupMenuPopup(Sender: TObject);
var
  I, ii, allItemCount: Integer;
  itemsGroupsVisibleCount: array [0 .. PJ_PARAM_GROUP_MAX - 1] of Integer;
begin
  for I := 0 to PJ_PARAM_GROUP_MAX - 1 do
    itemsGroupsVisibleCount[I] := 0;

  if FMIAdd.Count = 0 then
  begin
    allItemCount := Length(FKnownParams);
    ii := 0;
    for I := 0 to allItemCount - 1 do
    begin
      if I = ii then
        SetLength(FParamsMenuItems, ii + allItemCount);

      if ParamIgnored(FKnownParams[I].Name) then
        Continue;

      FParamsMenuItems[ii] := TMenuItem.Create(FPopupMenu);
      FParamsMenuItems[ii].Name := 'N' + I.ToString + FKnownParams[I].Name;
      FParamsMenuItems[ii].Caption := FKnownParams[I].DisplayName;
      FParamsMenuItems[ii].Tag := cMIParamsTag + I;
      FParamsMenuItems[ii].OnClick := DoOnEditorPopupMenuClick;
      FParamsMenuItems[ii].Visible := ParamAllowed(FKnownParams[I].Name);
      FParamsMenuItemsGroups[FKnownParams[I].Group].Add(FParamsMenuItems[ii]);

      if FParamsMenuItems[ii].Visible then
        Inc(itemsGroupsVisibleCount[FKnownParams[I].Group]);

      Inc(ii);
    end;

    SetLength(FParamsMenuItems, ii);

    FMIAdd.Add([FParamsMenuItemsGroups[PJ_PARAM_GROUP_GENERAL], FParamsMenuItemsGroups[PJ_PARAM_GROUP_ELLIPSOID], FParamsMenuItemsGroups[PJ_PARAM_GROUP_OTHER]]);

  end
  else
    for I := 0 to Length(FParamsMenuItems) - 1 do
    begin
      ii := KnownParamIndexFromMenuItemTag(FParamsMenuItems[I].Tag);
      FParamsMenuItems[I].Visible := (ii > -1) and ParamAllowed(FKnownParams[I].Name);
      if FParamsMenuItems[I].Visible then
        Inc(itemsGroupsVisibleCount[FKnownParams[I].Group]);
    end;

  for I := 0 to PJ_PARAM_GROUP_MAX - 1 do
    FParamsMenuItemsGroups[I].Visible := itemsGroupsVisibleCount[I] > 0;

  FMIDelete.Visible := MouseToEditorCell(FPopupMenu.PopupPoint).Y > 0;
end;

function TPROJ4CRSParametersEditor.GetCRSDefinition: string;
var
  i: Integer;

  function MakeParamPair(const AParam, AValue: string): string;
  begin
    Result := AParam;
    if AValue <> '' then
      Result := Result+'='+AValue;
  end;

begin
  if Length(FDefinitionParams) > 0 then
  begin
    Result := '';

    for i := 0 to High(FDefinitionParams) do
    begin
      Result := Result + '+' +MakeParamPair(
        FDefinitionParams[i].Name,
        GetParamValue(FDefinitionParams[i].DisplayName,FDefinitionParams[i].Value,True)
      ) + ' ';
    end;
    Result := Result.Trim;
    Exit;
  end;

  Result := FDefinition;
end;

function TPROJ4CRSParametersEditor.GetParamProps(AParamName: string): TPJParamProps;
begin
  Result := [];
  case System.StrUtils.IndexText(AParamName, ['proj', 'ellps', 'datum', 'axis', 'units']) of
    0 .. 4:
      Include(Result, ppReadOnly);
  end;
end;

function TPROJ4CRSParametersEditor.GetParamValue(const AName, ADefault: string; IsDisplayName: Boolean): string;
var
  i: Integer;
begin
  if IsDisplayName then
    Result := AName
  else
    Result := ParamDisplayName(AName);

  i := FEditor.Strings.IndexOfName(Result);
  if i < 0 then
    Result := ADefault
  else
    Result := FEditor.Strings.ValueFromIndex[i];
end;

procedure TPROJ4CRSParametersEditor.HandleEditorStringsChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TPROJ4CRSParametersEditor.KnownParamIndexFromMenuItemTag(ATag: Integer): Integer;
begin
  Result := ATag - cMIParamsTag;
  if Result > Length(FKnownParams) then
    Result := -1;
end;

procedure TPROJ4CRSParametersEditor.Loaded;
begin
  inherited;
  FOmmittedParams := TStringDynArray.Create('proj');
  LibProjKnownParams(FKnownParams);
  CreateControls;
end;

function TPROJ4CRSParametersEditor.MouseToEditorCell(const P: TPoint): TPoint;
begin
  Result := Self.ScreenToClient(P);
  FEditor.MouseToCell(Result.X, Result.Y, Result.X, Result.Y);
end;

function TPROJ4CRSParametersEditor.ParamAllowed(const ParamName: string): Boolean;
var
  P: TPJParamRec;
begin
  Result := Length(FAllowedParams) = 0;
  if not Result then
  begin
    for P in FAllowedParams do
    begin
      if SameText(P.Name, ParamName) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  Result := Result and not ParamNameExist(ParamName);
end;

function TPROJ4CRSParametersEditor.ParamDescription(const AParamName: string): string;
begin
  Result := LibProjParamExplain(DisplayNameToParam(AParamName));
end;

function TPROJ4CRSParametersEditor.ParamDisplayName(const AParamName: string): string;
var
  P: TPJParamRec;
begin

  for P in FKnownParams do
  begin
    if SameText(AParamName, P.Name) then
      Exit(P.DisplayName)
  end;

  Result := AParamName;
end;

function TPROJ4CRSParametersEditor.ParameterCount: Integer;
begin
  Result := FEditor.Strings.Count;
end;

function TPROJ4CRSParametersEditor.ParamIgnored(const AParamName: string): Boolean;
begin
  Result := System.StrUtils.IndexStr(AParamName, ['R_A', 'R_V', 'R_a', 'R_g', 'R_h', 'R_lat_a', 'R_lat_g']) > -1;
end;

function TPROJ4CRSParametersEditor.ParamOmitted(const AParamName: string): Boolean;
var
  P: string;
begin
  for P in OmittedParams do
  begin
    Result := SameText(P, AParamName);
    if Result then
      Exit;
  end;
  Result := False;
end;

function TPROJ4CRSParametersEditor.ParamNameExist(const AParamName: string): Boolean;
begin
  Result := FEditor.Strings.IndexOfName(ParamDisplayName(AParamName)) > -1;
end;

procedure TPROJ4CRSParametersEditor.ParseDefinition;
var
  param: TPJParamRec;
  pjID: string;
begin

  FAllowedParams := nil;
  CreateControls;
  FEditor.Strings.Clear;
  FAllowedParams := nil;

  if (FDefinition.IsEmpty) then
    Exit;

  LibProjParseParamsString(FDefinition, FDefinitionParams);

  pjID := '';
  for param in FDefinitionParams do
  begin
    if (pjID = '') and (param.Name = 'proj') then
      pjID := param.Value;

    if ParamOmitted(param.Name) or ParamNameExist(param.Name) then
      Continue;

    FEditor.Strings.AddPair(param.DisplayName, param.Value);
    SetParamProps(param.DisplayName, GetParamProps(param.DisplayName));
  end;

  LibProjProjectionAllowedParams(pjID, FAllowedParams);
end;

procedure TPROJ4CRSParametersEditor.SetCRSDefinition(const Value: string);
begin
  if FDefinition = Value then
    Exit;

  FDefinition := Value.Trim;
  ParseDefinition;
end;

procedure TPROJ4CRSParametersEditor.SetParamProps(const ADisplayName: string; AProps: TPJParamProps);
begin
  if ppReadOnly in AProps then
  begin
    FEditor.ItemProps[ADisplayName].ReadOnly := True;
  end;
end;

function Proj4DefininionQuery(const ADefinition: string): string;
var
  promts, Values: TStringDynArray;
begin
  promts := TStringDynArray.Create(rsPROJ4_CoordDefn_Caption + sLineBreak + rsPROJ4Format_Caption);
  Values := TStringDynArray.Create(ADefinition);
  if not InputQuery(rsPROJ4_InputValue_Caption, promts, Values,
    function(const Values: array of string): Boolean
    begin
      Result := Values[0].Contains('proj=');
    end) then
    Result := ADefinition
  else
    Result := Values[0];
end;

function Proj4ParametersEditDlg(var ADefinition: string): Boolean;
var
  Dlg: TForm;
  s: string;
  r: TRect;
  pnlBottom: TPanel;
  btnOk,btnCancel: TButton;
  crsEditor: TPROJ4CRSParametersEditor;
begin
  Dlg := TForm.Create(nil);
  try
    Dlg.PopupMode := pmAuto;
//    Dlg.Font.Size := Application.DefaultFont.Size;
    Dlg.Ctl3D := False;
    Dlg.Constraints.MinWidth := Dlg.Width -5;
    if ADefinition <> '' then
    begin
      s := WrapText(ADefinition,sLineBreak,[' '], Dlg.Width);
      Dlg.Canvas.TextRect(r,s,[tfCalcRect]);
      Dlg.Height := Max(r.Height,480);
    end
    else
      Dlg.Height := 240;

    pnlBottom := TPanel.Create(Dlg);
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Parent := Dlg;
    pnlBottom.Top := Dlg.Height - pnlBottom.Height;
    pnlBottom.Width := Dlg.Width;
    pnlBottom.Align := alBottom;

    btnOk := TButton.Create(Dlg);
    btnOk.Caption := rsPROJ4_OK_Caption;
    btnOk.Default := True;
    btnOk.ModalResult := mrOk;
    btnOk.Parent := pnlBottom;

    btnCancel := TButton.Create(Dlg);
    btnCancel.Caption := rsPROJ4_Cancel_Caption;
    btnCancel.ModalResult := mrCancel;
    btnCancel.Parent := pnlBottom;

    btnOk.Top := btnOk.Top + 7;
    btnCancel.Top := btnOk.Top;

    btnCancel.Left := pnlBottom.Width - (btnCancel.Width +25);
    btnOk.Left := btnCancel.Left - (5+btnCancel.Width);

    crsEditor := TPROJ4CRSParametersEditor.Create(Dlg);
    if ADefinition = '' then
      crsEditor.OmittedParams := []
    else
      crsEditor.OmittedParams := ['proj'];

    crsEditor.Left := 0;
    crsEditor.Top := 0;
    crsEditor.Width := Dlg.Width;
    crsEditor.Height := Dlg.Height - pnlBottom.Height;
    crsEditor.Align := alClient;

    crsEditor.CRSDefinition := ADefinition;

    Result := Dlg.ShowModal = mrOk;
    if Result then
      ADefinition := crsEditor.CRSDefinition;
  finally
    Dlg.Free;
  end;

end;

procedure ChangeResourceString(AResString: PResStringRec; ANewValue: PChar);
{$IFDEF MSWINDOWS}
var
  protect: DWORD;
begin
  if (AResString = nil) then
    Exit;

  if not VirtualProtect(AResString, SizeOf(AResString^), PAGE_EXECUTE_READWRITE, @protect) then
    Exit;

  AResString^.Identifier := NativeUINT(ANewValue);

  VirtualProtect(AResString, SizeOf(AResString^), protect, @protect);

end;
{$ELSE}

begin
  // todo
end;
{$ENDIF}
{ TPROJ4CRSSelector }

procedure TPROJ4CRSSelector.Change;
var
  i: Integer;
begin
  i := ItemIndex;
  if i = 0 then
  begin
    FKnownCRS[i].Value := Proj4DefininionQuery(FKnownCRS[i].Value);
    if FKnownCRS[i].Value = '' then
      ItemIndex := -1;
  end;

  inherited Change;

  if (i > -1) and Assigned(Editor) then
    Editor.CRSDefinition := SelectedCRSDefn;
end;

procedure TPROJ4CRSSelector.CloseUp;
begin
  if FHintWnd.HandleAllocated then
    FHintWnd.ReleaseHandle;
  inherited;
end;

procedure TPROJ4CRSSelector.CNCommand(var AMessage: TWMCommand);
begin
  inherited;

  if AllowFiltration and (AMessage.NotifyCode = CBN_EDITUPDATE) then
    FilterItems;
end;

constructor TPROJ4CRSSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintWnd := THintWindow.Create(Self);
  FHintWnd.Color := clWindow;
  FHintWnd.Parent := Self;
  ShowCRSHint := True;
  AlignWithMargins := True;
  FAllowFiltration := False;
  FStoredItems := TStringList.Create();
  TStringList(FStoredItems).OnChange := StoredItemsChange;
  Style := csDropDownList;
  Text := '';
  TextHint := rsPROJ4_EmptyProjection_Caption;
  ShowHint := True;
  FKnownCRS := nil;
end;

destructor TPROJ4CRSSelector.Destroy;
begin
  TStringList(FStoredItems).OnChange := nil;
  FreeAndNil(FStoredItems);
  inherited;
end;

procedure TPROJ4CRSSelector.DropDown;
var
  p: TPair<string, string>;
  lst: TStrings;
begin
  if Length(FKnownCRS) = 0 then
  begin
    GetKnownLCRCDefitions(FKnownCRS);
    Insert([TPair<string, string>.Create(rsPROJ4_CustomProjection_Caption, '')], FKnownCRS, 0);
  end;

  if AllowFiltration then
    lst := FStoredItems
  else
    lst := Items;

  if lst.Count = 0 then
  begin
    lst.BeginUpdate;
    try
      for P in FKnownCRS do
        lst.Add(p.key);

    finally
      lst.EndUpdate;
    end;
  end;

  inherited DropDown;
end;

procedure TPROJ4CRSSelector.FilterItems;
var
  I: Integer;
  selection: TSelection;
  candidate: string;
begin
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@selection.StartPos), LParam(@selection.EndPos));
  try

    Items.BeginUpdate;
    try
      if Text = '' then
        Items.Assign(FStoredItems)
      else
      begin
        Items.Clear;

        for I := 0 to FStoredItems.Count - 1 do
        begin
          candidate := FStoredItems[I];
          if candidate.ToLower.Contains(string(Text).ToLower) then
            Items.Add(candidate);
        end;
      end;

    finally
      Items.EndUpdate;
    end;

  finally
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(selection.StartPos, selection.EndPos))
  end;
end;

function TPROJ4CRSSelector.GetCRSByIndex(AIndex: Integer): TPair<string, string>;
var
  p: TPair<string, string>;
  crsName: string;
begin
  Result := TPair<string, string>.Create('', '');

  if not HandleAllocated or (ItemIndex < 0) then
    Exit;

  crsName := Items[ItemIndex];
  for p in FKnownCRS do
  begin
    if SameText(p.Key,crsName) then
      Exit(p)
  end;
end;

function TPROJ4CRSSelector.GetSelectedCRS: string;
begin
  Result := GetCRSByIndex(ItemIndex).key;
end;

function TPROJ4CRSSelector.GetSelectedCRSDefn: string;
begin
  Result := GetCRSByIndex(ItemIndex).Value;
end;

procedure TPROJ4CRSSelector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);

  if (Operation = opRemove) then
  begin
    if AComponent = Editor then
      Editor := nil;

    if AComponent = Manager then
      Manager := nil;
  end;
end;

procedure TPROJ4CRSSelector.SetAllowFiltration(const Value: Boolean);
begin
  if FAllowFiltration = Value then
    Exit;

  FAllowFiltration := Value;
  AutoComplete := not AllowFiltration;
  if AllowFiltration and (Style <> csDropDown) then
    Style := csDropDown;
end;

procedure TPROJ4CRSSelector.SetSelectedCRSDefn(const Value: string);
var
  p: TPair<string, string>;
begin
  if Value = '' then
    ItemIndex := -1
  else
  for p in FKnownCRS do
  begin
    if SameText(p.Value, Value) then
    begin
      ItemIndex := Items.IndexOf(p.Key);
      Break;
    end;
  end;
end;

procedure TPROJ4CRSSelector.StoredItemsChange(Sender: TObject);
begin
  if AllowFiltration and (FStoredItems.Count > 0) then
    FilterItems;
end;

procedure TPROJ4CRSSelector.WndProc(var Message: TMessage);
var
  itmText: string;
  i, n, scrollHeight, itmHeight: Integer;
  itmTextSize: TSize;
  pnt: TPoint;
  listRect, itmRect, txtRct: TRect;
begin
  inherited;

  case Message.Msg of
    WM_CTLCOLORLISTBOX:
    begin
      if GetWindowRect(Message.LParam, listRect) then
      begin
        FHintWnd.ReleaseHandle;
        if Winapi.Windows.GetCursorPos(pnt) then
        begin
          scrollHeight := GetSystemMetrics(SM_CYHSCROLL);
          pnt.Offset(scrollHeight,0);
          Winapi.Windows.ScreenToClient(Message.LParam, pnt);
          i := SendMessage(Message.LParam,LB_ITEMFROMPOINT,0,PointToLParam(pnt));
          // https://docs.microsoft.com/en-us/windows/win32/controls/lb-itemfrompoint
          // The HIWORD is zero if the specified point is in the client area of the list box,
          // or one if it is outside the client area.
          // index of the nearest item in the LOWORD.
          if HiWord(i) <> 0 then
            itmText := ''
          else
            itmText := Items[Loword(i)];

          if itmText <> '' then
          begin
            itmTextSize := Self.Canvas.TextExtent(itmText);
            if SendMessage(Message.LParam, LB_GETITEMRECT, i, Winapi.Windows.LParam(@itmRect)) <> LB_ERR then
            begin
              if ShowCRSHint or (itmTextSize.cx > (listRect.Width - scrollHeight)) then
              begin
                txtRct := itmRect;
                itmHeight := Max((scrollHeight + listRect.Height) div DropDownCount, itmTextSize.cy);
                n := itmRect.Width div itmHeight;
                itmText := WrapText(itmText.Replace(' ','#') + sLineBreak + SelectedCRSDefn, sLineBreak, [' '], n).TrimRight;
                Self.Canvas.TextRect(txtRct, itmText, [tfCalcRect, tfModifyString]);
                listRect.Offset(listRect.Width, itmRect.Top);
                listRect.Width := txtRct.Width + Self.Canvas.TextWidth('M');
                i := 1;
                n := 1;
                while i > 0 do
                begin
                  i := itmText.IndexOf(sLineBreak, i + 1);
                  if i < 1 then Break;
                  Inc(n);
                end;
                listRect.Height := itmHeight * n;
                listRect.Offset(0,-listRect.Height div 2);
                FHintWnd.ActivateHint(listRect, itmText.Replace('#',' '));
              end;

            end;

          end;

        end;



      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Proj4',[TPROJ4CRSParametersEditor, TPROJ4CRSSelector]);
end;


initialization
// ChangeResourceString(PResStringRec(@Consts.SMsgDlgYes), PResStringRec(@SMsgDlgYes).Identifier);
// ChangeResourceString(PResStringRec(@Consts.SMsgDlgNo), PResStringRec(@SMsgDlgNo).Identifier);
{$ifdef __proj_ru_explain}
ChangeResourceString(PResStringRec(@VCL.Consts.SMsgDlgOK), @rsPROJ4_OK_Caption);
ChangeResourceString(PResStringRec(@VCL.Consts.SMsgDlgCancel),@rsPROJ4_Cancel_Caption);
{$endif}
finalization

end.
