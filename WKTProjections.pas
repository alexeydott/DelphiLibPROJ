/// <summary>
///   Reader for WKT format
/// </summary>
/// <seealso href="http://www.geoapi.org/2.0/javadoc/org/opengis/referencing/doc-files/WKT.html">
///   see also
/// </seealso>
unit WKTProjections;

interface

uses
	Classes, Types, SysUtils, Contnrs;

type

	TWktNode = class;
	TWKTCRSDefinition = class;
  EWKTError = class(EArgumentException);


	TWktNodeList = class(TObjectList)
	private
		function DoGetItem(Index: Integer): TWktNode;
		procedure DoSetItem(Index: Integer; AObject: TWktNode);
	protected
	public
		/// <param name="AKey">
		///   node keyword
		/// </param>
		/// <returns>
		///   true if success and dest &lt;&gt; nil
		/// </returns>
		/// <remarks>
		///   use "\" symbol for specify full path to node eg PROJCS\GEOCS
		/// </remarks>
		function Find(const AKey: string; out Dest: TWktNode): Boolean; virtual;
		/// <summary>
		///   find node by attribute name
		/// </summary>
		/// <param name="AKey">
		///   node keyword
		/// </param>
		/// <param name="AAtributeName">
		///   attribute name
		/// </param>
		/// <param name="Dest">
		///   result
		/// </param>
		/// <returns>
		///   true if success
		/// </returns>
		function FindByAttributeName(const AKey, AAtributeName: string; out Dest: TWktNode): Boolean; virtual;
	 	property Items[Index: Integer]: TWktNode read DoGetItem write DoSetItem; default;
	end;

	TWktNode = class(TWktNodeList)
	private
		FAttributes: TStrings;
		FKeyword: string;
		FParentNode: TWktNode;
		FTreeDepth: Integer;
		function GetAttributesContainer: TStrings;
		function GetAttributesCount: Integer;
		function GetIndent(const IndentString: string): string;
	protected
		function AttibutesAsString: string;
		function GetAttribute(Index: Integer): string;
		procedure SetAttribute(Index: Integer; const Value: string);
		function GetKeyword(): string;
		procedure SetKeyword(const Value: string);
		procedure InitDefaults(); virtual;
		function Validate: Boolean; virtual;
		procedure NodeNotFound(const NodeName: string);
	public
		constructor Create(AParent: TWktNode);
		destructor Destroy(); override;
		procedure AddAttribute(const AValue: string);
		function AddChild(const Key,Name,Value: string): TWktNode;
		/// <summary>
		///   load from Source
		/// </summary>
		procedure ParseStream(Source: TStream); virtual;
		/// <summary>
		///   save to string
		/// </summary>
		/// <param name="PrettyPrint">
		///   pretty print
		/// </param>
		function SaveToString(const PrettyPrint: Boolean): string;
		// 0 = attribute name
		property AttributeValue[Index: Integer]: string read GetAttribute write SetAttribute;
		/// <summary>
		///   list of attributes (0 is name)
		/// </summary>
		property Attributes: TStrings read GetAttributesContainer;
		property AttributesCount: Integer read GetAttributesCount;
		/// <summary>
		///   node key word
		/// </summary>
		property Keyword: string read GetKeyWord write SetKeyword;
		/// <summary>
		///   parent node
		/// </summary>
		property Parent: TWktNode read FParentNode;
		/// <summary>
		///   node tree depth
		/// </summary>
		property TreeDepth: Integer read FTreeDepth;
	end;

	TWKTCRSDefinition = class(TWktNode)
	private
		function GetEmpty: Boolean;
	public
		procedure LoadFromFile(const AFilename: string);
		procedure LoadFromStream(Stream: TStream);
		procedure LoadFromString(const AString: string);
		procedure SaveToFile(const AFilename: string);
		function SaveToStream(AStream: TStream): Integer;
		property Empty: Boolean read GetEmpty;
		property Valid: Boolean read Validate;
	end;

	TWKTCRSDefinitionClass = class of TWKTCRSDefinition;

	TWKTGeoCRS = class(TWKTCRSDefinition)
	private
		FDatum: TWktNode;
		FSpheroid: TWktNode;
		FPrimem: TWktNode;
		FUnit: TWktNode;
		FToWGS: TWktNode;
		function GetName: string;
		procedure SetName(const Value: string);
		function GetDatumName: string;
		function GetPrimeMeridianName: string;
		function GetPrimeMeridianNameLon: string;
		function GetSpheroidAF: string;
		function GetSpheroidName: string;
		function GetToWGS: string;
		procedure SetSpheroidAF(const Value: string);
		procedure SetDatumName(const Value: string);
		procedure SetPrimeMeridianLon(const Value: string);
		procedure SetPrimeMeridianName(const Value: string);
		procedure SetSpheroidName(const Value: string);
		procedure SetToWGS(const Value: string);
		function GetAngularUnits: string;
		function GetUnitsConversionFactor: string;
		procedure SetAngularUnits(const Value: string);
		procedure SetUnitsConversionFactor(const Value: string);
		function GetQualifiedName: string;
		function GetDatumNode: TWktNode;
		function GetPrimemNode: TWktNode;
		function GetSpheroidNode: TWktNode;
		function GetUnitNode: TWktNode;
	protected
		procedure InitDefaults(); override;
		function Validate: Boolean; override;
	public

		function TryGetDatumNode(): TWktNode;
		function TrySpheroidNode(): TWktNode;
		property Name: string read GetName write SetName;
		property QualifiedName: string read GetQualifiedName;

		property DatumNode: TWktNode read GetDatumNode;
		property SpheroidNode: TWktNode read GetSpheroidNode;
		property PrimemNode: TWktNode read GetPrimemNode;
		property UnitNode: TWktNode read GetUnitNode;

		property DatumName: string read GetDatumName write SetDatumName;
		property SpheroidName: string read GetSpheroidName write SetSpheroidName;
		property SpheroidAF: string read GetSpheroidAF write SetSpheroidAF;
		property PrimeMeridianName: string read GetPrimeMeridianName write SetPrimeMeridianName;
		property PrimeMeridianLon: string read GetPrimeMeridianNameLon write SetPrimeMeridianLon;
		property UnitsName: string read GetAngularUnits write SetAngularUnits;
		property UnitsConversionFactor: string read GetUnitsConversionFactor write SetUnitsConversionFactor;
		property ToWGS: string read GetToWGS write SetToWGS;
	end;

	TWKTProjectedCRS = class(TWKTCRSDefinition)
	private
		FGeoCS: TWKTGeoCRS;
		FProjection: TWktNode;
		FUnits: TWktNode;
		function GetName: string;
		procedure SetName(const Value: string);
		function GetQualifiedName: string;
		function GetProjectionParameter(const AName: string): string;
		procedure SetProjectionParameter(const AName,AValue : string);
		function GetProjectionName: string;
		function GetUnitsName: string;
		function GetUnitsToMeters: string;
    procedure SetProjectionName(const Value: string);
		procedure SetUnitsName(const Value: string);
		procedure SetUnitsToMeters(const Value: string);
	protected
		function GetGeoCS: TWKTGeoCRS; virtual;
		function GetParameterNode(const AName,AValue: string): TWktNode; virtual;
		function GetProjectionNode(): TWktNode; virtual;
		function GetUnitsNode(): TWktNode; virtual;

		procedure InitDefaults(); override;
		function Validate: Boolean; override;
	public
		property QualifiedName: string read GetQualifiedName;
		property Name: string read GetName write SetName;
		property ProjectionNode : TWktNode read GetProjectionNode;
		property ParameterNode[const AName,AValue: string]: TWktNode read GetParameterNode;
		property UnitsNode: TWktNode read GetUnitsNode;

		property ProjectionName: string read GetProjectionName write SetProjectionName;
		property GeoCS: TWKTGeoCRS read GetGeoCS;
		property UnitsName: string read GetUnitsName write SetUnitsName;
		property UnitsToMeters: string read GetUnitsToMeters write SetUnitsToMeters;
		property Parameter[const Name: string]: string read GetProjectionParameter write SetProjectionParameter;
	end;




implementation

const
	WKT_REVERSE_SOLIDUS = '\';
	WKT_BRACKET_OPEN = '['; // Open bracket (node start) // Could also be ( in wkt geometry
	WKT_BRACKET_CLOSE = ']'; // Close bracket (node end) // Could also be ) in wkt geometry
	WKT_QUOTE_CHAR = '"';
	WKT_NEWLINE = sLineBreak;
	WKT_VALUE_SEPARATOR = ','; // Separator for values

	// legacy keywords
	WKT_KWD_GEOCCS = 'GEOCCS';
	WKT_KWD_GEOGCS = 'GEOGCS';
	WKT_KWD_PROJCS = 'PROJCS';
	WKT_KWD_VERTCS = 'VERT_CS';
	WKT_KWD_LOCALCS = 'LOCAL_CS';
	WKT_KWD_VDATUM = 'VERT_DATUM';
	WKT_KWD_LDATUM = 'LOCAL_DATUM';
	WKT_KWD_COMPDCS = 'COMPD_CS';
	WKT_KWD_ID = 'AUTHORITY';
	WKT_KWD_TOWGS84 = 'TOWGS84';
	WKT_KWD_EXTENSION = 'EXTENSION';
	//
	WKT_KWD_UNKNOWN = 'UNKNOWN';
	WKT_KWD_CITATION = 'CITATION';
	WKT_KWD_URI = 'URI';
	WKT_KWD_UNIT = 'UNIT';
	WKT_KWD_ANGUNIT = 'ANGLEUNIT';
	WKT_KWD_LENUNIT = 'LENGTHUNIT';
	WKT_KWD_SCALEUNIT = 'SCALEUNIT';
	WKT_KWD_TIMEUNIT = 'TIMEUNIT';
	WKT_KWD_PARAMUNIT = 'PARAMETRICUNIT';
	WKT_KWD_SCOPE = 'SCOPE';
	WKT_KWD_AREA_EXTENT = 'AREA';
	WKT_KWD_BBOX_EXTENT = 'BBOX';
	WKT_KWD_VERT_EXTENT = 'VERTICALEXTENT';
	WKT_KWD_TIME_EXTENT = 'TIMEEXTENT';
	WKT_KWD_REMARK = 'REMARK';
	WKT_KWD_PARAMETER = 'PARAMETER';
	WKT_KWD_PARAM_FILE = 'PARAMETERFILE';
	WKT_KWD_ELLIPSOID = 'ELLIPSOID';
	WKT_KWD_ANCHOR = 'ANCHOR';
	WKT_KWD_TIME_ORIGIN = 'TIMEORIGIN';
	WKT_KWD_GEOD_DATUM = 'DATUM';
	WKT_KWD_ENGR_DATUM = 'EDATUM';
	WKT_KWD_IMAGE_DATUM = 'IDATUM';
	WKT_KWD_PARAM_DATUM = 'PDATUM';
	WKT_KWD_TIME_DATUM = 'TDATUM';
	WKT_KWD_VERT_DATUM = 'VDATUM';
	WKT_KWD_PRIMEM = 'PRIMEM';
	WKT_KWD_ORDER = 'ORDER';
	WKT_KWD_MERIDIAN = 'MERIDIAN';
	WKT_KWD_BEARING = 'BEARING';
	WKT_KWD_AXIS = 'AXIS';
	WKT_KWD_CS = 'CS';
	WKT_KWD_CONVERSION = 'CONVERSION';
	WKT_KWD_DERIVING_CONV = 'DERIVINGCONVERSION';
	WKT_KWD_METHOD = 'METHOD';
	WKT_KWD_GEOD_CRS = 'GEODCRS';
	WKT_KWD_ENGR_CRS = 'ENGCRS';
	WKT_KWD_IMAGE_CRS = 'IMAGECRS';
	WKT_KWD_PARAM_CRS = 'PARAMETRICCRS';
	WKT_KWD_PROJ_CRS = 'PROJCRS';
	WKT_KWD_TIME_CRS = 'TIMECRS';
	WKT_KWD_VERT_CRS = 'VERTCRS';
	WKT_KWD_COMPOUND_CRS = 'COMPOUNDCRS';
	WKT_KWD_BASE_GEOD_CRS = 'BASEGEODCRS';
	WKT_KWD_BASE_ENGR_CRS = 'BASEENGCRS';
	WKT_KWD_BASE_PARAM_CRS = 'BASEPARAMETRICCRS';
	WKT_KWD_BASE_PROJ_CRS = 'BASEPROJCRS';
	WKT_KWD_BASE_TIME_CRS = 'BASETIMECRS';
	WKT_KWD_BASE_VERT_CRS = 'BASEVERTCRS';
	WKT_KWD_OP_ACCURACY = 'OPERATIONACCURACY';
	WKT_KWD_COORD_OP = 'COORDINATEOPERATION';
	WKT_KWD_BOUND_CRS = 'BOUNDCRS';
	WKT_KWD_ABRTRANS = 'ABRIDGEDTRANSFORMATION';
	// keywords for virtual objects
	WKT_KWD_EXTENT = 'extent';
	WKT_KWD_BASE_CRS = 'base-crs';
	WKT_KWD_CRS = 'crs';
	WKT_KWD_DATUM = 'datum';
	WKT_KWD_OBJECT = 'object';
	// keywords that are not real objects
	WKT_KWD_SOURCE_CRS = 'SOURCECRS';
	WKT_KWD_TARGET_CRS = 'TARGETCRS';
	WKT_KWD_INTERP_CRS = 'INTERPOLATIONCRS';
	// alternate keywords
	WKT_ALT_KWD_ID = 'ID';
	WKT_ALT_KWD_ENGR_DATUM = 'ENGINEERINGDATUM';
	WKT_ALT_KWD_GEOD_DATUM = 'GEODETICDATUM';
	WKT_ALT_KWD_IMAGE_DATUM = 'IMAGEDATUM';
	WKT_ALT_KWD_PARAM_DATUM = 'PARAMETRICDATUM';
	WKT_ALT_KWD_TIME_DATUM = 'TIMEDATUM';
	WKT_ALT_KWD_VERT_DATUM = 'VERTICALDATUM';
	WKT_ALT_KWD_ENGR_CRS = 'ENGINEERINGCRS';
	WKT_ALT_KWD_GEOD_CRS = 'GEODETICCRS';
	WKT_ALT_KWD_PARAM_CRS = 'PARAMETRICCRS';
	WKT_ALT_KWD_PROJ_CRS = 'PROJECTEDCCRS';
	WKT_ALT_KWD_VERT_CRS = 'VERTICALCRS';
	WKT_ALT_KWD_METHOD = 'PROJECTION';
	WKT_ALT_KWD_ELLIPSOID = 'SPHEROID';
	WKT_ALT_KWD_PRIMEM = 'PRIMEMERIDIAN';
	// crs keywords
	WKT_CRS_KWD_UNKNOWN = 'unknown';
	WKT_CRS_KWD_UNNNAMED = 'unnamed';
	WKT_CRS_KWD_GEOD =  'geodetic';
	WKT_CRS_KWD_PROJ =  'projected';
	WKT_CRS_KWD_VERT =  'vertical';
	WKT_CRS_KWD_ENGR =  'engineering';
	WKT_CRS_KWD_IMAGE =  'image';
	WKT_CRS_KWD_PARAM =  'parametric';
	WKT_CRS_KWD_TIME =  'time';
	WKT_CRS_KWD_BASE_GEOD =  'base-geodetic';
	WKT_CRS_KWD_BASE_PROJ =  'base-projected';
	WKT_CRS_KWD_BASE_VERT =  'base-vertical';
	WKT_CRS_KWD_BASE_ENGR =  'base-engineering';
	WKT_CRS_KWD_BASE_PARAM =  'base-parametric';
	WKT_CRS_KWD_BASE_TIME =  'base-time';
	WKT_CRS_KWD_COMPOUND =  'compound';
	WKT_KWD_CRS_KWD_DEGREE = 'degree';
  WKT_CRS_KWD_UNIT_METER = 'meter';

	WKT_KWD_EXTENSION_PROJ4 = 'PROJ4';
	WKT_KWD_DATUM_WGS84 = 'WGS_1984';
	WKT_KWD_ELLIPSOID_WGS84 = 'WGS 1984';
	WKT_KWD_ELLIPSOID_WGS84_AF = '6378137.0,298.257223563';



function FindChar(const C: Char; const Str: string; const StartIndex: Integer): Integer;
var
	I: Integer;
begin
	for I := StartIndex to Length(Str) do
		if Str[I] = C then
			Exit(I);

	Result := 0;
end;

function StripQuotes(const AValue: string): string;
begin
	if Length(AValue) > 1 then
	begin
		if (AValue[1] = WKT_QUOTE_CHAR) and (AValue[Length(AValue)] = WKT_QUOTE_CHAR) then
		begin
			Result := Copy(AValue, 2, Length(AValue) - 2);
			Exit;
		end;
	end;

	Result := AValue;
end;

function StripEscapedChars(const AValue: string): string;
begin
	Result := AValue;
	Result := StringReplace(Result,'\n','',[rfReplaceAll,rfIgnoreCase]);
	Result := StringReplace(Result,'\t','',[rfReplaceAll,rfIgnoreCase]);
	Result := StringReplace(Result,'\\','\',[rfReplaceAll]);
	Result := StringReplace(Result,'\"','"',[rfReplaceAll]);
end;

constructor TWktNode.Create(AParent: TWktNode);
begin
	inherited Create(True);

	FTreeDepth := 0;
	FParentNode := AParent;
	if FParentNode <> nil then
		FTreeDepth := FParentNode.TreeDepth + 1;

	InitDefaults();
end;

destructor TWktNode.Destroy;
begin
	FreeAndNil(FAttributes);

	inherited;
end;

procedure TWktNode.NodeNotFound(const NodeName: string);
resourcestring
	SRequiedNodeNotFound = 'requied node with keyword "%s" not found';
begin
	raise EWKTError.CreateResFmt(@SRequiedNodeNotFound,[NodeName]);
end;

{ TWktNode }

procedure TWktNode.AddAttribute(const AValue: string);
begin
	if (AValue <> '')then
		Attributes.Add(AValue);
end;

function TWktNode.AddChild(const Key,Name,Value: string): TWktNode;
begin
	Result := TWktNode.Create(Self);
	Result.Keyword := Key;
	Result.AddAttribute(Name);
	Result.AddAttribute(Value);
	Add(Result);
end;

function TWktNode.AttibutesAsString: string;
var
	I: Integer;
begin
	Result := '';
	for I := 0 to Attributes.Count - 1 do
	begin
		if Result <> '' then
			Result := Result + WKT_VALUE_SEPARATOR + Attributes[I]
		else
			Result := Result + Attributes[I]
	end;
end;

function TWktNode.GetAttribute(Index: Integer): string;
begin
	if (Index > -1) and (Index < Attributes.Count) then
		Result := StripQuotes(Attributes[Index])
	else
		Result := '';
end;

function TWktNode.GetAttributesContainer: TStrings;
begin
	if FAttributes = nil then
		FAttributes := TStringList.Create();

	Result := FAttributes;
end;

function TWktNode.GetAttributesCount: Integer;
begin
	if FAttributes = nil then
		Result := 0
	else
		Result := FAttributes.Count;
end;

function TWktNode.GetIndent(const IndentString: string): string;
var
	I: Integer;
begin
	Result := '';
	for I := 0 to TreeDepth - 1 do
		Result := Result + IndentString;
end;

function TWktNode.GetKeyWord: string;
begin
  Result := FKeyword;
end;

procedure TWktNode.InitDefaults;
begin
  //
end;

procedure TWktNode.ParseStream(Source: TStream);
var
	CurrentChar: AnsiChar;
	TempValue: RawByteString;
	BracketOpened, QuoteOpened: Boolean;
	LastCommaPos, CommaCount: Integer;
	Child: TWktNode;
begin
	if not Assigned(Source) or
			(Source.Size = 0) or
			(Source.Position = Source.Size) then
		Exit;

	BracketOpened := False;
	QuoteOpened := False;
	CommaCount := 0;
	LastCommaPos := Source.Position;
	TempValue := '';
	while Source.Read(CurrentChar, SizeOf(AnsiChar)) = SizeOf(AnsiChar) do
	begin
		case CurrentChar of
			WKT_BRACKET_OPEN:
				begin
					// node start
					if BracketOpened then
					begin
						AddAttribute(StripEscapedChars(string(TempValue)));
						TempValue := '';
						if CommaCount <> 0 then
						begin
              // This is a sub-node not a Attribute
							if AttributesCount <> 0 then
								Attributes.Delete(AttributesCount - 1);

							Source.Seek(LastCommaPos, soFromBeginning);
              // New child
							Child := TWktNode.Create(Self);
							Add(Child);
							Child.ParseStream(Source);
						end;
					end
					else
					begin
						// Name, Attribute
						BracketOpened := True;
						Keyword := StripEscapedChars(string(TempValue));
						TempValue := '';
					end;
				end;
			WKT_VALUE_SEPARATOR:
				begin
					LastCommaPos := Source.Position;
					AddAttribute(StripEscapedChars(string(TempValue)));
					TempValue := '';
					Inc(CommaCount);
				end;
			WKT_BRACKET_CLOSE:
				begin
					// End
					AddAttribute(StripEscapedChars(string(TempValue)));
					TempValue := '';
					Break;
				end;

			' ', #9, #10, #13:
				Continue;

			WKT_QUOTE_CHAR:
				begin
					QuoteOpened := not QuoteOpened;
					if QuoteOpened then
					begin
						TempValue := CurrentChar;
						while (Source.Read(CurrentChar, 1) = 1) do
						begin
							TempValue := TempValue + CurrentChar;
							if CurrentChar = WKT_QUOTE_CHAR then
							begin
								QuoteOpened := False;
								Break;
							end;
						end;
					end;
				end;
		else
			TempValue := TempValue + CurrentChar;
		end;
	end;
	Validate;
end;

function TWktNode.SaveToString(const PrettyPrint: Boolean): string;
var
	I: Integer;
begin
	if Keyword = '' then
		Exit('');

	Result := UpperCase(Keyword) + WKT_BRACKET_OPEN;

	if AttributesCount > 0 then
		Result := Result + AttibutesAsString;

	for I := 0 to Count - 1 do
		Result := Result + WKT_VALUE_SEPARATOR + Items[I].SaveToString(PrettyPrint);

	Result := Result + WKT_BRACKET_CLOSE;

	if PrettyPrint then
		Result := WKT_NEWLINE + GetIndent('  ') + Result;
end;

procedure TWktNode.SetAttribute(Index: Integer; const Value: string);
begin
	if (Index > -1) and (Index < Attributes.Count) and (Value <> '') then
		Attributes[Index] := Value
	else
		AddAttribute(Value);
end;

procedure TWktNode.SetKeyword(const Value: string);
begin
	if not SameText(Value,FKeyword) then
		FKeyword := Value;
end;

function TWktNode.Validate: Boolean;
begin
  Result := Count > 0;
end;

function TWktNodeList.DoGetItem(Index: Integer): TWktNode;
begin
	Result := TWktNode(GetItem(Index));
end;
//
procedure TWktNodeList.DoSetItem(Index: Integer; AObject: TWktNode);
begin
	SetItem(Index, AObject);
end;

function TWktNodeList.Find(const AKey: string; out Dest: TWktNode): Boolean;
var
	I, PathDelimPos: Integer;
	Keyword, Nested: string;
begin
	Dest := nil;
	PathDelimPos := FindChar(WKT_REVERSE_SOLIDUS, AKey, 1); // its full path n\n
	if PathDelimPos <> 0 then
	begin
		Keyword := Copy(AKey, 1, PathDelimPos - 1);
		Nested := Copy(AKey, PathDelimPos + 1, MaxInt);
	end
	else
	begin
		Keyword := AKey;
		Nested := '';
	end;

	for I := 0 to Count - 1 do
		if CompareText(Items[I].Keyword, Keyword) = 0 then
		begin
			Dest := Items[I];
			Break;
		end;

	Result := Assigned(Dest);

	if Result and (Nested <> '') then
		Result := Dest.Find(Nested, Dest);
end;

function TWktNodeList.FindByAttributeName(const AKey, AAtributeName: string; out Dest: TWktNode): Boolean;
var
	I: Integer;
begin
	for I := 0 to Count - 1 do
	begin
		Dest := Items[I];
		if SameText(Dest.Keyword, AKey) and (Dest.AttributesCount > 0) and
			 SameText(StripQuotes(Dest.Attributes[0]), AAtributeName) then
			Exit(True);
	end;
	Dest := nil;
	Result := False;
end;

{ TWKTProjection }

function TWKTCRSDefinition.GetEmpty: Boolean;
begin
	Result := (Count = 0);
end;

procedure TWKTCRSDefinition.LoadFromFile(const AFilename: string);
var
	S: TStream;
begin
	if not Empty then
		Clear;

	if FileExists(AFilename) then
	begin
		S := TFileStream.Create(AFilename, fmOpenRead);
		try
			LoadFromStream(S);
		finally
			FreeAndNil(S);
		end;
	end;
end;

procedure TWKTCRSDefinition.LoadFromStream(Stream: TStream);
begin
	if not Empty then
		Clear;

	ParseStream(Stream);
end;

procedure TWKTCRSDefinition.LoadFromString(const AString: string);
var
	S: TStream;
begin
	S := TStringStream.Create(AString);
	try
		LoadFromStream(S);
	finally
		FreeAndNil(S);
	end;
end;

procedure TWKTCRSDefinition.SaveToFile(const AFilename: string);
var
	Str: string;
	S: TStream;
begin
	Str := SaveToString(False);
	if Str <> '' then
	begin
		S := TFileStream.Create(AFilename, fmCreate);
		try
			S.Write(BytesOf(Str), ByteLength(Str));
		finally
			FreeAndNil(S);
		end;
	end;
end;

function TWKTCRSDefinition.SaveToStream(AStream: TStream): Integer;
var
	S: TStream;
begin
	Result := AStream.Position;
	S := TStringStream.Create(SaveToString(False));
	try
		AStream.CopyFrom(S, Self.Count);

		Result := AStream.Position - Result;
	finally
		FreeAndNil(S);
	end;
end;

{ TWKTProjectedCRS }

function TWKTProjectedCRS.GetGeoCS: TWKTGeoCRS;
begin
	if FGeoCS = nil then
	begin
		FGeoCS := TWKTGeoCRS.Create(Self);
		Add(FGeoCS);
	end;
	Result := FGeoCS;
end;

function TWKTProjectedCRS.GetName: string;
begin
  Result := AttributeValue[0];
end;

function TWKTProjectedCRS.GetProjectionName: string;
begin
	if FProjection = nil then
		FProjection := GetProjectionNode();

	Result := FProjection.AttributeValue[0];
end;

function TWKTProjectedCRS.GetProjectionNode(): TWktNode;
begin
	if FProjection = nil then
	begin
		if not Find(WKT_ALT_KWD_METHOD,FProjection) then
			FProjection := AddChild(WKT_ALT_KWD_METHOD,WKT_KWD_UNKNOWN,'');
	end;

	Result := FProjection;
end;

function TWKTProjectedCRS.GetProjectionParameter(const AName: string): string;
var
	ParamNode: TWktNode;
begin
	if FindByAttributeName(WKT_KWD_PARAMETER,AName,ParamNode) then
		Result := ParamNode.AttributeValue[1]
	else
		Result := ''
end;

function TWKTProjectedCRS.GetUnitsNode(): TWktNode;
begin
	if FUnits = nil then
	begin
		if not Find(WKT_KWD_UNIT,FUnits) then
			FUnits := AddChild(WKT_KWD_UNIT,WKT_CRS_KWD_UNKNOWN,'1')
	end;

	Result := FUnits;
end;

function TWKTProjectedCRS.GetQualifiedName: string;
begin
	Result := Name + WKT_REVERSE_SOLIDUS + FGeoCS.QualifiedName;
end;

function TWKTProjectedCRS.GetUnitsName: string;
begin
	Result := UnitsNode.AttributeValue[0];
end;

function TWKTProjectedCRS.GetUnitsToMeters: string;
begin
	Result := UnitsNode.AttributeValue[1];
end;

procedure TWKTProjectedCRS.InitDefaults;
begin
	inherited;
	Keyword := WKT_KWD_PROJCS;
	Name := WKT_CRS_KWD_UNNNAMED;
end;

procedure TWKTProjectedCRS.SetName(const Value: string);
begin
	AttributeValue[0] := Value;
end;

procedure TWKTProjectedCRS.SetProjectionName(const Value: string);
begin
	if Value.IsNullOrEmpty(Value) then
		ProjectionNode.AttributeValue[0] := WKT_CRS_KWD_UNKNOWN
	else
		ProjectionNode.AttributeValue[0] := Value;
end;

procedure TWKTProjectedCRS.SetProjectionParameter(const AName, AValue: string);
var
	ParamNode: TWktNode;
begin
	if AValue.IsNullOrEmpty(AValue) then
	begin
		if FindByAttributeName(WKT_KWD_PARAMETER,AName,ParamNode) then
			Remove(ParamNode);
	end
	else
		ParameterNode[AName, AValue].AttributeValue[1] := AValue;
end;

procedure TWKTProjectedCRS.SetUnitsName(const Value: string);
begin
	if Value.IsNullOrEmpty(Value) then
		UnitsNode.AttributeValue[0] := WKT_KWD_UNKNOWN
	else
		UnitsNode.AttributeValue[0] := Value;
end;

procedure TWKTProjectedCRS.SetUnitsToMeters(const Value: string);
begin
	if Value.IsNullOrEmpty(Value) then
		UnitsNode.AttributeValue[1] := '1'
	else
		UnitsNode.AttributeValue[1] := Value;
end;

function TWKTProjectedCRS.GetParameterNode(const AName, AValue: string): TWktNode;
begin
	if not FindByAttributeName(WKT_KWD_PARAMETER,AName,Result) then
	begin
		Result := AddChild(WKT_KWD_PARAMETER,AName,AValue)
	end
end;

function TWKTProjectedCRS.Validate: Boolean;
begin
  Result := Count > 0;
end;

{ TWKTGeoCRS }

function TWKTGeoCRS.GetAngularUnits: string;
begin
	Result := UnitNode.AttributeValue[0];
end;

function TWKTGeoCRS.GetUnitsConversionFactor: string;
begin
	Result := UnitNode.AttributeValue[1];
end;

function TWKTGeoCRS.GetDatumName: string;
begin
	Result := DatumNode.AttributeValue[0];
end;

function TWKTGeoCRS.GetDatumNode: TWktNode;
begin
	if FDatum = nil then
	begin
		if not Find(WKT_KWD_GEOD_DATUM,FDatum) then
			NodeNotFound(WKT_KWD_GEOD_DATUM);
	// FDatum := AddChild(WKT_KWD_GEOD_DATUM,'WGS_1984','');
	end;
	Result := FDatum;
end;

function TWKTGeoCRS.GetName: string;
begin
	Result := AttributeValue[0];
end;

function TWKTGeoCRS.GetPrimeMeridianName: string;
begin
	Result := PrimemNode.AttributeValue[0];
end;

function TWKTGeoCRS.GetPrimeMeridianNameLon: string;
begin
	Result := PrimemNode.AttributeValue[1];
end;

function TWKTGeoCRS.GetPrimemNode: TWktNode;
begin
	if FPrimem = nil then
	begin
		if not Find(WKT_KWD_PRIMEM,FPrimem) then
			FPrimem := AddChild(WKT_KWD_PRIMEM,'Greenwich','0.0'); // silenty set to default
	end;

	Result := FPrimem;
end;

function TWKTGeoCRS.GetQualifiedName: string;
begin
  Result := Name + '('+DatumName+')';
end;

function TWKTGeoCRS.GetSpheroidAF: string;
begin
	Result := SpheroidNode.AttributeValue[1];
end;

function TWKTGeoCRS.GetSpheroidName: string;
begin
	Result := SpheroidNode.AttributeValue[0];
end;

function TWKTGeoCRS.GetSpheroidNode: TWktNode;
begin
	if FSpheroid = nil then
	begin
		if not DatumNode.Find(WKT_ALT_KWD_ELLIPSOID,FSpheroid) then
			NodeNotFound(WKT_ALT_KWD_ELLIPSOID);
	end;

	Result := FSpheroid;
end;

function TWKTGeoCRS.GetToWGS: string;
begin
	if FToWGS = nil then
	begin
		if not Find(WKT_KWD_TOWGS84,FToWGS) then
			Exit('')
		else
			Result:= FToWGS.AttributeValue[0]
	end
	else
		Result:= FToWGS.AttributeValue[0]
end;

function TWKTGeoCRS.GetUnitNode: TWktNode;
begin
	if FUnit = nil then
	begin
		if not Find(WKT_KWD_UNIT,FUnit) then
		begin
			FUnit:= AddChild(WKT_KWD_UNIT,WKT_KWD_CRS_KWD_DEGREE,'0.0174532925199433');  // silently set to defalult
		end;
	end;
	Result := FUnit;
end;

procedure TWKTGeoCRS.InitDefaults;
begin
	inherited;

	Keyword := WKT_KWD_GEOCCS;
  Name := WKT_CRS_KWD_UNNNAMED;

	if not Empty then
	begin
		if not Find(WKT_KWD_GEOD_DATUM,FDatum) then
			FDatum := AddChild(WKT_KWD_GEOD_DATUM,WKT_KWD_DATUM_WGS84,'');

		if not FDatum.Find(WKT_ALT_KWD_ELLIPSOID,FSpheroid) then
			FSpheroid := FDatum.AddChild(WKT_ALT_KWD_ELLIPSOID,WKT_KWD_ELLIPSOID_WGS84,WKT_KWD_ELLIPSOID_WGS84_AF);

		FDatum.Find(WKT_KWD_TOWGS84,FToWGS);

		if not Find(WKT_KWD_PRIMEM,FPrimem) then
			FPrimem := AddChild(WKT_KWD_PRIMEM,'Greenwich','0.0');

		if not Find(WKT_KWD_UNIT,FUnit) then
			FUnit:= AddChild(WKT_KWD_UNIT,WKT_KWD_CRS_KWD_DEGREE,'0.0174532925199433');
	end
	else
	begin
		FDatum := AddChild(WKT_KWD_GEOD_DATUM,WKT_KWD_DATUM_WGS84,'');
		FSpheroid := FDatum.AddChild(WKT_ALT_KWD_ELLIPSOID,WKT_KWD_ELLIPSOID_WGS84,WKT_KWD_ELLIPSOID_WGS84_AF);
    FToWGS := nil;
		FPrimem := AddChild(WKT_KWD_PRIMEM,'Greenwich','0.0');
		FUnit := AddChild(WKT_KWD_UNIT,WKT_KWD_CRS_KWD_DEGREE,'0.0174532925199433');
  end;
end;

procedure TWKTGeoCRS.SetSpheroidAF(const Value: string);
begin
  TrySpheroidNode.AttributeValue[1] := Value;
end;

procedure TWKTGeoCRS.SetAngularUnits(const Value: string);
begin
	UnitNode.AttributeValue[0] := Value;
end;

procedure TWKTGeoCRS.SetUnitsConversionFactor(const Value: string);
begin
	UnitNode.AttributeValue[1] := Value;
end;

function TWKTGeoCRS.TryGetDatumNode: TWktNode;
begin
	if FDatum = nil then
	begin
		if not Find(WKT_KWD_GEOD_DATUM,FDatum) then
		begin
			FDatum := AddChild(WKT_KWD_GEOD_DATUM,WKT_KWD_DATUM_WGS84,'');
		end;
	end;
	Result := FDatum;
end;

function TWKTGeoCRS.TrySpheroidNode: TWktNode;
begin
	if FSpheroid = nil then
	begin
		if not TryGetDatumNode.Find(WKT_ALT_KWD_ELLIPSOID,FSpheroid) then
		begin
			TryGetDatumNode.AddChild(WKT_ALT_KWD_ELLIPSOID,
				WKT_KWD_ELLIPSOID_WGS84,WKT_KWD_ELLIPSOID_WGS84_AF);
		end;
	end;

	Result := FSpheroid;
end;

procedure TWKTGeoCRS.SetDatumName(const Value: string);
begin
	TryGetDatumNode.AttributeValue[0] := Value;
end;

procedure TWKTGeoCRS.SetName(const Value: string);
begin
	AttributeValue[0] := Value;
end;

procedure TWKTGeoCRS.SetPrimeMeridianLon(const Value: string);
begin
	PrimemNode.AttributeValue[1] := Value;
end;

procedure TWKTGeoCRS.SetPrimeMeridianName(const Value: string);
begin
	PrimemNode.AttributeValue[0] := Value;
end;

procedure TWKTGeoCRS.SetSpheroidName(const Value: string);
begin
  TrySpheroidNode.AttributeValue[0] := Value;
end;

procedure TWKTGeoCRS.SetToWGS(const Value: string);

begin
	if Value.IsNullOrEmpty(Value) then
	begin
		if FToWGS <> nil then
		begin
//			FToWGS.AttributeValue[0] := '0,0,0,0,0,0,0';
			Remove(FToWGS);
      FToWGS := nil;
		end;
	end
	else
	begin
		if FToWGS = nil then
		begin
			if not DatumNode.Find(WKT_KWD_TOWGS84,FToWGS) then
				FToWGS := DatumNode.AddChild(WKT_KWD_TOWGS84,Value,'');
		end
		else
			FToWGS.AttributeValue[0] := Value;
	end;
end;

function TWKTGeoCRS.Validate: Boolean;
begin
	Result := Assigned(FDatum) and Assigned(FSpheroid) and
		Assigned(FPrimem) and Assigned(FUnit);
end;

end.
