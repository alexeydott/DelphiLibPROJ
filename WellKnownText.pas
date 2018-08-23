unit WellKnownText;
// http://www.geoapi.org/2.0/javadoc/org/opengis/referencing/doc-files/WKT.html

interface

uses
	Classes, SysUtils, Contnrs;

type

	TWktNode = class;

	TWktNodeList = class(TObjectList)
	private
		function DoGetItem(Index: Integer): TWktNode;
		procedure DoSetItem(Index: Integer; AObject: TWktNode);
	public
		function Find(const AKey: string; out Dest: TWktNode): Boolean; virtual;
		function FindByAttributeName(const AKey, AAtributeName: string; out Dest: TWktNode): Boolean; virtual;

	 property Items[Index: Integer]: TWktNode read DoGetItem write DoSetItem; default;
	end;

	TWktNode = class(TWktNodeList)
	private
		FTreeDepth: Integer;
		FKeyword: string;
		FAttributes: TStrings;
		FParentNode: TWktNode;
		function GetAttributes: TStrings;
		function GetAttributesCount: Integer;
		function GetIndent(const IndentString: string): string;
	protected
		procedure AddAttribute(const AValue: string);
		function GetAttribute(Index: Integer): string;
		function AttibutesAsString: string;
	public
		constructor Create(AParent: TWktNode);
		destructor Destroy(); override;
		function SaveToString(const PrettyPrint: Boolean): string;
		procedure ParseStream(Stream: TStream); virtual;

		property TreeDepth: Integer read FTreeDepth;
		property Keyword: string read FKeyword;
		property Parent: TWktNode read FParentNode;
		property Attribute[Index: Integer]: string read GetAttribute;
		property Attributes: TStrings read GetAttributes;
		property AttributesCount: Integer read GetAttributesCount;
	end;

	TWKTDocument = class(TWktNodeList)
	private
		FRootNode: TWktNode;
		function GetRoot: TWktNode;
		function GetEmpty: Boolean;
	public
		procedure LoadFromFile(const AFilename: string);
		procedure LoadFromStream(Stream: TStream);
		procedure LoadFromString(const AString: string);

		function SaveToString(PrettyPrint: Boolean): string;
		function SaveToStream(AStream: TStream): Integer;
		procedure SaveToFile(const AFilename: string);

		property Empty: Boolean read GetEmpty;
		property Root: TWktNode read GetRoot;
	end;


implementation

const
	WKT_REVERSE_SOLIDUS = '\';
	WKT_BRACKET_OPEN = '['; // Open bracket (node start) // Could also be ( in wkt geometry
	WKT_BRACKET_CLOSE = ']'; // Close bracket (node end) // Could also be ) in wkt geometry
	WKT_QUOTE_CHAR = '"';
	WKT_NEWLINE = sLineBreak;
	WKT_VALUE_SEPARATOR = ','; // Separator for values

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

{ TWktNode }

procedure TWktNode.AddAttribute(const AValue: string);
begin
	if (AValue <> '') then
		Attributes.Add(AValue);
end;

constructor TWktNode.Create(AParent: TWktNode);
begin
	FTreeDepth := 0;
	FParentNode := AParent;
	if FParentNode <> nil then
		FTreeDepth := FParentNode.TreeDepth + 1;
end;

destructor TWktNode.Destroy;
begin
	FreeAndNil(FAttributes);

	inherited;
end;

function TWktNode.GetAttribute(Index: Integer): string;
begin
	if (Index > -1) and (Index < Attributes.Count) then
		Result := StripQuotes(Attributes[Index])
	else
		Result := '';
end;

function TWktNode.GetAttributes: TStrings;
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
	for I := 0 to TreeDepth -1 do
		Result := Result + IndentString;
end;

procedure TWktNode.ParseStream(Stream: TStream);
var
	CurrentChar: Char;
	TempValue: string;
	BracketOpened, QuoteOpened: Boolean;
	LastCommaPos, CommaCount: Integer;
	Child: TWktNode;
begin
	if not Assigned(Stream) or
		(Stream.Size = 0) or
		(Stream.Position = Stream.Size) then
		Exit;

	BracketOpened := False;
	QuoteOpened := False;
	CommaCount := 0;
	LastCommaPos := Stream.Position;
	TempValue := '';
	while Stream.Read(CurrentChar, 1) = 1 do
	begin
		case CurrentChar of
			WKT_BRACKET_OPEN:
				begin
				  // node start
					if BracketOpened then
					begin
						AddAttribute(TempValue);
						TempValue := '';
						if CommaCount <> 0 then
						begin
							// New child
							if FAttributes.Count <> 0 then
								FAttributes.Delete(FAttributes.Count - 1); // This is a sub-node not a Attribute

							Stream.Seek(LastCommaPos, soFromBeginning);
							Child := TWktNode.Create(Self);
							Add(Child);
							Child.ParseStream(Stream);
						end;
					end
					else
					begin
						// Name, Attribute
						BracketOpened := True;
						FKeyword := TempValue;
						TempValue := '';
					end;
				end;
			WKT_VALUE_SEPARATOR:
				begin
					LastCommaPos := Stream.Position;
					AddAttribute(TempValue);
					TempValue := '';
					Inc(CommaCount);
				end;
			WKT_BRACKET_CLOSE:
				begin
					// End
					AddAttribute(TempValue);
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
						while (Stream.Read(CurrentChar, 1) = 1) do
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
end;

function TWktNode.SaveToString(const PrettyPrint: Boolean): string;
var
	I: Integer;
begin
	Result := UpperCase(Keyword) + WKT_BRACKET_OPEN;

	if AttributesCount > 0 then
		Result := Result + AttibutesAsString;

	for I := 0 to Count - 1 do
		Result := Result + WKT_VALUE_SEPARATOR + Items[I].SaveToString(PrettyPrint);

	Result := Result + WKT_BRACKET_CLOSE;

	if PrettyPrint then
		Result := WKT_NEWLINE + GetIndent('  ') + Result;
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

{ TWktNodeList }

function TWktNodeList.DoGetItem(Index: Integer): TWktNode;
begin
	Result := TWktNode(GetItem(Index));
end;

procedure TWktNodeList.DoSetItem(Index: Integer; AObject: TWktNode);
begin
	SetItem(Index,AObject);
end;

function TWktNodeList.Find(const AKey: string; out Dest: TWktNode): Boolean;
var
	I, DelimPos: Integer;
	Keyword, Nested: string;
begin
	Dest := nil;
	DelimPos := FindChar(WKT_REVERSE_SOLIDUS, AKey, 1); // its full path n\n
	if DelimPos <> 0 then
	begin
		Keyword := Copy(AKey, 1, DelimPos - 1);
		Nested := Copy(AKey, DelimPos + 1, MaxInt);
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
		Result := Dest.Find(Nested,Dest);
end;

function TWktNodeList.FindByAttributeName(const AKey, AAtributeName: string; out Dest: TWktNode): Boolean;
var
	I: integer;
begin
	for I := 0 to Count - 1 do
	begin
		Dest := Items[I];
		if SameText(Dest.Keyword, AKey) and
			 (Dest.AttributesCount > 0) and
			 SameText(StripQuotes(Dest.Attributes[0]), AAtributeName) then
			 Exit(True);
	end;
	Dest := nil;
	Result := False;
end;

{ TWKTDocument }

function TWKTDocument.GetEmpty: Boolean;
begin
  Result := Root.Count = 0;
end;

function TWKTDocument.GetRoot: TWktNode;
begin
	if FRootNode = nil then
	begin
		FRootNode := TWktNode.Create(nil);
		Add(FRootNode);
	end;

	Result := FRootNode;
end;

procedure TWKTDocument.LoadFromFile(const AFilename: string);
var
	S: TStream;
begin
	if not Empty then
		FRootNode.Clear;

	if FileExists(AFilename) then
	begin
		S := TFileStream.Create(AFilename,fmOpenRead);
		try
      LoadFromStream(S);
		finally
			FreeAndNil(S);
		end;
	end;
end;

procedure TWKTDocument.LoadFromStream(Stream: TStream);
begin
	if not Empty then
		Root.Clear;

	FRootNode.ParseStream(Stream);
end;

procedure TWKTDocument.LoadFromString(const AString: string);
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

procedure TWKTDocument.SaveToFile(const AFilename: string);
var
	Str: string;
  S: TStream;
begin
	Str := SaveToString(False);
	if Str <> '' then
	begin
		S := TFileStream.Create(AFilename, fmCreate);
		try
			S.Write(BytesOf(Str),ByteLength(Str));
		finally
			FreeAndNil(S);
		end;
  end;
end;

function TWKTDocument.SaveToStream(AStream: TStream): Integer;
var
	S: TStream;
begin
	Result := AStream.Position;
	S := TStringStream.Create(SaveToString(False));
	try
		AStream.CopyFrom(S,Self.Count);

		Result := AStream.Position - Result;
	finally
		FreeAndNil(S);
  end;
end;

function TWKTDocument.SaveToString(PrettyPrint: Boolean): string;
begin
	Result := Root.SaveToString(PrettyPrint)
end;

end.
