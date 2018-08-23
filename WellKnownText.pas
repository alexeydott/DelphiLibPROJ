unit WellKnownText;
// http://www.geoapi.org/2.0/javadoc/org/opengis/referencing/doc-files/WKT.html

interface

uses
	Classes, SysUtils, Character, Generics.Collections;

type

	TWktNode = class;

	TWktNodeList = class(TObjectList<TWktNode>)
	public
		function Find(const AKey: string): TWktNode; virtual;
		function FindByAttributeName(const AKey, AAtributeName: string): TWktNode; virtual;
	end;

	TWktNode = class(TWktNodeList)
	private
		FKeyword: string;
		FAttributes: TStrings;
		FParentNode: TWktNode;
		function GetAttributes: TStrings;
	protected
		FLevel: Integer;
		procedure AddAttribute(const AValue: string);
		function GetAttribute(Index: Integer): string;
		function AttibutesAsString: string;
	public
		constructor Create(AParent: TWktNode);
		destructor Destroy(); override;
		function SaveToString(const PrettyPrint: Boolean): string;
		procedure ParseStream(Stream: TStream); virtual;

		property Keyword: string read FKeyword;
		property Parent: TWktNode read FParentNode;
		property Attribute[Index: Integer]: string read GetAttribute;
		property Attributes: TStrings read GetAttributes;
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
	FParentNode := AParent;
	FLevel := 0;
	if FParentNode <> nil then
		FLevel := FParentNode.FLevel + 1;
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

procedure TWktNode.ParseStream(Stream: TStream);
var
	CurrentChar: Char;
	AtributeValue: string;
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
	AtributeValue := '';
	while Stream.Read(CurrentChar, 1) = 1 do
	begin
		case CurrentChar of
			WKT_BRACKET_OPEN:
				begin
				  // node start
					if BracketOpened then
					begin
						AddAttribute(AtributeValue);
						AtributeValue := '';
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
						FKeyword := AtributeValue;
						AtributeValue := '';
					end;
				end;
			WKT_VALUE_SEPARATOR:
				begin
					LastCommaPos := Stream.Position;
					AddAttribute(AtributeValue);
					AtributeValue := '';
					Inc(CommaCount);
				end;
			WKT_BRACKET_CLOSE:
				begin
					// End
					AddAttribute(AtributeValue);
					AtributeValue := '';
					Break;
				end;

			' ', #9, #10, #13:
				Continue;

			WKT_QUOTE_CHAR:
				begin
					QuoteOpened := not QuoteOpened;
					if QuoteOpened then
					begin
						AtributeValue := CurrentChar;
						while (Stream.Read(CurrentChar, 1) = 1) do
						begin
							AtributeValue := AtributeValue + CurrentChar;
							if CurrentChar = WKT_QUOTE_CHAR then
							begin
								QuoteOpened := False;
								Break;
							end;
						end;
					end;
				end;
		else
			AtributeValue := AtributeValue + CurrentChar;
		end;
	end;
end;

function TWktNode.SaveToString(const PrettyPrint: Boolean): string;
var
	I: Integer;
	NodeIdent: string;
begin
	Result := '';
	if Count = 0 then
		Exit;
	Result := UpperCase(Keyword) + WKT_BRACKET_OPEN;

	if (FAttributes <> nil) and (FAttributes.Count > 0) then
		Result := Result + AttibutesAsString;

	if Count > 0 then
	begin
		if PrettyPrint and (FLevel > 0) then
			NodeIdent := WKT_NEWLINE + StringOfChar(' ', FLevel*2)
		else
			NodeIdent := '';

		for I := 0 to Count - 1 do
			Result := Result + WKT_VALUE_SEPARATOR + Items[I].SaveToString(PrettyPrint);
	end;

	Result := Result + WKT_BRACKET_CLOSE;
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

function TWktNodeList.Find(const AKey: string): TWktNode;
var
	I, DelimPos: Integer;
	Keyword, Nested: string;
begin
	Result := nil;
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
			Result := Items[I];
			Break;
		end;

	if (Result <> nil) and (Nested <> '') then
		Result := Result.Find(Nested);
end;

function TWktNodeList.FindByAttributeName(const AKey, AAtributeName: string): TWktNode;
var
	I, J: integer;
begin
	for I := 0 to Count - 1 do
	begin
		Result := Items[I];
		if SameText(Result.Keyword, AKey) then
		begin
			for J := 0 to Result.Attributes.Count -1 do
			begin
				if SameText(StripQuotes(Result.Attributes[J]), AAtributeName) then
					Exit;
			end;
		end;
  end;
  Result := nil;
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
