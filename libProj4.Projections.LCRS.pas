unit libProj4.Projections.LCRS;

interface
{$IFDEF MSWINDOWS}
  {$WEAKPACKAGEUNIT}
{$ENDIF}

{$I 'libProj4.config.inc'}

/// <remarks>
///adt:
/// this unit contain CRS descriptions to create lists of known CRSs for TPROJ4CRSSelector.
/// maybe later i will redesign this differently.
/// </remarks>

uses
  System.Types, System.Classes, System.Sysutils, System.Generics.Collections;

function GetKnownLCRCDefitions(var ADest: TArray<TPair<string,string>>): Integer;

implementation

const
  cGCSDefs: array[0..4] of string = (
    'Pulkovo 1942|+proj=longlat +ellps=krass +towgs84=23.92,-141.27,-80.9,-0,0.35,0.82,-0.12 +no_defs',
    'Pulkovo 1942 ГОСТ 51794-2008|+proj=longlat +ellps=krass +towgs84=23.57,-140.95,-79.8,0,0.35,0.79,-0.22 +no_defs',
    'ПЗ-90|+proj=longlat +a=6378136 +b=6356751.361745712 +towgs84=0,0,1.5,-0,-0,0.076,0 +no_defs',
		'Sphere Mercator (OSM/Google карты)|+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k_0=1.0 +units=m +nadgrids=@null +wktext +no_defs',
		'WGS84 Меркатор (Yandex карты)|+proj=merc +lon_0=0 +k_0=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  );


var FKnownCRS: TArray<TPair<string,string>> = nil;

function GetKnownLCRCDefitions(var ADest: TArray<TPair<string,string>>): Integer;
var
  i: Integer;
  kv: {$if compilerversion < 33.0}TArray<string>{$else}System.Types.TStringDynArray{$ifend};

  function ReallocKnownCRSArray(AMaxLen: Integer): Integer;
  const cCapacity = 500;
  begin
    Result := Length(FKnownCRS);
    if AMaxLen >= Result then
    begin
      Inc(Result, AMaxLen + cCapacity);
      SetLength(FKnownCRS, Result);
    end;
  end;

begin
  Result := Length(FKnownCRS);

  if Result = 0 then
  begin
    ReallocKnownCRSArray(Length(cGCSDefs));
    for i := 0 to High(cGCSDefs) do
    begin
      kv := cGCSDefs[i].Trim.Split(['|'],'"','"',TStringSplitOptions.ExcludeEmpty);

      if Length(kv) <> 2 then Continue;

      FKnownCRS[Result] := TPair<string,string>.Create(kv[0].Trim,kv[1].Trim);
      Inc(Result);
    end;
    SetLength(FKnownCRS,Result);
  end;
  ADest := FKnownCRS;
end;

initialization

finalization
FKnownCRS := nil;

end.
