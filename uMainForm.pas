unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.StdCtrls, Vcl.Samples.Spin,
	LibProj, WellKnownText;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
		procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
  private
		{ Private declarations }
		FMgr: TProjectionsManager;
		FFrom: IProjection;
		FTo: IProjection;
    procedure TransformPoint(var X, Y: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TransformPoint(var X,Y: string);
var
	_X,_Y: Double;
begin
	if _X.TryParse(X,_X) and _X.TryParse(Y,_Y) then
  begin
		if (FFrom <> nil) and (FTo <> nil) then
    begin
			FFrom.TransformPoint(FTo,_X,_Y);
      begin
        X := _x.ToString;
				Y := _y.ToString;
			end;
		end;

	end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S1,S2: string;
begin
  S1 := Edit1.Text;
  S2 := Edit2.Text;
  TransformPoint(S1,S2);
  Edit1.Text := S1;
  Edit2.Text := S2;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
	W: TWKTDocument;
	N: TWktNode;
begin
	W := TWKTDocument.Create();
	try
		W.LoadFromFile('C:\Develop\CVS_PROJECTS\gis_mgt\build\MapProject\PRJ\Moscow.prj');
		if W.Find('projcs',N) then
		begin
			OutputDebugString(PChar(N.Keyword));
			OutputDebugString(PChar(N.Attributes.CommaText));
			if N.Find('GEOGCS',N) then
			begin
				OutputDebugString(PChar(N.Keyword));
				OutputDebugString(PChar(N.Attributes.CommaText));
				if N.Find('DATUM',N) then
				begin
					OutputDebugString(PChar(N.Keyword));
					OutputDebugString(PChar(N.Attributes.CommaText));
				end;
			end;
		end;
		OutputDebugString(PChar(W.SaveToString(True)));
	finally
		FreeAndNil(W);
	end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	FMgr := TProjectionsManager.Create(Self);

	FFrom := FMgr.ProjectionByDefinition['+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'];
	FTo := FMgr.ProjectionByDefinition['+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'];

end;

end.
