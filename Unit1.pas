unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  libProj4.Classes, libProj4.Controls.VCL;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    PROJ4CRSSelector1: TPROJ4CRSSelector;
    PROJ4CRSParametersEditor1: TPROJ4CRSParametersEditor;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    PROJ4ProjectionsManager1: TPROJ4ProjectionsManager;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure PROJ4CRSSelector1Change(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
begin
  s := Proj4DefininionQuery(PROJ4CRSParametersEditor1.CRSDefinition);
  PROJ4CRSParametersEditor1.CRSDefinition := s;
end;

procedure TForm1.Button2Click(Sender: TObject);
const cLatWgs = 55.755864;
      cLonWgs = 37.617698;
var
  wgs,osm: IProjection;
  osmX,osmY: double;
begin
  wgs := PROJ4ProjectionsManager1.ProjectionByEpsgCode[4326];
  osm := PROJ4ProjectionsManager1.ProjectionByEpsgCode[900913];
  osmX := cLatWgs;
  osmY := cLonWgs;
  if PROJ4ProjectionsManager1.TransformPointXY(wgs,osm,osmX,osmY) then
  begin
    PROJ4ProjectionsManager1.TransformPointXY(osm,wgs, osmX, osmY);
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  PROJ4CRSSelector1.ShowCRSHint := CheckBox1.Checked;
end;

procedure TForm1.PROJ4CRSSelector1Change(Sender: TObject);
begin
  PROJ4CRSParametersEditor1.CRSDefinition := PROJ4CRSSelector1.SelectedCRSDefn;
end;

end.
