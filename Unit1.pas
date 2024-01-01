unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  libProj4.Controls.VCL;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    PROJ4CRSSelector1: TPROJ4CRSSelector;
    PROJ4CRSParametersEditor1: TPROJ4CRSParametersEditor;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
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
var
  s: string;
begin
  s := PROJ4CRSParametersEditor1.CRSDefinition;
  if Proj4ParametersEditDlg(s) then
    PROJ4CRSParametersEditor1.CRSDefinition := s;
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
