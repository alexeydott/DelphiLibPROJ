unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	LibPROJApi, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
		procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TransformPoint(var X,Y: string);
var
  _X,_Y: Double;
  P1,P2: Pointer;
begin
  if _X.TryParse(X,_X) and _X.TryParse(Y,_Y) then
  begin

    P1 := PJ_init_plus('+proj=tmerc +lat_0=55.6666666667 +lon_0=37.5 +k=1 +x_0=0 +y_0=0 +ellps=bessel +towgs84=41,-107.6,-93,0,0,0,0 +units=m +no_defs');
    P2 := PJ_init_plus('+proj=latlong +ellps=GRS80 +towgs84=-199.87,74.79,246.62');
    if (P1 <> nil) and (P2 <> nil) then
    begin
  	  if PJ_transform_point2D(P1,P2,@_X,@_Y,true) = 0 then
      begin
        X := _x.ToString;
        Y := _y.ToString;
      end
      else
      begin
        X := _x.ToString;
        Y := _y.ToString;
      end;

      PJ_Free(P1);
      PJ_Free(P2);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
	Self.Caption := PJ_get_version_string;


end;

end.
