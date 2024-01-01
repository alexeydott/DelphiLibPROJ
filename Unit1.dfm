object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'proj controls demo'
  ClientHeight = 633
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 54
    Height = 15
    Caption = 'select CRS'
  end
  object Label2: TLabel
    Left = 24
    Top = 80
    Width = 83
    Height = 15
    Caption = 'CRS parameters'
  end
  object CheckBox1: TCheckBox
    Left = 439
    Top = 43
    Width = 113
    Height = 17
    Caption = 'display crs hints'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object PROJ4CRSSelector1: TPROJ4CRSSelector
    AlignWithMargins = True
    Left = 24
    Top = 37
    Width = 409
    Height = 23
    Style = csDropDownList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'Not Selected'
    Editor = PROJ4CRSParametersEditor1
    Manager = PROJ4ProjectionsManager1
  end
  object PROJ4CRSParametersEditor1: TPROJ4CRSParametersEditor
    Left = 24
    Top = 101
    Width = 528
    Height = 244
    Caption = 'PROJ4CRSParametersEditor1'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = True
    TabOrder = 2
    StyleName = 'Windows'
  end
  object Button1: TButton
    Left = 24
    Top = 376
    Width = 528
    Height = 25
    Caption = 'test input enter CRS dlg'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 424
    Width = 528
    Height = 25
    Caption = 'test edit CRS dlg'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 464
    Width = 528
    Height = 25
    Caption = 'test CRS to CRS'
    TabOrder = 5
    OnClick = Button2Click
  end
  object PROJ4ProjectionsManager1: TPROJ4ProjectionsManager
    Left = 120
    Top = 520
  end
end
