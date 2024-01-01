object Form1: TForm1
  Left = 0
  Top = 0
  ClientHeight = 679
  ClientWidth = 1089
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object CheckBox1: TCheckBox
    Left = 24
    Top = 24
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object PROJ4CRSSelector1: TPROJ4CRSSelector
    AlignWithMargins = True
    Left = 24
    Top = 72
    Width = 409
    Height = 23
    Style = csDropDownList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'Not Selected'
    Editor = PROJ4CRSParametersEditor1
  end
  object PROJ4CRSParametersEditor1: TPROJ4CRSParametersEditor
    Left = 24
    Top = 101
    Width = 409
    Height = 244
    Caption = 'PROJ4CRSParametersEditor1'
    Ctl3D = False
    ParentCtl3D = False
    ShowCaption = True
    TabOrder = 2
    StyleName = 'Windows'
  end
  object Button1: TButton
    Left = 46
    Top = 392
    Width = 75
    Height = 25
    Caption = 'enter CRS'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 392
    Width = 75
    Height = 25
    Caption = 'edit CRS'
    TabOrder = 4
    OnClick = Button2Click
  end
end
