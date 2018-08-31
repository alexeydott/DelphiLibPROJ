object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 281
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '58,724554'
  end
  object Edit2: TEdit
    Left = 8
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '69,963443'
  end
  object Button1: TButton
    Left = 170
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 135
    Top = 17
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 136
    Width = 418
    Height = 145
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    ExplicitLeft = 8
    ExplicitTop = 128
    ExplicitWidth = 402
  end
  object Edit3: TEdit
    Left = 8
    Top = 86
    Width = 402
    Height = 21
    TabOrder = 5
    Text = 
      '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +to' +
      'wgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  end
end
