object Form1: TForm1
  Left = 313
  Top = 235
  Caption = 'Hello Client'
  ClientHeight = 236
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    355
    236)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Say Hello'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ebMessage: TEdit
    Left = 232
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Hello'
  end
  object lstMessages: TListBox
    Left = 104
    Top = 72
    Width = 249
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnMany: TButton
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Say Hello Lots'
    TabOrder = 3
    OnClick = btnManyClick
  end
  object ebHelloCount: TEdit
    Left = 8
    Top = 80
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '10'
  end
  object udHelloCount: TUpDown
    Left = 49
    Top = 80
    Width = 15
    Height = 21
    Associate = ebHelloCount
    Max = 1000
    Position = 10
    TabOrder = 5
  end
  object ebPort: TEdit
    Left = 104
    Top = 40
    Width = 65
    Height = 21
    TabOrder = 6
    Text = '8'#160'081'
  end
  object udPort: TUpDown
    Left = 169
    Top = 40
    Width = 15
    Height = 21
    Associate = ebPort
    Max = 32767
    Position = 8081
    TabOrder = 7
  end
  object ebHost: TEdit
    Left = 104
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'localhost'
  end
end
