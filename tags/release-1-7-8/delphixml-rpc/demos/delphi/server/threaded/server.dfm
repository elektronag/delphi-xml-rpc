object Form1: TForm1
  Left = 142
  Top = 163
  Width = 458
  Height = 336
  Caption = 'Hello Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object lstMessages: TListBox
    Left = 120
    Top = 40
    Width = 313
    Height = 249
    ItemHeight = 13
    TabOrder = 1
  end
  object rgrpThreadSynch: TRadioGroup
    Left = 0
    Top = 40
    Width = 113
    Height = 89
    Caption = 'Thread Synch'
    ItemIndex = 0
    Items.Strings = (
      'None - lets risk it'
      'Main Thread')
    TabOrder = 2
  end
  object ebPort: TEdit
    Left = 120
    Top = 8
    Width = 65
    Height = 21
    TabOrder = 3
    Text = '1,025'
    OnChange = ebPortChange
  end
  object udPort: TUpDown
    Left = 185
    Top = 8
    Width = 15
    Height = 21
    Associate = ebPort
    Min = 0
    Max = 32767
    Position = 1025
    TabOrder = 4
    Wrap = False
  end
end
