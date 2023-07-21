object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 504
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 395
    Width = 55
    Height = 15
    Caption = 'Start point'
  end
  object Label2: TLabel
    Left = 104
    Top = 395
    Width = 51
    Height = 15
    Caption = 'End point'
  end
  object Label3: TLabel
    Left = 312
    Top = 363
    Width = 7
    Height = 15
    Caption = 'X'
  end
  object Label4: TLabel
    Left = 552
    Top = 363
    Width = 7
    Height = 15
    Caption = 'Y'
  end
  object Button1: TButton
    Left = 8
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Cut once'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Refresh'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 416
    Width = 75
    Height = 23
    TabOrder = 2
    Text = 'x'
  end
  object Edit2: TEdit
    Left = 8
    Top = 456
    Width = 75
    Height = 23
    TabOrder = 3
    Text = 'y'
  end
  object Edit3: TEdit
    Left = 104
    Top = 416
    Width = 75
    Height = 23
    TabOrder = 4
    Text = 'x'
  end
  object Edit4: TEdit
    Left = 104
    Top = 456
    Width = 75
    Height = 23
    TabOrder = 5
    Text = 'y'
  end
  object TrackBar1: TTrackBar
    Left = 304
    Top = 384
    Width = 150
    Height = 45
    TabOrder = 6
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 544
    Top = 384
    Width = 150
    Height = 45
    TabOrder = 7
    OnChange = TrackBar2Change
  end
end
