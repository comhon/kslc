object dlgAbout: TdlgAbout
  Left = 294
  Top = 103
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About KSLC'
  ClientHeight = 389
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    409
    389)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 159
    Height = 13
    Caption = 'Knytt Stories Level Checker'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 20
    Top = 24
    Width = 221
    Height = 13
    Caption = 'The idea was woven in Nifflas'#39' Support Forums'
  end
  object Label3: TLabel
    Left = 20
    Top = 44
    Width = 203
    Height = 13
    Caption = 'The programming was taken on by _Xoft(o)'
  end
  object Label4: TLabel
    Left = 20
    Top = 64
    Width = 364
    Height = 13
    Caption = 
      'The constants needed were found out using a tileset by Paula (Gi' +
      'rl from mars)'
  end
  object Label5: TLabel
    Left = 20
    Top = 116
    Width = 169
    Height = 13
    Caption = 'The program is licensed under GPL:'
  end
  object Label6: TLabel
    Left = 36
    Top = 136
    Width = 104
    Height = 13
    Caption = 'You can use it for free'
  end
  object Label7: TLabel
    Left = 36
    Top = 156
    Width = 121
    Height = 13
    Caption = 'You can give it to anyone'
  end
  object Label8: TLabel
    Left = 36
    Top = 176
    Width = 116
    Height = 13
    Caption = 'You can get the sources'
  end
  object Label9: TLabel
    Left = 4
    Top = 208
    Width = 80
    Height = 13
    Caption = 'Big thanks go to:'
  end
  object Label10: TLabel
    Left = 20
    Top = 228
    Width = 198
    Height = 13
    Caption = 'Nifflas for creating such a wonderful game'
  end
  object Label11: TLabel
    Left = 20
    Top = 248
    Width = 144
    Height = 13
    Caption = 'All the level and media authors'
  end
  object Label12: TLabel
    Left = 20
    Top = 268
    Width = 196
    Height = 13
    Caption = 'Girl from mars for the Level-o-Metric tileset'
  end
  object Label13: TLabel
    Left = 20
    Top = 84
    Width = 306
    Height = 13
    Caption = 'Bugs found and fixed thanks to Pie_Sniper, LPChip and Drakkan'
  end
  object Label14: TLabel
    Left = 20
    Top = 288
    Width = 308
    Height = 13
    Caption = 'All the people in the forum who helped with bugs and suggestions'
  end
  object tlGoToKS: TButton
    Left = 4
    Top = 312
    Width = 401
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Go to Nifflas'#39' Knytt Stories page'
    TabOrder = 0
    OnClick = tlGoToKSClick
  end
  object tlGoToSF: TButton
    Left = 4
    Top = 336
    Width = 401
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Go to Nifflas'#39' Support Forums'
    TabOrder = 1
    OnClick = tlGoToSFClick
  end
  object tlGoToKSLC: TButton
    Left = 4
    Top = 360
    Width = 401
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Go to KSLC homepage'
    TabOrder = 2
    OnClick = tlGoToKSLCClick
  end
end
