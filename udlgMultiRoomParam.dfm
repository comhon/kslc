object dlgMultiRoomParam: TdlgMultiRoomParam
  Left = 443
  Top = 217
  Width = 679
  Height = 527
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Multi-room Change Param:'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 671
    Height = 160
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object pParam: TPanel
      Left = 0
      Top = 0
      Width = 260
      Height = 160
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        260
        160)
      object Label1: TLabel
        Left = 4
        Top = 4
        Width = 51
        Height = 13
        Caption = 'Parameter:'
      end
      object lbParam: TListBox
        Left = 4
        Top = 20
        Width = 252
        Height = 136
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pChange: TPanel
      Left = 260
      Top = 0
      Width = 411
      Height = 160
      Align = alClient
      BevelOuter = bvLowered
      Constraints.MinWidth = 237
      TabOrder = 1
      DesignSize = (
        411
        160)
      object Label2: TLabel
        Left = 4
        Top = 4
        Width = 40
        Height = 13
        Caption = 'Change:'
      end
      object rbChSetTo: TRadioButton
        Left = 12
        Top = 28
        Width = 49
        Height = 17
        Caption = 'Set to'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbChAdd: TRadioButton
        Left = 12
        Top = 52
        Width = 41
        Height = 17
        Caption = 'Add'
        TabOrder = 1
      end
      object rbChMultiplyBy: TRadioButton
        Left = 12
        Top = 76
        Width = 71
        Height = 17
        Caption = 'Multiply by'
        TabOrder = 2
      end
      object rbChCopyFrom: TRadioButton
        Left = 12
        Top = 100
        Width = 69
        Height = 17
        Caption = 'Copy from'
        TabOrder = 3
      end
      object cbChCopyFrom: TComboBox
        Left = 108
        Top = 98
        Width = 295
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        Constraints.MinWidth = 121
        ItemHeight = 13
        TabOrder = 4
      end
      object rbChExchangeWith: TRadioButton
        Left = 12
        Top = 124
        Width = 93
        Height = 17
        Caption = 'Exchange with'
        TabOrder = 5
      end
      object cbChExchangeWith: TComboBox
        Left = 108
        Top = 122
        Width = 295
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        Constraints.MinWidth = 121
        ItemHeight = 13
        TabOrder = 6
      end
      object eChSetTo: TEdit
        Left = 108
        Top = 26
        Width = 121
        Height = 21
        TabOrder = 7
      end
      object eChAdd: TEdit
        Left = 108
        Top = 50
        Width = 121
        Height = 21
        TabOrder = 8
      end
      object eChMultiplyBy: TEdit
        Left = 108
        Top = 74
        Width = 121
        Height = 21
        TabOrder = 9
      end
    end
  end
  object pMap: TPanel
    Left = 0
    Top = 160
    Width = 671
    Height = 314
    Align = alClient
    BevelOuter = bvLowered
    BorderWidth = 2
    TabOrder = 1
  end
  object pTl: TPanel
    Left = 0
    Top = 474
    Width = 671
    Height = 26
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    object tlChange: TThinIcoButton
      Left = 1
      Top = 1
      Width = 337
      Height = 24
      Align = alClient
      Caption = '&Change'
      OnClick = tlChangeClick
    end
    object tlCancel: TThinIcoButton
      Left = 338
      Top = 1
      Width = 332
      Height = 24
      Align = alRight
      Caption = 'Cancel'
      OnClick = tlCancelClick
    end
  end
end
