object dlgDuplicateRooms: TdlgDuplicateRooms
  Left = 403
  Top = 187
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Duplicate Rooms:'
  ClientHeight = 524
  ClientWidth = 501
  Color = clBtnFace
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pTl: TPanel
    Left = 0
    Top = 498
    Width = 501
    Height = 26
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    object tlOK: TThinIcoButton
      Left = 1
      Top = 1
      Width = 235
      Height = 24
      Align = alClient
      Caption = '&OK'
      OnClick = tlOKClick
    end
    object tlCancel: TThinIcoButton
      Left = 236
      Top = 1
      Width = 264
      Height = 24
      Align = alRight
      Caption = '&Cancel'
      OnClick = tlCancelClick
    end
  end
  object pMap: TPanel
    Left = 0
    Top = 149
    Width = 501
    Height = 349
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 1
  end
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 149
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object pSettings: TPanel
      Left = 201
      Top = 0
      Width = 300
      Height = 149
      Align = alClient
      BevelOuter = bvLowered
      Constraints.MinWidth = 300
      TabOrder = 0
      DesignSize = (
        300
        149)
      object Label3: TLabel
        Left = 4
        Top = 4
        Width = 123
        Height = 13
        Caption = 'Modify shifts / warps:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object chbModRelContainedShifts: TCheckBox
        Left = 20
        Top = 24
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify relative contained shifts'
        TabOrder = 0
      end
      object chbModAbsContainedShifts: TCheckBox
        Left = 20
        Top = 44
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify absolute contained shifts'
        TabOrder = 1
      end
      object chbModContainedWarps: TCheckBox
        Left = 20
        Top = 64
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify contained warps'
        TabOrder = 2
      end
      object chbModRelOutShifts: TCheckBox
        Left = 20
        Top = 84
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify relative outgoing shifts'
        TabOrder = 3
      end
      object chbModAbsOutShifts: TCheckBox
        Left = 20
        Top = 104
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify absolute outgoing shifts'
        TabOrder = 4
      end
      object chbModOutWarps: TCheckBox
        Left = 20
        Top = 124
        Width = 270
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Modify outgoing warps'
        TabOrder = 5
      end
    end
    object pOffset: TPanel
      Left = 0
      Top = 0
      Width = 201
      Height = 149
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Offset X:'
      end
      object Label2: TLabel
        Left = 8
        Top = 29
        Width = 41
        Height = 13
        Caption = 'Offset Y:'
      end
      object lCollisionWarning: TLabel
        Left = 40
        Top = 104
        Width = 106
        Height = 13
        Caption = 'Warning: collision!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      object eOffsetX: TEdit
        Left = 56
        Top = 5
        Width = 121
        Height = 21
        TabOrder = 0
        OnChange = eOffsetChange
      end
      object eOffsetY: TEdit
        Left = 56
        Top = 26
        Width = 121
        Height = 21
        TabOrder = 1
        OnChange = eOffsetChange
      end
      object chbUseSelection: TCheckBox
        Left = 8
        Top = 52
        Width = 93
        Height = 17
        Caption = 'Selection only'
        TabOrder = 2
        OnClick = eOffsetChange
      end
    end
  end
end
