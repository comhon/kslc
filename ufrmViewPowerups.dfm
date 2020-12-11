object frmViewPowerups: TfrmViewPowerups
  Left = 294
  Top = 103
  Width = 288
  Height = 220
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Powerups:'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lvPowerups: TListView
    Left = 0
    Top = 0
    Width = 280
    Height = 193
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
        Width = 60
      end
      item
        Caption = 'RoomX'
      end
      item
        Caption = 'RoomY'
      end
      item
        Caption = 'X'
      end
      item
        Caption = 'Y'
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvPowerupsDblClick
  end
end
