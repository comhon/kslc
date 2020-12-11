object frmMain: TfrmMain
  Left = 317
  Top = 167
  Width = 967
  Height = 600
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Knytt Stories Level Composer'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 900
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = mMain
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  DesignSize = (
    959
    554)
  PixelsPerInch = 96
  TextHeight = 13
  object sbMain: TStatusBar
    Left = 0
    Top = 535
    Width = 959
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 250
      end
      item
        Width = 80
      end
      item
        Width = 70
      end
      item
        Width = 50
      end>
    SimplePanel = False
    SizeGrip = False
  end
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 959
    Height = 535
    ActivePage = tsTiles
    Align = alClient
    TabIndex = 0
    TabOrder = 2
    TabStop = False
    object tsTiles: TTabSheet
      Caption = 'T&iles'
      DesignSize = (
        951
        507)
      object rvMain: TKSRoomView
        Left = 0
        Top = 0
        Width = 650
        Height = 290
        ShowNeighbors = True
        BackgroundVisible = True
        OnMouseDown = rvMainMouseDown
        OnMouseUp = rvMainMouseUp
        OnMouseMove = rvMainMouseMove
      end
      object imgTileBg: TImage
        Left = 376
        Top = 291
        Width = 192
        Height = 24
        Anchors = [akLeft, akBottom]
      end
      object tvMainA: TKSTilesetView
        Left = 0
        Top = 315
        Width = 384
        Height = 192
        BgColor1 = 1721939
        BgColor2 = 1526361
        Anchors = [akLeft, akBottom]
        OnMouseDown = tvMainMouseDown
        OnMouseUp = tvMainMouseUp
        OnMouseMove = tvMainMouseMove
      end
      object imgLayers: TImage
        Left = 132
        Top = 290
        Width = 240
        Height = 24
        OnMouseDown = imgLayersMouseDown
      end
      object tvMainB: TKSTilesetView
        Tag = 1
        Left = 386
        Top = 315
        Width = 384
        Height = 192
        BgColor1 = 1721939
        BgColor2 = 1526361
        Anchors = [akLeft, akBottom]
        OnMouseDown = tvMainMouseDown
        OnMouseUp = tvMainMouseUp
        OnMouseMove = tvMainMouseMove
      end
      object lTilesetA: TLabel
        Left = 0
        Top = 291
        Width = 126
        Height = 22
        Anchors = [akLeft, akBottom]
        AutoSize = False
        Caption = 'TilesetA: 255'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnMouseDown = lTilesetMouseDown
      end
      object lTilesetB: TLabel
        Tag = 1
        Left = 644
        Top = 291
        Width = 126
        Height = 22
        Anchors = [akLeft, akBottom]
        AutoSize = False
        Caption = 'TilesetB: 255'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnMouseDown = lTilesetMouseDown
      end
      object mvMainSmall: TKSMapView
        Left = 751
        Top = 0
        Width = 200
        Height = 290
        MapStyle = msOriginal
        HighlightCenter = True
        AllowMultiSelect = True
        OnGoToRoom = MapGotoRoom
        OnRoomSelectionChanged = mvMainSmallRoomSelectionChanged
        Anchors = [akLeft, akTop, akRight, akBottom]
        PopupMenu = pmMap
      end
      object ocMain: TKSObjectChooser
        Left = 650
        Top = 0
        Width = 100
        Height = 290
        Bank = 2
        Obj = 2
      end
      object imgPowerups: TImage
        Left = 4
        Top = 360
        Width = 288
        Height = 24
        OnMouseDown = imgPowerupsMouseDown
      end
      object lPowerupSelect: TLabel
        Left = 4
        Top = 344
        Width = 205
        Height = 13
        Caption = 'Select powerups that should be accessible:'
        Visible = False
      end
      object mKnyttScriptSmall: TMemo
        Left = 772
        Top = 292
        Width = 179
        Height = 215
        Anchors = [akLeft, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = mKnyttScriptSmallChange
      end
    end
    object tsLargeMap: TTabSheet
      Caption = 'Large&Map'
      ImageIndex = 2
      object mvMainLarge: TKSMapView
        Left = 0
        Top = 0
        Width = 1008
        Height = 507
        MapStyle = msOriginal
        HighlightCenter = True
        AllowMultiSelect = True
        OnGoToRoom = mvMainLargeGoToRoom
        OnRoomSelectionChanged = mvMainLargeRoomSelectionChanged
        Align = alClient
        PopupMenu = pmMap
      end
    end
    object tsKnyttScript: TTabSheet
      Caption = 'Knytt&Script'
      ImageIndex = 1
      object mKnyttScript: TMemo
        Left = 0
        Top = 0
        Width = 327
        Height = 507
        Align = alClient
        TabOrder = 0
        OnChange = mKnyttScriptChange
      end
      object sbKnyttScript: TScrollBox
        Left = 327
        Top = 0
        Width = 624
        Height = 507
        Align = alRight
        TabOrder = 1
        object lTODO1: TLabel
          Left = 224
          Top = 224
          Width = 126
          Height = 13
          Caption = 'TODO: KSManager-like UI'
        end
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 3
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 951
        Height = 507
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object pProgress: TPanel
    Left = 12
    Top = 120
    Width = 937
    Height = 55
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvLowered
    TabOrder = 1
    Visible = False
    DesignSize = (
      937
      55)
    object lProgress: TLabel
      Left = 4
      Top = 4
      Width = 929
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      WordWrap = True
    end
    object pbMain: TProgressBar
      Left = 4
      Top = 35
      Width = 929
      Height = 16
      Anchors = [akLeft, akRight, akBottom]
      Min = 0
      Max = 100
      TabOrder = 0
    end
  end
  object alMain: TActionList
    Left = 24
    Top = 72
    object actViewLog: TAction
      Category = 'View'
      Caption = 'Log'
    end
    object actFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      ShortCut = 24655
      OnExecute = actFileOpenExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Sa&ve as...'
      OnExecute = actFileSaveAsExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      ShortCut = 32856
      OnExecute = actFileExitExecute
    end
    object actCheckWallSwim: TAction
      Category = 'Checks'
      Caption = 'Possible Wall-swimming'
      OnExecute = actCheckWallSwimExecute
    end
    object actCheckL3Grass: TAction
      Category = 'Checks'
      Caption = 'L3 Grass'
      OnExecute = actCheckL3GrassExecute
    end
    object actViewPowerups: TAction
      Category = 'View'
      Caption = 'All powerups'
      OnExecute = actViewPowerupsExecute
    end
    object actCheckUnassignedEvents: TAction
      Category = 'Checks'
      Caption = 'Unassigned Events'
      OnExecute = actCheckUnassignedEventsExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = 'Show help...'
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      OnExecute = actHelpAboutExecute
    end
    object actToolsDuplicateAllRooms: TAction
      Category = 'Tools'
      Caption = 'Duplicate all rooms...'
      OnExecute = actToolsDuplicateAllRoomsExecute
    end
    object actToolsMultiRoomParam: TAction
      Category = 'Tools'
      Caption = 'Multi-room change param...'
      OnExecute = actToolsMultiRoomParamExecute
    end
    object actViewShifts: TAction
      Category = 'View'
      Caption = 'All shifts'
      OnExecute = actViewShiftsExecute
    end
    object actFileOpenInstalled: TAction
      Category = 'File'
      Caption = 'Open installed level...'
      ShortCut = 16463
      OnExecute = actFileOpenInstalledExecute
    end
    object actFollowShiftA: TAction
      Category = 'GoTo'
      Caption = 'Follow Shift A'
      OnExecute = actFollowShiftExecute
    end
    object actFollowShiftB: TAction
      Tag = 1
      Category = 'GoTo'
      Caption = 'Follow Shift B'
      OnExecute = actFollowShiftExecute
    end
    object actFollowShiftC: TAction
      Tag = 2
      Category = 'GoTo'
      Caption = 'Follow Shift C'
      OnExecute = actFollowShiftExecute
    end
    object actViewShiftsLeadingHere: TAction
      Category = 'View'
      Caption = 'Shifts Leading Here'
      OnExecute = actViewShiftsLeadingHereExecute
    end
    object actViewLayBkg: TAction
      Tag = -1
      Category = 'Layer'
      Caption = 'Gradient image'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay0: TAction
      Category = 'Layer'
      Caption = 'Layer 0 - background tile'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay1: TAction
      Tag = 1
      Category = 'Layer'
      Caption = 'Layer 1 - background tile'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay2: TAction
      Tag = 2
      Category = 'Layer'
      Caption = 'Layer 2 - background tile'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay3: TAction
      Tag = 3
      Category = 'Layer'
      Caption = 'Layer 3 - collision tile'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay4: TAction
      Tag = 4
      Category = 'Layer'
      Caption = 'Layer 4 - background sprite'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay5: TAction
      Tag = 5
      Category = 'Layer'
      Caption = 'Layer 5 - background sprite'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay6: TAction
      Tag = 6
      Category = 'Layer'
      Caption = 'Layer 6 - foreground sprite'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLay7: TAction
      Tag = 7
      Category = 'Layer'
      Caption = 'Layer 7 - foreground sprite'
      Checked = True
      OnExecute = actViewLayExecute
    end
    object actViewLayPass: TAction
      Tag = 8
      Category = 'Layer'
      Caption = 'Passability mask'
      OnExecute = actViewLayExecute
    end
    object actViewSettings: TAction
      Category = 'View'
      Caption = 'Settings...'
      OnExecute = actViewSettingsExecute
    end
    object actGotoX1000Y1000: TAction
      Category = 'GoTo'
      Caption = 'x1000 y1000'
      OnExecute = actGotoX1000Y1000Execute
    end
    object actGotoLevelStart: TAction
      Category = 'GoTo'
      Caption = 'Level start'
      OnExecute = actGotoLevelStartExecute
    end
    object actToolsTestLevel: TAction
      Category = 'Tools'
      Caption = 'Test-play level'
      OnExecute = actToolsTestLevelExecute
    end
    object actToolsSetStartPos: TAction
      Category = 'Tools'
      Caption = 'Set start position'
      OnExecute = actToolsSetStartPosExecute
    end
  end
  object mMain: TMainMenu
    Left = 108
    Top = 72
    object miFile: TMenuItem
      Caption = '&File'
      object miFileNew: TMenuItem
        Action = actFileNew
      end
      object miFileOpen: TMenuItem
        Action = actFileOpen
      end
      object miFileOpenInstalled: TMenuItem
        Action = actFileOpenInstalled
      end
      object miFileSave: TMenuItem
        Action = actFileSave
      end
      object miFileSaveAs: TMenuItem
        Action = actFileSaveAs
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miFileMRU1: TMenuItem
        Tag = 1
        Visible = False
        OnClick = miMRUClick
      end
      object miFileMRU2: TMenuItem
        Tag = 2
        Visible = False
        OnClick = miMRUClick
      end
      object miFileMRU3: TMenuItem
        Tag = 3
        Visible = False
        OnClick = miMRUClick
      end
      object miFileMRU4: TMenuItem
        Tag = 4
        Visible = False
        OnClick = miMRUClick
      end
      object miFileMRU5: TMenuItem
        Tag = 5
        Visible = False
        OnClick = miMRUClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object miLayer: TMenuItem
      Caption = '&Layer'
      object Gradientimage1: TMenuItem
        Action = actViewLayBkg
      end
      object Layer0background1: TMenuItem
        Action = actViewLay0
      end
      object actViewLay11: TMenuItem
        Action = actViewLay1
      end
      object actViewLay21: TMenuItem
        Action = actViewLay2
      end
      object actViewLay31: TMenuItem
        Action = actViewLay3
      end
      object actViewLay41: TMenuItem
        Action = actViewLay4
      end
      object actViewLay51: TMenuItem
        Action = actViewLay5
      end
      object actViewLay61: TMenuItem
        Action = actViewLay6
      end
      object actViewLay71: TMenuItem
        Action = actViewLay7
      end
      object actViewLayPass1: TMenuItem
        Action = actViewLayPass
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miLayerPassColorScheme: TMenuItem
        Caption = 'Passability color scheme'
        Enabled = False
        object miLayerPassRed: TMenuItem
          Caption = 'Red / Orange / Green'
        end
      end
    end
    object miGoTo: TMenuItem
      Caption = 'GoTo'
      object Levelstart1: TMenuItem
        Action = actGotoLevelStart
      end
      object x1000y10001: TMenuItem
        Action = actGotoX1000Y1000
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object FollowShiftA1: TMenuItem
        Action = actFollowShiftA
      end
      object FollowShiftB1: TMenuItem
        Action = actFollowShiftB
      end
      object FollowShiftC1: TMenuItem
        Action = actFollowShiftC
      end
    end
    object miView: TMenuItem
      Caption = '&View'
      object Log1: TMenuItem
        Action = actViewLog
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ShiftsLeadingHere1: TMenuItem
        Action = actViewShiftsLeadingHere
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miViewPowerups: TMenuItem
        Action = actViewPowerups
      end
      object Shifts1: TMenuItem
        Action = actViewShifts
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miViewSettings: TMenuItem
        Action = actViewSettings
      end
    end
    object miCheck: TMenuItem
      Caption = '&Check'
      ShortCut = 32856
      object miCheckWallSwim: TMenuItem
        Action = actCheckWallSwim
      end
      object miCheckL3Grass: TMenuItem
        Action = actCheckL3Grass
      end
      object miCheckUnassignedEvents: TMenuItem
        Action = actCheckUnassignedEvents
      end
    end
    object miTools: TMenuItem
      Caption = '&Tools'
      object miToolsDuplicateAllRooms: TMenuItem
        Action = actToolsDuplicateAllRooms
      end
      object miToolsMultiRoomParam: TMenuItem
        Action = actToolsMultiRoomParam
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Setstartposition1: TMenuItem
        Action = actToolsSetStartPos
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object estlevelinKS1: TMenuItem
        Action = actToolsTestLevel
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object miHelpShowHelp: TMenuItem
        Action = actHelpHelp
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miHelpAbout: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object pmMap: TPopupMenu
    Left = 232
    Top = 80
    object Selectedrooms1: TMenuItem
      Caption = 'Selected rooms:'
      Enabled = False
    end
    object pmiDuplicate: TMenuItem
      Caption = '    Duplicate...'
      OnClick = pmiDuplicateClick
    end
    object pmiChangeParams: TMenuItem
      Caption = '    Change parameters...'
      OnClick = pmiChangeParamsClick
    end
  end
end
