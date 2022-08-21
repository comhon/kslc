
unit ufrmMain;

{$MODE Delphi}

interface

uses
	SysUtils,
	Variants,
	Classes,
	Graphics,
	Controls,
	Forms,
	Dialogs,
	Menus,
	ActnList,
	ExtCtrls,
	ComCtrls,
	StdCtrls,
	uVectors,
	LCLIntf,
	Process,
	uVersionInfo,
	uKSLog,
	uKSRepresentations,
	uKSRoomView,
	udlgInstalledLevelList,
	uWebVersionCheckThread,
	uKSTilesetView,
	Buttons,
	uKSObjectChooser,
	uKSMapView;





type
	TCurrentAction = (caInsert, caSetStartPos, caTestLevel);





  { TfrmMain }

  TfrmMain = class(TForm)
		alMain: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    mMain: TMainMenu;
    miFile: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    N2: TMenuItem;
    miFileExit: TMenuItem;
    actCheckWallSwim: TAction;
    actCheckL3Grass: TAction;
    miView: TMenuItem;
    miCheck: TMenuItem;
    miCheckWallSwim: TMenuItem;
    miCheckL3Grass: TMenuItem;
    sbMain: TStatusBar;
    actViewPowerups: TAction;
    miViewPowerups: TMenuItem;
		actCheckUnassignedEvents: TAction;
    miCheckUnassignedEvents: TMenuItem;
    actViewLog: TAction;
    Log1: TMenuItem;
		N1: TMenuItem;
		actHelpHelp: TAction;
    actHelpAbout: TAction;
		miHelp: TMenuItem;
    miHelpShowHelp: TMenuItem;
    N3: TMenuItem;
    miHelpAbout: TMenuItem;
    N4: TMenuItem;
		miViewSettings: TMenuItem;
    pProgress: TPanel;
    pbMain: TProgressBar;
    lProgress: TLabel;
    actToolsDuplicateAllRooms: TAction;
    miTools: TMenuItem;
    miToolsDuplicateAllRooms: TMenuItem;
    actToolsMultiRoomParam: TAction;
    miToolsMultiRoomParam: TMenuItem;
    N5: TMenuItem;
    miFileMRU1: TMenuItem;
    miFileMRU2: TMenuItem;
    miFileMRU3: TMenuItem;
    miFileMRU4: TMenuItem;
    miFileMRU5: TMenuItem;
    N6: TMenuItem;
    actViewShifts: TAction;
    Shifts1: TMenuItem;
    actFileOpenInstalled: TAction;
    miFileOpenInstalled: TMenuItem;
    actFollowShiftA: TAction;
		actFollowShiftB: TAction;
		actFollowShiftC: TAction;
    actViewShiftsLeadingHere: TAction;
    ShiftsLeadingHere1: TMenuItem;
    actViewLay0: TAction;
		actViewLay1: TAction;
		actViewLay2: TAction;
    actViewLay3: TAction;
    actViewLay4: TAction;
    actViewLay5: TAction;
    actViewLay6: TAction;
    actViewLay7: TAction;
		actViewLayPass: TAction;
    Layer0background1: TMenuItem;
    actViewLay11: TMenuItem;
    actViewLay21: TMenuItem;
    actViewLay31: TMenuItem;
    actViewLay41: TMenuItem;
    actViewLay51: TMenuItem;
    actViewLay61: TMenuItem;
    actViewLay71: TMenuItem;
    actViewLayPass1: TMenuItem;
    actViewLayBkg: TAction;
    Gradientimage1: TMenuItem;
    miLayer: TMenuItem;
    N7: TMenuItem;
    miLayerPassColorScheme: TMenuItem;
    miLayerPassRed: TMenuItem;
    pcMain: TPageControl;
    tsTiles: TTabSheet;
    rvMain: TKSRoomView;
    ocMain: TKSObjectChooser;
    imgTileBg: TImage;
    tvMainA: TKSTilesetView;
    imgLayers: TImage;
    tvMainB: TKSTilesetView;
    tsKnyttScript: TTabSheet;
    lTilesetA: TLabel;
    lTilesetB: TLabel;
    mvMainSmall: TKSMapView;
    tsLargeMap: TTabSheet;
    mvMainLarge: TKSMapView;
		actViewSettings: TAction;
    mKnyttScript: TMemo;
    sbKnyttScript: TScrollBox;
    lTODO1: TLabel;
    pmMap: TPopupMenu;
    Selectedrooms1: TMenuItem;
    pmiDuplicate: TMenuItem;
		pmiChangeParams: TMenuItem;
    mKnyttScriptSmall: TMemo;
    tsLog: TTabSheet;
    miGoTo: TMenuItem;
    actGotoX1000Y1000: TAction;
    actGotoLevelStart: TAction;
    Levelstart1: TMenuItem;
    x1000y10001: TMenuItem;
    N8: TMenuItem;
    FollowShiftA1: TMenuItem;
    FollowShiftB1: TMenuItem;
    FollowShiftC1: TMenuItem;
    mLog: TMemo;
    imgPowerups: TImage;
    actToolsTestLevel: TAction;
    N9: TMenuItem;
    estlevelinKS1: TMenuItem;
    lPowerupSelect: TLabel;
    actToolsSetStartPos: TAction;
    N10: TMenuItem;
    Setstartposition1: TMenuItem;

    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actViewPowerupsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actCheckWallSwimExecute(Sender: TObject);
    procedure actCheckUnassignedEventsExecute(Sender: TObject);
		procedure actCheckL3GrassExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actViewSettingsExecute(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure OnLoadProgress(Sender: TObject; iCurrent, iMax: integer; iMessage: string);
		procedure OnWorkProgress(Sender: TObject; iCurrent, iMax: integer; iMessage: string);
		procedure actFileSaveExecute(Sender: TObject);
		procedure actFileSaveAsExecute(Sender: TObject);
		procedure actToolsDuplicateAllRoomsExecute(Sender: TObject);
		procedure actToolsMultiRoomParamExecute(Sender: TObject);
		procedure OnUpdateMRUMenu(Sender: TObject);
		procedure miMRUClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure OnWebVersionReceived(Sender: TObject; WebVersion: string);
		procedure actViewShiftsExecute(Sender: TObject);
		procedure actFileOpenInstalledExecute(Sender: TObject);
		procedure actFollowShiftExecute(Sender: TObject);
		procedure actViewShiftsLeadingHereExecute(Sender: TObject);
		procedure OnLevelChanged(Sender: TObject);
		procedure actViewLayExecute(Sender: TObject);
		procedure pcMainChange(Sender: TObject);
		procedure tlLayerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure rvMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
		procedure imgLayersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure lTilesetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure tvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure tvMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
		procedure tvMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure rvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure rvMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MapGotoRoom(Sender: TObject; iX, iY: Integer);
		procedure mvMainLargeGoToRoom(Sender: TObject; iX, iY: Integer);
    procedure mKnyttScriptChange(Sender: TObject);
    procedure pmiDuplicateClick(Sender: TObject);
    procedure pmiChangeParamsClick(Sender: TObject);
    procedure mvMainSmallRoomSelectionChanged(Sender: TObject);
    procedure mvMainLargeRoomSelectionChanged(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure mKnyttScriptSmallChange(Sender: TObject);
    procedure actGotoX1000Y1000Execute(Sender: TObject);
		procedure actGotoLevelStartExecute(Sender: TObject);
		procedure OnLogUpdate(Sender: TObject);
    procedure imgPowerupsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure actToolsTestLevelExecute(Sender: TObject);
    procedure actToolsSetStartPosExecute(Sender: TObject);

	private
		fLevel: TKSLevel;
		actLayerVis: array[-1..8] of TAction;
		fEditLayer: integer;
		CurrentTileset: integer;
		fCurrentAction: TCurrentAction;

		IsTilesetMouseDown: boolean;
		TilesetMouseDown: TPoint;
		TilesetSelection: TVGRect;

		IsRVMouseDown: boolean;

		UpdatingProps: boolean;
		UpdatingSelection: boolean;

		rvvInsertion: TVGRect;		// current insertion rectangle
		vStartPos: TVGCircle; 	// start pos circle

		PowerupSelected: array[0..11] of boolean;
		
		procedure fSetLevel(iLevel: TKSLevel);
		procedure fSetEditLayer(iVal: integer);
		procedure fSetCurrentAction(iVal: TCurrentAction);

		procedure RedrawImgLayers();
		procedure UpdateInsertionVector();
		procedure RedrawImgPowerups();
		procedure UpdateStartPos();

		procedure InsertTiles(iTileCoords: TPoint; iInCurrentRoom: boolean);
		procedure SetStartPos(iTileCoords: TPoint);
		procedure TestLevelSetPos(iTileCoords: TPoint);

		procedure FreeTilesetSelection();

		procedure FixPositions();		// fixes XP / Vista skin issues

	public
		CurrentRoom: TKSRoom;
		CurrentXPos, CurrentYPos: integer;
		bmpRoom: TBitmap;

		HasCheckedWebVersion: boolean;
		WebVersionCheckThread: TWebVersionCheckThread;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;

		procedure OpenFile(iFileName: string);

		procedure UpdateActionsAvail();
		procedure UpdateStatusbar();
		procedure GotoRoom(XPos, YPos: integer);

		procedure RegVector(iVector: TVGObject);
		procedure RemVector(iVector: TVGObject);
		procedure DelVector(iVector: TVGObject);

		procedure InitProgress(iMsg: string);
		procedure FinitProgress();

		procedure CheckWebVersion();
		function GetCurrentVersion(): string;
		function IsVersionHigherThanCurrent(iVersion: string): boolean;

		property Level: TKSLevel read fLevel write fSetLevel;
		property EditLayer: integer read fEditLayer write fSetEditLayer;
		property CurrentAction: TCurrentAction read fCurrentAction write fSetCurrentAction;
	end;





var
	frmMain: TfrmMain;
	gLog: TKSLog;























implementation

uses
	uKSObjects,
	uSettings,
	uWallSwimChecker,
	uRoomDuplicator,
	ufrmViewPowerups,
	ufrmWallSwimList,
	udlgSettings,
	udlgDuplicateRooms,
	udlgAbout,
	udlgMultiRoomParam,
	ufrmShiftList,
	ufrmShiftsToHere;





{$R *.lfm}





function ExtractLastFolderName(iFileName: string): string;
var
	i, CopyTo: integer;
begin
	Result := ExtractFilePath(iFileName);
	CopyTo := Length(Result);
	for i := Length(Result) downto 1 do
	begin
		if (Result[i] = PathDelim) then
		begin
			if (i = Length(Result)) then
			begin
				CopyTo := i - 1;
				continue;
			end;
			Result := Copy(Result, i + 1, CopyTo - i);
			Exit;
		end;
	end;		// for i
end;





function	MixColor(iOrigColor, iNewColor: TColor; iNewOpacity: byte): TColor;
var
	ro, go, bo: integer;
	rn, gn, bn: integer;
begin
	ro := iOrigColor and $ff;
	go := (iOrigColor and $ff00) shr 8;
	bo := (iOrigColor and $ff0000) shr 16;
	rn := iNewColor and $ff;
	gn := (iNewColor and $ff00) shr 8;
	bn := (iNewColor and $ff0000) shr 16;
	Result := (
		((ro * (255 - iNewOpacity) + rn * iNewOpacity) div 256) or
		(((go * (255 - iNewOpacity) + gn * iNewOpacity) div 256) shl 8) or
		(((bo * (255 - iNewOpacity) + bn * iNewOpacity) div 256) shl 16)
	);
end;





constructor TfrmMain.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	tsTiles.DoubleBuffered := true;
	tsLargeMap.DoubleBuffered := true;
	
	actLayerVis[-1] := Self.actViewLayBkg;
	actLayerVis[0] := Self.actViewLay0;
	actLayerVis[1] := Self.actViewLay1;
	actLayerVis[2] := Self.actViewLay2;
	actLayerVis[3] := Self.actViewLay3;
	actLayerVis[4] := Self.actViewLay4;
	actLayerVis[5] := Self.actViewLay5;
	actLayerVis[6] := Self.actViewLay6;
	actLayerVis[7] := Self.actViewLay7;
	actLayerVis[8] := Self.actViewLayPass;

	EditLayer := 3;
	CurrentAction := caInsert;

	rvvInsertion := TVGRect.Create(0, 0, 0, 0, $00ffff);
	rvMain.RegVector(rvvInsertion);

	gLog.OnUpdate.Add(OnLogUpdate);
	
	RedrawImgPowerups();
end;





destructor TfrmMain.Destroy();
begin
	// nothing needed yet
	inherited Destroy();
end;





procedure TfrmMain.actFileOpenExecute(Sender: TObject);
var
	dlgOpen: TOpenDialog;
begin
	// load level from file:
	dlgOpen := TOpenDialog.Create(Self);
	try
		dlgOpen.Filter := 'Knytt Stories level file|Map.bin|All files|*.*';
		dlgOpen.InitialDir := gSettings.LastLevelDir;
		if not(dlgOpen.Execute) then
		begin
			Exit;
		end;
		OpenFile(dlgOpen.FileName);
	finally
		dlgOpen.Free();
	end;
end;





procedure TfrmMain.actFileExitExecute(Sender: TObject);
begin
	Close();
end;





procedure TfrmMain.actViewPowerupsExecute(Sender: TObject);
begin
	frmViewPowerups.UpdateFromLevel(Level);
	frmViewPowerups.Show();
end;





procedure TfrmMain.UpdateActionsAvail();
var
	i: integer;
	sl: TKSShiftList;
	FollowShiftAEnable: boolean;
	FollowShiftBEnable: boolean;
	FollowShiftCEnable: boolean;
begin
	// actViewMap.Enabled                := Assigned(Level) and (Level.NumRooms > 0);
	actViewShifts.Enabled             := Assigned(Level) and (Level.NumRooms > 0);
	actViewPowerups.Enabled           := Assigned(Level) and (Level.NumRooms > 0);
	actCheckWallSwim.Enabled          := Assigned(Level) and (Level.NumRooms > 0);
	actCheckL3Grass.Enabled           := Assigned(Level) and (Level.NumRooms > 0);
	actCheckUnassignedEvents.Enabled  := Assigned(Level) and (Level.NumRooms > 0);
	actToolsDuplicateAllRooms.Enabled := Assigned(Level) and (Level.NumRooms > 0);
	actToolsMultiRoomParam.Enabled    := Assigned(Level) and (Level.NumRooms > 0);
	pmiDuplicate.Enabled              := Assigned(Level) and (Level.NumRooms > 0);
	pmiChangeParams.Enabled           := Assigned(Level) and (Level.NumRooms > 0);

	FollowShiftAEnable := false;
	FollowShiftBEnable := false;
	FollowShiftCEnable := false;
	if (Assigned(CurrentRoom)) then
	begin
		sl := Level.ShiftList.ExtractSingleFromRoom(CurrentRoom.XPos, CurrentRoom.YPos);
		try
			for i := 0 to sl.NumShifts - 1 do
			begin
				case sl.Shift[i].Kind of
					'a', 'A':
					begin
						FollowShiftAEnable := true;
					end;
					'b', 'B':
					begin
						FollowShiftBEnable := true;
					end;
					'c', 'C':
					begin
						FollowShiftCEnable := true;
					end;
				end;		// case sl.Shift[i].Kind
			end;		// for i - sl.Shift[]  	
		finally
			sl.Free();
		end;
	end;		// Assigned(CurrentRoom)
	actFollowShiftA.Enabled := FollowShiftAEnable;
	actFollowShiftB.Enabled := FollowShiftBEnable;
	actFollowShiftC.Enabled := FollowShiftCEnable;
end;





procedure TfrmMain.UpdateStatusbar();
begin
	// 0: current room coords
	// 1: level name
	// 2: num rooms
	// 3: tileset
	// 4: mouse pos (in rvMainMouseMove)
	
	if Assigned(CurrentRoom) then
	begin
		sbMain.Panels[3].Text := 'T: ' + IntToStr(CurrentRoom.Data.TilesetA) + ' / ' + IntToStr(CurrentRoom.Data.TilesetB);
	end
	else
	begin
		sbMain.Panels[3].Text := '---';
	end;
	if Assigned(Level) then
	begin
		sbMain.Panels[0].Text := '[' + IntToStr(CurrentXPos) + ', ' + IntToStr(CurrentYPos) + ']';
		if (Level.IsModified) then
		begin
			sbMain.Panels[1].Text := ExtractLastFolderName(Level.FileName) + ' *';
		end
		else
		begin
			sbMain.Panels[1].Text := ExtractLastFolderName(Level.FileName);
		end;
		sbMain.Panels[2].Text := 'Rooms: ' + IntToStr(Level.NumRooms);
	end
	else
	begin
		sbMain.Panels[0].Text := '---';
		sbMain.Panels[1].Text := '(no level loaded)';
		sbMain.Panels[2].Text := '---';
	end;
end;





procedure TfrmMain.GotoRoom(XPos, YPos: integer);
var
	nr: TKSRoom;
	ep: TStrings;
begin
	nr := Level.GetRoom(XPos, YPos);
	if not(Assigned(nr)) then
	begin
		Exit;
	end;
	UpdatingProps := true;
	try
		CurrentRoom := nr;
		CurrentXPos := XPos;
		CurrentYPos := YPos;
		mvMainSmall.GoToCoord(CurrentXPos, CurrentYPos);
		mvMainLarge.GoToCoord(CurrentXPos, CurrentYPos);

		ep := CurrentRoom.GetEventParams();
		try
			if Assigned(ep) then
			begin
				mKnyttScript.Lines.Assign(ep);
				mKnyttScriptSmall.Lines.Assign(ep);
			end
			else
			begin
				mKnyttScript.Lines.Clear();
				mKnyttScriptSmall.Lines.Clear();
			end;
		finally
			ep.Free();
		end;

		// TODO: KnyttScript UI

		frmShiftsToHere.SetRoom(CurrentRoom);
		rvMain.Room := CurrentRoom;
		tvMainA.Tileset := CurrentRoom.Tileset[0];
		tvMainB.Tileset := CurrentRoom.Tileset[1];
		lTilesetA.Caption := 'Tileset: ' + IntToStr(CurrentRoom.Data.TilesetA);
		lTilesetB.Caption := 'Tileset: ' + IntToStr(CurrentRoom.Data.TilesetB);
		FreeTilesetSelection();
		UpdateStatusbar();
		UpdateActionsAvail();
	finally
		UpdatingProps := true
	end;
end;





procedure TfrmMain.RegVector(iVector: TVGObject);
begin
	rvMain.RegVector(iVector);
end;





procedure TfrmMain.RemVector(iVector: TVGObject);
begin
	rvMain.RemVector(iVector);
end;





procedure TfrmMain.DelVector(iVector: TVGObject);
begin
	RemVector(iVector);
	iVector.Free();
end;





procedure TfrmMain.FormShow(Sender: TObject);
begin
	UpdateActionsAvail();
end;





procedure TfrmMain.FormCreate(Sender: TObject);
begin
	Application.Title := Caption;
	UpdateStatusbar();
	gSettings.OnUpdateMRUMenu := OnUpdateMRUMenu;
	OnUpdateMRUMenu(gSettings);
	FixPositions();
end;





procedure TfrmMain.actCheckWallSwimExecute(Sender: TObject);
var
	wsc: TWallSwimChecker;
begin
	wsc := TWallSwimChecker.Create(gLog);
	try
		InitProgress('Checking...');
		wsc.OnProgress := OnWorkProgress;
		wsc.ProcessLevel(Level);
		FinitProgress();
		frmWallSwimList.UpdateFromChecker(wsc);
		frmWallSwimList.Show();
		if (wsc.NumSwims = 0) then
		begin
			ShowMessage('No wallswims detected');
		end;
	finally
		wsc.Free();
	end;
end;





procedure TfrmMain.actCheckUnassignedEventsExecute(Sender: TObject);
begin
	// TODO
	ShowMessage('Not implemented yet');
end;





procedure TfrmMain.actCheckL3GrassExecute(Sender: TObject);
begin
	ShowMessage('Not implemented yet');
end;





procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
var
	dlgAbout: TdlgAbout;
begin
	dlgAbout := TdlgAbout.Create(Application);
	try
		dlgABout.ShowModal();
	finally
		dlgAbout.Free();
	end;
end;





procedure TfrmMain.actViewSettingsExecute(Sender: TObject);
var
	dlg: TdlgSettings;
begin
	dlg := TdlgSettings.Create(Self);
	try
		dlg.ShowModal();
	finally
		dlg.Free();
	end;
end;





procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	// gLog.Log(LOG_INFO, 'Key down: ' + IntToStr(Key));
	case Key of
		37:
		begin
			if (Shift = [ssShift]) then
			begin
				GotoRoom(CurrentXPos - 10, CurrentYPos);
			end
			else if (Shift = [ssCtrl]) then
			begin
				if Assigned(CurrentRoom.RoomLeft) then
				begin
					GotoRoom(CurrentRoom.RoomLeft.XPos, CurrentRoom.RoomLeft.YPos);
				end;
			end
			else
			begin
				GotoRoom(CurrentXPos - 1, CurrentYPos);
			end;
		end;		// left arrow

		38:		// up arrow:
		begin
			if (Shift = [ssShift]) then
			begin
				GotoRoom(CurrentXPos, CurrentYPos - 10);
			end
			else if (Shift = [ssCtrl]) then
			begin
				if Assigned(CurrentRoom.RoomUp) then
				begin
					GotoRoom(CurrentRoom.RoomUp.XPos, CurrentRoom.RoomUp.YPos);
				end;
			end
			else
			begin
				GotoRoom(CurrentXPos, CurrentYPos - 1);
			end;
		end;		// up arrow

		39:		// right arrow:
		begin
			if (Shift = [ssShift]) then
			begin
				GotoRoom(CurrentXPos + 10, CurrentYPos);
			end
			else if (Shift = [ssCtrl]) then
			begin
				if Assigned(CurrentRoom.RoomRight) then
				begin
					GotoRoom(CurrentRoom.RoomRight.XPos, CurrentRoom.RoomRight.YPos);
				end;
			end
			else
			begin
				GotoRoom(CurrentXPos + 1, CurrentYPos);
			end;
		end;		// right arrow

		40:		// down arrow:
		begin
			if (Shift = [ssShift]) then
			begin
				GotoRoom(CurrentXPos, CurrentYPos + 10);
			end
			else if (Shift = [ssCtrl]) then
			begin
				if Assigned(CurrentRoom.RoomDown) then
				begin
					GotoRoom(CurrentRoom.RoomDown.XPos, CurrentRoom.RoomDown.YPos);
				end;
			end
			else
			begin
				GotoRoom(CurrentXPos, CurrentYPos + 1);
			end;
		end;		// down arrow
	end;
end;





procedure TfrmMain.OnLoadProgress(Sender: TObject; iCurrent, iMax: integer; iMessage: string);
begin
	pbMain.Max := iMax;
	pbMain.Position := iCurrent;
	lProgress.Caption := 'Loading level...'#13#10 + iMessage;
	pbMain.Update();
	lProgress.Update();
end;





procedure TfrmMain.OnWorkProgress(Sender: TObject; iCurrent, iMax: integer; iMessage: string);
begin
	pbMain.Max := iMax;
	pbMain.Position := iCurrent;
	lProgress.Caption := iMessage;
	pbMain.Update();
	lProgress.Update();
end;





procedure TfrmMain.InitProgress(iMsg: string);
begin
	pbMain.Position := 0;
	lProgress.Caption := iMsg;
	pProgress.Visible := True;
	Screen.Cursor := crHourGlass;
end;





procedure TfrmMain.FinitProgress();
begin
	Screen.Cursor := crDefault;
	pProgress.Visible := False;
end;





procedure TfrmMain.actFileSaveExecute(Sender: TObject);
begin
	if not(Assigned(Level)) then Exit;
	if (Level.FileName = '') then
	begin
		actFileSaveAsExecute(Sender);
		Exit;
	end;
	Level.SaveToFile(Level.FileName);
end;





procedure TfrmMain.actFileSaveAsExecute(Sender: TObject);
var
	dlgSave: TSaveDialog;
begin
	if not(Assigned(Level)) then
	begin
		Exit;
	end;

	dlgSave := TSaveDialog.Create(Self);
	try
		if (Level.FileName <> '') then
		begin
			dlgSave.InitialDir := ExtractFilePath(Level.FileName);
			dlgSave.FileName := ExtractFileName(Level.FileName);
		end
		else
		begin
			dlgSave.InitialDir := gKSDir;
			dlgSave.FileName := 'Map.bin';
		end;
		dlgSave.Filter := 'Knytt Stories Level|Map.bin|Any file|*.*';
		if not(dlgSave.Execute) then
		begin
			Exit;
		end;
		Level.SaveToFile(dlgSave.FileName);
	finally
		dlgSave.Free();
	end;
end;





procedure TfrmMain.actToolsDuplicateAllRoomsExecute(Sender: TObject);
var
	dlg: TdlgDuplicateRooms;
	Sel: TList;
begin
	dlg := TdlgDuplicateRooms.Create(Application, Level);
	try
		if (pcMain.ActivePage = tsLargeMap) then
		begin
			Sel := mvMainLarge.Selection;
		end
		else
		begin
			sel := mvMainSmall.Selection;
		end;
		dlg.MapView.Selection.Assign(Sel);
		dlg.ShowModal();
	finally
		dlg.Free();
	end;
end;





procedure TfrmMain.fSetLevel(iLevel: TKSLevel);
begin
	fLevel := iLevel;
	iLevel.ChangedListeners.Add(OnLevelChanged);
	mvMainSmall.Level := iLevel;
	mvMainLarge.Level := iLevel;
	if not(Assigned(vStartPos)) then
	begin
		vStartPos := TVGCircle.Create(0, 0, 12, $ffff00);
		rvMain.RegVector(vStartPos);
	end;
	UpdateStartPos();
	if (Assigned(fLevel)) then
	begin
		if (fLevel.NumRooms > 0) then
		begin
			GotoRoom(fLevel.StartRoomX, fLevel.StartRoomY);
		end;
	end;
end;





procedure TfrmMain.UpdateStartPos();
var
	StartPosPt: TPoint;
begin
	if (Assigned(fLevel)) then
	begin
		rvMain.LogicalToCanvas(Point(fLevel.StartX, fLevel.StartY), Point(0, 0), StartPosPt);
		vStartPos.X := StartPosPt.X + 12;
		vStartPos.Y := StartPosPt.Y + 12;
		vStartPos.Room := fLevel.GetRoom(fLevel.StartRoomX, fLevel.StartRoomY);
		vStartPos.Visible := true;
		vStartPos.Changed();
	end;
end;





procedure TfrmMain.actToolsMultiRoomParamExecute(Sender: TObject);
var
	dlg: TdlgMultiRoomParam;
begin
	dlg := TdlgMultiRoomParam.Create(Self, Level);
	try
		dlg.ShowModal();
	finally
		dlg.Free();
	end;
end;





procedure TfrmMain.OpenFile(iFileName: string);
var
	lvl: TKSLevel;
begin
	lvl := TKSLevel.Create(gLog);
	InitProgress('Loading level...');
	lvl.OnProgress := OnLoadProgress;
	try
		lvl.LoadFromFile(iFileName);
	except
		lvl.Free();
		FinitProgress();
		Exit;
	end;
	FinitProgress();
	fLevel.Free();
	Level := lvl;
	gSettings.PushMRUItem(iFileName);
	if (fLevel.NumRooms > 0) then
	begin
		GotoRoom(fLevel.StartRoomX, fLevel.StartRoomY);
	end;
	UpdateActionsAvail();
	UpdateStatusbar();
	frmShiftList.UpdateFromLevel(fLevel);
	if (gSettings.AllowWebVersionCheck) then
	begin
		CheckWebVersion();
	end;
end;





procedure TfrmMain.OnUpdateMRUMenu(Sender: TObject);
begin
	miFileMRU1.Caption := '&1 - ' + ExtractLastFolderName(gSettings.MRUFileName[0]);
	miFileMRU1.Enabled := (gSettings.MRUFileName[0] <> '');
	miFileMRU1.Visible := miFileMRU1.Enabled;
	miFileMRU2.Caption := '&2 - ' + ExtractLastFolderName(gSettings.MRUFileName[1]);
	miFileMRU2.Enabled := (gSettings.MRUFileName[1] <> '');
	miFileMRU2.Visible := miFileMRU2.Enabled;
	miFileMRU3.Caption := '&3 - ' + ExtractLastFolderName(gSettings.MRUFileName[2]);
	miFileMRU3.Enabled := (gSettings.MRUFileName[2] <> '');
	miFileMRU3.Visible := miFileMRU3.Enabled;
	miFileMRU4.Caption := '&4 - ' + ExtractLastFolderName(gSettings.MRUFileName[3]);
	miFileMRU4.Enabled := (gSettings.MRUFileName[3] <> '');
	miFileMRU4.Visible := miFileMRU4.Enabled;
	miFileMRU5.Caption := '&5 - ' + ExtractLastFolderName(gSettings.MRUFileName[4]);
	miFileMRU5.Enabled := (gSettings.MRUFileName[4] <> '');
	miFileMRU5.Visible := miFileMRU5.Enabled;
end;





procedure TfrmMain.miMRUClick(Sender: TObject);
begin
	OpenFile(gSettings.MRUFileName[(Sender as TComponent).Tag - 1]);
end;





procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// TODO: ask the user about saving changes
	Action := caFree;
end;





procedure TfrmMain.CheckWebVersion();
var
	LevelName: string;
	AuthorName: string;
	CurrentVersion: string;
begin
	if (HasCheckedWebVersion and not(gSettings.AllowWebStats)) then
	begin
		// we have already checked once this session and we aren't reporting webstats, so bail out.
		Exit;
	end;
	LevelName := '';
	AuthorName := '';
	CurrentVersion := '';
	if (gSettings.AllowWebStats) then
	begin
		LevelName := Level.LevelName;
		AuthorName := Level.AuthorName;
		CurrentVersion := GetCurrentVersion();
	end;
	WebVersionCheckThread := TWebVersionCheckThread.Create(OnWebVersionReceived, LevelName, AuthorName, CurrentVersion);
end;





procedure TfrmMain.OnWebVersionReceived(Sender: TObject; WebVersion: string);
begin
	HasCheckedWebVersion := true;
	if (IsVersionHigherThanCurrent(WebVersion)) then
	begin
		if (MessageDlg('There is a newer version on the web: ' + WebVersion + #13#10#13#10'Would you like to go to the download page?', mtConfirmation, mbOKCancel, 0) = mrOK) then
		begin
			OpenUrl('http://xoft.cz/KSLC');
		end;
	end;
end;





function TfrmMain.GetCurrentVersion(): string;
var
	maj, min, rel, build: word;
	IsDebug: boolean;
begin
	//*h not compatible yet  
    (*
	if (ReadVersionInfo(ParamStr(0), maj, min, rel, build)) then
	begin
		Result := IntToStr(maj) + '.' + IntToStr(min) + '.' + IntToStr(rel) + '.' + IntToStr(build);
	end
	else*)
	begin
		Result := 'unknown';
	end;
end;





function TfrmMain.IsVersionHigherThanCurrent(iVersion: string): boolean;
var
	cmaj, cmin, crel, cbui: word;
	vmaj, vmin, vrel, vbui: word;
	IsDebug: boolean;
begin
    Exit(false);
	//*h not compatible yet
	(*
    If not(ParseVersionString(iVersion, vmaj, vmin, vrel, vbui)) then
	begin
		// unreadable version from the web, never update
		Result := false;
		Exit;
	end;

	if not(ReadVersionInfo(ParamStr(0), cmaj, cmin, crel, cbui, IsDebug)) then
	begin
		// unreadable version info in file, always update
		Result := true;
		Exit;
	end;
	Result := (cmaj < vmaj) or
		((cmaj = vmaj) and (cmin < vmin)) or
		((cmaj = vmaj) and (cmin = vmin) and (crel < vrel)) or
		((cmaj = vmaj) and (cmin = vmin) and (crel = vrel) and (cbui < vbui));
	*)
end;





procedure TfrmMain.actViewShiftsExecute(Sender: TObject);
begin
	frmShiftList.Show();
end;





procedure TfrmMain.actFileOpenInstalledExecute(Sender: TObject);
var
	dlg: TdlgInstalledLevelList;
begin
	dlg := TdlgInstalledLevelList.Create(nil);
	try
		case dlg.ShowModal() of
			mrKSDirNotFound:
			begin
				ShowMessage('KS folder not found. Please use View->Settings to set KS folder.'#13#10#13#10'Normal file open will commence now.');
				actFileOpenExecute(Sender);
				Exit;
			end;

			mrKSDirNoWorlds:
			begin
				ShowMessage('There are no levels installed in the KS folder. Please use View->Settings to check and set KS folder.'#13#10#13#10'Normal file open will commence now.');
				actFileOpenExecute(Sender);
				Exit;
			end;

			mrCancel:
			begin
				Exit;
			end;
		end;

		OpenFile(dlg.Path + 'Map.bin');
	finally
		dlg.Release();
	end;
end;





procedure TfrmMain.actFollowShiftExecute(Sender: TObject);
var
	i: integer;
	sl: TKSShiftList;
begin
	if not(Assigned(CurrentRoom)) then
	begin
		Exit;
	end;

	sl := Level.ShiftList.ExtractSingleFromRoom(CurrentRoom.XPos, CurrentRoom.YPos);
	try
		for i := 0 to sl.NumShifts - 1 do
		begin
			if (sl.Shift[i].Kind = (chr(ord('A') + (Sender as TComponent).Tag))) then
			begin
				GotoRoom(sl.Shift[i].ToRoomX, sl.Shift[i].ToRoomY);
				Exit;
			end;
		end;
	finally
		sl.Free();
	end;
end;





procedure TfrmMain.actViewShiftsLeadingHereExecute(Sender: TObject);
begin
	frmShiftsToHere.Show();
end;





procedure TfrmMain.OnLevelChanged(Sender: TObject);
begin
	UpdateStatusbar();
end;





procedure TfrmMain.actViewLayExecute(Sender: TObject);
var
	v: boolean;
	idx: integer;
begin
	if not(Sender is TAction) then
	begin
		Exit;
	end;
	v := not(TAction(Sender).Checked);
	TAction(Sender).Checked := v;
	idx := TComponent(Sender).Tag;
	rvMain.LayerVisible[idx] := v;
end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
	pcMain.ActivePage.SetFocus;
end;




procedure TfrmMain.tlLayerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) then
	begin
		EditLayer := TComponent(Sender).Tag;
	end;
end;





procedure TfrmMain.fSetEditLayer(iVal: integer);
begin
	fEditLayer := iVal;
	RedrawImgLayers();
end;





procedure TfrmMain.fSetCurrentAction(iVal: TCurrentAction);
begin
	fCurrentAction := iVal;
	tvMainA.Visible := (iVal = caInsert);
	tvMainB.Visible := (iVal = caInsert);
	lTilesetA.Visible := (iVal = caInsert);
	lTilesetB.Visible := (iVal = caInsert);
	imgTileBg.Visible := (iVal = caInsert);
	imgPowerups.Visible := ((iVal = caSetStartPos) or (iVal = caTestLevel));
	lPowerupSelect.Visible := imgPowerups.Visible;
end;





procedure TfrmMain.rvMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	TileCoords, PixelCoords: TPoint;
	Room: TKSRoom;
	InCurrentRoom: boolean;
	txt: string;
begin
	InCurrentRoom := rvMain.CanvasToLogical(Point(X, Y), TileCoords, PixelCoords, Room);
	if Assigned(Room) then
	begin
		txt := '[' + IntToStr(TileCoords.X) + ', ' + IntToStr(TileCoords.Y) + '], (' + intToStr(PixelCoords.X) + ', ' + IntToStr(PixelCoords.Y) + ')';
	end
	else
	begin
		txt := 'No hit';
	end;
	sbMain.Panels[4].Text := txt;

	UpdateInsertionVector();

	if (IsRVMouseDown) then
	begin
		case CurrentAction of
			caInsert:
			begin
				InsertTiles(TileCoords, InCurrentRoom);
			end;

			caSetStartPos:
			begin
				SetStartPos(TileCoords);
			end;

			caTestLevel:
			begin
				TestLevelSetPos(TileCoords);
			end;
		end;
	end;
end;





procedure TfrmMain.InsertTiles(iTileCoords: TPoint; iInCurrentRoom: boolean);
var
	HasChanged: boolean;
	Top, Left, Bottom, Right: integer;
	tx, ty: integer;		// tileset coords
	cx, cy: integer;		// room tile coords
	curtile: integer;
	Lay: PKSObjLayer;
begin
	if ((iTileCoords.X < 0) or (iTileCoords.Y < 0) or (iTileCoords.X >= 25) or (iTileCoords.Y >= 10)) then
	begin
		Exit;
	end;

	if (EditLayer <= 3) then
	begin
		if (Assigned(TilesetSelection) and iInCurrentRoom) then
		begin
			// Paste tileset selection into EditLayer:
			Top := TilesetSelection.Top div 24;
			Left := TilesetSelection.Left div 24;
			Bottom := TilesetSelection.Bottom div 24;
			Right := TilesetSelection.Right div 24;
			HasChanged := false;
			for ty := Top to Bottom - 1 do
			begin
				cy := iTileCoords.Y + ty - Top;
				if ((cy < 0) or (cy > 9)) then
				begin
					continue;
				end;
				for tx := Left to Right - 1 do
				begin
					cx := iTileCoords.X + tx - Left;
					if ((cx < 0) or (cx > 24)) then
					begin
						continue;
					end;
					curtile := CurrentTileset * 128 + ty * 16 + tx;
					if (CurrentRoom.Data.Tile[EditLayer].Tile[cy, cx] <> curtile) then
					begin
						CurrentRoom.Data.Tile[EditLayer].Tile[cy, cx] := curtile;
						HasChanged := true;
					end;
				end;		// for x
			end;		// for y
			if (HasChanged) then
			begin
				if (EditLayer = 3) then
				begin
					CurrentRoom.FreePassable();
					// Room.UpdatePassable();
				end;
				fLevel.Changed();
			end;
		end;
	end
	else
	begin
		// Paste object selection into EditLayer
		Lay := @(CurrentRoom.Data.Obj[EditLayer]);
		if (
			(Lay.Bank[iTileCoords.Y, iTileCoords.x] <> ocMain.Bank) or
			(Lay.Obj[iTileCoords.Y,  iTileCoords.x] <> ocMain.Obj)
		) then
		begin
			Lay.Bank[iTileCoords.Y, iTileCoords.x] := ocMain.Bank;
			Lay.Obj[iTileCoords.Y,  iTileCoords.x] := ocMain.Obj;
			CurrentRoom.FreePassable();
			fLevel.Changed();
		end;
	end;
end;





procedure TfrmMain.SetStartPos(iTileCoords: TPoint);
var
	i: integer;
begin
	try
		if (not(Assigned(fLevel)) or not(Assigned(CurrentRoom))) then
		begin
			Exit;
		end;

		fLevel.StartRoomX := CurrentRoom.XPos;
		fLevel.StartRoomY := CurrentRoom.YPos;
		fLevel.StartX := iTileCoords.X;
		fLevel.StartY := iTileCoords.Y;
		for i := 0 to 11 do
		begin
			fLevel.StartPower[i] := PowerupSelected[i];
		end;
		fLevel.Changed();
		UpdateStartPos();
	finally
		CurrentAction := caInsert;
		IsRVMouseDown := false;
	end;
end;


procedure ShellExecSimple(exename: string; arguments: array of string; startdir: string);
var
	proc: TProcess;
        i: integer;
begin
	proc := TProcess.Create(nil);
	try
		proc.Executable := exename;
                for i:= 0 to Length(arguments)-1 do
                begin
                     proc.Parameters.Add(arguments[i]);
                end;
                proc.CurrentDirectory:=startdir;

		proc.Execute;
	finally
		proc.Free;
	end;
end;


procedure TfrmMain.TestLevelSetPos(iTileCoords: TPoint);
var
	fnam: string;
	f: TextFile;
	i: integer;
begin
	try
		if not(Assigned(fLevel)) then
		begin
			Exit;
		end;
		fLevel.SaveToFile(fLevel.FileName);

		fnam := ConcatPaths([gKSDir,'Saves','TestLevel.temp']);
		AssignFile(f, fnam);
		try
			Rewrite(f);
		except
			ShowMessage('Cannot open file "' + fnam + '" for writing. Check for folder existence and permissions, then retry.');
			Exit;
		end;
		try
			WriteLn(f, '[Positions]');
			WriteLn(f, 'X Map=', CurrentRoom.XPos);
			WriteLn(f, 'Y Map=', CurrentRoom.YPos);
			WriteLn(f, 'X Pos=', iTileCoords.X);
			WriteLn(f, 'Y Pos=', iTileCoords.Y);
			WriteLn(f, '[World]');
			WriteLn(f, 'World Folder=', ExtractLastFolderName(ExtractFilePath(fLevel.FileName)));
			WriteLn(f, '[Powers]');
			for i := 0 to 11 do
			begin
				if (PowerupSelected[i]) then
				begin
					WriteLn(f, 'Power', i, '=1');
				end
				else
				begin
					WriteLn(f, 'Power', i, '=0');
				end;
			end;
		finally
			CloseFile(f);
		end;
        {$IFDEF WINDOWS}
        ShellExecSimple( gKSDir + 'Knytt Stories.exe', ['-Mode=Test'], gKSDir);
        {$ENDIF}
        {$IFDEF UNIX}
		ShellExecSimple( 'wine', [gKSDir + 'Knytt Stories.exe','-Mode=Test'], gKSDir);
        {$ENDIF}
	finally
		CurrentAction := caInsert;
		IsRVMouseDown := false;		// to prevent drawing tiles upon mouseup / another mousemove
	end;
end;





procedure TfrmMain.imgLayersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	i, Count: integer;
	Lay: integer;
begin
	if (
		((Button = mbLeft) and (ssRight in Shift)) or
		((Button = mbRight) and (ssLeft in Shift))
	)
	then
	begin
		Button := mbMiddle;
	end;
	
	Lay := (X div 24) - 1;
	case (Button) of
		mbLeft:
		begin
			if ((Lay >= 0) and (Lay <= 7)) then
			begin
				EditLayer := Lay;
				RedrawImgLayers();
			end;
		end;

		mbRight:
		begin
			rvMain.LayerVisible[Lay] := not(rvMain.LayerVisible[Lay]);
			actLayerVis[Lay].Checked := not(actLayerVis[Lay].Checked);
			RedrawImgLayers();
		end;

		mbMiddle:
		begin
			Count := 0;
			for i := -1 to 8 do
			begin
				if (rvMain.LayerVisible[i]) then
				begin
					Count := Count + 1;
				end;
			end;
			if (Count > 1) then
			begin
				// Solo
				for i := -1 to 8 do
				begin
					rvMain.LayerVisible[i] := (i = Lay);
				end;
			end
			else
			begin
				// Un-Solo:
				for i := -1 to 7 do
				begin
					rvMain.LayerVisible[i] := true;
				end;
				rvMain.LayerVisible[8] := false;
			end;
			RedrawImgLayers();
		end;
	end;
end;





procedure TfrmMain.RedrawImgLayers();
var
	bmp: TBitmap;

	procedure DrawButton(x, y: integer; iVis: boolean; iEdit: boolean; Caption: string);
	var
		Color: TColor;
	begin
		if (iVis) then
		begin
			if (iEdit) then
			begin
				Color := $ff;
			end
			else
			begin
				Color := $ffff;
			end
		end
		else
		begin
			if (iEdit) then
			begin
				Color := $bfbfff;
			end
			else
			begin
				Color := clBtnFace;
			end;
		end;
		bmp.Canvas.Font.Color:=ColorToRGB(clBlack);
		bmp.Canvas.TextOut(x - 1, y, Caption);
		bmp.Canvas.TextOut(x + 1, y, Caption);
		bmp.Canvas.TextOut(x, y - 1, Caption);
		bmp.Canvas.TextOut(x, y + 1, Caption);
		bmp.Canvas.Font.Color:=ColorToRGB(Color);
		bmp.Canvas.TextOut(x, y, Caption);
	end;

const
	LayerCaption: array[-1..8] of string = ('B', '0', '1', '2', '3', '4', '5', '6', '7', 'P');
var
	i: integer;
	x, y: integer;
begin
	bmp := imgLayers.Picture.Bitmap;
	bmp.Width := imgLayers.Width;
	bmp.Height := imgLayers.Height;
	bmp.Canvas.Brush.Color := clBtnFace;
	bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
	bmp.Canvas.Brush.Style:=bsClear;
	bmp.Canvas.Font.Name := 'Arial';
	bmp.Canvas.Font.Size := -20;
	bmp.Canvas.Font.Style := [fsBold];
	y := (24 - bmp.Canvas.TextHeight('B')) div 2;
	DrawButton(12, y, rvMain.BackgroundVisible, false, 'B');
	for i := 0 to 8 do
	begin
		x := i * 24 + 36;
		DrawButton(x, y, rvMain.LayerVisible[i], (i = fEditLayer), LayerCaption[i]);
	end;
end;





procedure TfrmMain.UpdateInsertionVector();
var
	pnt: TPoint;
	InCurrentRoom: boolean;
	TileCoords, PixCoords, ZeroPt: TPoint;
	Room: TKSRoom;
	vis: boolean;
begin
	vis := false;
	if (Assigned(TilesetSelection)) then
	begin
        pnt := rvMain.ScreenToClient(Mouse.CursorPos);
		InCurrentRoom := rvMain.CanvasToLogical(pnt, TileCoords, PixCoords, Room);
		if (InCurrentRoom) then
		begin
			ZeroPt.X := 0;
			ZeroPt.Y := 0;
			vis := rvMain.LogicalToCanvas(TileCoords, ZeroPt, pnt);
			rvvInsertion.Top := pnt.Y;
			rvvInsertion.Left := pnt.X;
			rvvInsertion.Bottom := rvvInsertion.Top + TilesetSelection.Bottom - TilesetSelection.Top;
			rvvInsertion.Right  := rvvInsertion.Left + TilesetSelection.Right - TilesetSelection.Left;
		end;
	end;
	rvvInsertion.Visible := vis;
	rvvInsertion.Changed();
end;





procedure TfrmMain.FreeTilesetSelection();
begin
	if (Assigned(TilesetSelection)) then
	begin
		TilesetSelection.Free();
		TilesetSelection := nil;
	end;
end;





procedure TfrmMain.lTilesetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	idx: integer;
	ts: integer;
	Add: integer;
begin
	if not(Assigned(CurrentRoom)) then
	begin
		Exit;
	end;

	case (Button) of
		mbLeft: Add := 1;
		mbRight: Add := -1;
		else Add := 0;
	end;
	if (ssShift in Shift) then
	begin
		Add := Add * 8;
	end;

	idx := TComponent(Sender).Tag;
	ts := (256 + CurrentRoom.Tileset[idx].Number + add) mod 256;
	if (idx = 0) then
	begin
		CurrentRoom.Data.TilesetA := ts;
	end
	else
	begin
		CurrentRoom.Data.TilesetB := ts;
	end;
	CurrentRoom.Tileset[idx] := fLevel.Tileset[ts];
	if not(Assigned(fLevel.Tileset[ts].Img)) then
	begin
		fLevel.Tileset[ts].Load(fLevel.LevelDir);
	end;
	CurrentRoom.FreePassable();
	CurrentRoom.UpdatePassable();

	// update tvMain:
	tvMainA.Tileset := CurrentRoom.Tileset[0];
	tvMainB.Tileset := CurrentRoom.Tileset[1];
	rvMain.InvalidateTileset();

	// update labels:
	lTilesetA.Caption := 'Tileset: ' + IntToStr(CurrentRoom.Data.TilesetA);
	lTilesetB.Caption := 'Tileset: ' + IntToStr(CurrentRoom.Data.TilesetB);
end;





procedure TfrmMain.tvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	IsTilesetMouseDown := true;
	TilesetMouseDown := Point(X, Y);
	tvMainMouseMove(Sender, Shift, X, Y);
end;





procedure TfrmMain.tvMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	top, left, bottom, right: integer;
begin
	if (IsTilesetMouseDown) then
	begin
		if ((abs(TilesetMouseDown.X - X) > 4) or (abs(TilesetMouseDown.Y - Y) > 4)) then
		begin
			if not(Assigned(TilesetSelection)) then
			begin
				TilesetSelection := TVGRect.Create(0, 0, 0, 0, clRed);
			end;
		end;
		if (Assigned(TilesetSelection)) then
		begin
			if (Sender = tvMainA) then
			begin
				tvMainA.RegUniqueVector(TilesetSelection);
				tvMainB.RemVector(TilesetSelection);
				CurrentTileset := 0;
			end
			else
			begin
				tvMainA.RemVector(TilesetSelection);
				tvMainB.RegUniqueVector(TilesetSelection);
				CurrentTileset := 1;
			end;
			if (Y < 0) then
			begin
				Y := 0;
			end;
			if (Y > 191) then
			begin
				Y := 191;
			end;
			if (X < 0) then
			begin
				X := 0;
			end;
			if (X > 383) then
			begin
				X := 383;
			end;
			if (Y > TilesetMouseDown.Y) then
			begin
				top := TilesetMouseDown.Y;
				bottom := Y;
			end
			else
			begin
				bottom := TilesetMouseDown.Y;
				top := Y;
			end;
			if (X > TilesetMouseDown.X) then
			begin
				left := TilesetMouseDown.X;
				right := X;
			end
			else
			begin
				right := TilesetMouseDown.X;
				left := X;
			end;
			TilesetSelection.Top    := (top div 24) * 24;
			TilesetSelection.Left   := (left div 24) * 24;
			TilesetSelection.Bottom := ((bottom + 24) div 24) * 24;
			TilesetSelection.Right  := ((right + 24) div 24) * 24;
			TilesetSelection.Changed();
		end;
	end;
end;





procedure TfrmMain.tvMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (IsTilesetMouseDown) then
	begin
		if not(Assigned(TilesetSelection)) then
		begin
			TilesetSelection := TVGRect.Create(0, 0, 0, 0, clRed);
		end;
		tvMainMouseMove(Sender, Shift, X, Y);
		IsTilesetMouseDown := false;
		// TODO: select single tile
	end;
end;





procedure TfrmMain.rvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	IsRVMouseDown := (Button = mbLeft);
	rvMainMouseMove(Sender, Shift, X, Y);
end;





procedure TfrmMain.rvMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbLeft) then
	begin
		rvMainMouseMove(Sender, Shift, x, y);
		IsRVMouseDown := false;
	end;
end;





procedure TfrmMain.MapGotoRoom(Sender: TObject; iX, iY: Integer);
begin
	GotoRoom(iX, iY);
end;





procedure TfrmMain.mvMainLargeGoToRoom(Sender: TObject; iX, iY: Integer);
begin
	GotoRoom(iX, iY);
	pcMain.ActivePage := tsTiles;
end;





procedure TfrmMain.mKnyttScriptChange(Sender: TObject);
begin
	if (UpdatingProps or not(Assigned(CurrentRoom))) then
	begin
		Exit;
	end;
	CurrentRoom.SetEventParams(mKnyttScript.Lines);
end;





procedure TfrmMain.pmiDuplicateClick(Sender: TObject);
var
	dlg: TdlgDuplicateRooms;
	Sel: TList;
begin
	if not(Assigned(fLevel)) then
	begin
		Exit;
	end;
	
	dlg := TdlgDuplicateRooms.Create(Application, fLevel);
	try
		if (pcMain.ActivePage = tsLargeMap) then
		begin
			Sel := mvMainLarge.Selection;
		end
		else
		begin
			sel := mvMainSmall.Selection;
		end;
		dlg.MapView.Selection.Assign(Sel);
		dlg.ShowModal();
	finally
		dlg.Free();
	end;
end;





procedure TfrmMain.pmiChangeParamsClick(Sender: TObject);
var
	dlg: TdlgMultiRoomParam;
	Sel: TList;
begin
	if not(Assigned(fLevel)) then
	begin
		Exit;
	end;
	
	dlg := TdlgMultiRoomParam.Create(Self, fLevel);
	try
		if (pcMain.ActivePage = tsLargeMap) then
		begin
			Sel := mvMainLarge.Selection;
		end
		else
		begin
			sel := mvMainSmall.Selection;
		end;
		dlg.MapView.Selection.Assign(Sel);
		dlg.ShowModal();
	finally
		dlg.Free();
	end;
end;





procedure TfrmMain.mvMainSmallRoomSelectionChanged(Sender: TObject);
begin
	if (UpdatingSelection) then
	begin
		Exit;
	end;
	UpdatingSelection := true;
	try
		mvMainLarge.CopySelection(mvMainSmall.Selection);
	finally
		UpdatingSelection := false;
	end;
end;





procedure TfrmMain.mvMainLargeRoomSelectionChanged(Sender: TObject);
begin
	if (UpdatingSelection) then
	begin
		Exit;
	end;
	UpdatingSelection := true;
	try
		mvMainSmall.CopySelection(mvMainLarge.Selection);
	finally
		UpdatingSelection := false;
	end;
end;





procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	Handled := false;
	if (pcMain.ActivePage = tsTiles) then
	begin
		MousePos := ocMain.ScreenToClient(MousePos);
		if (
			(MousePos.X >= 0) and
			(MousePos.X <= ocMain.Width) and
			(MousePos.Y >= 0) and
			(MousePos.Y <= ocMain.Height)
		) then
		begin
			Handled := ocMain.DoMouseWheel(Shift, WheelDelta, MousePos);
		end;
	end;
end;





procedure TfrmMain.mKnyttScriptSmallChange(Sender: TObject);
begin
	if (UpdatingProps or not(Assigned(CurrentRoom))) then
	begin
		Exit;
	end;
	CurrentRoom.SetEventParams(mKnyttScript.Lines);
end;





procedure TfrmMain.FixPositions();
begin
	tvMainA.Top := tsTiles.ClientHeight - tvMainA.Height;
	tvMainA.Left := 0;
	tvMainB.Top := tvMainA.Top;
	tvMainB.Left := tvMainA.Left + tvMainA.Width + 1;
	lTilesetA.Top := tvMainA.Top - lTilesetA.Height;
	lTilesetB.Top := lTilesetA.Top;
	mKnyttScriptSmall.Height := tvMainB.Height;
	mKnyttScriptSmall.Top := tvMainB.Top;
	mKnyttScriptSmall.Left := tvMainB.Left + tvMainB.Width + 1;
end;





procedure TfrmMain.actGotoX1000Y1000Execute(Sender: TObject);
begin
	GotoRoom(1000, 1000);
end;





procedure TfrmMain.actGotoLevelStartExecute(Sender: TObject);
begin
	if not(Assigned(fLevel)) then
	begin
		Exit;
	end;

	GotoRoom(fLevel.StartRoomX, fLevel.StartRoomY);
end;





procedure TfrmMain.OnLogUpdate(Sender: TObject);
begin
	mLog.Lines.AddStrings(gLog.Items);
end;





procedure TfrmMain.imgPowerupsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	idx: integer;
begin
	idx := x div 24;
	if ((idx < 0) or (idx > 11)) then
	begin
		Exit;
	end;
	PowerupSelected[idx] := not(PowerupSelected[idx]);
	RedrawImgPowerups();
end;





procedure TfrmMain.RedrawImgPowerups();
var
	i: integer;
	obj: TKSObject;
	Canvas: TCanvas;
begin
	imgPowerups.Picture.Bitmap.Width := imgPowerups.Width;
	imgPowerups.Picture.Bitmap.Height := imgPowerups.Height;
	Canvas := imgPowerups.Picture.Bitmap.Canvas;
	for i := 0 to 11 do
	begin
		if (PowerupSelected[i]) then
		begin
			Canvas.Brush.Color := $007fff;
		end
		else
		begin
			Canvas.Brush.Color := clBtnFace;
		end;
		Canvas.FillRect(Rect(i * 24, 0, i * 24 + 24, 24));
		
		obj := gKSObjects.GetObject(0, PowerupIndices[i]);
		if not(Assigned(obj)) then
		begin
			continue;
		end;
		obj.NeedImage();
		if not(Assigned(obj.Img)) then
		begin
			continue;
		end;
		obj.Img.Draw(Canvas,i*24,0)
	end;
end;





procedure TfrmMain.actToolsTestLevelExecute(Sender: TObject);
begin
	CurrentAction := caTestLevel;
end;





procedure TfrmMain.actToolsSetStartPosExecute(Sender: TObject);
begin
	CurrentAction := caSetStartPos;
end;





end.
