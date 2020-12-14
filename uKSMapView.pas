
unit uKSMapView;

{$MODE Delphi}

interface

uses
	Classes,
	Graphics,
	Controls,
	Types,
	uVectors,
	uKSRepresentations;





type
	TMapStyle = (msOriginal, msRendered);
	TGoToXYEvent = procedure(Sender: TObject; iX, iY: integer) of object;

	TKSMapView = class(TGraphicControl)
	protected
		fLevel: TKSLevel;
		fMapStyle: TMapStyle;
		fXPos, fYPos: integer;
		fSelection: TList;
		fHighlightCenter: boolean;
		fAllowMultiSelect: boolean;
		
		fOnGoToRoom: TGoToXYEvent;
		fOnRoomSelectionChanged: TNotifyEvent;
		BeginX, BeginY: integer;
		EndX, EndY: integer;
		IsMouseDown: boolean;
		SelectionRect: TVGRect;

		NumVectors: integer;
		Vector: array of TVGObject;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure fSetLevel(iLevel: TKSLevel);
		procedure fSetMapStyle(iMapStyle: TMapStyle);
		procedure fSetXPos(iXPos: integer);
		procedure fSetYPos(iYPos: integer);
		procedure fSetSelection(iSelection: TList);
		procedure fSetHighlightCenter(iHighlightCenter: boolean);
		procedure fSetAllowMultiSelect(iAllowMultiSelect: boolean);

		procedure PaintOriginal();
		procedure PaintRendered();

		procedure GoToRoom(iX, iY: integer);

		procedure OnLevelChanged(Sender: TObject);

		procedure Paint(); override;

		procedure SelectRoomsInRect(x1, y1, x2, y2: integer);
		procedure RoomSelectionChanged();

	public

		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;

		procedure RegVector(iVector: TVGObject);
		procedure RemVector(iVector: TVGObject);

		procedure GoToCoord(iXPos, iYPos: integer);

		procedure CopySelection(iSrc: TList);

		function CanvasXToRoomX(x: integer): integer;
		function CanvasYToRoomY(y: integer): integer;
		function RoomXToCanvasX(x: integer): integer;
		function RoomYToCanvasY(y: integer): integer;

		property Level: TKSLevel read fLevel write fSetLevel stored false;
		property Selection: TList read fSelection write fSetSelection stored false;
		property XPos: integer read FXPos write fSetXPos;
		property YPos: integer read FYPos write fSetYPos;

	published

		property MapStyle: TMapStyle read fMapStyle write fSetMapStyle;
		property HighlightCenter: boolean read fHighlightCenter write fSetHighlightCenter;
		property AllowMultiSelect: boolean read fAllowMultiSelect write fSetAllowMultiSelect;

		property OnGoToRoom: TGoToXYEvent read fOnGoToRoom write fOnGoToRoom;
		property OnRoomSelectionChanged: TNotifyEvent read fOnRoomSelectionChanged write fOnRoomSelectionChanged;

		property Align;
		property Anchors;
		property PopupMenu;

		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
	end;





procedure Register();



















implementation





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// globals:

procedure Register();
begin
	RegisterComponents('KSLC', [TKSMapView]);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSMapView:

constructor TKSMapView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FHighlightCenter := true;
	FAllowMultiSelect := true;
	NumVectors := 0;
	Selection := TList.Create();
	SelectionRect := TVGRect.Create(0, 0, 0, 0, $7fff00);
	RegVector(SelectionRect);
end;





destructor TKSMapView.Destroy();
begin
	Selection.Free();
	Selection := nil;
	// do NOT destroy individual vectors, they are owned by clients
	SetLength(Vector, 0);
	inherited Destroy();
end;





procedure TKSMapView.fSetLevel(iLevel: TKSLevel);
begin
	Selection.Clear();
	fLevel := iLevel;
	fLevel.ChangedListeners.Add(OnLevelChanged);
	Invalidate();
end;





procedure TKSMapView.fSetMapStyle(iMapStyle: TMapStyle);
begin
	fMapStyle := iMapStyle;
	Invalidate();
end;





procedure TKSMapView.fSetXPos(iXPos: integer);
begin
	fXPos := iXPos;
	Invalidate();
end;





procedure TKSMapView.fSetYPos(iYPos: integer);
begin
	fYPos := iYPos;
	Invalidate();
end;





procedure TKSMapView.fSetSelection(iSelection: TList);
begin
	fSelection := iSelection;
	Invalidate();
end;





procedure TKSMapView.fSetHighlightCenter(iHighlightCenter: boolean);
begin
	fHighlightCenter := iHighlightCenter;
	Invalidate();
end;





procedure TKSMapView.fSetAllowMultiSelect(iAllowMultiSelect: boolean);
begin
	fAllowMultiSelect := iAllowMultiSelect;
	Invalidate();
end;





procedure TKSMapView.Paint();
begin
	case fMapStyle of
		msOriginal: PaintOriginal();
		msRendered: PaintRendered();
	end;
end;





procedure TKSMapView.PaintOriginal();
var
	bmp: TBitmap;		// offscreen bitmap
	ox, oy: integer;		// offsets of the grid lines from [0, 0]
	x, y: integer;
	r: TRect;
	Wid, Hei: integer;
	HalfWid, HalfHei: integer;
	i: integer;
begin
	if not(Assigned(fLevel)) then Exit;
	Wid := Self.Width;
	Hei := Self.Height;
	HalfWid := Wid div 2;
	HalfHei := Hei div 2;
	bmp := TBitmap.Create();
	try
		bmp.Width := Wid;
		bmp.Height := Hei;
		bmp.PixelFormat := pf32bit;
		bmp.Canvas.Brush.Color := $87663a;
		bmp.Canvas.Brush.Style := bsSolid;
		bmp.Canvas.Pen.Color := $705530;
		r.Top := 0;
		r.Left := 0;
		r.Bottom := Hei;
		r.Right := Wid;
		bmp.Canvas.FillRect(r);
		
		ox := (Wid div 2) mod 8;
		oy := (Hei div 2) mod 8;
		x := ox;
		while (x < Wid) do
		begin
			bmp.Canvas.MoveTo(x, 0);
			bmp.Canvas.LineTo(x, Hei);
			x := x + 8;
		end;
		y := oy;
		while (y < Hei) do
		begin
			bmp.Canvas.MoveTo(0, y);
			bmp.Canvas.LineTo(Wid, y);
			y := y + 8;
		end;

		bmp.Canvas.Brush.Color := $f3c7ab;
		for i := 0 to Level.NumRooms - 1 do
		begin
			x := (Level.Room[i].XPos - FXPos) * 8 + HalfWid;
			y := (Level.Room[i].YPos - FYPos) * 8 + HalfHei;
			if (x <= -8) then continue;
			if (y <= -8) then continue;
			if (x > Wid) then continue;
			if (y > Hei) then continue;
			r.Top := y + 1;
			r.Left := x + 1;
			r.Bottom := y + 8;
			r.Right := x + 8;
			bmp.Canvas.FillRect(r);
			(*
			bmp.Canvas.MoveTo(x, y + 8);
			bmp.Canvas.LineTo(x + 7, y + 8);
			bmp.Canvas.LineTo(x + 7, y);
			*)
		end;

		if (FAllowMultiSelect and Assigned(Selection)) then
		begin
			bmp.Canvas.Brush.Color := $00ffff;
			for i := 0 to Selection.Count - 1 do
			begin
				x := (TKSRoom(Selection[i]).XPos - FXPos) * 8 + HalfWid;
				y := (TKSRoom(Selection[i]).YPos - FYPos) * 8 + HalfHei;
				if (x <= -8) then continue;
				if (y <= -8) then continue;
				if (x > Wid) then continue;
				if (y > Hei) then continue;
				r.Top := y + 1;
				r.Left := x + 1;
				r.Bottom := y + 8;
				r.Right := x + 8;
				bmp.Canvas.FillRect(r);
			end;
		end;

		if (FHighlightCenter) then
		begin
			bmp.Canvas.Brush.Color := $ffffff;
			r.Top := HalfHei;
			r.Left := HalfWid;
			r.Bottom := HalfHei + 8;
			r.Right := HalfWid + 8;
			bmp.Canvas.FrameRect(r);
		end;

		for i := 0 to NumVectors - 1 do
		begin
			Vector[i].Draw(bmp.Canvas);
		end;

		Self.Canvas.Brush.Color := $f3c7ab;
		// Self.Canvas.StretchDraw(Rect(0, 0, Wid, Hei), bmp);
		Self.Canvas.Draw(0, 0, bmp);
	finally
		bmp.Free();
	end;
end;





procedure TKSMapView.PaintRendered();
begin
	// TODO: paint the rendered view of the map
end;





procedure TKSMapView.RegVector(iVector: TVGObject);
begin
	SetLength(Vector, NumVectors + 1);
	Vector[NumVectors] := iVector;
	NumVectors := NumVectors + 1;
end;





procedure TKSMapView.RemVector(iVector: TVGObject);
var
	i: integer;
begin
	for i := 0 to NumVectors - 1 do
	begin
		if (Vector[i] = iVector) then
		begin
			Vector[i] := Vector[NumVectors - 1];
			NumVectors := NumVectors - 1;
			Exit;
		end;
	end;
end;





procedure TKSMapView.GoToCoord(iXPos, iYPos: integer);
begin
	fXPos := iXPos;
	fYPos := iYPos;
	Invalidate();
end;





procedure TKSMapView.CopySelection(iSrc: TList);
begin
	Selection.Clear();
	Selection.Assign(iSrc);
	RoomSelectionChanged();
	Invalidate();
end;





function TKSMapView.CanvasXToRoomX(x: integer): integer;
var
	ox: integer;
begin
	case MapStyle of
		msOriginal:
		begin
			ox := x - Self.Width div 2;
			Result := FXPos + ox div 8;
			if (ox < 0) then
			begin
				// correct for negative division
				Result := Result - 1;
			end;
		end;
		msRendered:
		begin
			// TODO: rendered view conversion
			Result := -2;
		end;
		else
			Result := -1;
	end;
end;





function TKSMapView.CanvasYToRoomY(y: integer): integer;
var
	oy: integer;
begin
	case MapStyle of
		msOriginal:
		begin
			oy := y - Self.Height div 2;
			Result := FYPos + oy div 8;
			if (oy < 0) then
			begin
				// correct for negative division
				Result := Result - 1;
			end;
		end;
		msRendered:
		begin
			// TODO: rendered view conversion
			Result := -2;
		end;
		else
			Result := -1;
	end;
end;





function TKSMapView.RoomXToCanvasX(x: integer): integer;
begin
	case MapStyle of
		msOriginal:
		begin
			Result := (x - FXPos) * 8 + Self.Width div 2;
		end;
		else
			Result := -1;
	end;
end;





function TKSMapView.RoomYToCanvasY(y: integer): integer;
begin
	case MapStyle of
		msOriginal:
		begin
			Result := (y - FYPos) * 8 + Self.Height div 2;
		end;
		else
			Result := -1;
	end;
end;





procedure TKSMapView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
	case Button of
		mbLeft:
		begin
			IsMouseDown := true;
			if (FAllowMultiSelect) then
			begin
				SelectionRect.Visible := true;
				BeginX := CanvasXToRoomX(X);
				BeginY := CanvasYToRoomY(Y);
			end;
			MouseMove(Shift, X, Y);
		end;
	end;

	inherited MouseDown(Button, Shift, X, Y);
end;





procedure TKSMapView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	x1, y1, x2, y2: integer;
begin
	x1 := CanvasXToRoomX(X) + 1;
	y1 := CanvasYToRoomY(Y) + 1;
	if ((EndX = x1) and (EndY = y1)) then
	begin
		Exit;
	end;
	EndX := x1;
	EndY := y1;
	if (fAllowMultiSelect and IsMouseDown and (ssLeft in Shift)) then
	begin
		// update the selection rectangle:
		if (BeginY > EndY) then
		begin
			y2 := BeginY;
			y1 := EndY;
		end
		else
		begin
			y1 := BeginY;
			y2 := EndY;
		end;
		if (BeginX > EndX) then
		begin
			x2 := BeginX;
			x1 := EndX;
		end
		else
		begin
			x1 := BeginX;
			x2 := EndX;
		end;
		SelectionRect.Top    := RoomYToCanvasY(y1);
		SelectionRect.Left   := RoomXToCanvasX(x1);
		SelectionRect.Bottom := RoomYToCanvasY(y2);
		SelectionRect.Right  := RoomXToCanvasX(x2);
		Invalidate();
	end;

	inherited MouseMove(Shift, X, Y);
end;





procedure TKSMapView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	MouseMove(Shift, X, Y);
	case Button of
		mbLeft:
		begin
			if (IsMouseDown) then
			begin
				if ((EndX = BeginX + 1) and (EndY = BeginY + 1)) then
				begin
					GoToRoom(BeginX, BeginY);
				end
				else
				begin
					if (fAllowMultiSelect) then
					begin
						if not((ssShift in Shift) or (ssCtrl in Shift)) then
						begin
							Selection.Clear();
						end;
						SelectRoomsInRect(BeginX, BeginY, EndX - 1, EndY - 1);
						RoomSelectionChanged();
					end;
				end;
				IsMouseDown := False;
			end;
			if Assigned(SelectionRect) then
			begin
				SelectionRect.Visible := False;
			end;
			Invalidate();
		end;
	end;

	inherited MouseUp(Button, Shift, X, Y);
end;





procedure TKSMapView.SelectRoomsInRect(x1, y1, x2, y2: integer);
var
	i: integer;
	pom: integer;
begin
	if not(Assigned(FLevel)) then Exit;
	if not(FAllowMultiSelect) then Exit;
	if not(Assigned(Selection)) then Exit;
	if (x1 > x2) then
	begin
		pom := x2;
		x2 := x1;
		x1 := pom;
	end;
	if (y1 > y2) then
	begin
		pom := y2;
		y2 := y1;
		y1 := pom;
	end;
	for i := 0 to Level.NumRooms - 1 do
	begin
		if (Level.Room[i].XPos < x1) then continue;
		if (Level.Room[i].XPos > x2) then continue;
		if (Level.Room[i].YPos < y1) then continue;
		if (Level.Room[i].YPos > y2) then continue;
		Selection.Add(Level.Room[i]);
	end;
	if Assigned(OnRoomSelectionChanged) then
	begin
		OnRoomSelectionChanged(Self);
	end;
end;





procedure TKSMapView.RoomSelectionChanged();
begin
	if Assigned(fOnRoomSelectionChanged) then
	begin
		fOnRoomSelectionChanged(Self);
	end;
end;





procedure TKSMapView.GoToRoom(iX, iY: integer);
begin
	if (
		not(Assigned(fLevel))
		or ((iX = fXPos) and (iY = fYPos))
	) then
	begin
		Exit;
	end;
	fXPos := iX;
	fYPos := iY;
	Invalidate();
	if Assigned(fOnGoToRoom) then
	begin
		fOnGotoRoom(Self, BeginX, BeginY);
	end;
end;





procedure TKSMapView.OnLevelChanged(Sender: TObject);
begin
	Invalidate();
end;





end.
