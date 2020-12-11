
unit uKSRoomView;

interface

uses
	Classes,
	Controls,
	Forms,
	Windows,
	Graphics,
	ExtCtrls,
	Messages,
	pngimage,
	uVectors,
	uKSRepresentations;





type

	TKSRoomView = class(TGraphicControl)
	protected
		fLevel: TKSLevel;
		fRoom: TKSRoom;
		fXPos, fYPos: integer;		// when fRoom is nil, these coords are displayed in the window
		fRoomBackup: TKSRoomRec;		// contains the last-drawn data present in fLayImg, useful to decide whether to invalidate fLayImg[]
		fShowNeighbors: boolean;
		fLayWid: integer;
		fLayHei: integer;
		fBkgImg: array of integer;		// fLayWid * fLayHei
		fLayImg: array[0..8] of array of integer;		// array of int32 pixels, each counts fLayWid * fLayHei items, precalced images of the layer
		fRoomImg: array of integer;		// fLayWid * fLayHei
		fLayVis: array[0..8] of boolean;
		fBkgVis: boolean;
		fBkgColor: integer;

		fShouldRedrawLay: boolean;		// upon Paint(), fLayImg & fRoomImg must be updated first
		fShouldCheckLay: boolean;		// upon Paint(), check whether fRoomBackup contains same data (update fLayImg & fRoomImg if not)
		fShouldRecompose: boolean;		// upon Paint(), fRoomImg must be recomposed

		fNumVectors: integer;
		fVector: array of TVGObject;

		fOnMouseLeave: TNotifyEvent;
		fOnMouseEnter: TNotifyEvent;

		procedure fSetRoom(iRoom: TKSRoom);
		procedure fSetShowNeighbors(iVal: boolean);
		function  fGetLayVis(iIdx: integer): boolean;
		procedure fSetLayVis(iIdx: integer; iVal: boolean);
		procedure fSetBkgVis(iVal: boolean);

		procedure UpdateSize();		// updates fLayWid, fLayHei, Width and Height according to fShowNeighbors; sets fXXXImg array lengths

		procedure Paint(); override;

		procedure OnLevelChanged(Sender: TObject);		// only schedule an update, post a message

		procedure CheckRoomContents();		// checks fRoomBackup against fRoom.Data, updates fLayImg and fRoomImg if necessary
		procedure RedrawImgs();		// updates fBkgImg, fLayImg and fRoomImg
		procedure RedrawBkgImg();
		procedure RedrawLayImgTile (iLayer: integer);		// updates fLayImg for tile layers
		procedure RedrawLayImgObj  (iLayer: integer);		// updates fLayImg for obj layers
		procedure RedrawLayImgPass ();		// updates fLayImg[8] for passability
		procedure RedrawBkgImgR    (t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom);
		procedure RedrawLayImgTileR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom; iLayer: integer);
		procedure RedrawLayImgObjR (t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom; iLayer: integer);
		procedure RedrawLayImgPassR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom);
		procedure RecomposeRoomImg();		// recomposes fRoomImg from fBkgImg and fLayImg[], updates bmpRoom
		procedure CrossOutBox(iCanvas: TCanvas; t, l, b, r: integer);

		procedure OnVectorChanged(Sender: TObject);
		procedure OnVectorDestroying(Sender: TObject);

		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

	public

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy(); override;

		procedure RegVector(iVector: TVGObject);
		procedure RegUniqueVector(iVector: TVGObject);
		procedure RemVector(iVector: TVGObject);

		procedure InvalidateTileset();
		
		function  CanvasToLogical(iCanvasPt: TPoint; var oTileCoords: TPoint; var oPixelCoords: TPoint; var oRoom: TKSRoom): boolean;		// returns true if iCanvasPt is inside fRoom
		function  LogicalToCanvas(iTileCoords, iPixelCoords: TPoint; var oCanvasPt: TPoint): boolean;		// returns true if oCanvasPt is inside fRoom

		procedure SetCoords(iXPos, iYPos: integer);

		property Room: TKSRoom read fRoom write fSetRoom;
		property LayerVisible[idx: integer]: boolean read fGetLayVis write fSetLayVis;

	published

		property ShowNeighbors: boolean read fShowNeighbors write fSetShowNeighbors;		// also sets size!
		property BackgroundVisible: boolean read fBkgVis write fSetBkgVis;

		property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
		property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;

		property Align;
		property Anchors;
		property PopupMenu;
		
		property OnMouseDown;
		property OnMouseUp;
		property OnMouseMove;
	end;





procedure Register;


















implementation





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// globals:

procedure Register;
begin
	RegisterComponents('KSLC', [TKSRoomView]);
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





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSRoomView:

constructor TKSRoomView.Create(AOwner: TComponent);
var
	i: integer;
begin
	inherited Create(AOwner);
	for i := 0 to 7 do
	begin
		fLayVis[i] := true;
	end;
	fLayVis[8] := false;
	fBkgColor := $7f7f7f;
	fBkgVis := true;
	UpdateSize();
end;






destructor  TKSRoomView.Destroy();
var
	i: integer;
begin
	fLayWid := 0;
	fLayHei := 0;
	for i := 0 to 8 do
	begin
		SetLength(fLayImg[i], 0);
	end;
	SetLength(fRoomImg, 0);
	if (Assigned(fLevel)) then
	begin
		fLevel.ChangedListeners.Del(OnLevelChanged);
	end;
	inherited Destroy();
end;





procedure TKSRoomView.RegVector(iVector: TVGObject);
begin
	SetLength(fVector, fNumVectors + 1);
	iVector.OnChanged.Add(OnVectorChanged);
	iVector.OnDestroying.Add(OnVectorDestroying);
	fVector[fNumVectors] := iVector;
	fNumVectors := fNumVectors + 1;
	Invalidate();
end;





procedure TKSRoomView.RegUniqueVector(iVector: TVGObject);
var
	i: integer;
begin
	for i := 0 to fNumVectors - 1 do
	begin
		if (fVector[i] = iVector) then
		begin
			Exit;
		end;
	end;

	SetLength(fVector, fNumVectors + 1);
	iVector.OnChanged.Add(OnVectorChanged);
	iVector.OnDestroying.Add(OnVectorDestroying);
	fVector[fNumVectors] := iVector;
	fNumVectors := fNumVectors + 1;
	Invalidate();
end;





procedure TKSRoomView.RemVector(iVector: TVGObject);
var
	i: integer;
begin
	for i := 0 to fNumVectors - 1 do
	begin
		if (fVector[i] = iVector) then
		begin
			fVector[i] := fVector[fNumVectors - 1];
			fNumVectors := fNumVectors - 1;
			Invalidate();
			Exit;
		end;
	end;
end;





procedure TKSRoomView.fSetRoom(iRoom: TKSRoom);
begin
	fRoom := iRoom;
	if (fRoom.Parent <> fLevel) then
	begin
		fLevel := fRoom.Parent;
		if (Assigned(fLevel)) then
		begin
			fLevel.ChangedListeners.Add(OnLevelChanged);
		end;
	end;
	OnLevelChanged(Self);
end;





procedure TKSRoomView.fSetShowNeighbors(iVal: boolean);
begin
	if (iVal = fShowNeighbors) then
	begin
		Exit;
	end;

	fShowNeighbors := iVal;
	UpdateSize();
end;





function  TKSRoomView.fGetLayVis(iIdx: integer): boolean;
begin
	if ((iIdx < -1) or (iIdx > 8)) then
	begin
		Result := false;
		Exit;
	end;
	if (iIdx = -1) then
	begin
		Result := fBkgVis;
		Exit;
	end;
	
	Result := fLayVis[iIdx];
end;





procedure TKSRoomView.fSetLayVis(iIdx: integer; iVal: boolean);
begin
	if ((iIdx < -1) or (iIdx > 8)) then
	begin
		Exit;
	end;
	if (iIdx = -1) then
	begin
		fSetBkgVis(iVal);
		Exit;
	end;
	
	if (fLayVis[iIdx] = iVal) then
	begin
		Exit;
	end;
	fLayVis[iIdx] := iVal;
	fShouldRecompose := true;
	Invalidate();
end;





procedure TKSRoomView.fSetBkgVis(iVal: boolean);
begin
	if (fBkgVis = iVal) then
	begin
		Exit;
	end;
	fBkgVis := iVal;
	fShouldRecompose := true;
	Invalidate();
end;





procedure TKSRoomView.UpdateSize();
var
	i: integer;
	ArrSize: integer;
begin
	if (fShowNeighbors) then
	begin
		fLayWid := 24 * 27 + 2;
		fLayHei := 24 * 12 + 2;
	end
	else
	begin
		fLayWid := 24 * 25;
		fLayHei := 24 * 10;
	end;

	// set array lengths for fBkgImg, fLayImg[] and fRoomImg:
	ArrSize := fLayWid * fLayHei;
	for i := 0 to 8 do
	begin
		SetLength(fLayImg[i], ArrSize);
	end;
	SetLength(fBkgImg, ArrSize);
	SetLength(fRoomImg, ArrSize);

	Self.Constraints.MaxHeight := fLayHei;
	Self.Constraints.MinHeight := fLayHei;
	Self.Constraints.MaxWidth := fLayWid;
	Self.Constraints.MinWidth := fLayWid;
	Self.Width := fLayWid;
	Self.Height := fLayHei;

	fShouldRedrawLay := true;
	Invalidate();
end;





procedure TKSRoomView.Paint();
var
	ptr: pointer;
	bmp: TBitmap;
	i: integer;
begin
	if (fShouldRedrawLay) then
	begin
		RedrawImgs();
	end;
	if (fShouldCheckLay) then
	begin
		CheckRoomContents();
	end;
	if (fShouldRecompose) then
	begin
		RecomposeRoomImg();
	end;

	ptr := @(fRoomImg[0]);
	// for unknown reasons SetDIBitsToDevice sometimes fails with ptr, so better use a per-scanline-set
	bmp := TBitmap.Create();
	try
		bmp.Width := fLayWid;
		bmp.Height := fLayHei;
		bmp.PixelFormat := pf32Bit;
		for i := 0 to fLayHei - 1 do
		begin
			Move(ptr^, (bmp.ScanLine[i])^, fLayWid * 4);
			integer(ptr) := integer(ptr) + fLayWid * 4;
		end;

		bmp.Canvas.Brush.Color := $ffffff;
		bmp.Canvas.Brush.Style := bsSolid;
		bmp.Canvas.FillRect(Rect(0, 0, 24, 24));
		bmp.Canvas.FillRect(Rect(626, 0, 650, 24));
		bmp.Canvas.FillRect(Rect(0, 266, 24, 290));
		bmp.Canvas.FillRect(Rect(626, 266, 650, 290));

		// draw missing rooms:
		bmp.Canvas.Pen.Color := 0;
		bmp.Canvas.Pen.Style := psSolid;
		bmp.Canvas.Brush.Color := 0;
		if not(Assigned(fRoom)) then
		begin
			CrossOutBox(bmp.Canvas, 0, 0, 289, 649);
		end
		else
		begin
			if not(Assigned(fRoom.RoomUp))    then CrossOutBox(bmp.Canvas,   0,  24,  24, 625);
			if not(Assigned(fRoom.RoomLeft))  then CrossOutBox(bmp.Canvas,  24,   0, 265,  24);
			if not(Assigned(fRoom.RoomRight)) then CrossOutBox(bmp.Canvas,  24, 625, 265, 649);
			if not(Assigned(fRoom.RoomDown))  then CrossOutBox(bmp.Canvas, 265,  24, 289, 624);
		end;

		// draw vectors:
		for i := 0 to fNumVectors - 1 do
		begin
			if (
				fVector[i].Visible and
				((fVector[i].Room = fRoom) or not(Assigned(fVector[i].Room)))
			) then
			begin
				fVector[i].Draw(bmp.Canvas);
			end;
		end;

		Canvas.Draw(0, 0, bmp);
	finally
		bmp.Free();
	end;
end;





procedure TKSRoomView.OnLevelChanged(Sender: TObject);
begin
	fShouldCheckLay := true;
	Invalidate();
end;





procedure TKSRoomView.CheckRoomContents();
begin
	// TODO: check layer by layer, update layers that don't match, recompose if needed
	RedrawImgs();		// until re-done, skip check and force an update every time

	fRoomBackup := fRoom.Data;

	fShouldCheckLay := false;
end;






procedure TKSRoomView.RedrawImgs();		// updates fBkgImg, fLayImg and fRoomImg
var
	i: integer;
begin
	if (not(Assigned(fRoom)) or not(Assigned(fLevel))) then
	begin
		for i := 0 to fLayWid * fLayHei - 1 do
		begin
			fRoomImg[i] := fBkgColor;
		end;
		fShouldCheckLay := false;
		fShouldRedrawLay := false;
		Exit;
	end;

	RedrawBkgImg();
	for i := 0 to 3 do
	begin
		RedrawLayImgTile(i);
	end;
	for i := 4 to 7 do
	begin
		RedrawLayImgObj(i);
	end;
	RedrawLayImgPass();
	RecomposeRoomImg();

	fRoomBackup := fRoom.Data;
	
	fShouldCheckLay := false;
	fShouldRedrawLay := false;
end;





procedure TKSRoomView.RedrawBkgImg();
begin
	if (fShowNeighbors) then
	begin
		RedrawBkgImgR(25, 25, 265, 625, 25, 25, fRoom);
		if (Assigned(fRoom)) then
		begin
			RedrawBkgImgR( 25,   0, 265,  24, -576,   25, fRoom.RoomLeft);
			RedrawBkgImgR(  0,  25,  24, 625,   25, -216, fRoom.RoomUp);
			RedrawBkgImgR( 25, 626, 265, 649,  626,   25, fRoom.RoomRight);
			RedrawBkgImgR(266,  25, 290, 625,   25,  266, fRoom.RoomDown);
		end;
	end
	else
	begin
		RedrawBkgImgR(0, 0, 240, 600, 0, 0, fRoom);
	end;
end;





procedure TKSRoomView.RedrawBkgImgR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom);
var
	bkg: TPNGObject;
	x, y: integer;
	ypos, ymod: integer;
begin
	if (not(Assigned(iRoom))) then
	begin
		Exit;
	end;

	bkg := fLevel.Background[iRoom.Data.Background];
	if (Assigned(bkg) and bkg.Empty) then
	begin
		fLevel.LoadBackground(iRoom.Data.Background);
	end;
	
	if (Assigned(bkg) and not(bkg.Empty)) then
	begin
		for y := t to b - 1 do
		begin
			ypos := fLayWid * y;
			ymod := (y - ypofs) mod bkg.Height;
			for x := l to r - 1 do
			begin
				fBkgImg[x + ypos] := bkg.Pixels[(x - xpofs) mod bkg.Width, ymod];
			end;		// for y
		end;		// for x
	end
	else
	begin
		// line-by-line fill
		for y := t to b - 1 do
		begin
			ypos := fLayWid * y + l;
			FillChar(fBkgImg[ypos], (r - l) * 4, 0);
		end;
	end;
end;





procedure TKSRoomView.RedrawLayImgTile(iLayer: integer);		// updates fLayImg for tile layers
begin
	// FillChar(fLayImg[iLayer][0], fLayWid * fLayHei * 4, $ff);
	if (fShowNeighbors) then
	begin
		RedrawLayImgTileR(25, 25, 265, 625, 25, 25, fRoom, iLayer);
		if Assigned(fRoom) then
		begin
			RedrawLayImgTileR( 25,   0, 265,  24, -576,   25, fRoom.RoomLeft,  iLayer);
			RedrawLayImgTileR(  0,  25,  24, 625,   25, -216, fRoom.RoomUp,    iLayer);
			RedrawLayImgTileR( 25, 626, 265, 649,  626,   25, fRoom.RoomRight, iLayer);
			RedrawLayImgTileR(266,  25, 290, 625,   25,  266, fRoom.RoomDown,  iLayer);
		end;
	end
	else
	begin
		RedrawLayImgTileR(0, 0, 240, 600, 0, 0, fRoom, iLayer);
	end;
end;





procedure TKSRoomView.RedrawLayImgTileR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom; iLayer: integer);
var
	RoomX, RoomY: integer;
	x, y: integer;		// coords in the tile
	tx, ty: integer;		// tile's coords in the tileset img
	pngal: pngimage.PByteArray;
	ts: TKSTileset;
	ypos: integer;
	rx24, ry24: integer;
	dstx, dsty: integer;
begin
	if not(Assigned(iRoom)) then
	begin
		Exit;
	end;

	if not(Assigned(iRoom.Tileset[0].Img)) then
	begin
		fLevel.LoadTileset(iRoom.Tileset[0].Number);
	end;
	if not(Assigned(iRoom.Tileset[1].Img)) then
	begin
		fLevel.LoadTileset(iRoom.Tileset[1].Number);
	end;

	for RoomY := (t - ypofs) div 24 to (b - ypofs + 23) div 24 do
	begin
		if (RoomY < 0) or (RoomY >= 10) then
		begin
			continue;
		end;
		ry24 := RoomY * 24;
		for RoomX := (l - xpofs) div 24 to (r - xpofs + 23) div 24 do
		begin
			if (RoomX < 0) or (RoomX >= 25) then
			begin
				continue;
			end;
			ts := iRoom.Tileset[iRoom.Data.Tile[iLayer].Tile[RoomY, RoomX] shr 7];
			if not(Assigned(ts.Img)) then
			begin
				continue;
			end;
			tx := ((iRoom.Data.Tile[iLayer].Tile[RoomY, RoomX] and $7f) mod 16) * 24;
			ty := ((iRoom.Data.Tile[iLayer].Tile[RoomY, RoomX] and $7f) div 16) * 24;
			rx24 := RoomX * 24;
			for y := 0 to 23 do
			begin
				dsty := ry24 + y + ypofs;
				if ((dsty < 0) or (dsty >= fLayHei)) then
				begin
					continue;
				end;
				ypos := fLayWid * dsty;
				pngal := ts.Img.AlphaScanline[ty + y];
				for x := 0 to 23 do
				begin
					dstx := rx24 + x + xpofs;
					if ((dstx < 0) or (dstx >= fLayWid)) then
					begin
						continue;
					end;
					fLayImg[iLayer][dstx + ypos] := (pngal[tx + x] shl 24) or (ts.Img.Pixels[tx + x, ty + y] and $ffffff);
				end;		// for y
			end;		// for x
		end;		// for RoomX
	end;		// for RoomY
end;





procedure TKSRoomView.RedrawLayImgObj (iLayer: integer);		// updates fLayImg for obj layers
var
	i: integer;
begin
	// set all to transparent:
	for i := 0 to fLayWid * fLayHei - 1 do
	begin
		fLayImg[iLayer][i] := 0;
	end;

	// draw objects:
	if (fShowNeighbors) then
	begin
		RedrawLayImgObjR(25, 25, 265, 625, 25, 25, fRoom, iLayer);
		if Assigned(fRoom) then
		begin
			RedrawLayImgObjR( 25,   0, 265,  24, -576,   25, fRoom.RoomLeft,  iLayer);
			RedrawLayImgObjR(  0,  25,  24, 625,   25, -216, fRoom.RoomUp,    iLayer);
			RedrawLayImgObjR( 25, 626, 265, 649,  626,   25, fRoom.RoomRight, iLayer);
			RedrawLayImgObjR(266,  25, 290, 625,   25,  266, fRoom.RoomDown,  iLayer);
		end;
	end
	else
	begin
		RedrawLayImgObjR(0, 0, 240, 600, 0, 0, fRoom, iLayer);
	end;
end;





procedure TKSRoomView.RedrawLayImgObjR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom; iLayer: integer);
var
	Obj: TKSObject;
	x, y: integer;
	ypos, wid: integer;
	RoomX, RoomY: integer;
	pngal: pngimage.PByteArray;
	rx24: integer;
	dstx, dsty: integer;
begin
	if not(Assigned(iRoom)) then
	begin
		// already set to transparent in parent
		Exit;
	end;

	for RoomY := (t - ypofs) div 24 to (b - ypofs) div 24 do
	begin
		if ((RoomY < 0) or (RoomY >= 10)) then
		begin
			continue;
		end;
		for RoomX := (l - xpofs) div 24 to (r - xpofs) div 24 do
		begin
			if (RoomX < 0) or (RoomX >= 25) then
			begin
				continue;
			end;
			if (iRoom.Data.Obj[iLayer].Obj[RoomY, RoomX] = 0) then
			begin
				continue;
			end;
			obj := gKSObjects.GetObject(iRoom.Data.Obj[iLayer].Bank[RoomY, RoomX], iRoom.Data.Obj[iLayer].Obj[RoomY, RoomX]);
			if not(Assigned(obj)) then
			begin
				continue;
			end;
			obj.NeedImage();
			if not(Assigned(obj.Img)) then
			begin
				continue;
			end;
			wid := obj.Img.Width - 1;
			rx24 := RoomX * 24;
			for y := 0 to obj.Img.Height - 1 do
			begin
				dsty := RoomY * 24 + y + ypofs;
				if ((dsty < 0) or (dsty >= fLayHei)) then
				begin
					continue;
				end;
				ypos := fLayWid * dsty;
				pngal := obj.Img.AlphaScanline[y];
				for x := 0 to wid do
				begin
					dstx := rx24 + x + xpofs;
					if ((dstx < 0) or (dstx >= fLayWid)) then
					begin
						continue;
					end;
					fLayImg[iLayer][dstx + ypos] := (pngal[x] shl 24) or (obj.Img.Pixels[x, y] and $ffffff);
				end;		// for y
			end;		// for x
		end;		// for RoomX
	end;		// for RoomY
end;





procedure TKSRoomView.RedrawLayImgPass();		// updates fLayImg[8] for passability
begin
	if (fShowNeighbors) then
	begin
		RedrawLayImgPassR(25, 25, 265, 625, 25, 25, fRoom);
		if Assigned(fRoom) then
		begin
			RedrawLayImgPassR( 25,   0, 265,  24, -576,   25, fRoom.RoomLeft);
			RedrawLayImgPassR(  0,  25,  24, 625,   25, -216, fRoom.RoomUp);
			RedrawLayImgPassR( 25, 626, 265, 649,  626,   25, fRoom.RoomRight);
			RedrawLayImgPassR(266,  25, 290, 625,   25,  266, fRoom.RoomDown);
		end;
	end
	else
	begin
		RedrawLayImgPassR(0, 0, 240, 600, 0, 0, fRoom);
	end;
end;





procedure TKSRoomView.RedrawLayImgPassR(t, l, b, r, xpofs, ypofs: integer; iRoom: TKSRoom);
var
	x, y: integer;
	ypos: integer;
	v: integer;
	upos, vpos: integer;
begin
	if not(Assigned(iRoom)) then
	begin
		for y := t to b - 1 do
		begin
			ypos := y * fLayWid;
			for x := l to r - 1 do
			begin
				fLayImg[8][x + ypos] := 0;
			end;		// for x
		end;		// for y
		Exit;
	end;
	if not(Assigned(iRoom.Passable)) then
	begin
		iRoom.UpdatePassable();
	end;

	for y := t to b - 1 do
	begin
		ypos := y * fLayWid;
		upos := y - ypofs;
		for x := l to r - 1 do
		begin
			vpos := (x - xpofs);
			case iRoom.Passable[vpos, upos] of
				KSPASS_IMPASSABLE: v := $7f0000ff;		// red
				KSPASS_INVWALL:    v := $7f00cfff;		// yellow?
				KSPASS_INVHOLE:    v := $7f00ff00;		// green
				else               v := 0;
			end;
			fLayImg[8][x + ypos] := v;
		end;
	end;		// for y
end;





procedure TKSRoomView.RecomposeRoomImg();		// recomposes fRoomImg from fBkgImg and fLayImg[]s
const
	LayerSeq: array[0..8] of integer = (0, 1, 2, 4, 5, 3, 6, 7, 8);
var
	ArrSize: integer;
	i, j: integer;
	Alpha: integer;
	LayerNum: integer;
begin
	ArrSize := fLayWid * fLayHei;

	if (not(Assigned(fRoom)) or not(Assigned(fLevel))) then
	begin
		for i := 0 to ArrSize - 1 do
		begin
			fRoomImg[i] := fBkgColor;
		end;
		fShouldRecompose := false;
		Exit;
	end;

	if (fBkgVis) then
	begin
		Move(fBkgImg[0], fRoomImg[0], ArrSize * 4);
	end
	else
	begin
		for j := 0 to ArrSize - 1 do
		begin
			fRoomImg[j] := fBkgColor;
		end;
	end;

	for i := 0 to 8 do
	begin
		LayerNum := LayerSeq[i];
		if not(fLayVis[LayerNum]) then
		begin
			continue;
		end;
		for j := 0 to ArrSize - 1 do
		begin
			Alpha := fLayImg[LayerNum][j] shr 24;
			case Alpha of
				0: ;		// nothing needed, full transparency
				255: fRoomImg[j] := fLayImg[LayerNum][j];		// full opacity
				else fRoomImg[j] := MixColor(fRoomImg[j], fLayImg[LayerNum][j], Alpha);
			end;
		end;
	end;

	for i := 0 to ArrSize - 1 do
	begin
		j := fRoomImg[i];
		fRoomImg[i] := ((j and $ff0000) shr 16) or ((j and $ff) shl 16) or (j and $ff00);
	end;

	fShouldRecompose := false;
end;





procedure TKSRoomView.CrossOutBox(iCanvas: TCanvas; t, l, b, r: integer);
begin
	iCanvas.Brush.Color := $ffffff;
	iCanvas.FillRect(Rect(l, t, r + 1, b + 1));
	iCanvas.Brush.Color := 0;
	iCanvas.FrameRect(Rect(l, t, r + 1, b + 1));
	iCanvas.MoveTo(l, t);
	iCanvas.LineTo(r, b);
	iCanvas.MoveTo(l, b);
	iCanvas.LineTo(r, t);
end;





function  TKSRoomView.CanvasToLogical(iCanvasPt: TPoint; var oTileCoords: TPoint; var oPixelCoords: TPoint; var oRoom: TKSRoom): boolean;
begin
	// Invaiants:
	//  Result = true when (oRoom = fRoom)
	//  When oRoom <> nil then oTileCoords and oPixelCoords are valid
	
	Result := false;
	if ((iCanvasPt.X < 0) or (iCanvasPt.X >= fLayWid) or (iCanvasPt.Y < 0) or (iCanvasPt.Y >= fLayHei) or not(Assigned(fRoom))) then
	begin
		oRoom := nil;
		Exit;
	end;

	if not(fShowNeighbors) then
	begin
		oRoom := fRoom;
		oTileCoords.X := iCanvasPt.X div 24;
		oPixelCoords.X := iCanvasPt.X mod 24;
		oTileCoords.Y := iCanvasPt.Y div 24;
		oPixelCoords.Y := iCanvasPt.Y mod 24;
		Result := true;
		Exit;
	end;

	if (iCanvasPt.X <= 24) then
	begin
		// left strip:
		if (iCanvasPt.Y <= 25) then
		begin
			oRoom := nil;
		end
		else if (iCanvasPt.Y <= 264) then
		begin
			oRoom := fRoom.RoomLeft;
			oTileCoords.X  := 24;
			oTileCoords.Y  := (iCanvasPt.Y - 25) div 24;
			oPixelCoords.X := iCanvasPt.X;
			oPixelCoords.Y := (iCanvasPt.Y - 25) mod 24;
		end
		else
		begin
			oRoom := nil;
		end;
		Exit;
	end
	else if (iCanvasPt.X = 25) then
	begin
		// left divider
		oRoom := nil;
		Exit;
	end
	else if (iCanvasPt.X <= 624) then
	begin
		// middle horz strip
		if (iCanvasPt.Y < 25) then
		begin
			oRoom := fRoom.RoomUp;
			oTileCoords.X  := (iCanvasPt.X - 25) div 24;
			oPixelCoords.X := (iCanvasPt.X - 25) mod 24;
			oTileCoords.Y  := 9;
			oPixelCoords.Y := iCanvasPt.Y;
		end
		else if (iCanvasPt.Y = 25) then
		begin
			oRoom := nil;
		end
		else if (iCanvasPt.Y <= 264) then
		begin
			oRoom := fRoom;
			oTileCoords.X  := (iCanvasPt.X - 25) div 24;
			oPixelCoords.X := (iCanvasPt.X - 25) div 24;
			oTileCoords.Y  := (iCanvasPt.Y - 25) div 24;
			oPixelCoords.Y := (iCanvasPt.Y - 25) mod 24;
			Result := true;
		end
		else if (iCanvasPt.Y = 265) then
		begin
			oRoom := nil;
		end
		else
		begin
			oRoom := fRoom.RoomDown;
			oTileCoords.X  := (iCanvasPt.X - 25) div 24;
			oPixelCoords.X := (iCanvasPt.X - 25) mod 24;
			oTileCoords.Y  := 0;
			oPixelCoords.Y := iCanvasPt.Y - 266;
		end;
		Exit;
	end
	else if (iCanvasPt.X = 625) then
	begin
		// right divider
		oRoom := nil;
		Exit;
	end
	else
	begin
		// right strip
		if (iCanvasPt.Y <= 25) then
		begin
			oRoom := nil;
		end
		else if (iCanvasPt.Y <= 264) then
		begin
			oRoom := fRoom.RoomRight;
			oTileCoords.X  := 0;
			oTileCoords.Y  := (iCanvasPt.Y - 626) div 24;
			oPixelCoords.X := iCanvasPt.X;
			oPixelCoords.Y := (iCanvasPt.Y - 626) mod 24;
		end
		else
		begin
			oRoom := nil;
		end;
		Exit;
	end
end;





function  TKSRoomView.LogicalToCanvas(iTileCoords, iPixelCoords: TPoint; var oCanvasPt: TPoint): boolean;
begin
	// returns true if oCanvasPt is inside fRoom
	oCanvasPt.X := iTileCoords.X * 24 + iPixelCoords.X;
	oCanvasPt.Y := iTileCoords.Y * 24 + iPixelCoords.Y;
	Result := (
		(oCanvasPt.X >= 0) and
		(oCanvasPt.X < 600) and
		(oCanvasPt.Y >= 0) and
		(oCanvasPt.Y < 239)
	);
	if (fShowNeighbors) then
	begin
		oCanvasPt.X := oCanvasPt.X + 25;
		oCanvasPt.Y := oCanvasPt.Y + 25;
	end;
end;





procedure TKSRoomView.SetCoords(iXPos, iYPos: integer);
begin
	fXPos := iXPos;
	fYPos := iYPos;
end;





procedure TKSRoomView.InvalidateTileset();
begin
	fShouldRedrawLay := true;
	Invalidate();
end;





procedure TKSRoomView.OnVectorChanged(Sender: TObject);
begin
	Invalidate();
end;





procedure TKSRoomView.OnVectorDestroying(Sender: TObject);
var
	i: integer;
begin
	for i := fNumVectors - 1 downto 0 do
	begin
		if (fVector[i] = Sender) then
		begin
			fNumVectors := fNumVectors - 1;
			fVector[i] := fVector[fNumVectors];
			// do not break, continue to search for ALL instances
		end;
	end;		// for i - fVector[]

	Invalidate();
end;





procedure TKSRoomView.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	if (Assigned(fOnMouseEnter)) then
	begin
		fOnMouseEnter(Self);
	end;
end;





procedure TKSRoomView.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if (Assigned(fOnMouseLeave)) then
	begin
		fOnMouseLeave(Self);
	end;
end;





end.

