unit uKSTilesetView;

{$MODE Delphi}

interface

uses
	Windows,
	Messages,
	SysUtils,
	Classes,
	Controls,
	Graphics,
	uKSRepresentations,
	uVectors,
	uKSRender,
	IntfGraphics;





type
	TKSTilesetView = class(TGraphicControl)
	protected
		fTileset: TKSTileset;
		fNumVectors, fCapVectors: integer;
		fVector: array of TVGObject;
		fBgColor1: TColor;
		fBgColor2: TColor;

		fShouldRedrawTileset: boolean;

		fOnMouseLeave: TNotifyEvent;
		fOnMouseEnter: TNotifyEvent;

		bmpBackground: TBitmap;
		intfTileset: TKSIntfImage;
		bmpTileset: TBitmap;
		intfBackground: TKSIntfImage;

		procedure fSetTileset(iTileset: TKSTileset);
		procedure fSetBgColor1(iVal: TColor);
		procedure fSetBgColor2(iVal: TColor);

		procedure RedrawTileset();
		procedure UpdateBackground();

		procedure Paint(); override;

		procedure OnVectorChanged(Sender: TObject);
		procedure OnVectorDestroying(Sender: TObject);

		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

		property Width;
		property Height;
		
	public

		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;
		procedure Clear();

		procedure RegVector(iVector: TVGObject);
		procedure RegUniqueVector(iVector: TVGObject);
		procedure RemVector(iVector: TVGObject);

		procedure InvalidateTileset();

		property Tileset: TKSTileset read fTileset write fSetTileset;

	published

		property BgColor1: TColor read fBgColor1 write fSetBgColor1;
		property BgColor2: TColor read fBgColor2 write fSetBgColor2;

		property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
		property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;

		property Align;
		property Anchors;
		property OnMouseDown;
		property OnMouseUp;
		property OnMouseMove;
	end;





procedure Register();


























implementation





procedure Register();
begin
  RegisterComponents('KSLC', [TKSTilesetView]);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TKSTilesetView:

constructor TKSTilesetView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	fTileset := nil;
	fNumVectors := 0;
	fCapVectors := 0;
	Width := 384;
	Height := 192;
	Constraints.MinHeight := Height;
	Constraints.MaxHeight := Height;
	Constraints.MinWidth := Width;
	Constraints.MaxWidth := Width;

	// offscreen tileset buffer:
	bmpTileset := TBitmap.Create();
	bmpTileset.PixelFormat := pf32Bit;
	bmpTileset.Width := Width;
	bmpTileset.Height := Height;

	intfBackground:=TKSIntfImage.Create(Width,Height);

	// background buffer:
	bmpBackground := TBitmap.Create();
	bmpBackground.PixelFormat := pf32Bit;
	bmpBackground.Width := Width;
	bmpBackground.Height := Height;

	intfTileset:=nil;

	fBgColor1 := $1A4653;
	BgColor2 := $174A59;
end;





destructor TKSTilesetView.Destroy();
begin
	Clear();
	intfBackground.Free();
	inherited Destroy();
end;





procedure TKSTilesetView.Clear();
begin
	fNumVectors := 0;
	fCapVectors := 0;
	intfTileset.Clear();
	SetLength(fVector, 0);
end;





procedure TKSTilesetView.fSetTileset(iTileset: TKSTileset);
begin
	if (fTileset = iTileset) then
	begin
		Exit;
	end;
	fTileset := iTileset;
	fShouldRedrawTileset := true;
	Invalidate();
end;





procedure TKSTilesetView.RedrawTileset();
begin
	if (Assigned(fTileset) and Assigned(fTileset.Img)) then
	begin
		intfBackground.Clear();
		intfBackground.Obj.CopyPixels(bmpBackground.CreateIntfImage,0,0);
		WriteLayer(intfBackground.Obj,fTileset.ImgLaz,TPoint.Create(0,0));
		bmpTileset.LoadFromIntfImage(intfBackground.Obj);
	end;
	fShouldRedrawTileset := false;
end;





procedure TKSTilesetView.fSetBgColor1(iVal: TColor);
begin
	if (fBgColor1 = iVal) then
	begin
		Exit;
	end;
	fBgColor1 := iVal;
	UpdateBackground();
end;





procedure TKSTilesetView.fSetBgColor2(iVal: TColor);
begin
	if (fBgColor2 = iVal) then
	begin
		Exit;
	end;
	fBgColor2 := iVal;
	UpdateBackground();
end;





procedure TKSTilesetView.UpdateBackground();
var
	scan: PIntegerArray;
	c1, c2, tmp: integer;
	x, y: integer;
begin
	c1 := ColorToRgb(fBgColor1);
	c2 := ColorToRgb(fBgColor2);
	for y := 0 to Height - 1 do
	begin
		scan := bmpBackground.ScanLine[y];
		for x := 0 to (Width div 12) - 1 do
		begin
			scan[12 * x] := c1;
			scan[12 * x + 1] := c1;
			scan[12 * x + 2] := c1;
			scan[12 * x + 3] := c1;
			scan[12 * x + 4] := c1;
			scan[12 * x + 5] := c1;
			scan[12 * x + 6] := c2;
			scan[12 * x + 7] := c2;
			scan[12 * x + 8] := c2;
			scan[12 * x + 9] := c2;
			scan[12 * x + 10] := c2;
			scan[12 * x + 11] := c2;
		end;
		if (y mod 6 = 5) then
		begin
			tmp := c1;
			c1 := c2;
			c2 := tmp;
		end;
	end;		// for y - ScanLine[]

	(*
	if (Assigned(fTileset) and Assigned(fTileset.Img)) then
	begin
		bmpTileset.Canvas.Draw(0, 0, bmpBackground);
		bmpTileset.Canvas.Draw(0, 0, fTileset.Img);
	end;
	*)

	Invalidate();
end;





procedure TKSTilesetView.Paint();
var
	bmp: TBitmap;
	i: integer;
begin
	if (csDesigning in ComponentState) then
	begin
		Canvas.Draw(0, 0, bmpBackground);
		Canvas.Brush.Color := 0;
		Canvas.FrameRect(Rect(0, 0, Width, Height));
		Exit;
	end;
	OutputDebugString(PChar('TKSTilesetView.Paint(): fTileset = 0x' + IntToHex(integer(fTileset), 8)));
	if (not(Assigned(fTileset)) or not(Assigned(fTileset.Img))) then
	begin
		Canvas.Brush.Color := Color;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		Exit;
	end;

	if (fShouldRedrawTileset) then
	begin
		RedrawTileset();
	end;

	bmp := TBitmap.Create();
	try
		bmp.PixelFormat := pf32Bit;
		bmp.Width := Width;
		bmp.Height := Height;
		bmp.Canvas.Draw(0, 0, bmpTileset);

		for i := 0 to fNumVectors - 1 do
		begin
			fVector[i].Draw(bmp.Canvas);
		end;

		Canvas.Draw(0, 0, bmp);
	finally
		bmp.Free();
	end;
end;





procedure TKSTilesetView.OnVectorChanged(Sender: TObject);
begin
	Invalidate();
end;





procedure TKSTilesetView.OnVectorDestroying(Sender: TObject);
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





procedure TKSTilesetView.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	if (Assigned(fOnMouseEnter)) then
	begin
		fOnMouseEnter(Self);
	end;
end;





procedure TKSTilesetView.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if (Assigned(fOnMouseLeave)) then
	begin
		fOnMouseLeave(Self);
	end;
end;





procedure TKSTilesetView.RegVector(iVector: TVGObject);
var
	cap: integer;
begin
	if (fNumVectors >= fCapVectors) then
	begin
		cap := fNumVectors + fNumVectors div 4 + 4;		// 4, 9, 15, 22, ...
		SetLength(fVector, cap);
		fCapVectors := cap;
	end;
	fVector[fNumVectors] := iVector;

	iVector.OnChanged.Add(OnVectorChanged);
	iVector.OnDestroying.Add(OnVectorDestroying);

	fNumVectors := fNumVectors + 1;
	Invalidate();
end;






procedure TKSTilesetView.RegUniqueVector(iVector: TVGObject);
var
	cap: integer;
	i: integer;
begin
	for i := 0 to fNumVectors - 1 do
	begin
		if (fVector[i] = iVector) then
		begin
			Exit;
		end;
	end;

	if (fNumVectors >= fCapVectors) then
	begin
		cap := fNumVectors + fNumVectors div 4 + 4;		// 4, 9, 15, 22, ...
		SetLength(fVector, cap);
		fCapVectors := cap;
	end;
	fVector[fNumVectors] := iVector;

	iVector.OnChanged.Add(OnVectorChanged);
	iVector.OnDestroying.Add(OnVectorDestroying);

	fNumVectors := fNumVectors + 1;
	Invalidate();
end;






procedure TKSTilesetView.RemVector(iVector: TVGObject);
var
	i: integer;
begin
	for i := 0 to fNumVectors - 1 do
	begin
		if (fVector[i] = iVector) then
		begin
			fNumVectors := fNumVectors - 1;
			fVector[i] := fVector[fNumVectors];

			// TODO: remove Vector registration
			
			Exit;
		end;
	end;
end;





procedure TKSTilesetView.InvalidateTileset();
begin
	fShouldRedrawTileset := true;
	Invalidate();
end;





end.
