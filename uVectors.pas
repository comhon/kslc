
unit uVectors;

{$MODE Delphi}

interface

uses
	Classes,
	Graphics,
	uMultiEvent;




type
	TVGObject = class
	public
		Visible: boolean;
		Color: TColor;
		Data: pointer;
		Room: pointer;		// TKSRoom for which the vector should be shown (if appropriate)

		OnChanged: TMultiEvent;
		OnDestroying: TMultiEvent;

		constructor Create();
		destructor Destroy(); override;

		procedure Draw(Canvas: TCanvas); virtual; abstract;

		procedure Changed();
	end;





	TVGLine = class(TVGObject)
	public
		X1, Y1, X2, Y2: integer;
		Width: integer;
		constructor Create(iX1, iY1, iX2, iY2: integer; iColor: TColor; iWidth: integer = 0);
		procedure Draw(Canvas: TCanvas); override;
	end;





	TVGCircle = class(TVGObject)
	public
		X, Y, R: integer;
		constructor Create(iX, iY, iR: integer; iColor: TColor);
		procedure Draw(Canvas: TCanvas); override;
	end;





	TVGRect = class(TVGObject)
	public
		Top, Left, Bottom, Right: integer;

		constructor Create(iTop, iLeft, iBottom, iRight: integer; iColor: TColor);
		procedure Draw(Canvas: TCanvas); override;
	end;























implementation






constructor TVGObject.Create();
begin
	inherited Create();
	OnChanged := TMultiEvent.Create();
	OnDestroying := TMultiEvent.Create();
	Visible := true;
end;





destructor TVGObject.Destroy();
begin
	OnDestroying.Trigger(Self);
	OnChanged.Free();
	OnDestroying.Free();
	inherited Destroy();
end;





procedure TVGObject.Changed();
begin
	OnChanged.Trigger(Self);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TVGLine:

constructor TVGLine.Create(iX1, iY1, iX2, iY2: integer; iColor: TColor; iWidth: integer = 0);
begin
	inherited Create();
	X1 := iX1;
	Y1 := iY1;
	X2 := iX2;
	Y2 := iY2;
	Color := iColor;
	Width := iWidth;
end;




procedure TVGLine.Draw(Canvas: TCanvas);
begin
	if not(Visible) then Exit;
	Canvas.Pen.Color := Color;
	Canvas.Pen.Style := psSolid;
	Canvas.Pen.Width := Width;
	Canvas.MoveTo(X1, Y1);
	Canvas.LineTo(X2, Y2);
end;





constructor TVGCircle.Create(iX, iY, iR: integer; iColor: TColor);
begin
	inherited Create();
	X := iX;
	Y := iY;
	R := iR;
	Color := iColor;
end;





procedure TVGCircle.Draw(Canvas: TCanvas);
begin
	if not(Visible) then Exit;
	Canvas.Brush.Style := bsClear;
	Canvas.Pen.Color := Color;
	Canvas.Ellipse(X - R, Y - R, X + R + 1, Y + R + 1);
end;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TVGRect:

constructor TVGRect.Create(iTop, iLeft, iBottom, iRight: integer; iColor: TColor);
begin
	inherited Create();
	Top := iTop;
	Left := iLeft;
	Bottom := iBottom;
	Right := iRight;
	Color := iColor;
end;





procedure TVGRect.Draw(Canvas: TCanvas);
begin
	if not(Visible) then Exit;
	Canvas.Brush.Color := Color;
	Canvas.FrameRect(Rect(Left, Top, RIght, Bottom));
end;





end.
